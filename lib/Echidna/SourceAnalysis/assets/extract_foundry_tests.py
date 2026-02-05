#!/usr/bin/env python3
"""
Extract transaction sequences from Foundry test functions using Slither.
Outputs JSON compatible with Echidna corpus format.

Usage: python3 extract_foundry_tests.py <contract.sol> [--crytic-args ARGS]

Output format:
{
  "sequences": [
    {
      "source": "TestContract.test_example",
      "transactions": [
        {
          "call": {"tag": "SolCall", "contents": ["functionName", []]},
          "src": "0x10000",
          "dst": "0x00a329c0648769a73afac7f9381e08fb43dbea72",
          "gas": 12500000,
          "gasprice": "0",
          "value": "0",
          "delay": ["0", "0"]
        }
      ]
    }
  ]
}
"""

import json
import sys
import argparse
import os
import hashlib
from pathlib import Path
from typing import List, Dict, Any, Optional, Tuple

try:
    from slither.slither import Slither
    from slither.core.declarations import Function, Contract
    from slither.core.expressions import (
        CallExpression, Literal, Identifier, MemberAccess,
        AssignmentOperation, TypeConversion
    )
    from slither.core.solidity_types import (
        ElementaryType, UserDefinedType, ArrayType
    )
    from slither.slithir.operations import (
        HighLevelCall, InternalCall, LibraryCall, SolidityCall
    )
    from slither.slithir.variables import Constant
except ImportError:
    print(json.dumps({"error": "Slither not installed. Run: pip install slither-analyzer"}),
          file=sys.stderr)
    sys.exit(1)


# Default addresses used by Echidna
DEFAULT_SENDER = "0x10000"
DEFAULT_CONTRACT_ADDR = "0x00a329c0648769a73afac7f9381e08fb43dbea72"
DEFAULT_GAS = 12500000


# =============================================================================
# Type Conversion Utilities
# =============================================================================

def convert_value_to_abi(value: Any, solidity_type: str) -> Optional[Dict[str, Any]]:
    """Convert a Slither constant value to Echidna AbiValue JSON format."""
    try:
        if solidity_type.startswith("uint"):
            bits = int(solidity_type[4:]) if len(solidity_type) > 4 else 256
            return {"tag": "AbiUInt", "contents": [bits, str(value)]}
        elif solidity_type.startswith("int"):
            bits = int(solidity_type[3:]) if len(solidity_type) > 3 else 256
            return {"tag": "AbiInt", "contents": [bits, str(value)]}
        elif solidity_type == "address":
            addr = str(value)
            if not addr.startswith("0x"):
                addr = hex(int(addr))
            return {"tag": "AbiAddress", "contents": addr}
        elif solidity_type == "bool":
            return {"tag": "AbiBool", "contents": bool(value)}
        elif solidity_type == "string":
            return {"tag": "AbiString", "contents": repr(str(value))}
        elif solidity_type.startswith("bytes"):
            if solidity_type == "bytes":
                return {"tag": "AbiBytesDynamic", "contents": repr(bytes(value))}
            else:
                size = int(solidity_type[5:])
                return {"tag": "AbiBytes", "contents": [size, repr(bytes(value))]}
        else:
            return None
    except (ValueError, TypeError):
        return None


def extract_constant_value(arg, arg_mapping: Optional[Dict] = None) -> Tuple[Optional[Any], Optional[str]]:
    """Extract a constant value and its type from a Slither IR argument."""
    if isinstance(arg, Constant):
        value = arg.value
        if hasattr(arg, 'type') and arg.type:
            type_str = str(arg.type)
        else:
            type_str = infer_type_from_value(value)
        return value, type_str

    # Check arg_mapping for TMP variables and parameters
    if arg_mapping:
        # Try direct lookup
        if arg in arg_mapping:
            return arg_mapping[arg]
        # Try by string name (for TMP variables)
        arg_name = str(arg)
        if arg_name in arg_mapping:
            return arg_mapping[arg_name]

    # Try to get type from the argument for default value generation
    arg_type = getattr(arg, 'type', None)
    if arg_type:
        type_str = str(arg_type)
        # Return a default/placeholder value based on type
        if 'address' in type_str:
            return 0, 'address'
        elif 'uint' in type_str or 'int' in type_str:
            return 0, type_str
        elif 'bool' in type_str:
            return False, 'bool'
        elif 'bytes' in type_str:
            return b'', type_str

    return None, None


def build_tmp_value_map(nodes) -> Dict:
    """Build a mapping from TMP variables and local variables to their values.

    Handles:
    - TypeConversion: TMP_X = CONVERT 0xABCD to address
    - Assignment from constant: x = 123
    - Assignment from tracked variable: sender = TMP_X (propagates value)
    """
    from slither.slithir.operations import TypeConversion, Assignment

    value_map = {}

    for node in nodes:
        try:
            for ir in node.irs:
                # Handle TypeConversion: TMP_X = CONVERT value to type
                if isinstance(ir, TypeConversion):
                    if hasattr(ir, 'lvalue') and hasattr(ir, 'variable'):
                        inner_var = ir.variable
                        # Get the value being converted
                        if isinstance(inner_var, Constant):
                            value = inner_var.value
                        elif hasattr(inner_var, 'value'):
                            value = inner_var.value
                        else:
                            continue

                        type_str = str(ir.type) if hasattr(ir, 'type') and ir.type else None
                        lvalue_name = str(ir.lvalue)
                        value_map[lvalue_name] = (value, type_str)
                        value_map[ir.lvalue] = (value, type_str)

                # Handle Assignment
                elif isinstance(ir, Assignment):
                    if hasattr(ir, 'lvalue') and hasattr(ir, 'rvalue'):
                        rvalue = ir.rvalue
                        lvalue_name = str(ir.lvalue)

                        # Case 1: Direct constant assignment
                        if isinstance(rvalue, Constant):
                            type_str = str(rvalue.type) if hasattr(rvalue, 'type') and rvalue.type else None
                            value_map[lvalue_name] = (rvalue.value, type_str)
                            value_map[ir.lvalue] = (rvalue.value, type_str)
                        else:
                            # Case 2: Assignment from a tracked variable (e.g., sender = TMP_X)
                            rvalue_name = str(rvalue)
                            if rvalue_name in value_map:
                                value_map[lvalue_name] = value_map[rvalue_name]
                                value_map[ir.lvalue] = value_map[rvalue_name]
                            elif rvalue in value_map:
                                value_map[lvalue_name] = value_map[rvalue]
                                value_map[ir.lvalue] = value_map[rvalue]
        except Exception:
            continue

    return value_map


def infer_type_from_value(value: Any) -> str:
    """Infer Solidity type from a Python value."""
    if isinstance(value, bool):
        return "bool"
    elif isinstance(value, int):
        return "uint256"
    elif isinstance(value, str):
        if value.startswith("0x") and len(value) == 42:
            return "address"
        return "string"
    return "uint256"


# =============================================================================
# Contract/Function Analysis Utilities
# =============================================================================

def is_test_function(func: Function) -> bool:
    """Check if function is a Foundry/Echidna test function."""
    name = func.name
    return (name.startswith("test") or
            name.startswith("testFuzz") or
            name.startswith("invariant_") or
            name.startswith("echidna_") or
            name.startswith("prove"))


def get_contract_name_from_destination(ir) -> Optional[str]:
    """Extract the contract type name from a HighLevelCall's destination."""
    if not hasattr(ir, 'destination') or ir.destination is None:
        return None

    dest = ir.destination
    dest_type = getattr(dest, 'type', None)
    if dest_type is None:
        return None

    if isinstance(dest_type, UserDefinedType):
        if hasattr(dest_type, 'type') and hasattr(dest_type.type, 'name'):
            return dest_type.type.name

    type_str = str(dest_type)
    if type_str not in ('address', 'address payable') and not type_str.startswith('uint') and not type_str.startswith('int'):
        return type_str

    return None


def get_contract_function_signatures(contract) -> set:
    """Get the set of function signatures for a contract."""
    signatures = set()
    for func in contract.functions:
        if func.is_constructor or func.is_fallback or func.is_receive:
            continue
        if func.visibility in ('external', 'public'):
            signatures.add(func.full_name)
    return signatures


def get_contract_inheritance_chain(slither: Slither, contract_name: str) -> set:
    """Get all contracts in the inheritance chain of a contract, plus compatible siblings.

    Only includes sibling contracts whose public/external functions are a subset of
    the target contract's functions. This ensures extracted sequences will have
    functions that actually exist on the target.
    """
    result = {contract_name}

    target = None
    for contract in slither.contracts:
        if contract.name == contract_name:
            target = contract
            break

    if target is None:
        return result

    # Get target's function signatures
    target_funcs = get_contract_function_signatures(target)

    # Add parents
    target_parents = set()
    for parent in target.inheritance:
        result.add(parent.name)
        target_parents.add(parent.name)

    # Add compatible siblings (contracts that share a parent AND have compatible interface)
    if target_parents:
        for contract in slither.contracts:
            if contract.name == contract_name:
                continue
            if contract.is_interface or contract.is_library:
                continue

            # Check if contract shares a parent with target
            shares_parent = False
            for parent in contract.inheritance:
                if parent.name in target_parents:
                    shares_parent = True
                    break

            if not shares_parent:
                continue

            # Check interface compatibility: sibling's functions should be subset of target's
            sibling_funcs = get_contract_function_signatures(contract)
            if sibling_funcs and sibling_funcs.issubset(target_funcs):
                result.add(contract.name)

    return result


def is_view_or_pure(func_obj) -> bool:
    """Check if a function is view or pure."""
    if func_obj is None:
        return False
    return getattr(func_obj, 'view', False) or getattr(func_obj, 'pure', False)


# =============================================================================
# Argument Mapping Utilities
# =============================================================================

def build_arg_mapping(ir, called_func, current_mapping: Dict) -> Dict:
    """Build argument mapping for a function call."""
    new_mapping = {}
    if hasattr(ir, 'arguments') and hasattr(called_func, 'parameters'):
        for param, arg in zip(called_func.parameters, ir.arguments):
            val, type_str = extract_constant_value(arg, current_mapping)
            if val is not None:
                new_mapping[param] = (val, type_str)
    return new_mapping


def extract_call_arguments(ir, arg_mapping: Dict) -> List[Dict]:
    """Extract and convert arguments from an IR call.

    IMPORTANT: Always includes all arguments to maintain correct function signatures.
    Uses default values (0 for addresses/uints, false for bools) when actual values
    cannot be extracted.
    """
    args = []
    if hasattr(ir, 'arguments') and ir.arguments:
        for arg in ir.arguments:
            value, type_str = extract_constant_value(arg, arg_mapping)

            # If we couldn't extract a value, try to get the type and use a default
            if value is None or type_str is None:
                arg_type = getattr(arg, 'type', None)
                if arg_type:
                    type_str = str(arg_type)
                    # Provide default values based on type
                    if 'address' in type_str.lower() or 'Currency' in type_str:
                        value = 0
                        type_str = 'address'  # Currency is just a wrapped address
                    elif 'uint' in type_str or 'int' in type_str:
                        value = 0
                    elif 'bool' in type_str:
                        value = False
                    elif 'bytes' in type_str:
                        value = b''
                    else:
                        # Unknown type - skip (will cause signature mismatch but better than crash)
                        continue

            if value is not None and type_str:
                abi_val = convert_value_to_abi(value, type_str)
                if abi_val:
                    args.append(abi_val)
    return args


def extract_call_value(ir, arg_mapping: Dict) -> str:
    """Extract the value (ETH) sent with a call."""
    if hasattr(ir, 'call_value') and ir.call_value:
        val, _ = extract_constant_value(ir.call_value, arg_mapping)
        if val is not None:
            return str(val)
    return "0"


# =============================================================================
# Call Extraction - IR Processing
# =============================================================================

def process_internal_or_library_call(ir, allowed_contracts: Optional[set],
                                      visited: set, arg_mapping: Dict,
                                      include_view: bool) -> List[Dict]:
    """Process an InternalCall or LibraryCall IR operation."""
    called_func = ir.function
    if called_func and hasattr(called_func, 'nodes'):
        new_mapping = build_arg_mapping(ir, called_func, arg_mapping)
        return extract_calls_from_function(
            called_func, allowed_contracts, visited, new_mapping, include_view
        )
    return []


def process_high_level_call_to_target(ir, arg_mapping: Dict,
                                       contract_name: str) -> Optional[Dict]:
    """Process a HighLevelCall to a target contract."""
    func_name = None
    called_func_obj = None

    if hasattr(ir, 'function') and ir.function:
        called_func_obj = ir.function
        func_name = getattr(called_func_obj, 'name', str(called_func_obj))
    elif hasattr(ir, 'function_name'):
        func_name = ir.function_name

    if not func_name:
        return None

    args = extract_call_arguments(ir, arg_mapping)
    value = extract_call_value(ir, arg_mapping)

    return {
        "function": func_name,
        "args": args,
        "value": value,
        "contract": contract_name,
        "_func_obj": called_func_obj  # Used for view/pure check
    }


def process_high_level_call_to_other(ir, allowed_contracts: Optional[set],
                                      visited: set, arg_mapping: Dict,
                                      include_view: bool) -> List[Dict]:
    """Process a HighLevelCall to a non-target contract (follow the call)."""
    called_func_obj = getattr(ir, 'function', None)
    if called_func_obj and hasattr(called_func_obj, 'nodes') and called_func_obj.contract_declarer:
        new_mapping = build_arg_mapping(ir, called_func_obj, arg_mapping)
        return extract_calls_from_function(
            called_func_obj, allowed_contracts, visited, new_mapping, include_view
        )
    return []


def process_ir_operation(ir, allowed_contracts: Optional[set],
                         visited: set, arg_mapping: Dict,
                         include_view: bool) -> List[Dict]:
    """Process a single IR operation and return any extracted calls."""
    calls = []

    if isinstance(ir, (InternalCall, LibraryCall)):
        calls.extend(process_internal_or_library_call(
            ir, allowed_contracts, visited, arg_mapping, include_view
        ))

    elif isinstance(ir, HighLevelCall):
        contract_name = get_contract_name_from_destination(ir)
        is_target = (allowed_contracts is None) or (contract_name in allowed_contracts)

        if is_target:
            call_info = process_high_level_call_to_target(ir, arg_mapping, contract_name)
            if call_info:
                func_obj = call_info.pop('_func_obj', None)
                if include_view or not is_view_or_pure(func_obj):
                    calls.append(call_info)
        else:
            calls.extend(process_high_level_call_to_other(
                ir, allowed_contracts, visited, arg_mapping, include_view
            ))

    return calls


# =============================================================================
# Call Extraction - AST Fallback
# =============================================================================

def extract_calls_from_ast_expression(expr, allowed_contracts: Optional[set]) -> List[Dict]:
    """Extract calls from an AST expression (fallback when IR is unavailable)."""
    calls = []

    if isinstance(expr, CallExpression):
        call_info = extract_call_from_call_expression(expr, allowed_contracts)
        if call_info:
            calls.append(call_info)

        for arg in expr.arguments:
            calls.extend(extract_calls_from_ast_expression(arg, allowed_contracts))

    elif hasattr(expr, 'arguments'):
        for arg in getattr(expr, 'arguments', []):
            calls.extend(extract_calls_from_ast_expression(arg, allowed_contracts))
    elif hasattr(expr, 'expression'):
        calls.extend(extract_calls_from_ast_expression(expr.expression, allowed_contracts))

    return calls


def extract_call_from_call_expression(expr: CallExpression,
                                       allowed_contracts: Optional[set]) -> Optional[Dict]:
    """Extract call info from a CallExpression AST node."""
    if not hasattr(expr, 'called') or not isinstance(expr.called, MemberAccess):
        return None

    called_func = expr.called.member_name
    called_contract = None

    if hasattr(expr.called.expression, 'type'):
        type_obj = expr.called.expression.type
        if isinstance(type_obj, UserDefinedType):
            called_contract = type_obj.type.name

    is_target = (allowed_contracts is None) or (called_contract in allowed_contracts)

    if not is_target or not called_func:
        return None

    abi_args = []
    for arg_expr in expr.arguments:
        if isinstance(arg_expr, Literal):
            abi_val = convert_value_to_abi(arg_expr.value, str(arg_expr.type))
            if abi_val:
                abi_args.append(abi_val)

    return {
        "function": called_func,
        "args": abi_args,
        "value": "0",
        "contract": called_contract
    }


# =============================================================================
# Main Call Extraction
# =============================================================================

def extract_calls_from_function(func: Function, allowed_contracts: Optional[set] = None,
                                 visited: Optional[set] = None,
                                 arg_mapping: Optional[Dict] = None,
                                 include_view: bool = False) -> List[Dict]:
    """Extract external calls from a function using Slither IR with AST fallback.

    Recursively follows both internal and external calls to find calls made to
    the target contract.
    """
    if visited is None:
        visited = set()
    if arg_mapping is None:
        arg_mapping = {}

    func_id = get_function_id(func)

    if func_id in visited:
        return []
    visited.add(func_id)

    calls = []
    try:
        calls = process_function_nodes(func, allowed_contracts, visited, arg_mapping, include_view)
    except Exception as e:
        print(f"Debug: Warning - failed to extract calls from {func_id}: {str(e)}", file=sys.stderr)
    finally:
        visited.discard(func_id)

    return calls


def get_function_id(func: Function) -> str:
    """Create a unique identifier for a function to handle overloads."""
    contract_name = func.contract.name if func.contract else "Global"
    return f"{contract_name}.{func.full_name}"


def process_function_nodes(func: Function, allowed_contracts: Optional[set],
                           visited: set, arg_mapping: Dict,
                           include_view: bool) -> List[Dict]:
    """Process all nodes in a function and extract calls."""
    calls = []
    nodes = getattr(func, 'nodes', [])

    # Build TMP variable value map from TypeConversion/Assignment operations
    tmp_value_map = build_tmp_value_map(nodes)

    # Merge with existing arg_mapping (arg_mapping takes precedence)
    combined_mapping = {**tmp_value_map, **arg_mapping}

    for node in nodes:
        node_calls = process_single_node(node, allowed_contracts, visited, combined_mapping, include_view)
        calls.extend(node_calls)

    return calls


def process_single_node(node, allowed_contracts: Optional[set],
                        visited: set, arg_mapping: Dict,
                        include_view: bool) -> List[Dict]:
    """Process a single function node and extract calls."""
    calls = []
    found_irs = False

    try:
        irs = node.irs
        if irs:
            found_irs = True
            for ir in irs:
                ir_calls = process_ir_operation(ir, allowed_contracts, visited, arg_mapping, include_view)
                calls.extend(ir_calls)
    except Exception:
        found_irs = False

    # AST fallback if IR is missing
    if not found_irs and hasattr(node, 'expression') and node.expression:
        ast_calls = extract_calls_from_ast_expression(node.expression, allowed_contracts)
        calls.extend(ast_calls)

    return calls


# =============================================================================
# Transaction Conversion
# =============================================================================

def convert_to_echidna_tx(call: Dict[str, Any], sender: str = DEFAULT_SENDER,
                          contract_addresses: Optional[Dict[str, str]] = None,
                          default_contract_addr: str = DEFAULT_CONTRACT_ADDR) -> Dict[str, Any]:
    """Convert extracted call info to Echidna Tx JSON format."""
    contract_name = call.get("contract")
    if contract_addresses and contract_name and contract_name in contract_addresses:
        dst = contract_addresses[contract_name]
    else:
        dst = default_contract_addr

    return {
        "call": {
            "tag": "SolCall",
            "contents": [call["function"], call["args"]]
        },
        "src": sender,
        "dst": dst,
        "gas": DEFAULT_GAS,
        "gasprice": "0",
        "value": call["value"],
        "delay": ["0", "0"],
        "_contractName": contract_name
    }


# =============================================================================
# Contract Processing
# =============================================================================

def process_contract(slither: Slither, target_contract: Optional[str] = None,
                     sender: str = DEFAULT_SENDER,
                     contract_addr: str = DEFAULT_CONTRACT_ADDR,
                     include_view: bool = False) -> Dict[str, Any]:
    """Process all contracts and extract test sequences."""
    result = {"sequences": [], "contractsFound": []}
    contracts_called = set()

    allowed_contracts = get_allowed_contracts(slither, target_contract)

    for contract in slither.contracts:
        if contract.is_interface or contract.is_library:
            continue

        sequences = process_contract_functions(
            contract, allowed_contracts, sender, contract_addr, include_view, contracts_called
        )
        result["sequences"].extend(sequences)

    result["contractsFound"] = sorted(list(contracts_called))
    return result


def get_allowed_contracts(slither: Slither, target_contract: Optional[str]) -> Optional[set]:
    """Get the set of allowed contracts (target + inheritance chain)."""
    if not target_contract:
        return None

    allowed = get_contract_inheritance_chain(slither, target_contract)
    if len(allowed) > 1:
        parents = ', '.join(allowed - {target_contract})
        print(f"Target contract '{target_contract}' inherits from: {parents}", file=sys.stderr)
    return allowed


def process_contract_functions(contract: Contract, allowed_contracts: Optional[set],
                                sender: str, contract_addr: str,
                                include_view: bool, contracts_called: set) -> List[Dict]:
    """Process all test functions in a contract."""
    sequences = []

    for func in contract.functions:
        if func.contract_declarer != contract:
            continue
        if not is_test_function(func):
            continue

        calls = extract_calls_from_function(func, allowed_contracts, include_view=include_view)

        for call in calls:
            if call.get("contract"):
                contracts_called.add(call["contract"])

        if calls:
            contract_addresses = {c: contract_addr for c in allowed_contracts} if allowed_contracts else None
            transactions = [
                convert_to_echidna_tx(call, sender, contract_addresses, contract_addr)
                for call in calls
            ]
            sequences.append({
                "source": f"{contract.name}.{func.name}",
                "transactions": transactions
            })

    return sequences


# =============================================================================
# Caching
# =============================================================================

def get_cache_key(filepath: str, target_contract: Optional[str], sender: str,
                  contract_addr: str, include_view: bool) -> str:
    """Generate a cache key based on the extraction parameters."""
    normalized_filepath = str(Path(filepath).resolve())
    key_parts = [normalized_filepath, target_contract or "", sender, contract_addr, str(include_view)]
    return hashlib.sha256("|".join(key_parts).encode()).hexdigest()[:16]


def get_test_files_hash(test_dir: str) -> str:
    """Compute a hash of all Solidity test files to detect changes."""
    hasher = hashlib.sha256()
    test_path = Path(test_dir).resolve()
    sol_files = sorted(test_path.rglob("*.sol"))

    for sol_file in sol_files:
        try:
            rel_path = sol_file.relative_to(test_path)
            hasher.update(str(rel_path).encode())
            hasher.update(str(sol_file.stat().st_mtime).encode())
        except (OSError, IOError, ValueError):
            continue

    return hasher.hexdigest()[:16]


def load_from_cache(cache_dir: str, cache_key: str, test_files_hash: str) -> Optional[Dict[str, Any]]:
    """Load extraction results from cache if valid.

    Cache is invalidated when test files change (based on test_files_hash).
    Cache key differentiates results for different extraction parameters (sender, contract_addr, etc.).
    """
    cache_file = os.path.join(cache_dir, f"echidna-prefill-{cache_key}.json")

    if not os.path.exists(cache_file):
        return None

    try:
        with open(cache_file, 'r') as f:
            cache_data = json.load(f)

        if cache_data.get("test_files_hash") != test_files_hash:
            return None

        print(f"Loaded from cache: {cache_file}", file=sys.stderr)
        return cache_data.get("result")
    except (json.JSONDecodeError, IOError, KeyError):
        pass

    return None


def save_to_cache(cache_dir: str, cache_key: str, test_files_hash: str,
                  result: Dict[str, Any]) -> None:
    """Save extraction results to cache.

    Cache key differentiates results for different extraction parameters.
    """
    os.makedirs(cache_dir, exist_ok=True)
    cache_file = os.path.join(cache_dir, f"echidna-prefill-{cache_key}.json")
    cache_data = {"test_files_hash": test_files_hash, "result": result}

    try:
        with open(cache_file, 'w') as f:
            json.dump(cache_data, f, indent=2)
        print(f"Saved to cache: {cache_file}", file=sys.stderr)
    except IOError as e:
        print(f"Warning: Could not save cache: {e}", file=sys.stderr)


def determine_cache_dir(filepath: str, explicit_cache_dir: Optional[str]) -> str:
    """Determine the cache directory based on filepath."""
    if explicit_cache_dir:
        return explicit_cache_dir

    filepath_path = Path(filepath).resolve()

    if filepath_path.is_dir() and filepath_path.name == "test":
        project_root = filepath_path.parent
    elif filepath_path.parent.name == "test":
        project_root = filepath_path.parent.parent
    else:
        project_root = filepath_path.parent

    return str(project_root / "crytic-export")


# =============================================================================
# Main Entry Point
# =============================================================================

def parse_arguments() -> argparse.Namespace:
    """Parse command line arguments."""
    parser = argparse.ArgumentParser(
        description="Extract transaction sequences from Foundry tests using Slither"
    )
    parser.add_argument("filepath", help="Path to the Solidity file")
    parser.add_argument("--crytic-args", nargs="*", default=[],
                       help="Additional arguments for crytic-compile")
    parser.add_argument("--target-contract", type=str, default=None,
                       help="Only extract calls to this contract type (e.g., 'MyContract')")
    parser.add_argument("--sender", type=str, default=DEFAULT_SENDER,
                       help="Sender address (default: 0x10000)")
    parser.add_argument("--contract-addr", type=str, default=DEFAULT_CONTRACT_ADDR,
                       help="Target contract address")
    parser.add_argument("--include-view", action="store_true", default=False,
                       help="Include view/pure function calls (default: exclude them)")
    parser.add_argument("--cache-dir", type=str, default=None,
                       help="Directory for caching results (default: crytic-export in project root)")
    parser.add_argument("--no-cache", action="store_true", default=False,
                       help="Disable caching")
    return parser.parse_args()


def get_foundry_out_directory(project_root: Path) -> Optional[str]:
    """Get the Foundry output directory from foundry.toml config."""
    foundry_toml = project_root / "foundry.toml"
    if not foundry_toml.exists():
        return None

    try:
        with open(foundry_toml, 'r') as f:
            content = f.read()
        # Simple TOML parsing for 'out' key
        for line in content.split('\n'):
            line = line.strip()
            if line.startswith('out') and '=' in line:
                # Extract value, handling quotes
                value = line.split('=', 1)[1].strip().strip('"\'')
                return value
    except (IOError, IndexError):
        pass
    return None


def get_foundry_project_root(filepath: str) -> Optional[Path]:
    """Find the Foundry project root by looking for foundry.toml."""
    target = Path(filepath).resolve()

    if target.is_dir() and (target / "foundry.toml").is_file():
        return target

    for p in target.parents:
        if (p / "foundry.toml").is_file():
            return p

    return None


def create_slither_instance(filepath: str, crytic_args: List[str]) -> Slither:
    """Create a Slither instance with optimized settings.

    Optimizations:
    1. printers_to_run='echidna': Skip data dependency analysis (not needed for call extraction)
       but keep IR generation which is required for extracting function calls
    2. foundry_compile_all=True: Include test files when compiling (required for test extraction)
    """
    project_root = get_foundry_project_root(filepath)

    slither_args = {
        "disallow_partial": False,
        # Skip data dependency analysis but keep IR generation
        # This is a Slither optimization specifically for echidna use cases
        "printers_to_run": "echidna",
    }

    if crytic_args:
        slither_args["solc_args"] = " ".join(crytic_args)

    # For Foundry projects, always compile with test files included
    if project_root:
        out_dir = get_foundry_out_directory(project_root) or "out"
        slither_args["foundry_out_directory"] = out_dir
        # Always include test files - required for extracting test functions
        slither_args["foundry_compile_all"] = True
        print("Compiling project with test files...", file=sys.stderr)
        return Slither(str(project_root), **slither_args)

    return Slither(filepath, **slither_args)


def run_extraction(args: argparse.Namespace) -> Dict[str, Any]:
    """Run the extraction process with caching support."""
    cache_dir = determine_cache_dir(args.filepath, args.cache_dir)
    cache_key = get_cache_key(
        args.filepath, args.target_contract, args.sender,
        args.contract_addr, args.include_view
    )
    test_files_hash = get_test_files_hash(args.filepath)

    # Try cache first
    result = None
    if not args.no_cache:
        result = load_from_cache(cache_dir, cache_key, test_files_hash)

    if result is None:
        slither = create_slither_instance(args.filepath, args.crytic_args)
        result = process_contract(
            slither, args.target_contract, args.sender,
            args.contract_addr, args.include_view
        )

        if not args.no_cache:
            save_to_cache(cache_dir, cache_key, test_files_hash, result)

    return result


def log_results(result: Dict[str, Any], target_contract: Optional[str]) -> None:
    """Log extraction results to stderr."""
    if result.get("contractsFound"):
        print(f"Contracts called in tests: {', '.join(result['contractsFound'])}", file=sys.stderr)
        if target_contract:
            print(f"Filtered to calls targeting: {target_contract}", file=sys.stderr)


def main():
    args = parse_arguments()

    try:
        result = run_extraction(args)
        log_results(result, args.target_contract)
        print(json.dumps(result))

    except Exception as e:
        print(json.dumps({"error": str(e), "sequences": []}), file=sys.stderr)
        print(json.dumps({"sequences": []}))
        sys.exit(0)


if __name__ == "__main__":
    main()
