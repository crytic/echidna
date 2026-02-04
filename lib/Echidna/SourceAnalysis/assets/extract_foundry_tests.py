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


def is_test_function(func: Function) -> bool:
    """Check if function is a Foundry/Echidna test function."""
    name = func.name
    
    # Standard Foundry/Echidna prefixes
    if (name.startswith("test") or
            name.startswith("testFuzz") or
            name.startswith("invariant_") or
            name.startswith("echidna_") or
            name.startswith("prove")):
        return True
        
    return False


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
            # Haskell's show format for ByteString
            return {"tag": "AbiString", "contents": repr(str(value))}
        elif solidity_type.startswith("bytes"):
            if solidity_type == "bytes":
                return {"tag": "AbiBytesDynamic", "contents": repr(bytes(value))}
            else:
                size = int(solidity_type[5:])
                return {"tag": "AbiBytes", "contents": [size, repr(bytes(value))]}
        else:
            # For complex types, return None (we'll skip these args)
            return None
    except (ValueError, TypeError):
        return None


def extract_constant_value(arg, arg_mapping: Optional[Dict[Any, Tuple[Any, str]]] = None) -> Tuple[Optional[Any], Optional[str]]:
    """Extract a constant value and its type from a Slither IR argument."""
    if isinstance(arg, Constant):
        value = arg.value
        if hasattr(arg, 'type') and arg.type:
            type_str = str(arg.type)
        else:
            # Infer type from value
            if isinstance(value, bool):
                type_str = "bool"
            elif isinstance(value, int):
                type_str = "uint256"
            elif isinstance(value, str):
                if value.startswith("0x") and len(value) == 42:
                    type_str = "address"
                else:
                    type_str = "string"
            else:
                type_str = "uint256"
        return value, type_str
    
    # Check if this is a parameter for which we have a constant value in the current context
    if arg_mapping and arg in arg_mapping:
        return arg_mapping[arg]

    return None, None


def get_contract_name_from_destination(ir) -> Optional[str]:
    """Extract the contract type name from a HighLevelCall's destination."""
    if not hasattr(ir, 'destination') or ir.destination is None:
        return None

    dest = ir.destination
    dest_type = getattr(dest, 'type', None)

    if dest_type is None:
        return None

    # Handle UserDefinedType (contract types)
    if isinstance(dest_type, UserDefinedType):
        # The type.type is the actual Contract object
        if hasattr(dest_type, 'type') and hasattr(dest_type.type, 'name'):
            return dest_type.type.name

    # Try string representation as fallback
    type_str = str(dest_type)
    # Filter out primitive types
    if type_str not in ('address', 'address payable') and not type_str.startswith('uint') and not type_str.startswith('int'):
        return type_str

    return None


def get_contract_inheritance_chain(slither: Slither, contract_name: str) -> set:
    """Get all contracts in the inheritance chain of a contract.

    Returns a set containing the contract name and all its parent contract names.
    This allows matching calls to parent contracts when the target inherits from them.
    """
    result = {contract_name}

    # Find the contract by name
    target = None
    for contract in slither.contracts:
        if contract.name == contract_name:
            target = contract
            break

    if target is None:
        return result

    # Get all inherited contracts (recursively)
    for parent in target.inheritance:
        result.add(parent.name)

    return result


def extract_calls_from_function(func: Function, allowed_contracts: Optional[set] = None,
                                 visited: Optional[set] = None,
                                 arg_mapping: Optional[Dict[Any, Tuple[Any, str]]] = None,
                                 include_view: bool = False) -> List[Dict[str, Any]]:
    """Extract external calls from a test function using Slither IR with AST fallback.

    Recursively follows both internal and external calls to find calls made to the target contract.
    If IR generation fails for a node, it falls back to AST-based extraction.

    Args:
        func: The function to extract calls from
        allowed_contracts: If specified, only extract calls to contracts in this set.
                          This should include the target contract and all its parents.
        visited: Set of already-visited function names to prevent infinite recursion
        arg_mapping: Mapping of parameters to constant values for the current call context
    """
    if visited is None:
        visited = set()
    if arg_mapping is None:
        arg_mapping = {}

    # Create a unique identifier for this function to handle overloads
    contract_name_decl = func.contract.name if func.contract else "Global"
    func_id = f"{contract_name_decl}.{func.full_name}"
    
    # Avoid infinite recursion (cycles)
    if func_id in visited:
        return []
    visited.add(func_id)

    calls = []

    try:
        nodes = getattr(func, 'nodes', [])
        for node in nodes:
            found_irs = False
            try:
                # Try to use IR if available
                irs = node.irs
                if irs:
                    found_irs = True
                    for ir in irs:
                        # --- Internal or Library Call ---
                        if isinstance(ir, (InternalCall, LibraryCall)):
                            called_func = ir.function
                            if called_func and hasattr(called_func, 'nodes'):
                                # Resolve arguments for the call
                                new_arg_mapping = {}
                                if hasattr(ir, 'arguments') and hasattr(called_func, 'parameters'):
                                    for param, arg in zip(called_func.parameters, ir.arguments):
                                        val, type_str = extract_constant_value(arg, arg_mapping)
                                        if val is not None:
                                            new_arg_mapping[param] = (val, type_str)
                                
                                nested_calls = extract_calls_from_function(called_func, allowed_contracts, visited, new_arg_mapping, include_view)
                                calls.extend(nested_calls)

                        # --- High Level Call ---
                        elif isinstance(ir, HighLevelCall):
                            called_contract_name = get_contract_name_from_destination(ir)
                            is_target_call = (allowed_contracts is None) or (called_contract_name in allowed_contracts)

                            if is_target_call:
                                func_name = None
                                called_func_obj = None
                                if hasattr(ir, 'function') and ir.function:
                                    called_func_obj = ir.function
                                    func_name = getattr(called_func_obj, 'name', str(called_func_obj))
                                elif hasattr(ir, 'function_name'):
                                    func_name = ir.function_name

                                # Skip view/pure functions unless include_view is True
                                if not include_view and called_func_obj:
                                    if getattr(called_func_obj, 'view', False) or getattr(called_func_obj, 'pure', False):
                                        continue

                                if func_name:
                                    args = []
                                    if hasattr(ir, 'arguments') and ir.arguments:
                                        for arg in ir.arguments:
                                            value, type_str = extract_constant_value(arg, arg_mapping)
                                            if value is not None and type_str:
                                                abi_val = convert_value_to_abi(value, type_str)
                                                if abi_val:
                                                    args.append(abi_val)
                                    
                                    value = "0"
                                    if hasattr(ir, 'call_value') and ir.call_value:
                                        val, _ = extract_constant_value(ir.call_value, arg_mapping)
                                        if val is not None:
                                            value = str(val)

                                    calls.append({
                                        "function": func_name, "args": args, "value": value, "contract": called_contract_name,
                                    })
                            else:
                                # Follow call to other contract (e.g. handler)
                                called_func_obj = getattr(ir, 'function', None)
                                if called_func_obj and hasattr(called_func_obj, 'nodes') and called_func_obj.contract_declarer:
                                    new_arg_mapping = {}
                                    if hasattr(ir, 'arguments') and hasattr(called_func_obj, 'parameters'):
                                        for param, arg in zip(called_func_obj.parameters, ir.arguments):
                                            val, type_str = extract_constant_value(arg, arg_mapping)
                                            if val is not None:
                                                new_arg_mapping[param] = (val, type_str)
                                    nested_calls = extract_calls_from_function(called_func_obj, allowed_contracts, visited, new_arg_mapping, include_view)
                                    calls.extend(nested_calls)
            except Exception:
                found_irs = False

            # --- AST Fallback ---
            # If IR is missing or empty, try to find calls in the AST expression
            if not found_irs and hasattr(node, 'expression') and node.expression:
                # Basic AST visitor for calls
                def visit_expression(expr):
                    if isinstance(expr, CallExpression):
                        # Try to resolve what's being called
                        called_func = None
                        if hasattr(expr, 'called') and isinstance(expr.called, MemberAccess):
                            # It's something.func()
                            called_func = expr.called.member_name
                            
                            # Try to get the contract name from the base of member access
                            called_contract = None
                            if hasattr(expr.called.expression, 'type'):
                                type_obj = expr.called.expression.type
                                if isinstance(type_obj, UserDefinedType):
                                    called_contract = type_obj.type.name
                            
                            is_target = (allowed_contracts is None) or (called_contract in allowed_contracts)
                            
                            if is_target and called_func:
                                # We found a call to the target!
                                # Arguments are harder in AST, we'll try to get literals
                                abi_args = []
                                for arg_expr in expr.arguments:
                                    if isinstance(arg_expr, Literal):
                                        abi_val = convert_value_to_abi(arg_expr.value, str(arg_expr.type))
                                        if abi_val:
                                            abi_args.append(abi_val)
                                            
                                calls.append({
                                    "function": called_func, "args": abi_args, "value": "0", "contract": called_contract
                                })
                        
                        # Recurse into arguments in case they contain calls
                        for arg in expr.arguments:
                            visit_expression(arg)
                    
                    elif hasattr(expr, 'arguments'): # handle other expressions with nested parts
                        for arg in getattr(expr, 'arguments', []):
                            visit_expression(arg)
                    elif hasattr(expr, 'expression'):
                         visit_expression(expr.expression)

                visit_expression(node.expression)

    except Exception as e:
        print(f"Debug: Warning - failed to extract calls from {func_id}: {str(e)}", file=sys.stderr)
    finally:
        if func_id in visited:
            visited.remove(func_id)

    return calls


def convert_to_echidna_tx(call: Dict[str, Any], sender: str = DEFAULT_SENDER,
                          contract_addresses: Optional[Dict[str, str]] = None,
                          default_contract_addr: str = DEFAULT_CONTRACT_ADDR) -> Dict[str, Any]:
    """Convert extracted call info to Echidna Tx JSON format.

    Args:
        call: The call info dict with function, args, value, contract
        sender: The sender address
        contract_addresses: Optional mapping of contract names to addresses
        default_contract_addr: Default contract address if not in mapping
    """
    # Determine destination address
    contract_name = call.get("contract")
    if contract_addresses and contract_name and contract_name in contract_addresses:
        dst = contract_addresses[contract_name]
    else:
        # Default to the main contract address
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
        # Include contract name as metadata for Echidna to use
        "_contractName": contract_name
    }


def process_contract(slither: Slither, target_contract: Optional[str] = None,
                     sender: str = DEFAULT_SENDER,
                     contract_addr: str = DEFAULT_CONTRACT_ADDR,
                     include_view: bool = False) -> Dict[str, Any]:
    """Process all contracts and extract test sequences.

    Args:
        slither: The Slither analysis object
        target_contract: If specified, only extract calls to this contract type.
                        This should typically be the main contract under test.
                        Also includes calls to parent contracts in the inheritance chain.
        sender: The sender address for transactions
        contract_addr: The target contract address
        include_view: If True, include view/pure function calls (default: False)
    """
    result = {
        "sequences": [],
        "contractsFound": [],  # List of contract types that were called
    }

    contracts_called = set()

    # Get the allowed contracts (target + all its parents in the inheritance chain)
    allowed_contracts = None
    if target_contract:
        allowed_contracts = get_contract_inheritance_chain(slither, target_contract)
        if len(allowed_contracts) > 1:
            print(f"Target contract '{target_contract}' inherits from: {', '.join(allowed_contracts - {target_contract})}",
                  file=sys.stderr)

    for contract in slither.contracts:
        # Skip interfaces and libraries
        if contract.is_interface or contract.is_library:
            continue

        for func in contract.functions:
            if func.contract_declarer != contract:
                continue
            
            # Only process test functions 
            if not is_test_function(func):
                continue

            # Extract calls from the test function
            calls = extract_calls_from_function(func, allowed_contracts, include_view=include_view)

            # Track which contracts are being called
            for call in calls:
                if call.get("contract"):
                    contracts_called.add(call["contract"])

            if calls:
                # Convert to Echidna format, passing sender and contract address
                # Map all allowed contracts (target + parents) to the same address
                contract_addresses = {c: contract_addr for c in allowed_contracts} if allowed_contracts else None
                transactions = [
                    convert_to_echidna_tx(call, sender, contract_addresses, contract_addr)
                    for call in calls
                ]

                result["sequences"].append({
                    "source": f"{contract.name}.{func.name}",
                    "transactions": transactions
                })

    result["contractsFound"] = sorted(list(contracts_called))
    return result


CACHE_FILENAME = "echidna-prefill-cache.json"


def get_cache_key(filepath: str, target_contract: Optional[str], sender: str,
                  contract_addr: str, include_view: bool) -> str:
    """Generate a cache key based on the extraction parameters."""
    # Normalize filepath to absolute path for consistent cache keys
    # This ensures ./test and /full/path/to/test produce the same key
    normalized_filepath = str(Path(filepath).resolve())
    key_parts = [
        normalized_filepath,
        target_contract or "",
        sender,
        contract_addr,
        str(include_view)
    ]
    return hashlib.sha256("|".join(key_parts).encode()).hexdigest()[:16]


def get_test_files_hash(test_dir: str) -> str:
    """Compute a hash of all Solidity test files to detect changes."""
    hasher = hashlib.sha256()
    test_path = Path(test_dir).resolve()

    # Find all .sol files in the test directory
    sol_files = sorted(test_path.rglob("*.sol"))

    for sol_file in sol_files:
        try:
            # Include file path (relative to test_path for consistency) and modification time
            rel_path = sol_file.relative_to(test_path)
            hasher.update(str(rel_path).encode())
            hasher.update(str(sol_file.stat().st_mtime).encode())
        except (OSError, IOError, ValueError):
            continue

    return hasher.hexdigest()[:16]


def load_from_cache(cache_dir: str, cache_key: str, test_files_hash: str) -> Optional[Dict[str, Any]]:
    """Load extraction results from cache if valid."""
    cache_file = os.path.join(cache_dir, CACHE_FILENAME)

    if not os.path.exists(cache_file):
        return None

    try:
        with open(cache_file, 'r') as f:
            cache_data = json.load(f)

        # Check if cache is valid
        if (cache_data.get("cache_key") == cache_key and
            cache_data.get("test_files_hash") == test_files_hash):
            print(f"Loaded from cache: {cache_file}", file=sys.stderr)
            return cache_data.get("result")
    except (json.JSONDecodeError, IOError, KeyError):
        pass

    return None


def save_to_cache(cache_dir: str, cache_key: str, test_files_hash: str,
                  result: Dict[str, Any]) -> None:
    """Save extraction results to cache."""
    # Ensure cache directory exists
    os.makedirs(cache_dir, exist_ok=True)

    cache_file = os.path.join(cache_dir, CACHE_FILENAME)
    cache_data = {
        "cache_key": cache_key,
        "test_files_hash": test_files_hash,
        "result": result
    }

    try:
        with open(cache_file, 'w') as f:
            json.dump(cache_data, f, indent=2)
        print(f"Saved to cache: {cache_file}", file=sys.stderr)
    except IOError as e:
        print(f"Warning: Could not save cache: {e}", file=sys.stderr)


def main():
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
                       help="Target contract address (default: 0x00a329c0648769a73afac7f9381e08fb43dbea72)")
    parser.add_argument("--include-view", action="store_true", default=False,
                       help="Include view/pure function calls (default: exclude them)")
    parser.add_argument("--cache-dir", type=str, default=None,
                       help="Directory for caching results (default: crytic-export in project root)")
    parser.add_argument("--no-cache", action="store_true", default=False,
                       help="Disable caching")

    args = parser.parse_args()

    try:
        # Determine cache directory
        cache_dir = args.cache_dir
        if cache_dir is None:
            # Default to crytic-export in the project root (parent of test dir)
            filepath_path = Path(args.filepath).resolve()
            # If filepath is a directory named "test", go up one level
            if filepath_path.is_dir() and filepath_path.name == "test":
                project_root = filepath_path.parent
            # If filepath is a file inside a "test" directory, go up two levels
            elif filepath_path.parent.name == "test":
                project_root = filepath_path.parent.parent
            else:
                project_root = filepath_path.parent
            cache_dir = str(project_root / "crytic-export")

        # Generate cache key and test files hash
        cache_key = get_cache_key(
            args.filepath, args.target_contract, args.sender,
            args.contract_addr, args.include_view
        )
        test_files_hash = get_test_files_hash(args.filepath)

        # Try to load from cache
        result = None
        if not args.no_cache:
            result = load_from_cache(cache_dir, cache_key, test_files_hash)

        if result is None:
            # Initialize Slither
            # Note: We pass the project root so Slither can reuse existing Forge compilation
            slither_args = {
                "disallow_partial": False,  # Handle incomplete sources gracefully
            }
            if args.crytic_args:
                slither_args["solc_args"] = " ".join(args.crytic_args)

            slither = Slither(args.filepath, **slither_args)

            # Process and extract sequences
            result = process_contract(
                slither,
                args.target_contract,
                args.sender,
                args.contract_addr,
                args.include_view
            )

            # Save to cache
            if not args.no_cache:
                save_to_cache(cache_dir, cache_key, test_files_hash, result)

        # Log which contracts were found
        if result.get("contractsFound"):
            print(f"Contracts called in tests: {', '.join(result['contractsFound'])}",
                  file=sys.stderr)
            if args.target_contract:
                print(f"Filtered to calls targeting: {args.target_contract}", file=sys.stderr)

        # Output JSON
        print(json.dumps(result))

    except Exception as e:
        print(json.dumps({"error": str(e), "sequences": []}), file=sys.stderr)
        # Still output empty sequences on stdout so Echidna can continue
        print(json.dumps({"sequences": []}))
        sys.exit(0)  # Don't fail hard, just return empty


if __name__ == "__main__":
    main()
