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
    """Check if function is a Foundry test function."""
    name = func.name
    return (name.startswith("test") or
            name.startswith("testFuzz") or
            name.startswith("invariant_"))


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


def extract_constant_value(arg) -> Tuple[Optional[Any], Optional[str]]:
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


def extract_calls_from_function(func: Function, target_contract: Optional[str] = None) -> List[Dict[str, Any]]:
    """Extract external calls from a test function using Slither IR.

    Args:
        func: The function to extract calls from
        target_contract: If specified, only extract calls to this contract type
    """
    calls = []

    for node in func.nodes:
        for ir in node.irs:
            # Look for high-level calls (external contract calls)
            if isinstance(ir, HighLevelCall):
                # Get the contract type being called
                contract_name = get_contract_name_from_destination(ir)

                # If we have a target contract filter, skip calls to other contracts
                if target_contract and contract_name and contract_name != target_contract:
                    continue

                # Get function being called
                called_func = None
                if hasattr(ir, 'function') and ir.function:
                    called_func = ir.function
                    if hasattr(called_func, 'name'):
                        func_name = called_func.name
                    else:
                        func_name = str(called_func)
                elif hasattr(ir, 'function_name'):
                    func_name = ir.function_name
                else:
                    continue

                # Skip view/pure functions - they don't modify state
                if called_func:
                    if getattr(called_func, 'view', False) or getattr(called_func, 'pure', False):
                        continue

                # Extract arguments
                args = []
                if hasattr(ir, 'arguments') and ir.arguments:
                    for arg in ir.arguments:
                        value, type_str = extract_constant_value(arg)
                        if value is not None and type_str:
                            abi_val = convert_value_to_abi(value, type_str)
                            if abi_val:
                                args.append(abi_val)
                        # For non-constant args, we skip them (fuzzer will fill in)

                # Check for value (ETH sent)
                value = "0"
                if hasattr(ir, 'call_value') and ir.call_value:
                    val, _ = extract_constant_value(ir.call_value)
                    if val is not None:
                        value = str(val)

                calls.append({
                    "function": func_name,
                    "args": args,
                    "value": value,
                    "contract": contract_name,  # Include contract name
                })

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
                     contract_addr: str = DEFAULT_CONTRACT_ADDR) -> Dict[str, Any]:
    """Process all contracts and extract test sequences.

    Args:
        slither: The Slither analysis object
        target_contract: If specified, only extract calls to this contract type.
                        This should typically be the main contract under test.
        sender: The sender address for transactions
        contract_addr: The target contract address
    """
    result = {
        "sequences": [],
        "contractsFound": [],  # List of contract types that were called
    }

    contracts_called = set()

    for contract in slither.contracts:
        # Skip interfaces and libraries
        if contract.is_interface or contract.is_library:
            continue

        for func in contract.functions:
            # Only process test functions that are declared in this contract
            if not is_test_function(func):
                continue
            if func.contract_declarer != contract:
                continue

            # Extract calls from the test function
            calls = extract_calls_from_function(func, target_contract)

            # Track which contracts are being called
            for call in calls:
                if call.get("contract"):
                    contracts_called.add(call["contract"])

            if calls:
                # Convert to Echidna format, passing sender and contract address
                contract_addresses = {target_contract: contract_addr} if target_contract else None
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

    args = parser.parse_args()

    try:
        # Initialize Slither
        slither_args = {}
        if args.crytic_args:
            slither_args["solc_args"] = " ".join(args.crytic_args)

        slither = Slither(args.filepath, **slither_args)

        # Process and extract sequences
        result = process_contract(
            slither,
            args.target_contract,
            args.sender,
            args.contract_addr
        )

        # Log which contracts were found
        if result["contractsFound"]:
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
