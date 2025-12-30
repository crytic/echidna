"""
Schema Validation Utilities
Feature: 001-mcp-agent-commands
Phase 5, Task T057

Provides schema validation for MCP tool responses.
"""

import json
from pathlib import Path
from typing import Dict, Any
from jsonschema import validate, ValidationError


# Schema directory
SCHEMA_DIR = Path(__file__).parent.parent / "contracts"


def validate_response(response: Dict[str, Any], schema_name: str) -> bool:
    """
    Validate MCP tool response against JSON schema.
    
    Args:
        response: Tool response as dict
        schema_name: Name of schema file (e.g., "read_logs", "show_coverage")
        
    Returns:
        True if validation passes
        
    Raises:
        ValidationError: If response doesn't match schema
        FileNotFoundError: If schema file not found
    """
    schema_path = SCHEMA_DIR / f"{schema_name}.json"
    
    if not schema_path.exists():
        raise FileNotFoundError(f"Schema file not found: {schema_path}")
    
    with open(schema_path) as f:
        schema = json.load(f)
    
    # Validate (raises ValidationError if invalid)
    validate(instance=response, schema=schema)
    
    return True


def load_schema(schema_name: str) -> Dict[str, Any]:
    """
    Load JSON schema from contracts directory.
    
    Args:
        schema_name: Name of schema file (without .json extension)
        
    Returns:
        Schema as dict
    """
    schema_path = SCHEMA_DIR / f"{schema_name}.json"
    
    with open(schema_path) as f:
        return json.load(f)
