# MCP Tools JSON Schema Definitions

**Feature**: 001-mcp-agent-commands  
**Phase**: 1 - Design  
**Format**: JSON Schema Draft-07

---

## Tool Input Schemas

### read_logs

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "title": "ReadLogsInput",
  "description": "Retrieve campaign event logs from EventLog ring buffer",
  "properties": {
    "count": {
      "type": "integer",
      "minimum": 1,
      "maximum": 2500,
      "default": 100,
      "description": "Number of most recent events to retrieve"
    },
    "eventType": {
      "type": "string",
      "enum": ["PropertyFalsified", "TransactionExecuted", "CoverageIncreased", "WorkerStarted", "WorkerStopped"],
      "description": "Filter by event type (optional)"
    },
    "workerId": {
      "type": "integer",
      "minimum": 0,
      "description": "Filter by worker ID (optional)"
    }
  },
  "additionalProperties": false
}
```

### show_coverage

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "title": "ShowCoverageInput",
  "description": "Retrieve line and branch coverage statistics",
  "properties": {
    "contract": {
      "type": "string",
      "description": "Filter by contract name (optional)"
    }
  },
  "additionalProperties": false
}
```

### dump_lcov

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "title": "DumpLcovInput",
  "description": "Export coverage in LCOV format",
  "properties": {
    "outputPath": {
      "type": "string",
      "description": "Output file path (optional, defaults to stdout)"
    }
  },
  "additionalProperties": false
}
```

### get_corpus_size

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "title": "GetCorpusSizeInput",
  "description": "Get transaction count in corpus (no parameters)",
  "additionalProperties": false
}
```

### inspect_corpus_transactions

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "title": "InspectCorpusInput",
  "description": "Paginated corpus transaction inspection",
  "properties": {
    "offset": {
      "type": "integer",
      "minimum": 0,
      "default": 0,
      "description": "Pagination offset"
    },
    "limit": {
      "type": "integer",
      "minimum": 1,
      "maximum": 100,
      "default": 20,
      "description": "Max transactions to return per page"
    }
  },
  "additionalProperties": false
}
```

### find_transaction_in_corpus

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "title": "FindTransactionInput",
  "description": "Search corpus for matching transactions",
  "properties": {
    "functionSignature": {
      "type": "string",
      "description": "Function signature (e.g., 'transfer(address,uint256)')"
    },
    "sender": {
      "type": "string",
      "pattern": "^0x[0-9a-fA-F]{40}$",
      "description": "Sender address filter"
    },
    "minValue": {
      "type": "string",
      "description": "Minimum ETH value in wei"
    }
  },
  "anyOf": [
    {"required": ["functionSignature"]},
    {"required": ["sender"]},
    {"required": ["minValue"]}
  ],
  "additionalProperties": false
}
```

### inject_transaction

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "title": "InjectTransactionInput",
  "description": "Inject transaction sequence to all workers (Clarification #5)",
  "properties": {
    "sequence": {
      "type": "array",
      "items": {
        "type": "string",
        "description": "Solidity-like function call (Clarification #1)",
        "pattern": "^[a-zA-Z_][a-zA-Z0-9_]*\\([^)]*\\)$",
        "examples": [
          "transfer(0x1234567890abcdef1234567890abcdef12345678, 100)",
          "approve(0xabcd..., 1000000000000000000)",
          "deposit()"
        ]
      },
      "minItems": 1,
      "maxItems": 100,
      "description": "Ordered list of function calls"
    },
    "sender": {
      "type": "string",
      "pattern": "^0x[0-9a-fA-F]{40}$",
      "default": "0x0000000000000000000000000000000000010000",
      "description": "Transaction sender address"
    },
    "value": {
      "type": "string",
      "default": "0",
      "pattern": "^[0-9]+$",
      "description": "ETH value in wei (decimal string)"
    }
  },
  "required": ["sequence"],
  "additionalProperties": false
}
```

### prioritize_function

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "title": "PrioritizeFunctionInput",
  "description": "Add function to prioritization set (90% weighting, Clarification #2)",
  "properties": {
    "functionSignature": {
      "type": "string",
      "pattern": "^[a-zA-Z_][a-zA-Z0-9_]*\\([^)]*\\)$",
      "description": "Canonical ABI function signature (no spaces)",
      "examples": [
        "transfer(address,uint256)",
        "approve(address,uint256)",
        "depositETH()"
      ]
    }
  },
  "required": ["functionSignature"],
  "additionalProperties": false
}
```

### clear_priorities

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "title": "ClearPrioritiesInput",
  "description": "Reset prioritization set to empty (no parameters)",
  "additionalProperties": false
}
```

---

## Tool Output Schemas

### read_logs (output)

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "title": "ReadLogsOutput",
  "properties": {
    "events": {
      "type": "array",
      "items": {
        "type": "object",
        "properties": {
          "timestamp": {
            "type": "string",
            "format": "date-time",
            "description": "ISO 8601 timestamp"
          },
          "eventType": {
            "type": "string",
            "description": "Event category"
          },
          "workerId": {
            "type": "integer",
            "minimum": 0,
            "description": "Worker that generated event"
          },
          "data": {
            "type": "object",
            "description": "Event-specific JSON payload"
          }
        },
        "required": ["timestamp", "eventType", "workerId", "data"]
      }
    },
    "totalCount": {
      "type": "integer",
      "minimum": 0,
      "maximum": 2500,
      "description": "Total events in EventLog buffer"
    }
  },
  "required": ["events", "totalCount"]
}
```

### show_coverage (output)

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "title": "ShowCoverageOutput",
  "properties": {
    "contracts": {
      "type": "array",
      "items": {
        "type": "object",
        "properties": {
          "name": {"type": "string"},
          "lineCoverage": {
            "type": "number",
            "minimum": 0,
            "maximum": 100
          },
          "branchCoverage": {
            "type": "number",
            "minimum": 0,
            "maximum": 100
          }
        },
        "required": ["name", "lineCoverage", "branchCoverage"]
      }
    },
    "overallCoverage": {
      "type": "number",
      "minimum": 0,
      "maximum": 100,
      "description": "Weighted average across all contracts"
    }
  },
  "required": ["contracts", "overallCoverage"]
}
```

### dump_lcov (output)

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "string",
  "title": "DumpLcovOutput",
  "description": "LCOV-formatted text (plain text, not JSON)",
  "contentMediaType": "text/plain"
}
```

### get_corpus_size (output)

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "title": "GetCorpusSizeOutput",
  "properties": {
    "size": {
      "type": "integer",
      "minimum": 0,
      "description": "Number of transactions in corpus"
    }
  },
  "required": ["size"]
}
```

### inspect_corpus_transactions (output)

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "title": "InspectCorpusOutput",
  "properties": {
    "transactions": {
      "type": "array",
      "items": {
        "type": "object",
        "properties": {
          "index": {"type": "integer"},
          "call": {
            "type": "string",
            "description": "Solidity-like syntax (Clarification #1)"
          },
          "sender": {"type": "string"},
          "value": {"type": "string"},
          "gasUsed": {"type": "integer"}
        },
        "required": ["index", "call", "sender", "value"]
      }
    },
    "total": {
      "type": "integer",
      "description": "Total corpus size"
    }
  },
  "required": ["transactions", "total"]
}
```

### find_transaction_in_corpus (output)

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "title": "FindTransactionOutput",
  "properties": {
    "matches": {
      "type": "array",
      "items": {
        "type": "object",
        "properties": {
          "index": {"type": "integer"},
          "call": {"type": "string"},
          "sender": {"type": "string"},
          "value": {"type": "string"},
          "gasUsed": {"type": "integer"}
        },
        "required": ["index", "call", "sender", "value"]
      }
    },
    "count": {
      "type": "integer",
      "minimum": 0,
      "description": "Number of matching transactions"
    }
  },
  "required": ["matches", "count"]
}
```

### inject_transaction (output)

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "title": "InjectTransactionOutput",
  "properties": {
    "success": {"type": "boolean"},
    "message": {
      "type": "string",
      "description": "Human-readable result"
    },
    "injectedCount": {
      "type": "integer",
      "minimum": 0,
      "description": "Number of transactions injected"
    }
  },
  "required": ["success", "message", "injectedCount"]
}
```

### prioritize_function (output)

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "title": "PrioritizeFunctionOutput",
  "properties": {
    "success": {"type": "boolean"},
    "message": {"type": "string"},
    "prioritizedFunctions": {
      "type": "array",
      "items": {"type": "string"},
      "description": "Current set of prioritized functions"
    }
  },
  "required": ["success", "message", "prioritizedFunctions"]
}
```

### clear_priorities (output)

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "title": "ClearPrioritiesOutput",
  "properties": {
    "success": {"type": "boolean"},
    "message": {"type": "string"},
    "clearedCount": {
      "type": "integer",
      "minimum": 0,
      "description": "Number of functions that were prioritized"
    }
  },
  "required": ["success", "message", "clearedCount"]
}
```

---

## Validation Rules

### Common Rules (All Tools)
- All timestamps in ISO 8601 format with timezone
- All addresses in lowercase hexadecimal with `0x` prefix
- All ETH values in wei as decimal strings (no hex)
- All function signatures in canonical ABI format (no spaces)

### Input Validation
- `read_logs.count`: Must be 1-2500 (EventLog capacity, Clarification #4)
- `inject_transaction.sequence`: Max 100 transactions (prevent DOS)
- `find_transaction_in_corpus`: At least one search criterion required
- `prioritize_function.functionSignature`: Must match ABI signature regex

### Output Guarantees
- `read_logs`: Events sorted by timestamp (newest first)
- `show_coverage`: Percentages rounded to 1 decimal place
- `inspect_corpus_transactions`: Results ordered by corpus index
- `find_transaction_in_corpus`: Results unordered (corpus scan order)

---

## Error Schemas

### Generic Error Response

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "title": "ErrorResponse",
  "properties": {
    "success": {
      "type": "boolean",
      "enum": [false]
    },
    "error": {
      "type": "object",
      "properties": {
        "code": {
          "type": "string",
          "enum": [
            "INVALID_INPUT",
            "TOOL_NOT_FOUND",
            "EXECUTION_TIMEOUT",
            "INTERNAL_ERROR"
          ]
        },
        "message": {
          "type": "string",
          "description": "Human-readable error description"
        },
        "details": {
          "type": "object",
          "description": "Additional error context"
        }
      },
      "required": ["code", "message"]
    }
  },
  "required": ["success", "error"]
}
```

### Error Codes

| Code | HTTP Status | Description | Example |
|------|-------------|-------------|---------|
| INVALID_INPUT | 400 | Schema validation failed | `{"code": "INVALID_INPUT", "message": "functionSignature must match pattern ^[a-zA-Z_]..."}` |
| TOOL_NOT_FOUND | 404 | Tool name not registered | `{"code": "TOOL_NOT_FOUND", "message": "Tool 'unknown_tool' not found"}` |
| EXECUTION_TIMEOUT | 504 | Tool exceeded 100ms limit | `{"code": "EXECUTION_TIMEOUT", "message": "Tool read_logs exceeded 100ms"}` |
| INTERNAL_ERROR | 500 | Unexpected error | `{"code": "INTERNAL_ERROR", "message": "Failed to read eventLog: IORef null"}` |

---

## Usage Examples

### Example 1: Read Last 50 Events

**Request**:
```json
{
  "count": 50,
  "eventType": "PropertyFalsified"
}
```

**Response**:
```json
{
  "events": [
    {
      "timestamp": "2025-12-20T10:35:12",
      "eventType": "PropertyFalsified",
      "workerId": 2,
      "data": {
        "property": "echidna_balance_invariant",
        "counterexample": ["deposit(100)", "withdraw(200)"],
        "gasUsed": 45000
      }
    }
  ],
  "totalCount": 1523
}
```

### Example 2: Inject Transaction Sequence

**Request**:
```json
{
  "sequence": [
    "approve(0x1234567890abcdef1234567890abcdef12345678, 1000000)",
    "transferFrom(0xabcd..., 0xdcba..., 500000)"
  ],
  "sender": "0x0000000000000000000000000000000000010000"
}
```

**Response**:
```json
{
  "success": true,
  "message": "Injected 2 transactions to 4 workers",
  "injectedCount": 2
}
```

### Example 3: Prioritize Transfer Function

**Request**:
```json
{
  "functionSignature": "transfer(address,uint256)"
}
```

**Response**:
```json
{
  "success": true,
  "message": "Prioritized function 'transfer(address,uint256)'",
  "prioritizedFunctions": [
    "transfer(address,uint256)",
    "approve(address,uint256)"
  ]
}
```

---

**Status**: Phase 1 Complete - Contracts Approved  
**Validation**: All 9 tools have complete input/output schemas  
**OpenAPI**: See [mcp-tools-openapi.json](mcp-tools-openapi.json)  
**Next Phase**: Quickstart Guide  
**Date**: 2025-12-20
