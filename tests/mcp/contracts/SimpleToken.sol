// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

/**
 * Simple ERC20-like Token Contract for MCP Testing
 * Feature: 001-mcp-agent-commands
 * Phase 5, Task T066
 *
 * Provides basic token functionality for testing:
 * - transfer, approve, transferFrom
 * - balanceOf, allowance
 */

contract SimpleToken {
    string public name = "Test Token";
    string public symbol = "TEST";
    uint8 public decimals = 18;
    uint256 public totalSupply;
    
    mapping(address => uint256) public balances;
    mapping(address => mapping(address => uint256)) public allowances;
    
    event Transfer(address indexed from, address indexed to, uint256 value);
    event Approval(address indexed owner, address indexed spender, uint256 value);
    
    constructor(uint256 _initialSupply) {
        totalSupply = _initialSupply;
        balances[msg.sender] = _initialSupply;
        emit Transfer(address(0), msg.sender, _initialSupply);
    }
    
    /**
     * Transfer tokens to another address
     * Used for: inject_transaction testing
     */
    function transfer(address to, uint256 amount) public returns (bool) {
        require(balances[msg.sender] >= amount, "Insufficient balance");
        require(to != address(0), "Invalid recipient");
        
        balances[msg.sender] -= amount;
        balances[to] += amount;
        
        emit Transfer(msg.sender, to, amount);
        return true;
    }
    
    /**
     * Get balance of an address
     * Used for: prioritization testing
     */
    function balanceOf(address account) public view returns (uint256) {
        return balances[account];
    }
    
    /**
     * Approve spender to transfer tokens
     * Used for: multi-function testing
     */
    function approve(address spender, uint256 amount) public returns (bool) {
        require(spender != address(0), "Invalid spender");
        
        allowances[msg.sender][spender] = amount;
        
        emit Approval(msg.sender, spender, amount);
        return true;
    }
    
    /**
     * Get allowance for spender
     */
    function allowance(address owner, address spender) public view returns (uint256) {
        return allowances[owner][spender];
    }
    
    /**
     * Transfer tokens from one address to another
     * Used for: complex transaction testing
     */
    function transferFrom(address from, address to, uint256 amount) public returns (bool) {
        require(balances[from] >= amount, "Insufficient balance");
        require(allowances[from][msg.sender] >= amount, "Insufficient allowance");
        require(to != address(0), "Invalid recipient");
        
        balances[from] -= amount;
        balances[to] += amount;
        allowances[from][msg.sender] -= amount;
        
        emit Transfer(from, to, amount);
        return true;
    }
    
    /**
     * Mint new tokens (for testing)
     */
    function mint(address to, uint256 amount) public {
        require(to != address(0), "Invalid recipient");
        
        totalSupply += amount;
        balances[to] += amount;
        
        emit Transfer(address(0), to, amount);
    }
}
