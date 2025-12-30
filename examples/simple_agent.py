#!/usr/bin/env python3
"""
Simple AI Agent Integration Example for Echidna MCP Server

This demonstrates how to build an autonomous agent that monitors
Echidna fuzzing campaigns and intelligently guides the fuzzer.
"""

import httpx
import time
import json
from typing import Dict, Any, Optional


class EchidnaMCPAgent:
    """AI Agent that interacts with Echidna via MCP protocol."""
    
    def __init__(self, mcp_url: str = "http://localhost:8080/mcp"):
        self.mcp_url = mcp_url
        self.client = httpx.Client(timeout=30.0)
        
    def call_tool(self, tool_name: str, arguments: Dict[str, Any] = None) -> Dict[str, Any]:
        """Call an MCP tool using JSON-RPC 2.0 protocol."""
        payload = {
            "jsonrpc": "2.0",
            "id": 1,
            "method": "tools/call",
            "params": {
                "name": tool_name,
                "arguments": arguments or {}
            }
        }
        
        try:
            response = self.client.post(self.mcp_url, json=payload)
            response.raise_for_status()
            result = response.json()
            
            if "error" in result:
                print(f"❌ Error calling {tool_name}: {result['error']}")
                return None
                
            return result.get("result", {})
        except Exception as e:
            print(f"❌ Exception calling {tool_name}: {e}")
            return None
    
    def get_status(self) -> Optional[Dict]:
        """Get current fuzzing campaign status."""
        result = self.call_tool("status")
        if result:
            # Parse the text response
            text = result.get("content", [{}])[0].get("text", "")
            return self._parse_status(text)
        return None
    
    def _parse_status(self, text: str) -> Dict:
        """Parse status text into structured data."""
        lines = text.split("\n")
        status = {}
        for line in lines:
            if ": " in line:
                key, value = line.split(": ", 1)
                status[key.strip()] = value.strip()
        return status
    
    def get_coverage(self, contract: str) -> Optional[str]:
        """Get coverage report for a contract."""
        result = self.call_tool("show_coverage", {"contract": contract})
        if result:
            return result.get("content", [{}])[0].get("text", "")
        return None
    
    def inject_transactions(self, transactions: str) -> bool:
        """Inject transaction sequences for targeted fuzzing."""
        result = self.call_tool("inject_fuzz_transactions", {
            "transactions": transactions
        })
        return result is not None
    
    def clear_priorities(self) -> bool:
        """Clear function call priorities."""
        result = self.call_tool("clear_fuzz_priorities")
        return result is not None
    
    def monitor_and_guide(self, duration_minutes: int = 5, check_interval: int = 30):
        """
        Autonomous monitoring loop that guides the fuzzer based on coverage.
        
        Strategy:
        1. Monitor coverage growth
        2. If coverage stagnates, inject targeted transactions
        3. Periodically reset priorities to explore new paths
        """
        print(f"🤖 Starting autonomous agent for {duration_minutes} minutes...")
        print(f"   Checking status every {check_interval} seconds\n")
        
        start_time = time.time()
        end_time = start_time + (duration_minutes * 60)
        last_coverage = 0
        stagnation_count = 0
        
        iteration = 0
        while time.time() < end_time:
            iteration += 1
            elapsed = int(time.time() - start_time)
            
            print(f"\n{'='*60}")
            print(f"🔍 Iteration {iteration} (elapsed: {elapsed}s)")
            print(f"{'='*60}")
            
            # Get current status
            status = self.get_status()
            if not status:
                print("⚠️  Could not get status, retrying...")
                time.sleep(check_interval)
                continue
            
            # Parse coverage
            coverage_str = status.get("Coverage", "0")
            try:
                current_coverage = int(coverage_str)
            except ValueError:
                current_coverage = 0
            
            iterations = status.get("Iterations", "0/0")
            corpus_size = status.get("Corpus Size", "0")
            
            print(f"📊 Coverage: {current_coverage} instructions")
            print(f"🔢 Iterations: {iterations}")
            print(f"📦 Corpus: {corpus_size} sequences")
            
            # Check for coverage stagnation
            if current_coverage == last_coverage:
                stagnation_count += 1
                print(f"⚠️  Coverage stagnant ({stagnation_count} checks)")
                
                if stagnation_count >= 3:
                    print("🎯 Taking action: Injecting targeted transactions...")
                    
                    # Example: Inject edge case transactions
                    transactions = [
                        "transfer(0x0000000000000000000000000000000000000000, 0)",  # Zero address
                        "transfer(0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF, 999999)",  # Max values
                        "approve(0x1234567890123456789012345678901234567890, 0)",  # Zero approval
                    ]
                    
                    for tx in transactions:
                        if self.inject_transactions(tx):
                            print(f"   ✅ Injected: {tx}")
                    
                    stagnation_count = 0  # Reset counter
                    
                    # Also clear priorities to encourage exploration
                    if self.clear_priorities():
                        print("   ✅ Cleared function priorities")
            else:
                print(f"✨ Coverage increased: {last_coverage} → {current_coverage}")
                stagnation_count = 0
            
            last_coverage = current_coverage
            
            # Periodic priority reset (every 5 iterations)
            if iteration % 5 == 0:
                print("🔄 Periodic priority reset...")
                self.clear_priorities()
            
            # Wait before next check
            print(f"\n⏳ Waiting {check_interval}s before next check...")
            time.sleep(check_interval)
        
        print(f"\n{'='*60}")
        print("🎉 Monitoring complete!")
        print(f"{'='*60}")
        
        # Final status
        final_status = self.get_status()
        if final_status:
            print(f"\n📊 Final Statistics:")
            for key, value in final_status.items():
                print(f"   {key}: {value}")


def main():
    """Example usage of the MCP agent."""
    import sys
    
    print("🚀 Echidna MCP Agent Integration Example")
    print("=" * 60)
    
    # Create agent
    agent = EchidnaMCPAgent()
    
    # Quick connectivity test
    print("\n1️⃣  Testing connectivity...")
    status = agent.get_status()
    if not status:
        print("❌ Cannot connect to Echidna MCP server")
        print("   Make sure Echidna is running with --server 8080")
        sys.exit(1)
    
    print("✅ Connected to Echidna MCP server")
    print(f"   Current coverage: {status.get('Coverage', 'N/A')}")
    
    # Choose mode
    print("\n" + "=" * 60)
    print("Choose a mode:")
    print("  1) Quick test (all tools)")
    print("  2) Autonomous monitoring (5 minutes)")
    print("  3) Custom duration monitoring")
    print("=" * 60)
    
    choice = input("\nEnter choice (1-3): ").strip()
    
    if choice == "1":
        print("\n🧪 Running quick test of all tools...\n")
        
        # Test each tool
        print("2️⃣  Getting status...")
        status = agent.get_status()
        print(f"   ✅ Status: {json.dumps(status, indent=2)}")
        
        print("\n3️⃣  Getting coverage...")
        coverage = agent.get_coverage("EchidnaMCPTest")
        if coverage:
            print(f"   ✅ Coverage report (first 200 chars):\n   {coverage[:200]}...")
        
        print("\n4️⃣  Injecting test transaction...")
        if agent.inject_transactions("transfer(0x1234567890123456789012345678901234567890, 100)"):
            print("   ✅ Transaction injected")
        
        print("\n5️⃣  Clearing priorities...")
        if agent.clear_priorities():
            print("   ✅ Priorities cleared")
        
        print("\n✅ Quick test complete!")
    
    elif choice == "2":
        agent.monitor_and_guide(duration_minutes=5, check_interval=30)
    
    elif choice == "3":
        minutes = int(input("Enter duration in minutes: "))
        interval = int(input("Enter check interval in seconds (e.g., 30): "))
        agent.monitor_and_guide(duration_minutes=minutes, check_interval=interval)
    
    else:
        print("Invalid choice")


if __name__ == "__main__":
    main()
