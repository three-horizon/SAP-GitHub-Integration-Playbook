#!/bin/bash
# SAP-GitHub Integration Test Script
# 
# This script performs a series of tests to validate the integration between
# SAP systems and GitHub repositories.
#
# Usage:
#   ./integration-test.sh --sap-system=<system_id> --git-repo=<repo_url>
#                        [--test-type=<basic|full>] [--report-format=<text|json>]
#
# Author: SAP-GitHub Integration Team
# Version: 1.0.0

set -e

# Default values
TEST_TYPE="basic"
REPORT_FORMAT="text"
TIMEOUT=300  # 5 minutes
LOG_FILE="integration-test.log"
RESULT_FILE="test-results.json"

# Color codes for output formatting
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
NC='\033[0m' # No Color

# Test statistics
TESTS_TOTAL=0
TESTS_PASSED=0
TESTS_FAILED=0
TESTS_SKIPPED=0
START_TIME=$(date +%s)

# Print usage information
function print_usage() {
    echo "Usage: $0 --sap-system=<system_id> --git-repo=<repo_url> [--test-type=<basic|full>] [--report-format=<text|json>]"
    echo ""
    echo "Options:"
    echo "  --sap-system     SAP system ID to test against"
    echo "  --git-repo       GitHub repository URL"
    echo "  --test-type      Test type: 'basic' or 'full' (default: basic)"
    echo "  --report-format  Report format: 'text' or 'json' (default: text)"
    echo "  --help           Display this help message"
    echo ""
    echo "Example:"
    echo "  $0 --sap-system=DEV --git-repo=https://github.com/org/repo --test-type=full"
}

# Parse command line arguments
function parse_arguments() {
    for arg in "$@"; do
        case $arg in
            --sap-system=*)
                SAP_SYSTEM="${arg#*=}"
                shift
                ;;
            --git-repo=*)
                GIT_REPO="${arg#*=}"
                shift
                ;;
            --test-type=*)
                TEST_TYPE="${arg#*=}"
                shift
                ;;
            --report-format=*)
                REPORT_FORMAT="${arg#*=}"
                shift
                ;;
            --help)
                print_usage
                exit 0
                ;;
            *)
                echo "Unknown argument: $arg"
                print_usage
                exit 1
                ;;
        esac
    done

    # Validate required arguments
    if [ -z "$SAP_SYSTEM" ] || [ -z "$GIT_REPO" ]; then
        echo "Error: Missing required arguments"
        print_usage
        exit 1
    fi

    # Validate test type
    if [ "$TEST_TYPE" != "basic" ] && [ "$TEST_TYPE" != "full" ]; then
        echo "Error: Invalid test type '$TEST_TYPE'. Must be 'basic' or 'full'."
        exit 1
    fi

    # Validate report format
    if [ "$REPORT_FORMAT" != "text" ] && [ "$REPORT_FORMAT" != "json" ]; then
        echo "Error: Invalid report format '$REPORT_FORMAT'. Must be 'text' or 'json'."
        exit 1
    fi
}

# Log message to file and stdout
function log() {
    local level=$1
    local message=$2
    local timestamp=$(date '+%Y-%m-%d %H:%M:%S')
    
    echo "[$timestamp] [$level] $message" >> "$LOG_FILE"
    
    case $level in
        INFO)
            echo -e "[${GREEN}INFO${NC}] $message"
            ;;
        ERROR)
            echo -e "[${RED}ERROR${NC}] $message"
            ;;
        WARNING)
            echo -e "[${YELLOW}WARNING${NC}] $message"
            ;;
        *)
            echo "[$level] $message"
            ;;
    esac
}

# Execute a test case and record result
function run_test() {
    local test_name=$1
    local test_command=$2
    local timeout_seconds=${3:-$TIMEOUT}
    
    log "INFO" "Running test: $test_name"
    
    TESTS_TOTAL=$((TESTS_TOTAL + 1))
    
    local start_time=$(date +%s)
    local result=0
    local output=""
    
    # Execute the test with a timeout
    output=$(timeout $timeout_seconds bash -c "$test_command" 2>&1) || result=$?
    
    local end_time=$(date +%s)
    local duration=$((end_time - start_time))
    
    if [ $result -eq 0 ]; then
        log "INFO" "Test '$test_name' passed in $duration seconds"
        TESTS_PASSED=$((TESTS_PASSED + 1))
    elif [ $result -eq 124 ]; then
        log "ERROR" "Test '$test_name' timed out after $timeout_seconds seconds"
        TESTS_FAILED=$((TESTS_FAILED + 1))
    else
        log "ERROR" "Test '$test_name' failed with exit code $result in $duration seconds"
        log "ERROR" "Output: $output"
        TESTS_FAILED=$((TESTS_FAILED + 1))
    fi
    
    # Record test result
    TEST_RESULTS+=("{\"name\":\"$test_name\",\"result\":\"$([ $result -eq 0 ] && echo "pass" || echo "fail")\",\"duration\":$duration,\"exit_code\":$result}")
}

# Skip a test and record it
function skip_test() {
    local test_name=$1
    local reason=$2
    
    log "WARNING" "Skipping test: $test_name - $reason"
    
    TESTS_TOTAL=$((TESTS_TOTAL + 1))
    TESTS_SKIPPED=$((TESTS_SKIPPED + 1))
    
    # Record test result
    TEST_RESULTS+=("{\"name\":\"$test_name\",\"result\":\"skip\",\"reason\":\"$reason\"}")
}

# Test SAP connectivity
function test_sap_connectivity() {
    log "INFO" "Testing SAP connectivity to $SAP_SYSTEM"
    
    # This is a placeholder. In a real script, use appropriate SAP connection tool
    # For example, using SAP RFC SDK or similar
    if [ "$SAP_SYSTEM" = "TEST" ]; then
        # Simulating a successful connection for demo
        return 0
    else
        # Try to actually connect in a real implementation
        log "INFO" "Attempting to connect to SAP system $SAP_SYSTEM"
        return 0
    fi
}

# Test GitHub connectivity
function test_github_connectivity() {
    log "INFO" "Testing GitHub connectivity to $GIT_REPO"
    
    # Extract owner and repo from the URL
    local repo_path=${GIT_REPO#*github.com/}
    repo_path=${repo_path%.git}
    
    # Try to access the repository API
    curl -s -o /dev/null -w "%{http_code}" \
         "https://api.github.com/repos/$repo_path" \
         -H "Accept: application/vnd.github.v3+json"
}

# Test ABAP to Git synchronization
function test_abap_to_git_sync() {
    log "INFO" "Testing ABAP to Git synchronization"
    
    # Create a test ABAP object change and verify it gets synchronized to Git
    # This is a placeholder implementation
    sleep 2
    return 0
}

# Test Git to ABAP synchronization
function test_git_to_abap_sync() {
    log "INFO" "Testing Git to ABAP synchronization"
    
    # Create a test Git commit and verify it gets synchronized to ABAP
    # This is a placeholder implementation
    sleep 2
    return 0
}

# Test transport request integration
function test_transport_integration() {
    log "INFO" "Testing transport request integration"
    
    # Create a transport request and verify GitHub integration
    # This is a placeholder implementation
    sleep 3
    return 0
}

# Generate report in requested format
function generate_report() {
    local end_time=$(date +%s)
    local total_duration=$((end_time - START_TIME))
    
    if [ "$REPORT_FORMAT" = "json" ]; then
        cat > "$RESULT_FILE" << EOF
{
  "summary": {
    "total": $TESTS_TOTAL,
    "passed": $TESTS_PASSED,
    "failed": $TESTS_FAILED,
    "skipped": $TESTS_SKIPPED,
    "duration": $total_duration,
    "sap_system": "$SAP_SYSTEM",
    "git_repo": "$GIT_REPO",
    "test_type": "$TEST_TYPE",
    "timestamp": "$(date -u +"%Y-%m-%dT%H:%M:%SZ")"
  },
  "tests": [
    $(IFS=,; echo "${TEST_RESULTS[*]}")
  ]
}
EOF
        log "INFO" "Test report saved to $RESULT_FILE"
    else
        cat << EOF

====================================
SAP-GitHub Integration Test Results
====================================

Summary:
  SAP System: $SAP_SYSTEM
  GitHub Repo: $GIT_REPO
  Test Type: $TEST_TYPE
  Duration: $total_duration seconds
  
Results:
  Total Tests: $TESTS_TOTAL
  Passed: $TESTS_PASSED
  Failed: $TESTS_FAILED
  Skipped: $TESTS_SKIPPED
  
  Success Rate: $(awk "BEGIN {printf \"%.1f%%\", ($TESTS_PASSED * 100.0 / ($TESTS_TOTAL - $TESTS_SKIPPED))}")

EOF
    fi
}

# Main function to run the tests
function main() {
    # Initialize
    TEST_RESULTS=()
    echo "SAP-GitHub Integration Test - $(date)" > "$LOG_FILE"
    log "INFO" "Starting integration tests for SAP system $SAP_SYSTEM with GitHub repo $GIT_REPO"
    log "INFO" "Test type: $TEST_TYPE, Report format: $REPORT_FORMAT"
    
    # Run connectivity tests
    run_test "SAP_Connectivity" "test_sap_connectivity"
    
    # If SAP connectivity test failed, skip the rest
    if [ $TESTS_FAILED -gt 0 ]; then
        log "ERROR" "SAP connectivity test failed, skipping remaining tests"
        skip_test "GitHub_Connectivity" "SAP connectivity failed"
        skip_test "ABAP_to_Git_Sync" "SAP connectivity failed" 
        skip_test "Git_to_ABAP_Sync" "SAP connectivity failed"
        skip_test "Transport_Integration" "SAP connectivity failed"
    else
        # Run GitHub connectivity test
        run_test "GitHub_Connectivity" "test_github_connectivity"
        
        # If GitHub connectivity test failed, skip the sync tests
        if [ $TESTS_FAILED -gt 1 ]; then
            log "ERROR" "GitHub connectivity test failed, skipping sync tests"
            skip_test "ABAP_to_Git_Sync" "GitHub connectivity failed"
            skip_test "Git_to_ABAP_Sync" "GitHub connectivity failed"
            skip_test "Transport_Integration" "GitHub connectivity failed"
        else
            # Run basic sync tests
            run_test "ABAP_to_Git_Sync" "test_abap_to_git_sync"
            run_test "Git_to_ABAP_Sync" "test_git_to_abap_sync"
            
            # Run full tests if requested
            if [ "$TEST_TYPE" = "full" ]; then
                run_test "Transport_Integration" "test_transport_integration"
            else
                skip_test "Transport_Integration" "Not included in basic test"
            fi
        fi
    fi
    
    # Generate report
    generate_report
    
    # Return exit code based on test results
    if [ $TESTS_FAILED -gt 0 ]; then
        log "ERROR" "Tests completed with $TESTS_FAILED failures"
        return 1
    else
        log "INFO" "All tests completed successfully"
        return 0
    fi
}

# Parse arguments
parse_arguments "$@"

# Run the main function
main

exit $? 