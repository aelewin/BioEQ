# Incident Response Plan — BioEQ Application

## Purpose

This document outlines the response procedures if a security vulnerability is discovered in the BioEQ application after deployment.

---

## Severity Classification

| Level | Description | Response Time |
|-------|-------------|---------------|
| **Critical** | Remote code execution, data breach, authentication bypass | Immediate (< 1 hour) |
| **High** | Unauthorized access, data exposure, injection vulnerability | < 4 hours |
| **Medium** | Information disclosure, DoS, missing security controls | < 24 hours |
| **Low** | Minor configuration issue, informational finding | < 1 week |

---

## Immediate Response Steps

### 1. Contain the Incident

- **If critical/high severity:**
  - Take the application offline immediately
  - Revoke any exposed credentials
  - Preserve logs for forensic analysis

```bash
# Stop the Shiny app
# If running directly:
kill $(pgrep -f "shiny::runApp")

# If running via systemd:
sudo systemctl stop bioeq
```

### 2. Assess the Impact

- Identify which data may have been accessed
- Check application logs for suspicious activity
- Review file upload directory for malicious files
- Check for unauthorized temp directory contents

```bash
# Check for suspicious uploads
find /tmp/bioeq_plots -type f -newer /path/to/last-known-good-timestamp

# Check R logs
tail -100 /var/log/shiny-server/*.log
```

### 3. Document the Incident

Record the following in an incident report:
- Date and time of discovery
- How the vulnerability was found
- Affected components (files, functions, data)
- Potential impact assessment
- Steps taken to contain

---

## Remediation Procedures

### For Code Vulnerabilities

1. Create a fix on a new branch
2. Run the full security test suite:
   ```bash
   Rscript -e 'library(testthat); test_dir("tests/")'
   ```
3. Add a new test case that specifically tests the new vulnerability
4. Verify all 248+ tests pass
5. Review the fix (peer review required for critical/high)
6. Deploy the fix

### For Dependency Vulnerabilities

1. Identify the affected package and CVE
2. Check if an update is available:
   ```r
   old.packages()
   ```
3. Update the package:
   ```r
   install.packages("affected_package")
   ```
4. Update DESCRIPTION if version is pinned
5. Run full test suite to verify no regressions
6. Deploy the update

### For Configuration Issues

1. Update the configuration (environment variables, .gitignore, etc.)
2. Rotate any exposed secrets
3. Verify with `test_security_config.R`
4. Deploy the fix

---

## Post-Incident Actions

### Within 24 Hours
- [ ] Incident report filed
- [ ] Root cause identified
- [ ] Fix deployed and verified
- [ ] Affected users notified (if data was exposed)

### Within 1 Week
- [ ] Post-incident review conducted
- [ ] Security test suite updated with new test cases
- [ ] VULNERABILITIES.md updated
- [ ] Documentation updated
- [ ] Lessons learned documented

### Within 1 Month
- [ ] Full security audit repeated
- [ ] CI/CD pipeline updated if needed
- [ ] Training conducted if human error was involved

---

## Contact Information

| Role | Contact |
|------|---------|
| Application Owner | dev@bioeq.org |
| Security Lead | (Update with actual contact) |
| System Administrator | (Update with actual contact) |

---

## Useful Commands

```bash
# Check if app is running
pgrep -f "shiny::runApp"

# Check listening ports
ss -tlnp | grep 4000

# View recent logs
tail -f /var/log/shiny-server/*.log

# Run security tests
cd /path/to/BioEQ && Rscript -e 'library(testthat); test_dir("tests/")'

# Check for hardcoded secrets
grep -rn "password\|secret\|api_key" --include="*.R" . | grep -v test | grep -v "#"

# Verify no eval() handler
grep -rn "eval(data" shiny/app.R
```
