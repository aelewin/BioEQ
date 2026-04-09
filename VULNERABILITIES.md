# Security Vulnerability Report — BioEQ Shiny Application

**Date:** 2026-04-09
**Auditor:** Copilot Security Engineer
**Application:** BioEQ v1.0.0

---

## Executive Summary

A comprehensive security audit of the BioEQ R Shiny application identified **9 vulnerabilities** across 4 severity levels. The most critical finding is an unused JavaScript `eval()` handler that could enable arbitrary code execution in the browser context. No SQL injection vulnerabilities were found since the application uses only in-memory data frames. No hardcoded secrets were found in the codebase.

| Severity | Count |
|----------|-------|
| CRITICAL | 1 |
| HIGH     | 1 |
| MEDIUM   | 4 |
| LOW      | 3 |

---

## Vulnerability Details

### VULN-001: JavaScript eval() Handler (CRITICAL)

- **CWE:** CWE-95 (Improper Neutralization of Directives in Dynamically Evaluated Code)
- **File:** `shiny/app.R`, lines 217–220
- **Severity:** CRITICAL
- **CVSS:** 9.8

**Vulnerable Code:**
```javascript
Shiny.addCustomMessageHandler('addCustomJS', function(data) {
  eval(data.script);
});
```

**Description:** The `addCustomJS` custom message handler calls `eval()` on arbitrary JavaScript sent from the server. Although currently unused, its presence creates a code execution attack surface.

**Impact:** An attacker who compromises server code or injects a `session$sendCustomMessage("addCustomJS", ...)` call could execute arbitrary JavaScript in every connected browser.

**Remediation:** Remove the unused `addCustomJS` handler entirely.

---

### VULN-002: Network Binding to All Interfaces (HIGH)

- **CWE:** CWE-284 (Improper Access Control)
- **File:** `launch_app.R`, line 21
- **Severity:** HIGH
- **CVSS:** 7.5

**Vulnerable Code:**
```r
shiny::runApp(host = "0.0.0.0", port = 4000, launch.browser = TRUE)
```

**Description:** The application binds to `0.0.0.0`, exposing it on all network interfaces without any authentication. Any machine on the network can access, upload files, and run analyses.

**Impact:** Unauthorized access, data exfiltration, denial of service.

**Remediation:** Default to `127.0.0.1` (localhost). Use an environment variable to allow override when behind an authenticated reverse proxy.

---

### VULN-003: Missing HTTP Security Headers (MEDIUM)

- **CWE:** CWE-693 (Protection Mechanism Failure)
- **File:** `shiny/app.R`
- **Severity:** MEDIUM
- **CVSS:** 5.3

**Description:** No security-related HTTP headers are set (Content-Security-Policy, X-Frame-Options, X-Content-Type-Options, X-XSS-Protection, Referrer-Policy).

**Impact:** Susceptible to clickjacking, MIME-sniffing attacks, and lack of XSS defense-in-depth.

**Remediation:** Add `<meta>` security headers in the UI `tags$head()`.

---

### VULN-004: Insufficient File Upload Validation (MEDIUM)

- **CWE:** CWE-434 (Unrestricted Upload of File with Dangerous Type)
- **File:** `shiny/server/data_upload_server.R`, lines 399–435
- **Severity:** MEDIUM
- **CVSS:** 5.0

**Description:** File upload validation relies solely on file extension string matching. There is no MIME type or content-based validation.

**Impact:** A crafted file with a `.csv` extension but malicious content could be processed.

**Remediation:** Add content-based file validation and tighter size limits.

---

### VULN-005: Insecure Temporary File Handling (MEDIUM)

- **CWE:** CWE-377 (Insecure Temporary File)
- **File:** `shiny/server/plots_server.R`, lines 44–51
- **Severity:** MEDIUM
- **CVSS:** 4.3

**Description:** Session-specific temp directories are created but cleanup depends on clean session termination. Crashed sessions may leave data behind.

**Impact:** Disk exhaustion, information leakage from stale analysis results.

**Remediation:** Implement `session$onSessionEnded()` cleanup callback.

---

### VULN-006: No CSRF Protection Configured (MEDIUM)

- **CWE:** CWE-352 (Cross-Site Request Forgery)
- **File:** `shiny/app.R`
- **Severity:** MEDIUM
- **CVSS:** 4.3

**Description:** No explicit CSRF protection or SameSite cookie configuration is present.

**Impact:** If exposed on a network, cross-site request forgery attacks could trigger unintended actions.

**Remediation:** Rely on Shiny's built-in CSRF protection and document it; add SameSite cookie guidance in deployment docs.

---

### VULN-007: Relative Path Source Statements (LOW)

- **CWE:** CWE-426 (Untrusted Search Path)
- **Files:** `shiny/app.R` (lines 81–91), `shiny/server/*.R`
- **Severity:** LOW
- **CVSS:** 3.7

**Description:** Source statements use relative paths (`../R/...`), which depend on the correct working directory being set by `launch_app.R`.

**Impact:** If the working directory is changed, a different (potentially malicious) file could be sourced.

**Remediation:** Document the expected execution context; consider using `normalizePath()` for path resolution.

---

### VULN-008: Missing .gitignore Entries for Secrets (LOW)

- **CWE:** CWE-312 (Cleartext Storage of Sensitive Information)
- **File:** `.gitignore`
- **Severity:** LOW
- **CVSS:** 3.1

**Description:** The `.gitignore` does not exclude `.env`, `.Renviron`, or other common secrets files.

**Impact:** Environment files containing secrets could be accidentally committed.

**Remediation:** Add `.env`, `.Renviron`, `config/`, and `secrets/` to `.gitignore`.

---

### VULN-009: No Output Encoding Verification (LOW)

- **CWE:** CWE-79 (Cross-Site Scripting)
- **File:** `shiny/server/data_upload_server.R`
- **Severity:** LOW
- **CVSS:** 2.1

**Description:** While Shiny's `renderText()` auto-escapes HTML by default, uploaded file column names are displayed without explicit escaping verification.

**Impact:** Extremely low risk since Shiny framework handles escaping, but defense-in-depth is advisable.

**Remediation:** Create explicit output escaping utilities and use them for any user-derived content.

---

## Secure Findings (No Vulnerabilities)

| Category | Status | Notes |
|----------|--------|-------|
| SQL Injection | ✅ N/A | No database connections; in-memory data frames only |
| Command Injection | ✅ Safe | No `system()` or `system2()` calls |
| Path Traversal (uploads) | ✅ Safe | Shiny manages upload temp paths automatically |
| XSS in Data Tables | ✅ Safe | `DT::datatable()` escapes HTML by default |
| eval/parse on user input | ✅ Safe | No R-side `eval()` or `parse()` on user input |
| Hardcoded Credentials | ✅ Clean | No API keys, passwords, or secrets in code |
