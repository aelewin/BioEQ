# Security Checklist — BioEQ Application

Use this checklist for manual verification before each deployment.

---

## Pre-Deployment Checks

### Code Security
- [ ] `shiny/app.R` does NOT contain `eval(data.script)` or any `addCustomJS` handler
- [ ] `launch_app.R` defaults to `127.0.0.1` (not `0.0.0.0`)
- [ ] `launch_app.R` reads host/port from environment variables
- [ ] Security headers present in `shiny/app.R` `tags$head()`:
  - [ ] X-Frame-Options
  - [ ] X-Content-Type-Options
  - [ ] X-XSS-Protection
  - [ ] Referrer-Policy
- [ ] No `system()` or `system2()` calls with user-supplied input
- [ ] No `eval()` or `parse()` calls on user-supplied input
- [ ] No hardcoded passwords, API keys, or secrets in source code

### File Handling
- [ ] File upload validates extension AND content (magic bytes)
- [ ] File upload enforces size limit
- [ ] Temporary directories use `tempdir()` with session-specific subdirectories
- [ ] `session$onSessionEnded()` cleanup registered

### Configuration
- [ ] `.env` file is NOT committed (check `.gitignore`)
- [ ] `.Renviron` is NOT committed
- [ ] `.env.example` exists with documented variables
- [ ] `data/` directory excluded from version control

### Dependencies
- [ ] All packages listed in DESCRIPTION
- [ ] No known CVEs in dependency versions
- [ ] `shiny` >= 1.7.0
- [ ] `DT` >= 0.20
- [ ] `digest` >= 0.6.25

### Test Suite
- [ ] All 248 tests pass: `Rscript -e 'testthat::test_dir("tests/")'`
- [ ] No test failures or skips (except documented)

---

## Production Deployment Checks

- [ ] Application behind reverse proxy with TLS/HTTPS
- [ ] Authentication enabled (if multi-user)
- [ ] Rate limiting configured on reverse proxy
- [ ] Log monitoring configured
- [ ] File upload directory has restricted permissions
- [ ] R process runs as non-root user
- [ ] Firewall rules restrict access to application port
