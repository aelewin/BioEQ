# Deployment Validation Guide

Verification steps to confirm security fixes are active in production.

---

## Step 1: Verify eval() Handler Removed

**Check:** Open browser developer tools (F12), go to Console tab, type:
```javascript
Shiny.shinyapp.$inputValues
```
Then verify there is no `addCustomJS` handler by searching the page source for `eval(data.script)`.

**Expected:** No `addCustomJS` handler exists.

---

## Step 2: Verify Network Binding

**Check:** On the deployment server, run:
```bash
ss -tlnp | grep 4000
# or
netstat -tlnp | grep 4000
```

**Expected:** If running directly (not behind proxy), should show `127.0.0.1:4000`, NOT `0.0.0.0:4000`.

If behind a proxy, `0.0.0.0:4000` is acceptable only if the proxy handles authentication.

---

## Step 3: Verify Security Headers

**Check:** Use browser developer tools → Network tab → reload the page → click the main document request → check Response Headers.

Or use curl:
```bash
curl -I http://localhost:4000
```

**Expected headers in page source (meta tags):**
- `X-Frame-Options: SAMEORIGIN`
- `X-Content-Type-Options: nosniff`
- `X-XSS-Protection: 1; mode=block`

---

## Step 4: Verify File Upload Restrictions

**Check:** Try uploading:
1. A `.exe` file → should be rejected with "Unsupported file format"
2. A file > 50MB → should be rejected with "File too large"
3. A valid `.csv` file → should be accepted

---

## Step 5: Verify No Secrets in Source

**Check:**
```bash
grep -rn "password\|secret\|api_key" --include="*.R" --include="*.js" . | grep -v test | grep -v "#"
```

**Expected:** No results (or only environment variable lookups via `Sys.getenv`).

---

## Step 6: Run Test Suite

**Check:**
```bash
cd /path/to/BioEQ
Rscript -e 'library(testthat); test_dir("tests/")'
```

**Expected:** All 248 tests pass with 0 failures.

---

## Step 7: Verify .gitignore

**Check:**
```bash
cat .gitignore | grep -E "\.env|\.Renviron|secrets"
```

**Expected:** `.env`, `.Renviron`, and `secrets/` are listed.

---

## Step 8: Verify HTTPS (Production Only)

**Check:**
```bash
curl -v https://your-domain.com 2>&1 | grep "SSL connection"
```

**Expected:** Valid SSL/TLS connection established.

---

## Automated Validation Script

Save and run this from the project root:

```bash
#!/bin/bash
echo "=== BioEQ Deployment Validation ==="

echo -n "1. eval() handler removed: "
grep -q "eval(data.script)" shiny/app.R && echo "FAIL" || echo "PASS"

echo -n "2. Default host is 127.0.0.1: "
grep -q "127.0.0.1" launch_app.R && echo "PASS" || echo "FAIL"

echo -n "3. Security headers present: "
grep -q "X-Frame-Options" shiny/app.R && echo "PASS" || echo "FAIL"

echo -n "4. .env in .gitignore: "
grep -q ".env" .gitignore && echo "PASS" || echo "FAIL"

echo -n "5. No hardcoded secrets: "
SECRETS=$(grep -rn "password\s*=\s*['\"]" --include="*.R" . | grep -v test | grep -v "^#" | grep -v "Sys.getenv" | wc -l)
[ "$SECRETS" -eq 0 ] && echo "PASS" || echo "FAIL ($SECRETS found)"

echo "=== Done ==="
```
