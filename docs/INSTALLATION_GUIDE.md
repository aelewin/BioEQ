# BioEQ Installation Guide

A step-by-step guide for installing BioEQ on Windows or macOS.
**No prior R or programming experience is required.** Just follow the steps in order.

> Total time: **15–25 minutes** (most of it is waiting for downloads).
> You will need an active **internet connection** for the entire install.

---

## What You Will Install

To run BioEQ you need **three things**, in this order:

1. **R** — the programming language BioEQ is written in.
2. **RStudio** — a friendly window for running R (much easier than a black terminal).
3. **BioEQ** — the application itself, plus a list of helper packages.

Think of it like this: **R is the engine**, **RStudio is the dashboard**, and **BioEQ is the car**. You need all three.

---

## Before You Start: Pick a Place to Save Files

This is the #1 thing that trips people up. Save BioEQ in a simple, short folder path that does **not** contain spaces, accents, or special characters, and that is **not** inside OneDrive, iCloud Drive, Google Drive, or Dropbox.

**Recommended location:**

| Operating System | Recommended folder | What to type |
|---|---|---|
| **Windows** | `C:\BioEQ` | Open File Explorer → click `This PC` → double-click `Local Disk (C:)` → right-click empty space → **New** → **Folder** → name it `BioEQ` |
| **macOS** | `/Users/YourName/BioEQ` (your home folder) | Open Finder → press **Cmd + Shift + H** to go to your home folder → right-click empty space → **New Folder** → name it `BioEQ` |

**Avoid these locations:**

- ❌ Desktop *if* your Desktop syncs to OneDrive / iCloud (very common on Windows)
- ❌ `Documents` folder *if* it syncs to OneDrive / iCloud
- ❌ Any path with spaces or special characters (e.g. `C:\My Files\BioEQ — Copy`)
- ❌ A USB stick or external drive
- ❌ Inside a folder you don't have permission to write to (e.g. `Program Files`)

> **Why?** Cloud-syncing folders (OneDrive, iCloud, Dropbox) sometimes lock or move files in the background while R is using them. This is the most common cause of "package install failed" errors.

---

# Step 1 — Install R and RStudio

You only do this once per computer.

## 1.1 Install R

1. Open your web browser and go to: **https://cran.r-project.org/**
2. Click the link for your operating system:
   - Windows: **Download R for Windows**
   - macOS: **Download R for macOS**
3. **On Windows:** Click **base**, then click the big **Download R-x.x.x for Windows** link at the top of the next page. Save the `.exe` file and double-click it to run the installer.
4. **On macOS:** Click the `.pkg` file that matches your Mac:
   - **Apple Silicon** Macs (M1, M2, M3, M4 — bought 2020 or later): pick the file labeled **arm64** (e.g. `R-4.x.x-arm64.pkg`).
   - **Intel** Macs (older): pick the file **without** "arm64" in the name.
   - Not sure? Click the Apple menu (top-left) → **About This Mac**. If it says "Apple M1/M2/M3/M4" → arm64. If it says "Intel" → the non-arm64 file.
5. Run the installer and **accept all the defaults** by clicking Next/Continue/Install on every screen. Do not change the install location.
6. When it finishes, you can close the installer. You will not open R directly — you will use RStudio in the next step.

## 1.2 Install RStudio

1. Go to: **https://posit.co/download/rstudio-desktop/**
2. Scroll down to the table titled **"All Installers and Tarballs"**.
3. Click the row for your operating system:
   - **Windows 10/11**: download the `.exe` file
   - **macOS 12+**: download the `.dmg` file
4. **Windows:** double-click the `.exe` file → click Next/Install on every screen → Finish.
5. **macOS:** double-click the `.dmg` file → drag the **RStudio** icon into the **Applications** folder when the window opens. Then close the window and **eject** the disk image (right-click the RStudio disk icon on your desktop → Eject).
6. Open RStudio:
   - **Windows:** Start menu → type `RStudio` → click it.
   - **macOS:** Open Finder → Applications → double-click **RStudio**. (If macOS warns "RStudio cannot be opened because Apple cannot check it for malicious software", click **OK**, then go to **System Settings → Privacy & Security**, scroll to the bottom, and click **Open Anyway**.)

When RStudio opens you should see a window split into panes. The big pane on the left labeled **Console** is where you will paste commands.

> ✅ **Checkpoint:** In the Console pane, type `R.version.string` and press **Enter**. You should see something like `R version 4.4.x ...`. If you do, R and RStudio are installed correctly. If you get an error, restart RStudio and try again.

---

# Step 2 — Install BioEQ Dependencies and Launch the App

## 2.1 Download BioEQ

You can do this with a simple ZIP download — no Git knowledge needed.

1. Go to the BioEQ GitHub page in your web browser.
2. Click the green **`< > Code`** button (near the top of the file list).
3. Click **Download ZIP** at the bottom of the menu that opens.
4. Move the downloaded ZIP file to your `BioEQ` folder (`C:\BioEQ` on Windows or `~/BioEQ` on macOS — see [Before You Start](#before-you-start-pick-a-place-to-save-files)).
5. **Unzip** it:
   - **Windows:** right-click the ZIP → **Extract All...** → set the destination to `C:\BioEQ` → click **Extract**.
   - **macOS:** double-click the ZIP file. It will unzip itself next to the original.
6. After unzipping, you should have a folder like `C:\BioEQ\BioEQ-main` (Windows) or `~/BioEQ/BioEQ-main` (macOS) containing files like `install_dependencies.R`, `launch_app.R`, and folders called `R`, `shiny`, `validation`.

> 💡 **Tip:** Rename the unzipped folder from `BioEQ-main` to just `BioEQ` to make the path shorter and easier to type. The final path should look like `C:\BioEQ\BioEQ` (Windows) or `~/BioEQ/BioEQ` (macOS). The exact name doesn't matter as long as you remember it.

## 2.2 Tell RStudio Where BioEQ Lives

This is the second-most-common point of confusion. R needs to know where the BioEQ files are.

1. In RStudio, click the menu **Session → Set Working Directory → Choose Directory...**
2. Navigate to the folder you unzipped (e.g. `C:\BioEQ\BioEQ` or `~/BioEQ/BioEQ`).
3. Click **Open** / **Choose**.

You should now see a line in the Console that looks like:

```r
setwd("C:/BioEQ/BioEQ")     # Windows example
setwd("/Users/yourname/BioEQ/BioEQ")   # macOS example
```

To double-check you're in the right place, type this in the Console and press Enter:

```r
list.files()
```

You should see `install_dependencies.R`, `launch_app.R`, `README.md`, `R`, `shiny`, and so on. **If you don't see those files, you are in the wrong folder — repeat step 1.**

## 2.3 Install BioEQ's Required Packages

BioEQ needs about 30 helper packages from the R community. The script `install_dependencies.R` installs them all for you.

1. In the RStudio Console, paste the line below and press **Enter**:

   ```r
   source("install_dependencies.R")
   ```

2. Wait. This step downloads and compiles many packages and **can take 5–15 minutes** the first time. You will see a lot of text scroll past — that's normal. Do **not** close RStudio.

3. **If you are on macOS** and RStudio asks
   *"Do you want to install from sources the package which needs compilation?"*
   → type **`no`** and press Enter. (Pre-built binaries are faster and avoid needing developer tools.)

4. When it finishes, you should see a line like:

   ```
   ✓ All required packages verified.
   ```

   If you see ✗ or "failed to load" for any package, jump to [Troubleshooting](#troubleshooting) below.

## 2.4 Launch the App

In the same RStudio Console, paste this line and press **Enter**:

```r
shiny::runApp("shiny", host = "127.0.0.1", port = 4000, launch.browser = TRUE)
```

After 5–20 seconds your default web browser should open automatically and load BioEQ at the address **http://127.0.0.1:4000**. You should see the BioEQ home page with a navigation sidebar on the left.

🎉 **You're done.** Use BioEQ as long as you want — close the browser tab when finished.

## 2.5 Stopping and Re-Launching the App

- **To stop the app:** click back into RStudio and press the red **STOP** sign at the top right of the Console (or press **Esc**).
- **To re-launch later:** open RStudio → run **Session → Set Working Directory → Choose Directory...** to point at your BioEQ folder again → paste the `shiny::runApp(...)` line from step 2.4. You do **not** need to re-run `install_dependencies.R` — packages stay installed.

---

# Troubleshooting

### "Cannot open file 'install_dependencies.R': No such file or directory"
You are not in the BioEQ folder. Go back to **Step 2.2** and set the working directory.

### "Package 'XYZ' is not available for this version of R"
Your R is too old. Update R: re-run **Step 1.1** to install the latest version. Then restart RStudio and re-run `source("install_dependencies.R")`.

### "Error in install.packages: cannot open URL ..."
Internet problem or firewall. Check your connection. If you're on a corporate or university network, the firewall may block CRAN — try a personal Wi-Fi or hotspot.

### Install gets stuck on one package, or you see "binary version is available but the source version is later — Do you want to install from sources?"
Type **`no`** and press Enter. The pre-built ("binary") version is fine.

### macOS only — "xcrun: error: invalid active developer path" or "make: command not found"
A package wants to compile from source and needs Xcode Command Line Tools. Easiest fix: type `no` whenever R asks "install from sources". If you really need them, open the **Terminal** app (Applications → Utilities → Terminal) and run:
```
xcode-select --install
```
A pop-up will appear; click **Install** and wait. Then retry `source("install_dependencies.R")`.

### Windows only — "Rtools is required to build R packages"
Same fix: type `no` when asked to install from sources. You don't need Rtools to use BioEQ.

### "Address already in use" or "Port 4000 is already in use"
The app is already running in another R session, or another program is using the port. Either close the other RStudio session, or change the port number to anything between 3000 and 8000:
```r
shiny::runApp("shiny", host = "127.0.0.1", port = 4567, launch.browser = TRUE)
```

### App launches but the page is blank / never loads
Wait 30 seconds (first launch is slow). Then refresh the browser tab. If still blank, stop the app, close and reopen RStudio, and try Step 2.4 again.

### "could not find function 'runApp'" or similar
The packages didn't install correctly. Re-run:
```r
source("install_dependencies.R")
```
and read the output for any ✗ marks. Re-install any failed packages individually:
```r
install.packages("name_of_failed_package")
```

### Word (`.docx`) report export doesn't work
Word reports are optional. To enable them, run:
```r
install.packages(c("officer", "flextable"))
```
Then restart the app. PDF and HTML reports work without these.

### Still stuck?
Take a screenshot of the **last 30 lines of red text in the RStudio Console**, plus the output of:
```r
sessionInfo()
getwd()
list.files()
```
…and share those with whoever is helping you.

---

# Appendix — Quick Reference Card

Once everything is installed, this is your day-to-day cheat sheet:

```r
# 1. Open RStudio
# 2. Set working directory:
#    Session → Set Working Directory → Choose Directory... → pick BioEQ folder
# 3. Launch:
shiny::runApp("shiny", host = "127.0.0.1", port = 4000, launch.browser = TRUE)
# 4. Use the app in your browser.
# 5. Stop it: click the red STOP sign in RStudio (or press Esc).
```

That's the whole loop.
