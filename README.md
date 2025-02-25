# 📊 ProjectTrackingApp

## 🚀 App Overview

1. **Hosting:**  
   - Currently hosted on my free **shinyapps.io** account.  
   - Plan to migrate to a paid domain (Biostats has a token for this?).

2. **Data Updates:**  
   - App updates by editing `data/projects.csv` and redeploying.  
   - Only I can redeploy due to token-based authentication.

3. **Data Storage:**  
   - Raw data and reports are hosted on **Dropbox**, which seems more manageable than HiPerGator for now.  
   - Raw sequencing files can be compressed for easier uploading.

---

## 📈 Improvements Needed

1. Host app on a **paid named domain**.  
2. Enable **Google Sheets integration** for live updates without redeployment (UFIT manages Google Cloud authentication).

---

## 🔄 How to Update Data (as-is)

### 1️⃣ Clone the Repo

```bash
# Clone the GitHub repository
git clone https://github.com/YOUR_USERNAME/YOUR_REPO.git

# Move into the project folder
cd YOUR_REPO
```

---

### 2️⃣ Edit `projects.csv`

- Open `data/projects.csv` in Excel or a text editor.
- Make desired updates (add new projects, update statuses, etc.).
- Save the file.

---

### 3️⃣ Commit and Push Changes

```bash
# Stage the updated CSV
git add data/projects.csv

# Commit the change with a message
git commit -m "Update project tracking CSV"

# Push changes to the remote GitHub repository
git push origin main

# Email me so I can redeploy
[hkates@ufl.edu](mailto:hkates@ufl.edu)
```

---

## 🚀 How to Redeploy App

1. Pull the latest changes from GitHub:

```bash
git pull origin main
```

2. Redeploy the app to ShinyApps.io from your R session:

```r
library(rsconnect)
rsconnect::deployApp()
```

---

## 📬 Contact

For issues or questions, contact **BCB-SR** at [hkates@ufl.edu](mailto:hkates@ufl.edu).

