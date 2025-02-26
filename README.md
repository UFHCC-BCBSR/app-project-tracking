# 📊 ProjectTrackingApp

## 🚀 App Overview

1. **Hosting:**  
   - Hosted on the BCB-SR shinyapp.io https://ufhcc-bqs.shinyapps.io/projecttrackingapp/
   - Account, token, and secret are in the repo so you should not need to authenticate before deploying.

2. **Data Updates:**  
   - App updates automatically when project csv files are edited in https://www.dropbox.com/scl/fo/6wz676apw47spbbt9zcv3/AEsFkqN560lTK6xae7rcoqM?rlkey=h8m8qz7vks0ur2coldl7txdon&st=y6jtdzy2&dl=0
   - To edit csv files in dropbox you *must* have Dropbox desktop installed and syncing (at least this folder, anyway). csv files cannot be edited directly in dropbox online.
   - To add PIs and change fields in the project spreadsheet (or make other changes to the site), follow steps in "How to Update" below.

3. **Data Storage:**  
   - Raw data and reports are hosted on **Dropbox**, which seems more manageable than HiPerGator. You can use any link you want, though. OneDrive should work too. Set permissions with usual care.
   - Raw sequencing files can be compressed and easily copied from /orange to dropbox using globus and dropbox Desktop.

---

## 🔄 How to Update Data (as-is)

### 1️⃣ Clone the Repo

```bash
# Clone the GitHub repository
git clone https://github.com/HeatherKates/ProjectTrackingApp.git

# Move into the project folder
cd ProjectTrackingApp
```

---

### 2️⃣ Edit `projects.csv`

- Make desired updates (add new PIs, change PI passwords, update project fields, change visuals, etc.).

---

### 3️⃣ Commit and Push Changes

```bash
# Stage the updated files (whichever you have updated)
git add app.R
git add scripts/init_db.R

# Commit the change with a message
git commit -m "Add a notes field"

# Push changes to the remote GitHub repository
git push origin main

# Redeploy
library(rsconnect)
rsconnect::deployApp()

```

---

## 🚀 Subsequent redeployments 

1. Pull the latest changes from GitHub:

```bash
git pull origin main
```

2. Make changes if needed following steps above.

3. Redeploy the app to ShinyApps.io from your R session:

```r
library(rsconnect)
rsconnect::deployApp()
```

---

## 📬 Contact

For issues or questions, contact **BCB-SR** at [hkates@ufl.edu](mailto:hkates@ufl.edu).

