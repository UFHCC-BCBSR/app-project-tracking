# 📊 ProjectTrackingApp

## 🚀 App Overview

1. **Hosting:**  
   - Hosted on the BCB-SR shinyapp.io https://ufhcc-bqs.shinyapps.io/projecttrackingapp/
   - Contact me if you need account, token, and secret to authenticate before re-deploying.

2. **Data Updates:**  
   - App updates automatically when project xlsx files are edited in https://www.dropbox.com/scl/fo/6wz676apw47spbbt9zcv3/AEsFkqN560lTK6xae7rcoqM?rlkey=h8m8qz7vks0ur2coldl7txdon&st=y6jtdzy2&dl=0
   - To add PIs and change fields in the project spreadsheet (or make other changes to the site), follow steps in "How to Update" below and/or open an issue.

3. **Data Storage:**  
   - Raw data and reports are hosted on **Dropbox**, which seems more manageable than HiPerGator. You can use any link you want, though. OneDrive should work too. Set permissions with usual care.
   - Raw sequencing files can be easily copied from /orange to dropbox using globus and dropbox Desktop (files will transfer to your local machine but can be made online only immediately after transfer compeltes.)

---

## 🔄 How to Update

### 1️⃣ Clone the Repo

```bash
# Clone the GitHub repository
git clone https://github.com/HeatherKates/ProjectTrackingApp.git

# Move into the project folder
cd ProjectTrackingApp
```

---

### 2️⃣ Edit `project files

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

