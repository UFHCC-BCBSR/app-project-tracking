# scripts/init_db.R
library(DBI)
library(RSQLite)

conn <- dbConnect(SQLite(), "data/projects.db")
dbExecute(conn, "DROP TABLE IF EXISTS Projects")
dbExecute(conn, "
CREATE TABLE Projects (
  ProjectID INTEGER PRIMARY KEY,
  StudyContact TEXT,
  Bioinformatician TEXT,
  RawData TEXT,
  Report TEXT
)")

# Example Data
example_data <- data.frame(
  ProjectID = "NSXXXX",
  StudyContact = "LichtLabExample@ufl.edu",
  Bioinformatician = "hkates@ufl.edu",
  Status="Data Received",
  RawData = "https://www.dropbox.com/scl/fi/pvouv80g6b4v464gr9i2h/P3_fastq.tar.gz?rlkey=t5zltvymtquwo9yjwebh3rjoc&st=1zfpyjhi&dl=0",
  Report = "https://www.dropbox.com/scl/fi/eqb0t8pt5ah55yycynjf0/Czyz_02132024.Report-1.html?rlkey=hf1x7mjlixhcs3xcfqo1ocgs3&st=sgnkh218&dl=0"
)
dbWriteTable(conn, "Projects", example_data, append = TRUE, row.names = FALSE)
dbDisconnect(conn)

# Ensure the app directory and run `Rscript scripts/init_db.R` to set up the database.
# Then, run `Rscript app.R` to launch the app.

