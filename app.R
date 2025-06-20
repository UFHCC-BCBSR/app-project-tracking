library(shiny)
library(DT)
library(shinyjs)
library(httr)
library(readr)
library(readxl)
library(shinythemes)

# Dropbox File URLs for each PI
#pi_csv_urls <- list(
#  "Licht" = "https://www.dropbox.com/scl/fi/rtl1ugx5q88jdbn75p5cl/Licht.xlsx?rlkey=gkx0fs5kgxovlq83hvopo9h1j&st=24l84lb8&raw=1",
#  "Sharma" = "https://www.dropbox.com/scl/fi/pwxbohyjenyjw7wbqrzjb/Sharma.xlsx?rlkey=8lwi7t58bhmv7r7jhvmjben4z&st=zu3mb9e3&raw=1",
#  "Zhang"  = "https://www.dropbox.com/scl/fi/xp4apspwizjfnu8f447s6/Zhang.xlsx?rlkey=pme7lzpqzjw7rselrwjkcv9nl&st=qup96sxl&raw=1",
#  "Xing" = "https://www.dropbox.com/scl/fi/hmje516zfe9eqmehpiloz/Xing.xlsx?rlkey=umh6hahlzt7lq5bpkbbo9xyug&st=zsqj5rdw&raw=1"
#)
multi_pi_excel_url <- "https://www.dropbox.com/scl/fi/25m6c712m20wsomj9kf7o/PI_Projects.xlsx?rlkey=txap5o2fs7kxqz0ii0zw27lll&raw=1"
load_pi_sheets_and_credentials <- function(url) {
  # Download the file locally
  temp_file <- tempfile(fileext = ".xlsx")
  download.file(url, temp_file, mode = "wb")
  
  # Get sheet names
  sheet_names <- excel_sheets(temp_file)
  
  # Read all sheets into a named list
  sheet_data <- lapply(sheet_names, function(sheet) {
    read_xlsx(temp_file, sheet = sheet, col_names = TRUE)
  })
  names(sheet_data) <- sheet_names
  
  # Generate credentials based on order
  creds <- setNames(
    paste0("pass", seq_along(sheet_names)),
    sheet_names
  )
  
  return(list(data = sheet_data, credentials = creds))
}

# User credentials
valid_users <- list(
  "Licht"  = "pass1",
  "Sharma" = "pass2",
  "Zhang"  = "pass3L",
  "Xing"="pass4",
  "admin"  = "adminpass"  # Master Login
)
# Function to check last modification time
get_last_modified <- function(url) {
  tryCatch({
    headers <- httr::HEAD(url)$headers
    as.numeric(as.POSIXct(headers[["last-modified"]], format = "%a, %d %b %Y %H:%M:%S GMT", tz = "GMT"))
  }, error = function(e) {
    Sys.time()  # Fallback if header fails
  })
}

# Function to safely read CSV or Excel files directly from Dropbox
read_data_safe <- function(url) {
  tryCatch({
    # Ensure correct Dropbox format (forces direct file access)
    url <- sub("\\?dl=1$", "?raw=1", url)
    
    # Check file type and set temporary file extension
    temp_file <- tempfile(fileext = ifelse(grepl("\\.xlsx", url, ignore.case = TRUE), ".xlsx", ".csv"))
    
    # Download file from Dropbox
    download.file(url, temp_file, mode = "wb")
    
    # Read based on file type
    if (grepl("\\.xlsx", url, ignore.case = TRUE)) {
      data <- read_xlsx(temp_file, sheet = 1, col_names = TRUE)
    } else {
      data <- read_csv(temp_file, show_col_types = FALSE)
    }
    
    return(data)
    
  }, error = function(e) {
    return(data.frame(Message = paste("Failed to load projects. Error:", e$message)))
  })
}

# Define UI
ui <- tagList(
  #useShinyjs(), 
  tags$head(
    tags$script(type="text/javascript", src = "code.js"),
    tags$style(HTML("
    body {font-family: 'Myriad Pro', sans-serif;}
    h1, h2, h3, h4, h5, h6 {font-family: 'Minion Pro', serif;}
                    .navbar { background-color: #00274D; padding: 10px; display: flex; align-items: flex-end; }
                        .navbar-brand { font-size: 30px !important; font-weight: bold; color: white !important; display: flex; align-items: flex-end; }
                      .navbar .container-fluid { display: flex; align-items: flex-end; }
                      .navbar .container-fluid img { max-height: 50px !important; align-self: flex-end;}
    .login-panel {background-color: #00274D; color: white; padding: 10px; border-radius: 8px;}
    .btn-primary {background-color: #00274D; border: none; color: white;}
    .btn-danger {background-color: #D9534F; border: none; color: white;}
    
    /* Adjust Logo Size */
    .navbar .container-fluid img {
      max-height: 100px !important;  /* Adjust logo height */
      width: auto;
    }
"))
  ),
  
  # Navbar with Logo (Logo placed via JS in code.js)
  navbarPage(
    title = div(class = "navbar-brand", "UFHCC BCB-SR Bioinformatics Project Portal"),
    windowTitle = "UFHCC Portal",
    collapsible = TRUE,
    fluid = TRUE,
    div(id = "login-page",
        div(class = "login-panel",
            h3("PI Portal Login"),
            textInput("username", "Username"),
            passwordInput("password", "Password"),
            actionButton("login", "Login", class = "btn-primary"),
            verbatimTextOutput("login_status"),
            
            # ✅ HIDDEN LOGOUT BUTTON ON PAGE LOAD
            hidden(
              div(
                id = "logout-container",
                style = "padding-top: 10px;",
                actionButton("logout", "Logout", class = "btn-danger")
              )
            )
        )
    ),
    
    hidden(
      div(id = "main-page",
          
          # Description Block
          div(class = "app-description",
              h3("UFHCC BCB-SR Bioinformatics Project Portal"),
              p(HTML("This portal provides a way to track and manage bioinformatics projects at UFHCC BCB-SR.")),
              p(HTML("Users can log in to view their projects, track progress, and download relevant reports and data.")),
              p(HTML("Note that <strong>access to the portal does not grant access to data</strong>; data access is controlled by file storage services (e.g., Dropbox)."),
              p(HTML("<strong>Github Repositories are often private</strong>; If you require acceess, contact your collaborating bioinformatician.")))
          ),
          
          textOutput("admin_message"),  # Admin message placeholder
          
          fluidRow(
            column(12,
                  
                   DTOutput("projects_table")  # <- Works for both Admin & PI
            )
          ),
          
          br(),
          p("For inquiries, contact ", a("BCB-SR", href = "mailto:UFHCC-BCB-SR@ufl.edu"))
      )
    )
  )
  
)


server <- function(input, output, session) {
  hide("logout-container")
  
  # Load PI sheets and credentials once per app session
  load_pi_sheets_and_credentials <- function(url) {
    temp_file <- tempfile(fileext = ".xlsx")
    download.file(url, temp_file, mode = "wb")
    
    sheet_names <- excel_sheets(temp_file)
    sheet_data <- lapply(sheet_names, function(sheet) read_xlsx(temp_file, sheet = sheet))
    names(sheet_data) <- sheet_names
    
    credentials <- setNames(paste0("pass", seq_along(sheet_names)), sheet_names)
    list(data = sheet_data, credentials = credentials)
  }
  
  pi_info <- load_pi_sheets_and_credentials(multi_pi_excel_url)
  pi_data_list <- pi_info$data
  valid_users <- c(pi_info$credentials, admin = "adminpass")
  
  user_session <- reactiveVal(NULL)
  user_csv_url <- reactiveVal(NULL)
  
  observeEvent(input$login, {
    user <- trimws(input$username)
    pass <- input$password
    
    if (!is.null(valid_users[[user]]) && valid_users[[user]] == pass) {
      user_session(user)
      output$login_status <- renderText("")
      hide("login-page")
      show("main-page")
      show("logout-container")
      print("✅ Logout button should now be visible!")
      
      if (user == "admin") {
        required_columns <- c("ProjectID", "Initiated", "StudyContact", "Bioinformatician", 
                              "DataDictionary", "DataType", "Status", "RawData", "Report", 
                              "Notes", "AdditionalFiles", "LastUpdate", "MultiQC Report", 
                              "PI", "hipergator filepath", "Dropbox Project Folder", "Github_Repo")
        
        
        data_list <- lapply(names(pi_data_list), function(pi_name) {
          data <- pi_data_list[[pi_name]]
          data$PI <- pi_name
          
          if (!"ProjectID" %in% colnames(data)) {
            data$ProjectID <- NA
          } else {
            data$ProjectID <- as.character(data$ProjectID)
          }
          
          missing_cols <- setdiff(required_columns, colnames(data))
          for (col in missing_cols) {
            data[[col]] <- NA
          }
          
          if ("LastUpdate" %in% colnames(data)) {
            data$LastUpdate <- as.character(data$LastUpdate)
          }
          if ("Initiated" %in% colnames(data)) {
            data$Initiated <- as.character(data$Initiated)
          }
          
          data[, required_columns, drop = FALSE]
        })
        
        data_to_display <- do.call(rbind, data_list)
        
      } else {
        data_to_display <- pi_data_list[[user]]
        
        if ("PI" %in% colnames(data_to_display)) {
          data_to_display$PI <- NULL
        }
        
        if ("ProjectID" %in% colnames(data_to_display)) {
          data_to_display$ProjectID <- as.character(data_to_display$ProjectID)
        }
      }
      
      output$projects_table <- renderDT({
        req(data_to_display)
        
        required_columns <- c("ProjectID", "Initiated", "StudyContact", "Bioinformatician", 
                              "DataDictionary", "DataType", "Status", "RawData", "Report", 
                              "Notes", "AdditionalFiles", "LastUpdate", "MultiQC Report", 
                              "PI", "hipergator filepath", "Dropbox Project Folder", "Github_Repo")
        
        
        missing_cols <- setdiff(required_columns, colnames(data_to_display))
        for (col in missing_cols) {
          data_to_display[[col]] <- NA
        }
        
        data_to_display$LastUpdate <- as.character(
          as.Date(data_to_display$LastUpdate, tryFormats = c("%Y-%m-%d", "%m/%d/%Y"))
        )
        if (!"hipergator filepath" %in% colnames(data_to_display)) {
          data_to_display$`hipergator filepath` <- NA
        }
        
        displayed_cols <- colnames(data_to_display)
        if (user_session() != "admin") {
          displayed_cols <- setdiff(displayed_cols, "hipergator filepath")
        }
        
        data_to_display$`MultiQC Report` <- sapply(seq_along(data_to_display$`MultiQC Report`), function(i) {
          entry <- data_to_display$`MultiQC Report`[i]
          if (is.na(entry) || entry == "") return("NA")
          
          file_entries <- unlist(strsplit(entry, ";\\s*"))
          file_entries <- file_entries[grepl("https?://", file_entries)]  # keep only valid URLs
          
          if (length(file_entries) == 0) return("NA")  # nothing valid
          
          # If only 1 valid file, simple download link
          if (length(file_entries) == 1) {
            url <- sub("^[^:]+:\\s*", "", file_entries[1])  # remove label if exists
            return(paste0("<a href='", url, "' download>Download MultiQC Report</a>"))
          }
          
          # If multiple valid files, create collapse panel
          formatted_links <- lapply(file_entries, function(file_entry) {
            match <- regexec("^([^:]+):\\s*(https?://.+)$", file_entry)
            parts <- regmatches(file_entry, match)[[1]]
            if (length(parts) == 3) {
              label <- parts[2]
              url <- parts[3]
            } else {
              url <- file_entry
              label <- basename(sub("\\?.*$", "", url))  # remove query string
            }
            paste0("<a href='", url, "' download>", label, "</a>")
          })
          
          unique_id <- paste0("collapse-multiqc-", i)
          paste0(
            "<button class='btn btn-secondary' data-toggle='collapse' data-target='#", unique_id, "' data-parent='#multiqc-container'>MultiQC Reports</button>
<div id='", unique_id, "' class='collapse' style='border: 1px solid #ccc; padding: 10px; margin-top: 5px;'>",
            paste(formatted_links, collapse = "<br>"), "</div>"
          )
        })
        
        data_to_display$Github_Repo <- sapply(data_to_display$Github_Repo, function(entry) {
          if (is.na(entry) || entry == "") return("")
          
          label <- basename(sub("\\?.*$", "", entry))  # Clean label if needed
          tooltip <- "If you get a 404 error, the repository may be private. Contact BCB-SR for access."
          
          if (grepl("^https?://", entry)) {
            return(paste0("<a href='", entry, "' target='_blank' title='", tooltip, "'>", label, "</a>"))
          } else {
            return(entry)
          }
        })
        
        data_to_display$`Dropbox Project Folder` <- ifelse(
          is.na(data_to_display$`Dropbox Project Folder`) | data_to_display$`Dropbox Project Folder` == "",
          "",
          paste0("<a href='", data_to_display$`Dropbox Project Folder`, "' target='_blank' rel='noopener noreferrer'>Visit Dropbox Project Folder</a>")
        )
        
        if ("Notes" %in% colnames(data_to_display)) {
          data_to_display$Notes <- sapply(seq_along(data_to_display$Notes), function(i) {
            entry <- data_to_display$Notes[i]
            if (is.na(entry) || entry == "") return("No Notes")
            notes_list <- unlist(strsplit(entry, "; "))
            formatted_notes <- paste0("<strong>", gsub(": ", "</strong>: ", notes_list), collapse = "<br>")
            unique_id <- paste0("collapse-notes-", i)
            paste0("
              <button class='btn btn-secondary' data-toggle='collapse' data-target='#", unique_id, "' data-parent='#notes-container'>View Notes</button>
              <div id='", unique_id, "' class='collapse' style='border: 1px solid #ccc; padding: 10px; margin-top: 5px;'>
                <p style='white-space: normal;'>", formatted_notes, "</p>
              </div>")
          })
        }
        
        data_to_display$Report <- sapply(seq_along(data_to_display$Report), function(i) {
          entry <- data_to_display$Report[i]
          if (is.na(entry) || entry == "") return("")
          
          report_entries <- unlist(strsplit(entry, ";\\s*"))
          
          # If only 1 file, simple download link
          if (length(report_entries) == 1) {
            url <- sub("^[^:]+:\\s*", "", report_entries[1])  # remove label if exists
            return(paste0("<a href='", url, "' download>Download Report</a>"))
          }
          
          # If multiple files, create dropdown menu
          formatted_links <- lapply(seq_along(report_entries), function(j) {
            file_entry <- report_entries[j]
            match <- regexec("^([^:]+):\\s*(https?://.+)$", file_entry)
            parts <- regmatches(file_entry, match)[[1]]
            if (length(parts) == 3) {
              label <- parts[2]
              url <- parts[3]
            } else {
              url <- file_entry
              label <- paste0("Report V", j)
            }
            paste0("<li><a class='dropdown-item' href='", url, "' download>", label, "</a></li>")
          })
          
          unique_id <- paste0("dropdown-report-", i)
          
          paste0(
            "<div class='dropdown'>
      <button class='btn btn-secondary dropdown-toggle' type='button' data-toggle='dropdown'>Reports</button>
      <ul class='dropdown-menu'>", paste(formatted_links, collapse = ""), "</ul>
    </div>"
          )
        })
        
        
        data_to_display$DataDictionary <- sapply(data_to_display$DataDictionary, function(entry) {
          if (is.na(entry) || entry == "") return("Unsubmitted")
          
          parts <- unlist(strsplit(entry, ";\\s*"))  # allow optional space after semicolon
          
          # Case: label + URL
          if (length(parts) > 1 && grepl("^https?://", parts[2])) {
            status_text <- parts[1]
            url <- parts[2]
            return(paste0("<a href='", url, "' download>", status_text, "</a>"))
          }
          
          # Case: single entry that is a URL
          if (length(parts) == 1 && grepl("^https?://", parts[1])) {
            url <- parts[1]
            label <- basename(sub("\\?.*$", "", url))  # fallback to filename
            return(paste0("<a href='", url, "' download>", label, "</a>"))
          }
          
          # Otherwise, just return the raw text
          return(entry)
        })
        
        
        data_to_display$Initiated <- as.character(
          as.Date(data_to_display$Initiated, tryFormats = c("%Y-%m-%d", "%m/%d/%Y"))
        )
        data_to_display$RawData <- ifelse(
          is.na(data_to_display$RawData) | data_to_display$RawData == "",
          "",
          paste0("<a href='", data_to_display$RawData, "' download>Raw Data</a>")
        )
        
        if ("AdditionalFiles" %in% colnames(data_to_display)) {
          data_to_display$AdditionalFiles <- sapply(seq_along(data_to_display$AdditionalFiles), function(i) {
            entry <- data_to_display$AdditionalFiles[i]
            if (is.na(entry) || entry == "") return("None")
            file_entries <- unlist(strsplit(entry, ";\\s*"))
            formatted_links <- lapply(file_entries, function(file_entry) {
              match <- regexec("^([^:]+):\\s*(https?://.+)$", file_entry)
              parts <- regmatches(file_entry, match)[[1]]
              if (length(parts) == 3) {
                label <- parts[2]
                url <- parts[3]
              } else {
                url <- file_entry
                label <- basename(sub("\\?.*$", "", url))
              }
              paste0("<a href='", url, "'>", label, "</a>")
            })
            unique_id <- paste0("collapse-files-", i)
            paste0(
              "<button class='btn btn-secondary' data-toggle='collapse' data-target='#", unique_id, "' data-parent='#files-container'>Additional Files</button>
              <div id='", unique_id, "' class='collapse' style='border: 1px solid #ccc; padding: 10px; margin-top: 5px;'>",
              paste(formatted_links, collapse = "<br>"), "</div>")
          })
        }
        
        if ("PI" %in% displayed_cols & "ProjectID" %in% displayed_cols) {
          displayed_cols <- c("PI", "ProjectID", setdiff(displayed_cols, c("PI", "ProjectID")))
        } else if ("PI" %in% displayed_cols) {
          displayed_cols <- c("PI", setdiff(displayed_cols, "PI"))
        } else if ("ProjectID" %in% displayed_cols) {
          displayed_cols <- c("ProjectID", setdiff(displayed_cols, "ProjectID"))
        }
        
        datatable(
          data_to_display[, displayed_cols, drop = FALSE],
          escape = FALSE,
          rownames = FALSE,
          options = list(
            autoWidth = TRUE,
            scrollX = TRUE,
            scrollY = "500px",
            paging = FALSE,
            fixedHeader = TRUE
          )
        ) %>%
          formatStyle(
            "Status",
            target = "row",
            backgroundColor = styleEqual(
              c("Data received", "Initial QC", "Pipeline", "Post-pipeline QC",
                "Differential Analysis", "Report Delivered", "Additional Visualizations",
                "Manuscript Writing", "Halted due to QC"),
              c("#BBDEFB", "#BBDEFB", "#A5D6A7", "#A5D6A7", "#A5D6A7",
                "#CE93D8", "#CE93D8", "#CE93D8", "#A9A9A9")
            )
          )
      })
      
    } else {
      output$login_status <- renderText("❌ Invalid username or password.")
    }
  })
  
  observeEvent(input$logout, {
    user_session(NULL)
    user_csv_url(NULL)
    output$projects_table <- renderDT(NULL)
    output$login_status <- renderText("✅ Successfully logged out.")
    hide("main-page")
    show("login-page")
    hide("logout-container")
  })
}

shinyApp(ui, server)
