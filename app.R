library(shiny)
library(DT)
library(shinyjs)
library(httr)
library(readr)
library(readxl)
library(shinythemes)

multi_pi_excel_url <- "https://www.dropbox.com/scl/fi/25m6c712m20wsomj9kf7o/PI_Projects.xlsx?rlkey=txap5o2fs7kxqz0ii0zw27lll&raw=1"

# After libraries and before load_passwords function:
password_file <- file.path(getwd(), "user_passwords.rds")

# Initialize file if it doesn't exist or is corrupted
if (!file.exists(password_file)) {
  tryCatch({
    saveRDS(list(), password_file)
    cat("Initialized password file at:", password_file, "\n")
  }, error = function(e) {
    warning(paste("Could not initialize password file:", e$message))
  })
}

# Keep your original load_passwords function as-is:
load_passwords <- function() {
  if (file.exists(password_file)) {
    return(readRDS(password_file))
  } else {
    return(list())
  }
}


# Legacy passwords for sheets 1-9
legacy_passwords <- c(
  "Licht" = "pass1",
  "Sharma" = "pass2",
  "Zhang" = "pass3",
  "Xing" = "pass4",
"Lamba" = "pass5",
"Sarkisian" = "pass6",
"Clanton" = "pass7",
"Reams" = "pass8",
"Farrer" = "pass9"
)

load_passwords <- function() {
  if (file.exists(password_file)) {
    return(readRDS(password_file))
  } else {
    return(list())
  }
}

save_passwords <- function(passwords) {
  saveRDS(passwords, password_file)
}

load_pi_sheets_and_credentials <- function(url) {
  temp_file <- tempfile(fileext = ".xlsx")
  download.file(url, temp_file, mode = "wb")
  sheet_names <- excel_sheets(temp_file)
  sheet_data <- lapply(sheet_names, function(sheet) {
    read_xlsx(temp_file, sheet = sheet, col_names = TRUE)
  })
  names(sheet_data) <- sheet_names
  
  # Load saved passwords
  saved_passwords <- load_passwords()
  
  # Generate credentials
  credentials <- list()
  for (i in seq_along(sheet_names)) {
    sheet_name <- sheet_names[i]
    
    # Priority: saved password > legacy password > new format
    if (!is.null(saved_passwords[[sheet_name]])) {
      credentials[[sheet_name]] <- saved_passwords[[sheet_name]]
    } else if (i <= 9 && !is.null(legacy_passwords[[sheet_name]])) {
      credentials[[sheet_name]] <- legacy_passwords[[sheet_name]]
    } else {
      credentials[[sheet_name]] <- paste0(sheet_name, "123")
    }
  }
  
  # Add admin
  credentials[["admin"]] <- ifelse(
    !is.null(saved_passwords[["admin"]]),
    saved_passwords[["admin"]],
    "adminpass"
  )
  
  list(data = sheet_data, credentials = credentials)
}

get_last_modified <- function(url) {
  tryCatch({
    headers <- httr::HEAD(url)$headers
    as.numeric(as.POSIXct(headers[["last-modified"]], format = "%a, %d %b %Y %H:%M:%S GMT", tz = "GMT"))
  }, error = function(e) {
    Sys.time()
  })
}

read_data_safe <- function(url) {
  tryCatch({
    url <- sub("\\?dl=1$", "?raw=1", url)
    temp_file <- tempfile(fileext = ifelse(grepl("\\.xlsx", url, ignore.case = TRUE), ".xlsx", ".csv"))
    download.file(url, temp_file, mode = "wb")
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

ui <- tagList(
tags$head(
  tags$script(type="text/javascript", src = "code.js"),
  tags$script(HTML("
    Shiny.addCustomMessageHandler('toggleDisplay', function(message) {
      var element = document.getElementById(message.id);
      if (element) {
        if (message.show) {
          element.style.display = '';
        } else {
          element.style.display = 'none';
        }
      }
    });
  ")),
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
    .btn-warning {background-color: #F0AD4E; border: none; color: white;}
    .navbar .container-fluid img {
      max-height: 100px !important;
      width: auto;
    }
    .password-change-section {
      background-color: #003366; 
      padding: 15px; 
      border-radius: 5px; 
      margin-top: 15px;
      border: 1px solid #004d99;
    }
  "))
),
navbarPage(
    title = div(class = "navbar-brand", "UF Health Cancer Institute BCB-SR Bioinformatics Project Portal"),
    windowTitle = "UFHCI Portal",
    collapsible = TRUE,
    fluid = TRUE,
div(id = "login-page",
    div(class = "login-panel",
        h3("PI Portal Login"),
        textInput("username", "Username"),
        passwordInput("password", "Password"),
        actionButton("login", "Login", class = "btn-primary"),
        verbatimTextOutput("login_status"),
        # Use inline style instead of hidden()
        div(
          id = "logout-container",
          style = "display: none; padding-top: 10px;",
          actionButton("logout", "Logout", class = "btn-danger"),
          actionButton("show_change_password", "Change Password", class = "btn-warning", style = "margin-left: 10px;")
        ),
        # Use inline style instead of hidden()
        div(
          id = "password-change-section",
          class = "password-change-section",
          style = "display: none;",
          h4("Change Password"),
          passwordInput("old_password", "Current Password"),
          passwordInput("new_password", "New Password"),
          passwordInput("confirm_password", "Confirm New Password"),
          actionButton("submit_password_change", "Update Password", class = "btn-primary"),
          actionButton("cancel_password_change", "Cancel", class = "btn-danger", style = "margin-left: 10px;"),
          verbatimTextOutput("password_change_status")
        )
    )
),
    hidden(
      div(id = "main-page",
          div(class = "app-description",
              h3("UFHCC BCB-SR Bioinformatics Project Portal"),
              p(HTML("This portal provides a way to track and manage bioinformatics projects at UFHCC BCB-SR.")),
              p(HTML("Users can log in to view their projects, track progress, and download relevant reports and data.")),
              p(HTML("Note that <strong>access to the portal does not grant access to data</strong>; data access is controlled by file storage services (e.g., Dropbox)."),
              p(HTML("<strong>Github Repositories are often private</strong>; If you require access, contact your collaborating bioinformatician.")))
          ),
          textOutput("admin_message"),
          fluidRow(
            column(12,
                   DTOutput("projects_table")
            )
          ),
          br(),
          p("For inquiries, contact ", a("BCB-SR", href = "mailto:UFHCC-BCB-SR@ufl.edu"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  pi_info <- load_pi_sheets_and_credentials(multi_pi_excel_url)
  pi_data_list <- pi_info$data
  valid_users <- reactiveVal(pi_info$credentials)
  
  user_session <- reactiveVal(NULL)
  user_csv_url <- reactiveVal(NULL)
  
  observeEvent(input$login, {
    user <- trimws(input$username)
    pass <- input$password
    
    current_users <- valid_users()
    
    if (!is.null(current_users[[user]]) && current_users[[user]] == pass) {
      user_session(user)
      output$login_status <- renderText("")
      output$password_change_status <- renderText("")
      
      session$sendCustomMessage("toggleDisplay", list(id = "login-page", show = FALSE))
      session$sendCustomMessage("toggleDisplay", list(id = "main-page", show = TRUE))
      session$sendCustomMessage("toggleDisplay", list(id = "logout-container", show = TRUE))
      
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
          file_entries <- file_entries[grepl("https?://", file_entries)]
          if (length(file_entries) == 0) return("NA")
          if (length(file_entries) == 1) {
            url <- sub("^[^:]+:\\s*", "", file_entries[1])
            return(paste0("<a href='", url, "' download>Download MultiQC Report</a>"))
          }
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
          label <- basename(sub("\\?.*$", "", entry))
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
          if (length(report_entries) == 1) {
            url <- sub("^[^:]+:\\s*", "", report_entries[1])
            return(paste0("<a href='", url, "' download>Download Report</a>"))
          }
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
          parts <- unlist(strsplit(entry, ";\\s*"))
          if (length(parts) > 1 && grepl("^https?://", parts[2])) {
            status_text <- parts[1]
            url <- parts[2]
            return(paste0("<a href='", url, "' download>", status_text, "</a>"))
          }
          if (length(parts) == 1 && grepl("^https?://", parts[1])) {
            url <- parts[1]
            label <- basename(sub("\\?.*$", "", url))
            return(paste0("<a href='", url, "' download>", label, "</a>"))
          }
          return(entry)
        })
        data_to_display$Initiated <- as.character(
          as.Date(data_to_display$Initiated, tryFormats = c("%Y-%m-%d", "%m/%d/%Y"))
        )
        # Replace the RawData processing section with this:
        data_to_display$RawData <- sapply(data_to_display$RawData, function(entry) {
          if (is.na(entry) || entry == "") return("")
          
          # Check if it's a URL:
          # 1. Starts with http:// or https://
          # 2. Contains common domain suffixes (.edu, .com, .org, etc.)
          is_url <- grepl("^https?://", entry) || 
            grepl("\\.(edu|com|org|net|gov|io|co|us|uk|ca)(/|$|\\?|#)", entry, ignore.case = TRUE)
          
          if (is_url) {
            # Add https:// if missing
            url <- ifelse(grepl("^https?://", entry), entry, paste0("https://", entry))
            return(paste0("<a href='", url, "' target='_blank'>Raw Data</a>"))
          } else {
            # It's a filepath (e.g., hipergator path), display as text
            return(entry)
          }
        })
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
  
  observeEvent(input$show_change_password, {
    session$sendCustomMessage("toggleDisplay", list(id = "password-change-section", show = TRUE))
    output$password_change_status <- renderText("")
  })
  
  observeEvent(input$cancel_password_change, {
    session$sendCustomMessage("toggleDisplay", list(id = "password-change-section", show = FALSE))
    updateTextInput(session, "old_password", value = "")
    updateTextInput(session, "new_password", value = "")
    updateTextInput(session, "confirm_password", value = "")
    output$password_change_status <- renderText("")
  })
  
observeEvent(input$submit_password_change, {
  user <- user_session()
  current_users <- valid_users()
  if (is.null(user)) {
    output$login_status <- renderText("❌ No user logged in.")
    return()
  }
  if (input$old_password != current_users[[user]]) {
    output$login_status <- renderText("❌ Current password is incorrect.")
    return()
  }
  if (input$new_password == "") {
    output$login_status <- renderText("❌ New password cannot be empty.")
    return()
  }
  if (input$new_password != input$confirm_password) {
    output$login_status <- renderText("❌ New passwords do not match.")
    return()
  }
  # Try to update password with error handling
  tryCatch({
    saved_passwords <- load_passwords()
    saved_passwords[[user]] <- input$new_password
    save_passwords(saved_passwords)
    
    # Update current session
    current_users[[user]] <- input$new_password
    valid_users(current_users)
    
    # Clear the password change inputs
    updateTextInput(session, "old_password", value = "")
    updateTextInput(session, "new_password", value = "")
    updateTextInput(session, "confirm_password", value = "")
    
    # Clear the login fields too
    updateTextInput(session, "username", value = "")
    updateTextInput(session, "password", value = "")
    
    # Hide the password change section
    session$sendCustomMessage("toggleDisplay", list(id = "password-change-section", show = FALSE))
    
    # Clear the success message after 5 seconds
# Show success message
output$login_status <- renderText("✅ Password updated successfully!")

# Clear it after 5 seconds
later::later(function() {
  output$login_status <- renderText("")
}, 5)
    
  }, error = function(e) {
    output$password_change_status <- renderText(paste0("❌ Error saving password: ", e$message))
  })
})  
  observeEvent(input$logout, {
    user_session(NULL)
    user_csv_url(NULL)
    output$projects_table <- renderDT(NULL)
    output$login_status <- renderText("✅ Successfully logged out.")
    
    session$sendCustomMessage("toggleDisplay", list(id = "main-page", show = FALSE))
    session$sendCustomMessage("toggleDisplay", list(id = "login-page", show = TRUE))
    session$sendCustomMessage("toggleDisplay", list(id = "logout-container", show = FALSE))
    session$sendCustomMessage("toggleDisplay", list(id = "password-change-section", show = FALSE))
  })
}

shinyApp(ui, server)
