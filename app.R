library(shiny)
library(DT)
library(shinyjs)
library(httr)
library(readr)
library(readxl)
library(shinythemes)

# Dropbox File URLs for each PI
pi_csv_urls <- list(
  "Licht" = "https://www.dropbox.com/scl/fi/rtl1ugx5q88jdbn75p5cl/Licht.xlsx?rlkey=gkx0fs5kgxovlq83hvopo9h1j&st=24l84lb8&raw=1",
  "Sharma" = "https://www.dropbox.com/scl/fi/pwxbohyjenyjw7wbqrzjb/Sharma.xlsx?rlkey=8lwi7t58bhmv7r7jhvmjben4z&st=zu3mb9e3&raw=1",
  "Zhang"  = "https://www.dropbox.com/scl/fi/xp4apspwizjfnu8f447s6/Zhang.xlsx?rlkey=pme7lzpqzjw7rselrwjkcv9nl&st=qup96sxl&raw=1",
  "Xing" = "https://www.dropbox.com/scl/fi/hmje516zfe9eqmehpiloz/Xing.xlsx?rlkey=umh6hahlzt7lq5bpkbbo9xyug&st=zsqj5rdw&raw=1"
)
# User credentials
valid_users <- list(
  "Licht"  = "pass1",
  "Sharma" = "pass2",
  "Zhang"  = "pass3",
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
            verbatimTextOutput("login_status")
        )
    ),
    hidden(
      div(id = "main-page",
          textOutput("admin_message"),  # Admin message placeholder
          fluidRow(
            column(12,
                   actionButton("logout", "Logout", class = "btn-danger pull-right"),
                   DTOutput("projects_table")  # <- This will now work for both Admin & PI
            )
          ),
          br(),
          p("For inquiries, contact ", a("BCB-SR", href = "mailto:example@ufhcc.edu"))
      )
    )
    
  )
  
)


# Server
server <- function(input, output, session) {
  
  user_session <- reactiveVal(NULL)
  user_csv_url <- reactiveVal(NULL)
  
  observeEvent(input$login, {
    user <- trimws(input$username)
    pass <- input$password
    
    valid_users <- c("Licht" = "pass1", "Sharma" = "pass2", "Zhang" = "pass3", "Xing"="pass4", "admin" = "adminpass")
    
    if (!is.null(valid_users[[user]]) && valid_users[[user]] == pass) {
      user_session(user)
      output$login_status <- renderText("")  # Clear logout message
      
      if (user == "admin") {
        data_to_display <- do.call(rbind, lapply(names(pi_csv_urls), function(pi_name) {
          data <- read_data_safe(pi_csv_urls[[pi_name]])
          data$PI <- pi_name  # Add PI column
          
          # Fix: Ensure "LastUpdate" is always read as character
          if ("LastUpdate" %in% colnames(data)) {
            data$LastUpdate <- as.character(data$LastUpdate)
          }
          
          if ("Initiated" %in% colnames(data)) {
            data$Initiated <- as.character(data$Initiated)
          }
          
          return(data)
        }))
      } else {
        user_csv_url(pi_csv_urls[[user]])
        data_to_display <- read_data_safe(user_csv_url())  
      }
      
      # Apply formatting before rendering
      output$projects_table <- renderDT({
        req(data_to_display)
        
        # Ensure required columns exist
        required_columns <- c("Initiated", "StudyContact", "Bioinformatician", "DataDictionary", 
                              "DataType", "Status", "RawData", "Report", "Notes", 
                              "AdditionalFiles", "LastUpdate", "MultiQC", "PI")
        missing_cols <- setdiff(required_columns, colnames(data_to_display))
        
        for (col in missing_cols) {
          data_to_display[[col]] <- NA
        }
        
        # Convert "LastUpdate" to a consistent date format
        data_to_display$LastUpdate <- as.character(
          as.Date(data_to_display$LastUpdate, tryFormats = c("%Y-%m-%d", "%m/%d/%Y"))
        )

        # Formatting MultiQC Report
        data_to_display$`MultiQC Report` <- ifelse(
          is.na(data_to_display$`MultiQC Report`) | data_to_display$`MultiQC Report` == "",
          "",
          paste0("<a href='", data_to_display$`MultiQC Report`, "' download>MultiQC Report</a>")
        )
        # Format Notes Column: Convert semicolon-separated values into a dropdown list
        if ("Notes" %in% colnames(data_to_display)) {
          data_to_display$Notes <- sapply(seq_along(data_to_display$Notes), function(i) {
            entry <- data_to_display$Notes[i]
            
            if (is.na(entry) || entry == "") {
              return("No Notes")
            }
            
            notes_list <- unlist(strsplit(entry, "; "))
            formatted_notes <- paste0("<strong>", gsub(": ", "</strong>: ", notes_list), collapse = "<br>")
            
            unique_id <- paste0("collapse-notes-", i)  # Unique ID for each row
            
            return(paste0("
      <button class='btn btn-secondary' data-toggle='collapse' data-target='#", unique_id, "' data-parent='#notes-container'>
        View Notes
      </button>
      <div id='", unique_id, "' class='collapse' style='border: 1px solid #ccc; padding: 10px; margin-top: 5px;'>
        <h4 style='margin-top: 0; font-size: 16px;'>Notes</h4>
        <p style='white-space: normal;'>", formatted_notes, "</p>
      </div>
    "))
          })
        }
        
        
        # Formatting Reports
        data_to_display$Report <- ifelse(
          is.na(data_to_display$Report) | data_to_display$Report == "",
          "",
          paste0("<a href='", data_to_display$Report, "' download>Report</a>")
        )
        
        # Formatting Data Dictionary
        data_to_display$DataDictionary <- sapply(data_to_display$DataDictionary, function(entry) {
          if (is.na(entry) || entry == "") {
            return("Unsubmitted")  # Show "Unsubmitted" if the field is empty
          }
          
          # Expecting "Status; URL" format, split into parts
          parts <- unlist(strsplit(entry, "; "))
          
          # Extract status text (default to "Data Dictionary" if only URL exists)
          status_text <- ifelse(length(parts) > 1, parts[1], "Submitted")
          
          # Extract URL (if it exists)
          url <- ifelse(length(parts) > 1, parts[2], parts[1])
          
          # Ensure proper hyperlink formatting
          if (grepl("^https?://", url)) {
            return(paste0("<a href='", url, "' download>", status_text, "</a>"))
          } else {
            return(status_text)  # Just return the status if no URL is present
          }
        })
        
        # Formatting Raw Data
        data_to_display$RawData <- ifelse(
          is.na(data_to_display$RawData) | data_to_display$RawData == "",
          "",
          paste0("<a href='", data_to_display$RawData, "' download>Raw Data</a>")
        )
        
        # Formatting Additional Files
        data_to_display$AdditionalFiles <- ifelse(
          is.na(data_to_display$AdditionalFiles) | data_to_display$AdditionalFiles == "",
          "None",
          paste0("<a href='", data_to_display$AdditionalFiles, "' download>Additional Files</a>")
        )
        
        # Display the table with proper formatting
        datatable(data_to_display, escape = FALSE, options = list(autoWidth = TRUE))
      })
      
      hide("login-page")
      show("main-page")
    } else {
      output$login_status <- renderText("❌ Invalid username or password.")
    }
  })
  
  projects <- reactivePoll(
    10000,
    session,
    checkFunc = function() {
      url <- user_csv_url()
      if (!is.null(url)) get_last_modified(url) else Sys.time()
    },
    valueFunc = function() {
      url <- user_csv_url()
      if (!is.null(url)) {
        read_data_safe(url)
      } else {
        return(data.frame(Message = "No file URL available."))
      }
    }
  )
  

  
  observeEvent(input$logout, {
    user_session(NULL)  # Reset user session
    user_csv_url(NULL)  # Reset PI file selection
    output$projects_table <- renderDT(NULL)  # Clear table output
    output$login_status <- renderText("✅ Successfully logged out.")  # Show logout message
    
    hide("main-page")  # Hide main page
    show("login-page")  # Show login page again
  })
  
}

shinyApp(ui, server)
