library(shiny)
library(DT)
library(shinyjs)
library(httr)
library(readr)
library(readxl)

# Dropbox File URLs for each PI
pi_csv_urls <- list(
  #"Licht"  = "https://dl.dropboxusercontent.com/scl/fi/1lvauptlumrljx0q99vjh/Licht.xlsx?rlkey=w06dezs3w1bgdagqha0o744jz&st=sdhwds2n",
  "Licht" = "https://www.dropbox.com/scl/fi/rtl1ugx5q88jdbn75p5cl/Licht.xlsx?rlkey=gkx0fs5kgxovlq83hvopo9h1j&st=24l84lb8&raw=1",
  "Sharma" = "https://www.dropbox.com/scl/fi/pwxbohyjenyjw7wbqrzjb/Sharma.xlsx?rlkey=8lwi7t58bhmv7r7jhvmjben4z&st=zu3mb9e3&raw=1",
  "Zhang"  = "https://www.dropbox.com/scl/fi/xp4apspwizjfnu8f447s6/Zhang.xlsx?rlkey=pme7lzpqzjw7rselrwjkcv9nl&st=qup96sxl&raw=1"
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

# UI
ui <- fluidPage(
  useShinyjs(),
  div(id = "login-page",
      titlePanel("UFHCC BCB-SR: PI Portal"),
      textInput("username", "Username"),
      passwordInput("password", "Password"),
      actionButton("login", "Login"),
      verbatimTextOutput("login_status")
  ),
  hidden(
    div(id = "main-page",
        titlePanel("PI Project Tracking"),
        actionButton("logout", "Logout"),
        DTOutput("projects_table"),
        br(),
        p("For inquiries, contact ", a("BCB-SR", href = "mailto:example@ufhcc.edu"))
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
    
    valid_users <- c("Licht" = "pass1", "Sharma" = "pass2", "Zhang" = "pass3")
    
    if (!is.null(valid_users[[user]]) && valid_users[[user]] == pass) {
      user_session(user)
      user_csv_url(pi_csv_urls[[user]])
      
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
  
  output$projects_table <- renderDT({
    req(user_csv_url())  # Ensure a URL exists
    
    data <- projects()
    
    if ("Message" %in% colnames(data)) {
      return(data.frame(Message = data$Message))  # Show error messages if any
    }
    
    # Ensure required columns exist
    required_columns <- c("Initiated", "StudyContact", "Bioinformatician", "DataDictionary", 
                          "DataType", "Status", "RawData", "Report", "Notes", 
                          "AdditionalFiles", "LastUpdate","MultiQC")
    missing_cols <- setdiff(required_columns, colnames(data))
    
    for (col in missing_cols) {
      data[[col]] <- NA  # Create empty columns for missing fields
    }
    
    # Convert "LastUpdate" to a consistent date format
    data$LasteUpdate <- as.character(
      as.Date(data$LastUpdate, tryFormats = c("%Y-%m-%d", "%m/%d/%Y"))
    )
    
    data$`MultiQC Report` <- ifelse(
      is.na(data$`MultiQC Report`) | data$`MultiQC Report` == "",
      "",
      paste0("<a href='", data$`MultiQC Report`, "' download>MultiQC Report</a>")
    )
    
    data$Report <- ifelse(
      is.na(data$Report) | data$Report == "",
      "",
      paste0("<a href='", data$Report, "' download>Report</a>")
    )
    
    data$DataDictionary <- sapply(data$DataDictionary, function(entry) {
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
    
    data$RawData <- ifelse(
      is.na(data$RawData) | data$RawData == "",
      "",
      paste0("<a href='", data$RawData, "' download>Raw Data</a>")
    )
    
    data$AdditionalFiles <- ifelse(
      is.na(data$AdditionalFiles) | data$AdditionalFiles == "",
      "None",
      paste0("<a href='", data$AdditionalFiles, "' download>Additional Files</a>")
    )
    
    
    # Display the table with proper formatting
    datatable(data, escape = FALSE, options = list(autoWidth = TRUE))
  })
  
  observeEvent(input$logout, {
    user_session(NULL)
    user_csv_url(NULL)
    hide("main-page")
    show("login-page")
    output$login_status <- renderText("✅ Successfully logged out.")
  })
}

shinyApp(ui, server)
