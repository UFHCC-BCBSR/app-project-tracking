library(shiny)
library(DT)
library(shinyjs)
library(httr)
library(readr)

# Dropbox CSV URLs for each PI (User-specific)
pi_csv_urls <- list(
  "Licht"  = "https://www.dropbox.com/scl/fi/3f6mcl8qqdpy02c1kolhc/Licht.csv?rlkey=uuresz960g9t7c5wnj02ocm8w&st=btr55int&raw=1",
  "Sharma" = "https://www.dropbox.com/scl/fi/3bmwn8u8raxiz9nnehpow/Sharma.csv?rlkey=u1qt2whmtebgw1ujybt2glynp&st=j7gg3qex&raw=1",
  
  "Zhang"  = "https://www.dropbox.com/scl/fi/ee0toedp8ed5ptzi4el9y/Zhang.csv?rlkey=xcbxf4dejaj9lw46hknygay5b&st=8nlef3bq&raw=1"
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

# Corrected logging function (append mode)
log_message <- function(msg) {
  log_path <- file.path(getwd(), "shiny_app_log.txt")
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  
  # Open file in append mode and write log
  tryCatch({
    log_conn <- file(log_path, open = "a")  # Open in append mode
    writeLines(paste0("[", timestamp, "] ", msg), log_conn)
    close(log_conn)
  }, error = function(e) {
    message("❌ Failed to write log: ", e$message)
  })
}


# UI
ui <- fluidPage(
  useShinyjs(),
  
  # Login Page
  div(id = "login-page",
      titlePanel("UFHCC BCB-SR: PI Portal"),
      textInput("username", "Username"),
      passwordInput("password", "Password"),
      actionButton("login", "Login"),
      verbatimTextOutput("login_status")
  ),
  
  # Main App (Hidden until login)
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
  
  # Store user session and corresponding CSV URL
  user_session <- reactiveVal(NULL)
  user_csv_url <- reactiveVal(NULL)
  
  # Login process
  observeEvent(input$login, {
    user <- trimws(input$username)
    pass <- input$password
    log_message(paste("🔑 Login attempt by:", user))
    
    # User credentials
    valid_users <- c("Licht" = "pass1", "Sharma" = "pass2", "Zhang" = "pass3")
    
    if (!is.null(valid_users[[user]]) && valid_users[[user]] == pass) {
      user_session(user)
      csv_url <- pi_csv_urls[[user]]
      user_csv_url(csv_url)
      log_message(paste("✅ Successful login for:", user, "| CSV URL:", csv_url))
      
      hide("login-page")
      show("main-page")
    } else {
      log_message(paste("❌ Failed login for:", user))
      output$login_status <- renderText("❌ Invalid username or password.")
    }
  })
  
  # Reactive polling to check for CSV updates (only if logged in)
  projects <- reactivePoll(
    10000,
    session,
    checkFunc = function() {
      url <- user_csv_url()
      log_message(paste("🔄 Checking for CSV changes at:", url))
      if (!is.null(url)) get_last_modified(url) else Sys.time()
    },
    valueFunc = function() {
      url <- user_csv_url()
      log_message(paste("📥 Fetching CSV from:", url))
      
      if (!is.null(url)) {
        tryCatch({
          data <- read_csv(url, show_col_types = FALSE)
          log_message(paste("✅ Successfully loaded CSV for user:", user_session()))
          return(data)
        }, error = function(e) {
          log_message(paste("❌ Error reading CSV for:", user_session(), ":", e$message))
          return(data.frame(Message = paste("Failed to load projects. Error:", e$message)))
        })
      } else {
        log_message("❌ No CSV URL available for this user.")
        return(data.frame(Message = "No CSV URL available."))
      }
    }
  )
  
  # Render DataTable (only if user is logged in)
  observeEvent(user_csv_url(), {
    output$projects_table <- renderDT({
      req(user_csv_url())  # Ensure URL exists
      log_message(paste("📊 Rendering table for:", user_session()))
      
      data <- projects()
      
      if ("Message" %in% colnames(data)) {
        log_message("❌ Failed to load project data.")
        return(data.frame(Message = data$Message))
      }
      
      # Format columns while checking for blank values
      if (!is.null(data) && nrow(data) > 0) {
        # Convert emails to clickable links (only if not blank)
        data$StudyContact <- ifelse(
          is.na(data$StudyContact) | data$StudyContact == "",
          "",  # Leave blank if missing
          paste0("<a href='mailto:", data$StudyContact, "'>", data$StudyContact, "</a>")
        )
        
        data$Bioinformatician <- ifelse(
          is.na(data$Bioinformatician) | data$Bioinformatician == "",
          "",  # Leave blank if missing
          paste0("<a href='mailto:", data$Bioinformatician, "'>", data$Bioinformatician, "</a>")
        )
        
        # Format RawData link (only if not blank)
        data$RawData <- ifelse(
          is.na(data$RawData) | data$RawData == "",
          "",  # Empty if missing
          paste0("<a href='", data$RawData, "' target='_blank'>Link</a>")
        )
        
        # Handle multiple reports with dropdown (only if not blank)
        data$Report <- sapply(data$Report, function(report) {
          if (is.na(report) || report == "") {
            return("")  # Empty if missing
          }
          
          reports <- unlist(strsplit(report, ";"))
          if (length(reports) > 1) {
            paste0(
              "<select onchange=\"window.open(this.value, '_blank')\">",
              "<option value=''>Select Version</option>",
              paste0("<option value='", reports, "'>Version ", seq_along(reports), "</option>", collapse = ""),
              "</select>"
            )
          } else {
            paste0("<a href='", reports, "' target='_blank'>Report</a>")
          }
        })
      } else {
        log_message("⚠️ No valid project data available.")
        return(data.frame(Message = "No project data available."))
      }
      
      # Display DataTable
      datatable(data, escape = FALSE, options = list(autoWidth = TRUE))
    })
    
  })
  
  # Logout functionality
  observeEvent(input$logout, {
    log_message(paste("👋", user_session(), "logged out."))
    user_session(NULL)
    user_csv_url(NULL)
    hide("main-page")
    show("login-page")
    output$login_status <- renderText("✅ Successfully logged out.")
  })
}

# Run App
shinyApp(ui, server)
