library(shiny)
library(DT)
library(shinyjs)
library(httr)
library(readr)

 # "Licht"  = "https://www.dropbox.com/scl/fi/3f6mcl8qqdpy02c1kolhc/Licht.csv?rlkey=uuresz960g9t7c5wnj02ocm8w&st=btr55int&raw=1",
library(readxl)

# Dropbox File URLs for each PI
pi_csv_urls <- list(
  #"Licht"  = "https://dl.dropboxusercontent.com/scl/fi/1lvauptlumrljx0q99vjh/Licht.xlsx?rlkey=w06dezs3w1bgdagqha0o744jz&st=sdhwds2n",
  "Licht" = "https://www.dropbox.com/scl/fi/rtl1ugx5q88jdbn75p5cl/Licht.xlsx?rlkey=gkx0fs5kgxovlq83hvopo9h1j&st=24l84lb8&raw=1",
  "Sharma" = "https://www.dropbox.com/scl/fi/pwxbohyjenyjw7wbqrzjb/Sharma.xlsx?rlkey=8lwi7t58bhmv7r7jhvmjben4z&st=sqecbghz&raw=1",
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
# Function to safely read CSV or Excel files from Dropbox
read_data_safe <- function(url) {
  tryCatch({
    # Ensure correct Dropbox format (forces direct file access)
    url <- sub("\\?dl=1$", "?raw=1", url)
    
    # Check file type and set temporary file extension
    temp_file <- tempfile(fileext = ifelse(grepl("\\.xlsx$", url, ignore.case = TRUE), ".xlsx", ".csv"))
    
    # 🔽 Download file from Dropbox
    download.file(url, temp_file, mode = "wb")
    
    # 📥 Read based on file type
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
    req(user_csv_url())  
    data <- projects()
    
    if ("Message" %in% colnames(data)) {
      return(data.frame(Message = data$Message))
    }
    
    # Convert emails and links to clickable format
    data$StudyContact <- ifelse(
      is.na(data$StudyContact) | data$StudyContact == "",
      "", paste0("<a href='mailto:", data$StudyContact, "'>", data$StudyContact, "</a>")
    )
    
    data$Bioinformatician <- ifelse(
      is.na(data$Bioinformatician) | data$Bioinformatician == "",
      "", paste0("<a href='mailto:", data$Bioinformatician, "'>", data$Bioinformatician, "</a>")
    )
    
    data$RawData <- ifelse(
      is.na(data$RawData) | data$RawData == "",
      "", paste0("<a href='", data$RawData, "' target='_blank'>Link</a>")
    )
    
    data$Report <- sapply(data$Report, function(report) {
      if (is.na(report) || report == "") {
        return("")  
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
