library(shiny)
library(DT)
library(shinyjs)
library(httr)
library(readr)

# Dropbox CSV URL (ensure ?raw=1 at the end)
csv_url <- "https://www.dropbox.com/scl/fi/3f6mcl8qqdpy02c1kolhc/Licht.csv?rlkey=uuresz960g9t7c5wnj02ocm8w&st=btr55int&raw=1"

# Function to check last modification time of the file
get_last_modified <- function(url) {
  tryCatch({
    headers <- HEAD(url)$headers
    as.numeric(as.POSIXct(headers[["last-modified"]], format = "%a, %d %b %Y %H:%M:%S GMT", tz = "GMT"))
  }, error = function(e) {
    Sys.time()  # Fallback if header fails
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
  
  # User session storage
  user_session <- reactiveVal(NULL)
  
  # Login process
  observeEvent(input$login, {
    user <- input$username
    pass <- input$password
    
    # Dummy user credentials (replace with secure method if needed)
    valid_user <- user %in% c("Licht", "PI2", "PI3") && pass %in% c("pass1", "pass2", "pass3")
    
    if (valid_user) {
      user_session(TRUE)
      hide("login-page")
      show("main-page")
    } else {
      output$login_status <- renderText("❌ Invalid username or password.")
    }
  })
  
  # Reactive polling to check for CSV updates every 10 seconds
  projects <- reactivePoll(
    10000,  # Check every 10 seconds
    session,
    checkFunc = function() get_last_modified(csv_url),
    valueFunc = function() {
      tryCatch({
        read_csv(csv_url, show_col_types = FALSE)
      }, error = function(e) {
        message("❌ Failed to read CSV: ", e$message)
        data.frame(Message = "Failed to load projects.")
      })
    }
  )
  
  # Render DataTable
  output$projects_table <- renderDT({
    data <- projects()
    
    # Convert emails and links to clickable format
    data$StudyContact <- paste0("<a href='mailto:", data$StudyContact, "'>", data$StudyContact, "</a>")
    data$Bioinformatician <- paste0("<a href='mailto:", data$Bioinformatician, "'>", data$Bioinformatician, "</a>")
    data$RawData <- paste0("<a href='", data$RawData, "' target='_blank'>Link</a>")
    
    # Handle multiple reports with dropdown
    data$Report <- sapply(data$Report, function(report) {
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
    
    # Display table
    datatable(data, escape = FALSE, options = list(autoWidth = TRUE))
  })
  
  # Logout functionality
  observeEvent(input$logout, {
    user_session(NULL)
    hide("main-page")
    show("login-page")
    output$login_status <- renderText("✅ Successfully logged out.")
  })
}

# Run App
shinyApp(ui, server)
