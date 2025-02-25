library(shiny)
library(DT)
library(googlesheets4)
library(shinyjs)

# Authenticate Google Sheets (Ensure gs4_auth() is run once)
gs4_auth()

# Store usernames, passwords, and corresponding Google Sheet IDs
user_credentials <- data.frame(
  username = c("Licht", "PI2", "PI3"),
  password = c("pass1", "pass2", "pass3"),
  sheet_id = c("1AbCdEfGhIjKlMnOpQrStUvWxYz123456",   # Licht's Sheet ID
               "1BcDeFgHiJkLmNoPqRsTuVwXyZ789012345", # PI2's Sheet ID
               "1CdEfGhIjKlMnOpQrStUvWxYz987654321"), # PI3's Sheet ID
  stringsAsFactors = FALSE
)

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
    
    # Validate credentials
    valid_user <- user_credentials[user_credentials$username == user & user_credentials$password == pass, ]
    
    if (nrow(valid_user) == 1) {
      user_session(valid_user)
      hide("login-page")
      show("main-page")
    } else {
      output$login_status <- renderText("❌ Invalid username or password.")
    }
  })
  
  # Load and render PI-specific Google Sheet data
  output$projects_table <- renderDT({
    user_info <- user_session()
    req(user_info)  # Ensure user is logged in
    
    sheet_id <- user_info$sheet_id
    
    tryCatch({
      # Read data from the user's specific Google Sheet
      projects <- read_sheet(sheet_id, col_types = "cccccc")
      
      # Hyperlink formatting
      projects$StudyContact <- paste0("<a href='mailto:", projects$StudyContact, "'>", projects$StudyContact, "</a>")
      projects$Bioinformatician <- paste0("<a href='mailto:", projects$Bioinformatician, "'>", projects$Bioinformatician, "</a>")
      projects$RawData <- paste0("<a href='", projects$RawData, "' target='_blank'>Link</a>")
      
      # Handle multiple reports with dropdown
      projects$Report <- sapply(projects$Report, function(report) {
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
      
      datatable(projects, escape = FALSE, options = list(autoWidth = TRUE))
    }, error = function(e) {
      return(data.frame(Message = "❌ Failed to load projects. Check Google Sheet permissions."))
    })
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
