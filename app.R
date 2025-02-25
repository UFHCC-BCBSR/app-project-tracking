library(shiny)
library(DT)
library(readr)
library(shinyjs)

# Store usernames, passwords, and corresponding CSVs
user_credentials <- data.frame(
  username = c("Licht", "PI2", "PI3"),
  password = c("pass1", "pass2", "pass3"),
  csv_path = c("data/Licht.csv", "data/PI2.csv", "data/PI3.csv"),
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
  
  # Render PI-specific table
  output$projects_table <- renderDT({
    user_info <- user_session()
    req(user_info)  # Ensure user is logged in
    
    csv_path <- user_info$csv_path
    if (file.exists(csv_path)) {
      projects <- read_csv(csv_path, show_col_types = FALSE)
      
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
    } else {
      return(data.frame(Message = "No projects found for this PI."))
    }
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
