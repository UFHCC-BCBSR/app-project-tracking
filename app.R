library(shiny)
library(DT)
library(readr)

# Read projects from local CSV
get_projects <- function() {
  read_csv("data/projects.csv", show_col_types = FALSE)
}

ui <- fluidPage(
  titlePanel("UFHCC BCB-SR: Licht Lab Project Tracking and Products"),
  DTOutput("projects_table"),
  br(),
  p("For inquiries related to the data and projects presented here, contact ",
    a("BCB-SR", href = "mailto:example@ufhcc.edu"))
)

server <- function(input, output, session) {
  output$projects_table <- renderDT({
    projects <- get_projects()
    projects$StudyContact <- paste0("<a href='mailto:", projects$StudyContact, "'>", projects$StudyContact, "</a>")
    projects$Bioinformatician <- paste0("<a href='mailto:", projects$Bioinformatician, "'>", projects$Bioinformatician, "</a>")
    projects$RawData <- paste0("<a href='", projects$RawData, "' target='_blank'>Link</a>")
    
    # Handle multiple report links with versioning
    projects$Report <- sapply(projects$Report, function(report) {
      reports <- unlist(strsplit(report, ";"))
      if (length(reports) > 1) {
        paste0("<select onchange=\"window.open(this.value, '_blank')\">",
               "<option value=''>Select Version</option>",
               paste0("<option value='", reports, "'>Version ", seq_along(reports), "</option>", collapse = ""),
               "</select>")
      } else {
        paste0("<a href='", reports, "' target='_blank'>Report</a>")
      }
    })
    
    datatable(projects, escape = FALSE, options = list(autoWidth = TRUE))
  })
}

shinyApp(ui, server)