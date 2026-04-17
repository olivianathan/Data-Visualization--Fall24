#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(shiny)
library(ggplot2)
library(DT)
library(readxl)
library(dplyr)
library(stringr)

# Load the data
offenses_data <- read_excel("data/Offenses_Involving_Narcotics_and_Alcohol.xlsx", 
                            skip = 3)

# Rename columns
colnames(offenses_data) <- c('Offense Category', 'Total Offenses', 
                             'Drugs/ Narcotics Offenses', 
                             'Percentage of Offense Category', 'Alcohol Offenses', 
                             'Percentage of Offense')

# Remove rows with text that is non-data
offenses_data <- offenses_data %>%
  filter(!grepl("figures under Drugs/Narcotics and Alcohol", `Offense Category`))


crime_categories <- c("Total", "Crimes Against Persons", "Crimes Against Property",
                      "Crimes Against Society")

specific_crimes <- list(
  "Crimes Against Persons" = c("Assault Offenses", "Homicide Offenses", 
                               "Human Trafficking Offenses",
                               "Kidnapping/ Abduction", "Sex Offenses"),
  "Crimes Against Property" = c("Arson", "Bribery", "Burglary/ Breaking & Entering", 
                                "Counterfeiting/ Forgery", 
                                "Destruction/ Damage/ Vandalism", 
                                "Embezzlement", "Extortion/ Blackmail", 
                                "Fraud Offenses", 
                                "Larceny/ Theft Offenses", "Motor Vehicle Theft", 
                                "Robbery", 
                                "Stolen Property Offenses"),
  "Crimes Against Society" = c("Animal Cruelty", "Drug /Narcotic Offenses", 
                               "Gambling Offenses", 
                               "Pornography/ Obscene Material", 
                               "Prostitution Offenses", 
                               "Weapon Law Violations")
)

# Define UI
ui <- navbarPage("Offenses Involving Drugs/Narcotics and Alcohol in the US in 2023",
                 
                 # First Tab: Data Visualization
                 tabPanel("Visualization",
                          # Title and overview
                          titlePanel("Offenses Involving Drugs/Narcotics and Alcohol"),
                          HTML("<p><strong>Overview:</strong> This application 
                          allows you to explore data on offenses involving 
                          drugs/narcotics and alcohol. You can filter by crime 
                          category, drug percentage, and alcohol percentage to view
                          a detailed analysis of different types of offenses and see 
                          the impact of drugs and alcohol on various crime 
                          categories.</p>"),
                          
                          selectInput("crime_category", "Select Crime Category", 
                                      choices = crime_categories),
                          
                          sliderInput("drug_percentage", "Drug Percentage", min = 0, 
                                      max = 100, 
                                      value = c(0, 100)),
                          sliderInput("alcohol_percentage", "Alcohol Percentage", 
                                      min = 0, max = 100, 
                                      value = c(0, 100)),
                          
                          DTOutput("table_output"),
                          
                          plotOutput("offense_plot"),
                          
                          HTML("<br><br><p><em>Source:</em> 
                               https://www.fbi.gov/how-we-can-help-you/more-fbi
                               -services-and-information/ucr., 2023.</p>")
                 ),
                 
                 # Second Tab: Information Tab
                 tabPanel("Additional Information",
                          p("This application allows you to explore the relationship
                          between narcotics, alcohol, and various crime categories. 
                          By using the filters to adjust the drug and alcohol 
                          percentages, users can see how different levels of 
                          involvement affect crime rates. Visually through the bar 
                          graphs, we can proportionally analyze the types of crimes 
                          that have offenses including narcotic and alcohol use, 
                          establishing causation trends and predictions, furthermore 
                          giving users and understanding into  how crime rates are 
                          linked to substance involvement. This data and visualization 
                          demonstrates how crime trends over time, which is imperative 
                          when concerned with the overall safety and well being of 
                          communities. While this graphic focuses on the United States,
                          large trends in the country as a whole can make a big impact 
                          across regions, especially in major cities. Identifying 
                          temporal patterns and shifts in crime behavior that would be
                          harder to detect with static graphs so creating an 
                          interactive app allows for users to compare different 
                          categories of the data with different variables easily. 
                          This app allows users to filter the data by offense type 
                          and the 'Drug Percentage' and 'Alcohol Percentage' sliders 
                          let users adjust these variables to see how they influence 
                          the overall crime rates. While the values in the table are
                          given to you based on the offense type, you are able to sort
                          them either by their given order, or by ascending or 
                          descending values of a column.
")
              
                 )
)

# Define server logic
server <- function(input, output, session) {
  
  # Filter data based on input
  filtered_data <- reactive({
    req(input$crime_category)  
    
    if(input$crime_category == "Total") {
      filtered <- offenses_data %>%
        filter(!grepl("^Crimes Against", `Offense Category`), 
               `Percentage of Offense Category` >= input$drug_percentage[1],
               `Percentage of Offense Category` <= input$drug_percentage[2],
               `Percentage of Offense` >= input$alcohol_percentage[1],
               `Percentage of Offense` <= input$alcohol_percentage[2])
    } else {
      
      specific_crimes_selected <- specific_crimes[[input$crime_category]]
      
      filtered <- offenses_data %>%
        filter(`Offense Category` %in% specific_crimes_selected, 
               `Percentage of Offense Category` >= input$drug_percentage[1],
               `Percentage of Offense Category` <= input$drug_percentage[2],
               `Percentage of Offense` >= input$alcohol_percentage[1],
               `Percentage of Offense` <= input$alcohol_percentage[2])
    }
    
    return(filtered)
  })
  
  # Render the filtered table
  output$table_output <- renderDT({
    req(filtered_data()) 
    
    filtered_data() %>%
      mutate(
        `Percentage of Offense Category` = 
          scales::percent(`Percentage of Offense Category`),
        `Percentage of Offense` = scales::percent(`Percentage of Offense`)
      ) %>%
      datatable()
  })
  
  # Render the plot based on Crime Category
  output$offense_plot <- renderPlot({
    req(filtered_data())
    
    # Exclude "Total" category when plotting
    plot_data <- filtered_data() %>%
      filter(`Offense Category` != "Total")
    
    # Inserting line breaks on x-axis
    plot_data$`Offense Category` <- str_wrap(plot_data$`Offense Category`, width = 8) 
    
    ggplot(plot_data, aes(x = `Offense Category`, y = `Total Offenses`, 
                          fill = `Offense Category`)) +
      geom_bar(stat = "identity") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1, 
                                       size = 10, lineheight = 0.9)) +
      labs(title = paste("Offenses in", input$crime_category), 
           x = "Offense Category", 
           y = "Total Offenses") +
      scale_y_continuous(labels = scales::label_comma()) +
      theme_minimal() +
      theme(plot.margin = margin(10, 10, 30, 10), 
            legend.position = "none")  
  })
}

# Run the application
shinyApp(ui = ui, server = server)
