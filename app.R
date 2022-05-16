############################################
# Arbeitslose in der Stadt Leipzig         #
############################################

library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)

#options(OutDec=".")

####################################
# Datenaufbereitung                #
####################################

#Einlesen der Daten 
arbeitslose <- read.csv(url("https://statistik.leipzig.de/opendata/api/kdvalues?kategorie_nr=7&rubrik_nr=2&periode=y&format=csv"), dec = ",")

#Umbenennen der Jahresspalten
colnames(arbeitslose)[3:24] <- as.character(2000:2021)

arbeitslose[,5] <- as.numeric(gsub(",", ".", arbeitslose[,5]))

#Runde alle numerischen Einträge auf eine Nachkommastellen (die Anteile an den
#Erwerbsfähigen enthalten in dem eingelesenen Datensatz zum Teil mehr als eine 
#Nachkommastellen)
arbeitslose <- arbeitslose %>% 
  mutate(across(is.numeric, round, digits=1))

#Ersetze NA's und Nullen durch ""
arbeitslose[(is.na(arbeitslose)) | (arbeitslose == 0)] <- ""

arbeitslose[2] <- lapply(arbeitslose[2],function(x) {x <- gsub("Anteil an den Erwerbsfähigen","Anteil an den Erwerbsfähigen (in Prozent)",x)})

#Vektor mit Leipziger Stadtbezirken sowie der Stadt Leipzig 
bezirke_und_stadt <- tail(unique(arbeitslose$Gebiet), 11)

####################################
# User Interface                   #
####################################
ui <- fluidPage(theme = shinytheme("readable"),
                navbarPage("Arbeitslose in der Stadt Leipzig",
                           
                           tabPanel("Übersicht",
                                    # Auswahl der Parameter
                                    sidebarPanel(
                                      sliderInput("jahr",
                                                  label = "Jahr",
                                                  value = 2021, 
                                                  min = 2000, 
                                                  max = 2021,
                                                  sep = ""
                                      ),
                                      selectInput("sachmerkmal",
                                                  label = "Sachmerkmal",
                                                  choices = unique(arbeitslose$Sachmerkmal)
                                      )
                                    ),
                                    
                                    mainPanel(
                                      tabsetPanel(
                                        tabPanel("Tabelle", tableOutput("table_uebersicht")),
                                        tabPanel("Diagramm", plotOutput("plot"))
                                      )
                                    )
                    
                           )
                           
                           
                  
                )
)


####################################
# Server                           #
####################################
server <- function(input, output, session){
  
  arbeitslose_jahr <- function(){
    arbeitslose[arbeitslose$Gebiet %in% bezirke_und_stadt,c(1,2,input$jahr-1997)] %>% 
      filter(Sachmerkmal == input$sachmerkmal) %>%
      select(-2)
      #mutate_if(is.numeric,
       #         round,
        #        digits = 1) %>%
      #mutate_all(as.character)
      #filter(year == input$jahr) %>% 
      #top_n(11, prop)
  }
  
  #Extrahiere Zeilen, in denen die gewählten Parameter vorkommen
  #arbeitslose_subset <- d[d$Gebiet %in% bezirke_und_stadt, input$jahr]
  #arbeitslose_bezirke_subset <- arbeitslose_bezirke[arbeitslose_bezirke$Sachmerkmal == "Arbeitslose insgesamt", c("Gebiet", "X2000")]
  
  #Tabelle für Übersichtsseite 
  output$table_uebersicht <- renderTable({arbeitslose_jahr()})
  
  #Diagramm für Übersichtsseite
  output$plot <- renderPlot({if(sum(arbeitslose_jahr()[, 2] == "") != 11)
    {ggplot(data = arbeitslose_jahr(), aes(x = Gebiet, y = as.numeric(unlist(arbeitslose_jahr()[, 2])))) + geom_bar(stat = "identity", fill = "blue") + labs(y = input$sachmerkmal) + theme_bw()}
  })
    
  #})
  
}


####################################
# Erstelle Shiny App               #
####################################
shinyApp(ui = ui, server = server)