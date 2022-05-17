############################################
# Arbeitslose in der Stadt Leipzig         #
############################################

library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)

####################################
# Datenaufbereitung                #
####################################

#Einlesen der Daten 
arbeitslose <- read.csv(url("https://statistik.leipzig.de/opendata/api/kdvalues?kategorie_nr=7&rubrik_nr=2&periode=y&format=csv"), dec = ",")

#Umbenennen der Jahresspalten
colnames(arbeitslose)[3:24] <- as.character(2000:2021)

#Konvertieren der Spalte "2002" in den Datentyp numeric (andere Jahrsspalten sind
#bereits numerisch)
arbeitslose[,5] <- as.numeric(gsub(",", ".", arbeitslose[,5]))

#Runde alle numerischen Einträge auf eine Nachkommastellen (die Anteile an den
#Erwerbsfähigen enthalten in dem eingelesenen Datensatz zum Teil mehr als eine 
#Nachkommastellen)
arbeitslose <- arbeitslose %>% 
  mutate(across(is.numeric, round, digits=1))

#Ersetze NA's und Nullen durch ""
arbeitslose[(is.na(arbeitslose)) | (arbeitslose == 0)] <- ""

#Ergänze hinter "Anteil an den Erwerbsfähigkeiten" den Zusatz "(in Prozent)"
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
                                        tabPanel("Tabelle", tableOutput("table_uebersicht"),
                                                 p("Quelle:",
                                                   a("Open Data-Portal der Stadt Leipzig", 
                                          href = "https://opendata.leipzig.de/"))),
                                        tabPanel("Diagramm", plotOutput("plot"),
                                                 p("Quelle:",
                                                   a("Open Data-Portal der Stadt Leipzig", 
                                                     href = "https://opendata.leipzig.de/")))
                                      )
                                    )
                           ),
                           
                           tabPanel("Ortsteile/Stadtbezirke",
                                    # Auswahl des Ortsteils/Stadtbezirks
                                    sidebarPanel(
                                    selectInput("gebiet",
                                                label = "Ortsteil/Stadtbezirk",
                                                choices = sort(unique(arbeitslose$Gebiet))),
                                    selectInput("sachmerkmal2",
                                                label = "Sachmerkmal",
                                                choices = unique(arbeitslose$Sachmerkmal))
                                    ),
                                    
                                    mainPanel(
                                      tabsetPanel(
                                        tabPanel("Tabelle", tableOutput("table_gebiet"), 
                                                 p("Quelle:",
                                                   a("Open Data-Portal der Stadt Leipzig", 
                                                     href = "https://opendata.leipzig.de/"))),
                                        tabPanel("Diagramm", plotOutput("plot_gebiet"),
                                                 p("Quelle:",
                                                   a("Open Data-Portal der Stadt Leipzig", 
                                                     href = "https://opendata.leipzig.de/")))
                                      )
                                    )
                                    ),
                           
                           tabPanel("Informationen",
                             p("Im Februar 2004 hat der Stadtrat der Stadt Leipzig das Konzept der
                               Strategischen Kommunalpolitik beschlossen. Das Ziel besteht darin, die
                               Finanzmittel der Stadt im Sinne festgelegter strategischer Ziele einzusetzen.
                               Die Hauptmotivation hinter dem Konzept der Strategischen Kommunalpolitik ist, die
                               knappen Finanzmittel der Stadt so einzusetzen, dass sie in optimaler Weise dazu beitragen,
                               die Lebensqualität, die wirtschaftliche Stärke und die Attraktivität der Stadt Leipzig
                               zu sichern. Die einzelnen Ziele sind auf der", a("Internetseite der Stadtverwaltung",
                               href = "https://www.leipzig.de/buergerservice-und-verwaltung/stadtverwaltung/strategische-kommunalpolitik"),
                               "näher beschrieben."),
                             
                             p("Eines der strategischen Ziele der Stadt Leipzig ist die Schaffung von Rahmenbedingungen
                               zum Erhalt bzw. zur Neuschaffung von Arbeitsplätzen. Um die Erreichung dieses Ziels beurteilen zu 
                               können, ist es wichtig, die Erwerbstätigkeit und den Arbeitsmarkt in der Stadt Leipzig
                               zu beobachten. Zu diesem Sachgebiet stellt das Amt für Statistik und Wahlen der Stadt Leipzig verschiedene 
                               Daten zur Verfügung. Diese können, so wie auch die anderen durch das Amt veröffentlichten Daten, im", 
                               a("Leipzig-Informationssystem", href = "https://statistik.leipzig.de/"), "und im", a("Open Data-Portal der Stadt Leipzig", 
                               href = "https://opendata.leipzig.de/"), "gefunden werden."),
                             
                             p("Die vom Amt für Statistik und Wahlen der Stadt Leipzig im Sachgebiet Erwerbstätigkeit und Arbeitsmarkt 
                               erhobenen und veröffentlichten Daten umfassen unter anderem verschiedene Daten zur Arbeitslosigkeit in
                               Leipzig, z. B. die Anzahl der Arbeitslosen insgesamt, den Anteil der Arbeitslosen an allen Erwerbsfähigen, die Anzahl 
                               der Langzeitarbeitslosen und die Anzahl der Frauen bzw. der Männer unter den Arbeitslosen. Das Ziel dieser Anwendung
                               ist, die Übersicht über das umfangreiche Datenmaterial zu erleichtern."),
                             
                             p("Auf der Seite Übersicht können Sie ein Jahr (2000-2021) sowie ein Sachmerkmal auswählen. Die Daten für das ausgewählte Jahr und
                             das ausgewählte Sachgebiet werden dann für alle Leipziger Stadtbezirke sowie für die Stadt Leipzig insgesamt ausgegeben. Auf der 
                             Seite Stadtbezirke/Ortsteile können Sie einen Stadtbezirk oder einen Ortsteil sowie ein Sachmerkmal auswählen. Sie erhalten dann eine
                             Darstellung der Daten für das ausgewählte Sachmerkmal und für den ausgewählten Stadtbezirk/Ortsteil für die Jahre
                             2000-2021. Einzelne Daten können dabei fehlen, etwa weil das jeweilige Sachmerkmal nicht in jedem Jahr oder zumindest nicht kleinräumig 
                             erhoben wurde.")
                               
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
  }
  
  arbeitslose_gebiet <- function(){
    arbeitslose %>% 
      filter((Sachmerkmal == input$sachmerkmal2) & (Gebiet == input$gebiet)) %>%
      select(-c(1,2)) 
  }
  
  #Tabelle für Übersichtsseite 
  output$table_uebersicht <- renderTable({arbeitslose_jahr()})
  
  #Diagramm für Übersichtsseite
  output$plot <- renderPlot({if(sum(arbeitslose_jahr()[, 2] == "") != 11)
  {ggplot(data = arbeitslose_jahr(), 
            aes(x = Gebiet, y = as.numeric(unlist(arbeitslose_jahr()[, 2])))) + geom_bar(stat = "identity", fill = "blue") + labs(y = input$sachmerkmal) + theme_bw()}
  })
  
  #Tabelle für einzelne Stadtbezirke/Ortsteile
  output$table_gebiet <- renderTable({matrix(c(as.character(2000:2021), 
                                               arbeitslose_gebiet()), 
                                               ncol= 2, 
                                               dimnames = list(2000:2021, 
                                               c("Jahr", c(input$sachmerkmal2))))
    }) 
  
  #Plot für einzelne Stadtbezirke/Ortsteile
  output$plot_gebiet <- renderPlot({
    plot(2000:2021, as.numeric(arbeitslose_gebiet()), type = "b", pch = 19, 
         col = "blue", xlab = "Jahr", ylab = input$sachmerkmal2)
  })
  
}


####################################
# Erstelle Shiny App               #
####################################
shinyApp(ui = ui, server = server)