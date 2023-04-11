# Bibliotecas:
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)



# Dados:
df <- read.csv("annual_deaths_by_causes.csv")
anos <- sort(c(unique(df$year)))
pais <- sort(unique(df$country))
mortes <- colnames(df[c(-1,-2,-3)])
mortes_alfabetica <- sort(mortes)
mortes_alfabetica



# Funções:
FiltraDados <- function(dados, morte_causa){
  dados_filtrados <- dados[, ColunaMorte(morte_causa)]
  
  return(dados_filtrados)
}

MontaTabela <- function(dados, morte_causa){
  dados_filtrados <- FiltraDados(dados, morte_causa)
  tabela <- data.frame(Classe = morte_causa,
                       Media = mean(dados_filtrados),
                       Moda = getmode(FormataModa(dados_filtrados)),
                       Mediana = median(dados_filtrados),
                       Desvio_Padrao = sd(dados_filtrados),
                       Maximo = max(dados_filtrados),
                       Minimo = min(dados_filtrados))
  return(tabela)
}

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

FormataModa <- function(dados) {
  if (length(dados) > 1) {
    return(c('Sem Moda', dados))
  }
  else {
    return(dados)
  }
}

ColunaMorte <- function(morte_causa) {
  return(3 + which(mortes == morte_causa))
}



# Dashboard:
cabecalho <- dashboardHeader(title = 'Mortes por causa')

barra_lateral <- dashboardSidebar(
  sidebarMenu(
    menuItem('Analisa classe', tabName = '1classe'),
    menuItem('Compara Classes', tabName = '2classes') 
  )
)

corpo <- dashboardBody(
  tabItems(
    tabItem(tabName = '1classe', 
      fluidRow(
        column(width = 12,
          box(width = '100%',
            column(
              width = 6,
              selectInput('idPais', 'País:', choices = pais, selected = 'Brazil')
            ),
            column(
              width = 6,
              selectInput('idMortes', 'Causa de mortes:', choices = mortes_alfabetica, selected = 'road_injuries')
            ),
            column(
              width = 12,
              sliderInput('idAnos', 'Anos:', min = min(anos), max = max(anos), value = c(min(anos), max(anos)), sep = "", width = '100%', step = 1)
            ),
            column(width = 12, actionButton('idBotaoFiltro', 'Filtrar'))
          )
        )
      ),
      fluidRow(
        column(width = 12,
          box(width = '100%',
            column(width = 8, plotOutput("GraficoLinha")),
            column(width = 4, plotOutput("GraficoBoxplot"))
          )
        )
      ),
      fluidRow(
        column(width = 12,
          box(width = '100%',
            column(width = 12, align = 'center',tableOutput("TabelaDados"))
          )
        )
      )
    ),
    tabItem(tabName = '2classes',
      fluidRow(
        column(width = 12,
          box(width = '100%',
            column(width = 12,
            
            )
          )
        )
      )
    ) 
  )
)



# UI:
ui <- dashboardPage(header = cabecalho, sidebar = barra_lateral, body = corpo)

# Server:
server <- function(input, output) {
    dados_filtrados <- eventReactive(input$idBotaoFiltro, ignoreNULL = FALSE, {
      return(df[df$country == input$idPais & df$year >= input$idAnos[1] & df$year <= input$idAnos[2],])
    })
    
    Morte_selecionado <- eventReactive(input$idBotaoFiltro, ignoreNULL = FALSE, {
      return(input$idMortes)
    })
    
    ano_inicio <- reactive(input$idAnoInicio)

    tabela <- eventReactive(input$idBotaoFiltro, ignoreNULL = FALSE, {
        MontaTabela(dados_filtrados(), input$idMortes)
    })

        
    output$GraficoLinha <- renderPlot({
      ggplot(dados_filtrados(), aes(x=year, y=dados_filtrados()[, ColunaMorte(Morte_selecionado())])) +
        geom_line() + labs(title="Grafico de linha:", x = "Ano", y = "Causa da morte")
    })
    
    output$GraficoBoxplot <- renderPlot({
      ggplot(dados_filtrados(), aes(x=year, y=dados_filtrados()[, ColunaMorte(Morte_selecionado())])) +
        geom_boxplot() + labs(title="Grafico de boxplot:", x = "Ano", y = "Causa da morte")
    })
    
    output$TabelaDados <- renderTable(tabela())
}

# Executa o app:
shinyApp(ui = ui, server = server)
