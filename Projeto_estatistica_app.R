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
ColunaMorte <- function(morte_causa) {
  return(3 + which(mortes == morte_causa))
}

MontaTabela1Classe <- function(dados, morte_causa){
  paises <- c(unique(dados$country))
  coluna <- ColunaMorte(morte_causa)
  
  pais_tabela <- c()
  morte_tabela <- c()
  media_tabela <- c()
  moda_tabela <- c()
  mediana_tabela <- c()
  desvio_tabela <- c()
  min_tabela <- c()
  max_tabela <- c()
  for (p in paises){
    pais_tabela <- c(pais_tabela, p)
    morte_tabela <- c(morte_tabela, morte_causa)
    media_tabela <- c(media_tabela, mean(dados[dados$country == p, coluna]))
    moda_tabela <- c(moda_tabela, getmode(FormataModa(dados[dados$country == p, coluna])))
    mediana_tabela <- c(mediana_tabela, median(dados[dados$country == p, coluna]))
    desvio_tabela <- c(desvio_tabela, sd(dados[dados$country == p, coluna]))
    min_tabela <- c(min_tabela, min(dados[dados$country == p, coluna]))
    max_tabela <- c(max_tabela, max(dados[dados$country == p, coluna]))
  }
  tabela <- data.frame(
    País = pais_tabela,
    Classe = morte_tabela,
    Media = media_tabela,
    Moda = moda_tabela,
    Mediana = mediana_tabela,
    Desvio_Padrao = desvio_tabela,
    Maximo = max_tabela,
    Minimo = min_tabela
  )
  return(tabela)
} 

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

FormataModa <- function(dados_analise) {
  if (length(dados_analise) > 1) {
    return(c('Sem Moda', dados_analise))
  }
  else {
    return(dados_analise)
  }
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
    # Pagina 1:
    tabItem(tabName = '1classe',
      # Filtros:
      fluidRow(
        column(width = 12,
          box(width = '100%',
            column(
              width = 6,
              selectizeInput('idPais', 'País:', options = list(maxItems = 5), choices = pais, selected = 'Brazil')
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
      
      # Graficos:
      fluidRow(
        column(width = 12,
          box(width = '100%',
            column(width = 7, plotOutput("GraficoLinha")),
            column(width = 5, plotOutput("GraficoBoxplot"))
          )
        )
      ),
      
      # Tabela:
      fluidRow(
        column(width = 12,
          box(width = '100%',
            column(width = 12, align = 'center',tableOutput("TabelaDados"))
          )
        )
      )
    ),
    
    # Pagina 2:
    tabItem(tabName = '2classes',
      # Filtros:
      fluidRow(
        column(width = 12,
          box(width = '100%',
            column(
              width = 6,
              selectInput('idPaisCompara', 'País:', choices = pais, selected = 'Brazil')
            ),
            column(
              width = 6,
              selectizeInput('idMortesCompara', 'Causa de mortes:', options = list(maxItems = 2), choices = mortes_alfabetica, selected = 'road_injuries')
            ),
            column(
              width = 12,
              sliderInput('idAnosCompara', 'Anos:', min = min(anos), max = max(anos), value = c(min(anos), max(anos)), sep = "", width = '100%', step = 1)
            ),
            column(width = 12, actionButton('idBotaoFiltroCompara', 'Filtrar'))
          )
        )
      ),
      
      # Graficos:
      fluidRow(
        column(width = 12,
          box(width = '100%',
            column(width = 12, plotOutput("GraficoLinhaCompara"))
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
  # Pagina 1:
    dados_filtrados <- eventReactive(input$idBotaoFiltro, ignoreNULL = FALSE, {
      return(df[(df$country %in% input$idPais) & df$year >= input$idAnos[1] & df$year <= input$idAnos[2],])
    })
    
    Morte_selecionado <- eventReactive(input$idBotaoFiltro, ignoreNULL = FALSE, {
      return(input$idMortes)
    })

    tabela <- eventReactive(input$idBotaoFiltro, ignoreNULL = FALSE, {
      MontaTabela1Classe(dados_filtrados(), input$idMortes)
    })
    
        
    output$GraficoLinha <- renderPlot({
      ggplot(dados_filtrados(), aes(x=year, y=dados_filtrados()[, ColunaMorte(Morte_selecionado())], group = country, color = country)) +
        geom_line(size = 2) + labs(title="Grafico de linha:", x = "Anos", y = "Numero de mortes")
    })
    
    output$GraficoBoxplot <- renderPlot({
      ggplot(dados_filtrados(), aes(x=country, y=dados_filtrados()[, ColunaMorte(Morte_selecionado())], group = country, color = country)) +
        geom_boxplot() + labs(title="Grafico de boxplot:", x = "País", y = "Numero de mortes") +
        theme(axis.text.x=element_blank())
    })
    
    output$TabelaDados <- renderTable(tabela())
    
    
    
    # Pagina 2:
    dados_filtrados_compara <- eventReactive(input$idBotaoFiltroCompara, ignoreNULL = FALSE, {
      dados <- df[(df$country %in% input$idPaisCompara) & df$year >= input$idAnosCompara[1] & df$year <= input$idAnosCompara[2],]
      
      pais <- c()
      ano <- c()
      valor_morte <- c()
      tipo_morte <- c()
      for (m in c(1 : length(input$idMortesCompara))){
        for (i in c(1 : length(dados[, ColunaMorte(input$idMortesCompara[m])]))){
          pais <- c(pais , dados[i, 1])
          ano <- c(ano , dados[i, 3])
          valor_morte <- c(valor_morte , dados[i, ColunaMorte(input$idMortesCompara[m])])
          tipo_morte <- c(tipo_morte, input$idMortesCompara[m])
        }
      }
      dados_formatados <- data.frame(
                          País = pais,
                          Ano = ano,
                          Numeros = valor_morte,
                          Classe = tipo_morte
      )
      return(dados_formatados)
    })
    
    
    output$GraficoLinhaCompara <- renderPlot({
      ggplot(dados_filtrados_compara(), aes(x=Ano, y=Numeros, group = Classe, color = Classe)) +
        geom_line(size = 2) + labs(title="Grafico de linha:", x = "Anos", y = "Numero de mortes")
    })
}

# Executa o app:
shinyApp(ui = ui, server = server)
