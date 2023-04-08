# Bibliotecas:
library(shiny)
library(dplyr)


# Dados:
df <- read.csv("annual_deaths_by_causes.csv")
anos <- sort(c(unique(df$year)))
pais <- sort(unique(df$country))
mortes <- colnames(df[c(-1,-2,-3)])
mortes_alfabetica <- sort(mortes)
mortes_alfabetica

# Funções:
FiltraDados <- function(dados, ano_inicio, ano_fim, pais, morte_causa){
  dados_filtrados <- dados[(dados$year >= ano_inicio & dados$year <= ano_fim &
                            dados$country == pais), 3 + which(mortes == morte_causa)]
  
  return(dados_filtrados)
}

MontaTabela <- function(dados, ano_inicio, ano_fim, pais, morte_causa){
  dados_filtrados <- FiltraDados(dados, ano_inicio, ano_fim, pais, morte_causa)
  moda_dados <- 
  tabela <- data.frame(Classe = morte_causa,
                       Media = mean(dados_filtrados),
                       Moda = getmode(c('Sem Moda', dados_filtrados)),
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


# UI:
ui <- fluidPage(

    # Application title
    titlePanel("Numero de mortes"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput('idPais', 'País:', choices = pais, selected = 'Brazil'),
          selectInput('idMortes', 'Causa de mortes:', choices = mortes_alfabetica, selected = 'road_injuries'),
          actionButton('idBotaoFiltro', 'Filtrar')
        ),

        # Show a plot of the generated distribution
        mainPanel(
           tableOutput("TabelaDados")
        )
    )
)

# Server:
server <- function(input, output) {
    tabela <- eventReactive(input$idBotaoFiltro, ignoreNULL = FALSE, {
        MontaTabela(df, anos[1], anos[30], input$idPais, input$idMortes)
    })
    
    output$TabelaDados <- renderTable(tabela())
    
    
}

# Executa o app:
shinyApp(ui = ui, server = server)
