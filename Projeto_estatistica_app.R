# Bibliotecas:
library(shiny)
library(dplyr)


df <- read.csv("annual_deaths_by_causes.csv")
# View(deaths_df)


# Funções:
FiltraDados <- function(dados, ano_inicio, ano_fim, pais, morte_causa){
  dados_filtrados <- dados[(dados$year >= ano_inicio & dados$year <= ano_fim), 3 + which(mortes == morte_causa)]
  
  return(dados_filtrados)
}

MontaTabela <- function(dados, ano_inicio, ano_fim, pais, morte_causa){
  dados_filtrados <- FiltraDados(dados, ano_inicio, ano_fim, pais, morte_causa)
  dados_filtrados <- dados_filtrados[!is.na(dados_filtrados)]
  tabela <- data.frame(Classe = morte_causa, media = mean(dados_filtrados),
                       moda = getmode(c('Sem Moda', dados_filtrados)),
                       mediana = median(dados_filtrados))
  return(tabela)
}

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


anos <- sort(c(unique(df$year)))
pais <- c(unique(df$country))
mortes <- c(colnames(df))[c(-1,-2,-3)]


# UI:
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           tableOutput("TabelaDados")
        )
    )
)

# Server:
server <- function(input, output) {
    tabela <- MontaTabela(df, anos[1], anos[30], pais[1], mortes[1])
    
    output$TabelaDados <- renderTable(tabela)
}

# Executa o app:
shinyApp(ui = ui, server = server)
