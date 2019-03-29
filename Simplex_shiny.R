# =========================================================================================================================\
# === Método simplex para Maximização com 3 restrições ====================================================================\
# =========================================================================================================================\
rm(list = ls())
#---------------------------------------------------------------------------------------------------------------------
library(shiny); library(xtable); library(latex2exp)
library(dplyr); library(nnet);library(DT)
#---------------------------------------------------------------------------------------------------------------------
# Definindo o UI -----------------------------------------------------------------------------------------------------
fields <- c("z", "x1", "x2", "f1", "f2", "f3", "b")
ui <- fluidPage(
  tags$head(tags$style(
    HTML('
        #save{background-color:black; color:white}
        #do{background-color:black; color:white}
        body, #selector, 
          .container, 
          .navbar-inner {
          background-color: #e3e3e3 !important;
        } 
         #sidebar {
            background-color: #cfcfcf;
        }
        
        body, label, input, button, select { 
          font-family: "Arial";
          color: #000000;
          color-bottom-color: coral;
        }')
  )),
  titlePanel("Método Simplex para Maximização"),
  # Criando a Sidebar 
  sidebarLayout(
    sidebarPanel(id="sidebar",
      textInput("z","z","", width = "20%"),
      textInput("x1","x1","", width = "20%"),
      textInput("x2","x2","", width = "20%"),
      textInput("f1","f1","", width = "20%"), 
      textInput("f2","f2","", width = "20%"), 
      textInput("f3","f3","", width = "20%"), 
      textInput("b","b","", width = "20%"), 
      actionButton("save","Adicionar")),
    # Tabelas de entrada e saída 
    mainPanel(        
      mainPanel(
        DT::dataTableOutput("entradas", width = 500), tags$hr(), # Tabela de entrada
        actionButton("do", "Resolver"), # Botão de ação
        dataTableOutput("table") # Tabela de saída
      )
    )
  )
)
#---------------------------------------------------------------------------------------------------------------------
# Definindo o Server -------------------------------------------------------------------------------------------------
server <- function(input, output, session) {
  # Criando dataframe para as entradas
  saveData <- function(data) {
    data <- as.data.frame(t(data))
    if (exists("entradas")) {
      entradas <<- rbind(entradas, data)
    } else {
      entradas <<- data
    }
  }
  loadData <- function() {
    if (exists("entradas")) {
      entradas
    }
  }
  # Criando forma de entrada 
  formData <- reactive({
    data <- sapply(fields, function(x) input[[x]])
    data
  })
  # Função para salvar os dados
  observeEvent(input$save, {
    saveData(formData())
  })
  output$entradas <- renderDataTable({
    input$save
    loadData()}, rownames = T, 
    options = list(searching = F, paging = F, aoColumnDefs = list(list(sClass="alignRight"))))
observeEvent(input$do, {    
#-----------------------------------------------------------------------------------------------------------------------
# -Iniciando algoritmo de Maximização ----------------------------------------------------------------------------------
  isPos <- function(x) x>=0 # pegar apenas os positivos
  tab <- list()
  i <- 1   
  dados    <- lapply(entradas, function(x) as.numeric(as.character(x)))%>%as.data.frame()
  tab[[i]] <- dados
  exit <- 1
  # Iniciando o Loop
  while(exit == 1) {
    # 1º Identificar posição da variável de entrada na coluna
    col1 <- which.min(tab[[i]][1,]); col1
    # 2º Identificar posição da variável de saída pivô linha 
    row1 <- which.min(Filter(isPos, (tab[[i]][2:4,7]/tab[[i]][2:4, col1]))) + 1; row1
    # 3º Operação com a linha pivô
    arow <- seq(1:dim(tab[[i]])[1])
    a    <- arow[-row1]
    NLP <- tab[[i]][row1,]/tab[[i]][row1,col1]
    # Linha 1
    d1 <- (-tab[[i]][a[1],col1])*NLP + tab[[i]][a[1],]
    rownames(d1) <- a[1]
    # Linha 2
    d2 <- (-tab[[i]][a[2],col1])*NLP + tab[[i]][a[2],]
    rownames(d2) <- a[2]
    # Linha 3
    d3 <- (-tab[[i]][a[3],col1])*NLP + tab[[i]][a[3],]
    rownames(d3) <- a[3]
# - Nova Tabela --------------------------------------------------------------------------------------------------------
    tab1 <- rbind(NLP,d1,d2,d3)%>%as.data.frame(); tab1
    tab1 <- tab1[ order(row.names(tab1)), ]; tab1
    # Verificando se todos elemento de z são positivos
    aux <- isPos(tab1[1,])
    if (all(aux) == TRUE){
      # Criando a saída da solução
      zeros         <- apply(tab1, 2,function(x) length(x[x == 0]))
      vars          <- zeros[zeros == 3]%>%names()  
      prod          <- tab1[,1:6]*tab1[,7]
      prod          <- prod[vars]
      b             <- prod[prod!=0]%>%round(4)
      solu          <- vars%>%as.data.frame()
      solu$..       <- "="
      solu$b        <- b  
      # Adicionando as variáveis em que a solução é 0
      vars.         <- tab1[,!(names(tab1[1:dim(tab1)[2]-1])%in%vars)]
      solu.         <- names(vars.)%>%as.data.frame()
      solu.$..      <- "="
      solu.$b       <- 0
      tabela        <- rbind(solu, solu.)%>%as.data.frame()
      names(tabela) <- c("Variáveis", ".", "Solução") 
      tabela        <- xtable(tabela)
      exit          <- 0
    } else {
      i <- i +1
      tab[[i]] <- tab1
    }
  }
#-----------------------------------------------------------------------------------------------------------------------
#-Saída dos resultados
  output$table <- renderDataTable(datatable(tabela, rownames = F, 
                                  options = list(searching = F, paging = F,
                                                 aoColumnDefs = list(list(sClass="alignRight")))))
  })
}
shinyApp(ui = ui, server = server)
#-----------------------------------------------------------------------------------------------------------------------
#- FIM -----------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------

# Exemplos
# https://www.youtube.com/watch?v=OD0BVZbDieY&index=32&list=PLVWA23fHCKz-XEuEVhTTzc15GiT2-KLTX
# https://www.youtube.com/watch?v=ryyOkGvW0vU&list=PLVWA23fHCKz-XEuEVhTTzc15GiT2-KLTX&index=41
# 

