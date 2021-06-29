# Pacotes -----------------------------------------------------------------

library(shiny)
library(bs4Dash)
library(thematic)
library(readr)
library(data.table)
library(tidyr)
library(dplyr)
library(echarts4r)
library(leaflet)
library(prophet)
library(shinyWidgets)
library(formattable)
library(dygraphs)
library(readxl)
library(waiter)

# Server Side -------------------------------------------------------------

server <- function(input, output, session) {
  
  # Configurações do Loading (Tela de Carregamento - Gráficos)
  w <- Waiter$new(
    id = c("GeoBrasil","NumEstado","TimeEstado","ConfirmedE_BR","DeathsE_BR"), 
    # Estilo do Spin
    html = spin_whirly(), 
    # Cor de fundo
    color = transparent(.5))
  
  # Tema - Auto Color
  useAutoColor()
  
  # Modo Claro | Escuro
  observeEvent(input$dark_mode, {
    toast(
      title = if (input$dark_mode) "Modo escuro ativado!" else "Modo claro ativado",
      options = list(position = "topRight", class = "bg-warning", autohide = TRUE, delay = 2000)
    )
  })
  
  # Base de Dados
  # Carregamento
  w$show()
  # Série Temporal - Brasil
  citiesTimes <- gzcon(url(paste("https://github.com/wcota/covid19br/blob/master/cases-brazil-cities-time.csv.gz?raw=false",
                                 "citiesTimes.csv.gz", sep="")))
  citiesTimes <- read_csv(citiesTimes)
  #citiesTimes <- read_csv(url('https://github.com/wcota/covid19br/blob/master/cases-brazil-cities-time.csv.gz?raw=true'))
  # Cidades
  cities <- read_csv(url('https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-cities.csv'))
  # GPS Cidades
  citiesGPS <- read_csv(url('https://raw.githubusercontent.com/wcota/covid19br/master/cases-gps.csv'))
  # Estados
  states <- read_csv(url('https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-states.csv'))
  # Total Brasil - Estados
  brasil <- read_csv(url('https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-total.csv'))
  # Base de Recuperados Global
  recup_global <- read_csv(url('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv'))
  # Carregamento
  w$hide()
  
  # Caixa de Informações - Brasil (Confirmados e Mortes)
  output$ConfirmedBR <- renderbs4InfoBox({
    bs4InfoBox(
      title = 'Confirmados',
      value = subset(brasil$totalCasesMS, brasil$state == 'TOTAL'),
      icon = shiny::icon('head-side-cough'),
      color = 'warning',
      elevation  = 4
    )
  })
  output$DeathBR <- renderbs4InfoBox({
    bs4InfoBox(
      title = 'Mortes',
      value = subset(brasil$deathsMS, brasil$state == 'TOTAL'),
      icon = shiny::icon('chart-line'),
      color = 'danger',
      elevation  = 4
    )
  })
  output$indBR <- renderbs4InfoBox({
    bs4InfoBox(
      title = 'Índice Mortalidade',
      value = paste0(subset(round(brasil$deathsMS/brasil$totalCasesMS * 100, digits = 2), brasil$state == 'TOTAL'),"%"),
      icon = shiny::icon('percent'),
      color = 'danger',
      elevation  = 4
    )
  })
  output$recupBR <- renderbs4InfoBox({
    bs4InfoBox(
      title = 'Recuperados',
      value = subset(brasil$recovered, brasil$state == 'TOTAL'),
      icon = shiny::icon('head-side-cough-slash'),
      color = 'success',
      elevation  = 4
    )
  })
  
  # Mapa - Distribuição do COVID-19 no Brasil
  output$GeoBrasil <- renderLeaflet({
    citiesGPS <- subset(citiesGPS, citiesGPS$type == "1")
    citiesGPS %>%
      leaflet() %>%
      addTiles() %>%
      addCircles(lng = ~lon,
                 lat = ~lat,
                 weight = ~scale(total),
                 color = 'Orange',
                 opacity = .8,
                 popup = ~paste0('<b>Localidade: </b>', name,
                                 '<br>',
                                 '<b>Volumetria: </b>', total)
      )
  })
  
  # Volumetria por Estados (COVID-19)
  output$NumEstado <- renderEcharts4r({
    brasil <- as.data.table(brasil)
    brasil <- subset(brasil, brasil$state != 'TOTAL')
    brasil <- arrange(brasil, desc(totalCasesMS))
    brasil %>%
      e_chart(state) %>%
      e_bar(totalCasesMS, name = 'Confirmados') %>%
      e_line(deathsMS, name = 'Mortes') %>%
      e_tooltip(trigger = 'axis') %>%
      e_theme("infographic") %>%
      e_color(c('Orange','FireBrick'))
  })
  
  # Evolutivo por Tempo - Estados
  output$TimeEstado <- renderEcharts4r({
    states$date <- lubridate::ymd(states$date)
    states$state <- replace(states$state, states$state == 'TOTAL', 'BRASIL')
    states <- subset(states, states$state != 'BRASIL')
    states$state <- as.character(states$state)
    states <- as.data.frame(states)
    states %>%
      group_by(state) %>%
      e_charts(date, timeline = T) %>%
      e_line(totalCases, legend = F) %>%
      e_tooltip(trigger = 'axis') %>% 
      e_datazoom() %>% 
      e_timeline_opts(top = 0)
  })
  
  # Previsão do COVID-19 no Brasil (Casos e Mortes) - Projeção utilizando Facebook Phophet
  # Casos Confirmados
  output$ConfirmedE_BR <- renderDygraph({
    # Carregamento
    w$show()
    time <- states
    time$state <- replace(time$state, time$state == 'TOTAL', 'BRASIL')
    time <- subset(time, time$state == input$Estados)
    time <- data.frame(
      ds = lubridate::ymd(time$date),
      y = time$totalCasesMS
    )
    holiday <- subset(generated_holidays, generated_holidays$country == 'BR')
    m <- prophet(time, holidays = holiday)
    future <- make_future_dataframe(m, periods = 15, freq = 'day')
    forecast <- predict(m, future)
    dyplot.prophet(m, forecast)
  })
  # Mortes Confirmadas
  output$DeathsE_BR <- renderDygraph({
    # Carregamento
    w$show()
    time <- states
    time$state <- replace(time$state, time$state == 'TOTAL', 'BRASIL')
    time <- subset(time, time$state == input$Estados)
    time <- data.frame(
      ds = lubridate::ymd(time$date),
      y = time$deathsMS
    )
    holiday <- subset(generated_holidays, generated_holidays$country == 'BR')
    m <- prophet(time, holidays = holiday)
    future <- make_future_dataframe(m, periods = 15, freq = 'day')
    forecast <- predict(m, future)
    dyplot.prophet(m, forecast)
  })
  
  # PREPARAÇÃO PARA O AGRUPAMENTO - Clusters
  nei <- citiesGPS
  nei <- subset(nei, nei$type == '1')
  km <- kmeans(nei[5], centers = 3)
  nei$Clusters <- km$cluster
  centros <- data.frame(km$centers)
  centros$Clusters <- c(1,2,3)
  centros <- arrange(centros, total)
  centros$Risco <- c('Risco: Baixo', 'Risco: Médio', 'Risco: Alto')
  pal <- colorFactor(c('Gold','DarkOrange','FireBrick'), levels = centros$Clusters)
  # Mapa - Cluster do Brasil - Riscos
  output$ClusterBrasil <- renderLeaflet({
    nei %>%
      leaflet() %>%
      addTiles() %>%
      addCircles(lng = ~lon,
                 lat = ~lat,
                 weight = ~scale(total),
                 color = ~pal(nei$Clusters),
                 opacity = .8,
                 label = ~name
      ) 
  })
  # Tabela - Dados do Agrupamento
  output$ClusterTable <- renderFormattable({
    names(centros)[1] <- 'Média de Casos Confirmados'
    formattable(centros,
                align = c("l", rep("r", NCOL(centros) - 1)),
                list(
                  `total` = formatter("span", style = ~ style(color = "grey", font.weight = "bold")),
                  `Clusters` = formatter("span", style = ~ style(color = c('Gold','DarkOrange','FireBrick'), font.weight = "bold")),
                  `Risco` = formatter("span", style = ~ style(color = c('Gold','DarkOrange','FireBrick'), font.weight = "bold"))
                ))
  })
  
}

