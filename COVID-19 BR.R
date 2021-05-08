# Pacotes -----------------------------------------------------------------

library(shiny)
library(bs4Dash)
library(readr)
library(data.table)
library(tidyr)
library(dplyr)
library(plotly)
library(echarts4r)
library(leaflet)
library(prophet)
library(shinyWidgets)
library(formattable)
library(dygraphs)
library(readxl)
library(shinybusy)

# User Interface ----------------------------------------------------------

ui = bs4DashPage(
  old_school = FALSE,
  sidebar_collapsed = FALSE,
  controlbar_collapsed = TRUE,
  enable_preloader = TRUE,
  loading_duration = 5,
  loading_background = "#1C1C1C",
  # Nome do Dashboard
  title = "COVID-19 (BRASIL)",
  # Menu Superior
  navbar = bs4DashNavbar(
    skin = 'light'
  ),
  # Menu Lateral Esquerdo
  sidebar = bs4DashSidebar(
    skin = "light",
    status = "primary",
    title = "COVID-19 (BRASIL)",
    src = 'https://www.monmouth.edu/covid-19/wp-content/uploads/sites/770/2020/03/cdc-w9KEokhajKw-unsplash.jpg',
    brandColor = "gray-light",
    bs4SidebarMenu(
      # Páginas do Dashboard
      bs4SidebarHeader("Análise Descritiva"),
      bs4SidebarMenuItem(
        startExpanded = T,
        tabName = "descritive",
        icon = "chart-pie",
        text = "Coronavírus no Brasil"
      ),
      bs4SidebarHeader("Análise Estatística"),
      bs4SidebarMenuItem(
        tabName = "statistic",
        icon = "chart-line",
        text = "Projeção de Contágios"
      ),
      bs4SidebarMenuItem(
        tabName = "knn",
        icon = "project-diagram",
        text = "Riscos de Disseminação"
      ),
      # Detalhes sobre o Dashboard
      bs4SidebarHeader("Informações"),
      bs4SidebarMenuItem(
        tabName = "about",
        icon = "info",
        text = "Aplicação"
      )
    )
  ),
  # Footer
  footer = bs4DashFooter(
    copyrights = a(
      href = "https://mppallante.wixsite.com/mppallante", 
      target = "_blank", "©MPPallante"
    ),
    right_text = lubridate::year(Sys.time())
  ), 
  # Corpo do Dahboard
  body = bs4DashBody(
    bs4TabItems(
      # Página Inicial
      bs4TabItem(
        tabName = 'descritive',
        fluidPage(
          # Caixa de Informações Brasil (Confirmados e Mortes)
          bs4Card(title = 'Indicadores de Casos Confirmados, Recuperados e Mortes',
                  status = 'primary', width = NULL, closable = F, maximizable = F, collapsible = F,
                  fluidRow(
                    bs4InfoBoxOutput('ConfirmedBR', width = 6),
                    bs4InfoBoxOutput('recupBR', width = 6),
                    bs4InfoBoxOutput('DeathBR', width = 6),
                    bs4InfoBoxOutput('indBR', width = 6)
                  )),
          # Mapa COVID-19 (Volumetria por Cidades)
          bs4Card(title = 'Contágio nos Estados (Casos Confirmados)',
                  status = 'primary', width = NULL, closable = F, maximizable = T, collapsible = F, height = 600,
                  leafletOutput('GeoBrasil', width = '100%', height = '100%')),
          # Volumetria por Estado (Confirmados e Mortes)
          bs4Card(title = 'Casos Confirmados versus Mortes por Estado', height = 500,
                  status = 'primary', width = NULL, closable = F, maximizable = T, collapsible = F,
                  echarts4rOutput('NumEstado', width = '100%', height = "100%")),
          # Volumetria por Estado (Confirmados e Mortes)
          bs4Card(title = 'Evolutivo de Contágio por Estado', height = 500,
                  status = 'primary', width = NULL, closable = F, maximizable = T, collapsible = F,
                  echarts4rOutput('TimeEstado', width = '100%', height = "100%"))
        )
      ),
      # Análises Estatísticas
      bs4TabItem(
        tabName = 'statistic',
        fluidRow(
          column(width = 11,
                 # Previsão e Comportamento do COVID-19 no Brasil para os proximos 15 dias (Casos)
                 bs4Card(title = 'Projeção de Contágios para os próximos 15 dias', height = 350,
                         status = 'primary', width = NULL, closable = F, maximizable = T, collapsible = F,
                         dygraphOutput('ConfirmedE_BR', width = '100%', height = '100%')),
                 # Previsão e Comportamento do COVID-19 no Brasil para os proximos 15 dias (Mortes)
                 bs4Card(title = 'Projeção de Mortes para os próximos 15 dias', height = 350,
                         status = 'primary', width = NULL, closable = F, maximizable = T, collapsible = F,
                         dygraphOutput('DeathsE_BR', width = '100%', height = '100%'))
          ),
          column(width = 1,
                 prettyRadioButtons(
                   inputId = "Estados",
                   label = "Localiddade:", 
                   choices = c("BRASIL","SP","RJ","AC","AL","AM","AP","BA","CE","DF","ES","GO","MA","MG","MS","MT","PA","PB","PE","PI","PR","RN","RO","RR","RS","SC","SE","TO"),
                   selected = "BRASIL",
                   status = "primary",
                   animation = "smooth"
                 ))
        )
      ),
      # Análises de Agrupamento
      bs4TabItem(
        tabName = 'knn',
        fluidPage(
          # Mapa Clusterizado (Agrupamento) de locais confirmados para o COVID-19
          bs4Card(title = 'Concentração de Casos Confirmados (Agrupamentos) ', height = 450, 
                  status = 'primary', width = NULL, closable = F, maximizable = T, collapsible = F,
                  leafletOutput('ClusterBrasil', width = '100%', height = '100%')),
          # Tabela de Estatisticas e Cluster
          bs4Card(title = 'Informações sobre os Agrupamentos', height = 220,
                  status = 'primary', width = NULL, closable = F, maximizable = T, collapsible = F,
                  formattableOutput('ClusterTable', width = '100%', height = '100%'))
        )
      ),
      # Sobre
      bs4TabItem(
        tabName = 'about',
        fluidPage(
          bs4Jumbotron(
            title = "COVID-19",
            lead = "Desenvolvido para análise dos casos de COVID-19 no Brasil",
            status = "primary",
            btn_name = 'COVID-19 - GITHUB',
            href = "https://github.com/mppallante/COVID19-BR"
          )
        )
      )
    )
  ) 
)

# Server Side -------------------------------------------------------------

server <- function(input, output, session) {
  # Base de Dados
  # Série Temporal - Brasil
  show_modal_spinner(spin = "semipolar", text = "Processando")
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
  remove_modal_spinner()
  
 # Caixa de Informações - Brasil (Confirmados e Mortes)
  output$ConfirmedBR <- renderbs4InfoBox({
    bs4InfoBox(
      title = 'Confirmados',
      value = subset(brasil$totalCasesMS, brasil$state == 'TOTAL'),
      icon = 'head-side-cough',
      status = 'warning'
    )
  })
  output$DeathBR <- renderbs4InfoBox({
    bs4InfoBox(
      title = 'Mortes',
      value = subset(brasil$deathsMS, brasil$state == 'TOTAL'),
      icon = 'chart-line',
      status = 'danger'
    )
  })
  output$indBR <- renderbs4InfoBox({
    bs4InfoBox(
      title = 'Índice Mortalidade',
      value = paste0(subset(round(brasil$deathsMS/brasil$totalCasesMS * 100, digits = 2), brasil$state == 'TOTAL'),"%"),
      icon = 'percent',
      status = 'danger'
    )
  })
  output$recupBR <- renderbs4InfoBox({
    bs4InfoBox(
      title = 'Recuperados',
      value = subset(brasil$recovered, brasil$state == 'TOTAL'),
      icon = 'head-side-cough-slash',
      status = 'success'
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
    show_modal_spinner(spin = "semipolar", text = "Processando")
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
    remove_modal_spinner()
    dyplot.prophet(m, forecast)
  })
  # Mortes Confirmadas
  output$DeathsE_BR <- renderDygraph({
    show_modal_spinner(spin = "semipolar", text = "Processando")
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
    remove_modal_spinner()
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

# Run Application Options/Settings ----------------------------------------

shinyApp(ui = ui, server = server)
