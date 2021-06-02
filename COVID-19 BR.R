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

# User Interface ----------------------------------------------------------

ui = bs4DashPage(
  
  # Opções
  fullscreen = TRUE,
  help = FALSE,
  dark = FALSE,
  scrollToTop = FALSE,

  # Navbar (Menu Superior) 
  header = bs4DashNavbar(
    disable = FALSE, 
    fixed = TRUE,
    border = TRUE,
    compact = FALSE,
    skin = "light",
    status = "white",
    sidebarIcon = shiny::icon("bars"),
    controlbarIcon = shiny::icon("th"),
    # Cabeçalho do Dashboard
    title = dashboardBrand(
      title = "©MPPallante",
      color = "primary",
      image = "https://lh3.googleusercontent.com/ogw/ADGmqu_hZZbh1ioBDSRRb8W85PrmMbB07wcshDOJcM8V9g=s83-c-mo", 
      href = "https://mppallante.wixsite.com/mppallante",
      opacity = 0.8
    ),
    # Caixa de Mensagens
    rightUi = tagList(
      dropdownMenu(
        headerText = "Você tem 1 notificação",
        badgeStatus = "danger",
        type = "messages",
        icon = shiny::icon("bell"),
        messageItem(
          inputId = "triggerAction1",
          from = HTML("<strong>Desenvolvedor</strong>"),
          message = HTML("Atualização realizada!
                         <br>Layout:2.0
                         <br>R: 4.1.0
                         <br>Rstudio: 1.4.1106"),
          image = "https://lh3.googleusercontent.com/ogw/ADGmqu_hZZbh1ioBDSRRb8W85PrmMbB07wcshDOJcM8V9g=s83-c-mo",
          time = "Hoje",
          color = "navy",
          icon = shiny::icon("code")
        )
      )
    )
  ),
  
  # Sidebar (Menu Lateral)
  sidebar = bs4DashSidebar(
    # Opções
    id = "sidebar",
    disable = FALSE,
    fixed = TRUE,
    collapsed = FALSE,
    minified = TRUE,
    expandOnHover = TRUE,
    width = NULL,
    elevation = 4,
    skin = "light",
    status = "primary",
    customArea = NULL,
    # Segundo Titulo
    sidebarUserPanel(
      name = HTML("<strong>COVID-19 BRASIL</strong>"),
      image = NULL
    ),
    # Menu
    sidebarMenu(
      sidebarHeader("Análise Descritiva"),
      # Página 1
      menuItem(
        selected = TRUE,
        text = "Coronavírus no Brasil",
        tabName = "descritive",
        icon = shiny::icon("chart-pie")
      ),
      sidebarHeader("Análise Estatística"),
      # Página 2
      menuItem(
        text = "Projeção de Contágios",
        tabName = "statistic",
        icon = shiny::icon("chart-line")
      ),
      # Página 3
      menuItem(
        text = "Riscos de Disseminação",
        tabName = "knn",
        icon = shiny::icon("project-diagram")
      ),
      sidebarHeader("Informações"),
      # Página 4
      menuItem(
        text = "Aplicação",
        tabName = "about",
        icon = shiny::icon("info")
      )
    )
  ),
  
  # Controlbar (Menu de Controles)
  # controlbar = dashboardControlbar(
  #   # Opções
  #   id = "controlbar",
  #   disable = FALSE,
  #   pinned = FALSE,
  #   collapsed = TRUE,
  #   overlay = TRUE,
  #   width = 250,
  #   skin = "light",
  #   controlbarMenu(
  #     # Opções
  #     id = "controlbarMenu",
  #     type = "pills",
  #     selected = "Controles",
  #     #  Menu de Controles
  #     controlbarItem(
  #       title = "Controles"
  #       
  #     ),
  #     # Menu de temas
  #     controlbarItem(
  #       title = "Temas",
  #       skinSelector()
  #     )
  #   )
  # ),
  
  # Main Body (Corpo Principal)
  body = bs4DashBody(
    bs4TabItems(
      # Página 1 - Coronavírus no Brasil
      bs4TabItem(
        use_waiter(),
        tabName = "descritive",
        # Indicadores de Casos Confirmados, Recuperados e Mortes
        bs4Card(
          title = "Indicadores de Casos Confirmados, Recuperados e Mortes", 
          closable = FALSE,
          collapsible = FALSE,
          collapsed = FALSE,
          maximizable = TRUE,
          solidHeader = TRUE, 
          elevation = 4,
          width = 12,
          height = NULL,
          status = "primary",
          fluidRow(
            bs4InfoBoxOutput("ConfirmedBR", width = 6),
            bs4InfoBoxOutput("recupBR", width = 6),
            bs4InfoBoxOutput("DeathBR", width = 6),
            bs4InfoBoxOutput("indBR", width = 6)
          )
        ),
        # Contágio nos Estados (Casos Confirmados)
        bs4Card(
          title = "Contágio nos Estados (Casos Confirmados)", 
          closable = FALSE,
          collapsible = FALSE,
          collapsed = FALSE,
          maximizable = TRUE,
          solidHeader = TRUE, 
          elevation = 4,
          width = 12,
          height = 600,
          status = "primary",
          leafletOutput('GeoBrasil', width = "100%", height = "100%")
        ),
        # Casos Confirmados versus Mortes por Estado
        bs4Card(
          title = "Casos Confirmados versus Mortes por Estado", 
          closable = FALSE,
          collapsible = FALSE,
          collapsed = FALSE,
          maximizable = TRUE,
          solidHeader = TRUE, 
          elevation = 4,
          width = 12,
          height = 600,
          status = "primary",
          echarts4rOutput('NumEstado', width = "100%", height = "100%")
        ),
        # Evolutivo de Contágio por Estado
        bs4Card(
          title = "Evolutivo de Contágio por Estado", 
          closable = FALSE,
          collapsible = FALSE,
          collapsed = FALSE,
          maximizable = TRUE,
          solidHeader = TRUE, 
          elevation = 4,
          width = 12,
          height = 600,
          status = "primary",
          echarts4rOutput('TimeEstado', width = "100%", height = "100%")
        )
      ),
      # Página 2 - Projeção de Contágios
      bs4TabItem(
        use_waiter(),
        tabName = "statistic",
        fluidRow(
          column(
            width = 11,
            # Projeção de Contágios para os próximos 15 dias
            bs4Card(
              title = "Projeção de Contágios para os próximos 15 dias", 
              closable = FALSE,
              collapsible = FALSE,
              collapsed = FALSE,
              maximizable = TRUE,
              solidHeader = TRUE, 
              elevation = 4,
              width = 12,
              height = 350,
              status = "primary",
              dygraphOutput('ConfirmedE_BR', width = '100%', height = '100%')
            ),
            # Projeção de Mortes para os próximos 15 dias
            bs4Card(
              title = "Projeção de Mortes para os próximos 15 dias", 
              closable = FALSE,
              collapsible = FALSE,
              collapsed = FALSE,
              maximizable = TRUE,
              solidHeader = TRUE, 
              elevation = 4,
              width = 12,
              height = 350,
              status = "primary",
              dygraphOutput('DeathsE_BR', width = '100%', height = '100%')
            )
          ),
          column(
            width = 1,
            prettyRadioButtons(
              inputId = "Estados",
              label = "Localiddade:", 
              choices = c("BRASIL","SP","RJ","AC","AL","AM","AP","BA","CE","DF","ES","GO","MA","MG","MS","MT","PA","PB","PE","PI","PR","RN","RO","RR","RS","SC","SE","TO"),
              selected = "BRASIL",
              status = "primary",
              animation = "smooth"
            )
          )
        )
      ),
      # Página 3 - Riscos de Disseminação
      bs4TabItem(
        use_waiter(),
        tabName = "knn",
        # Concentração de Casos Confirmados (Agrupamentos)
        bs4Card(
          title = "Concentração de Casos Confirmados (Agrupamentos)", 
          closable = FALSE,
          collapsible = FALSE,
          collapsed = FALSE,
          maximizable = TRUE,
          solidHeader = TRUE, 
          elevation = 4,
          width = 12,
          height = 450,
          status = "primary",
          leafletOutput('ClusterBrasil', width = '100%', height = '100%')
        ),
        # Informações sobre os Agrupamentos
        bs4Card(
          title = "Informações sobre os Agrupamentos", 
          closable = FALSE,
          collapsible = FALSE,
          collapsed = FALSE,
          maximizable = TRUE,
          solidHeader = TRUE, 
          elevation = 4,
          width = 12,
          height = 220,
          status = "primary",
          formattableOutput('ClusterTable', width = '100%', height = '100%')
        )
      ),
      # Página 4 - Aplicação
      bs4TabItem(
        tabName = "about",
        use_waiter(),
        bs4Jumbotron(
          title = "COVID-19 BRASIL",
          lead = "Desenvolvido para análise dos casos de COVID-19 no Brasil",
          status = "primary",
          btnName = "GITHUB",
          href = "https://github.com/mppallante/COVID19-BR"
        )
      )
    )
  ),
  
  # Footer
  footer = dashboardFooter(
    fixed = FALSE,
    left = a(
      href = "https://mppallante.wixsite.com/mppallante",
      target = "_blank", "©MPPallante. Todos os direitos reservados."
    ),
    right = lubridate::year(Sys.time())
  )
  
)

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

# Run Application Options/Settings ----------------------------------------

shinyApp(ui = ui, server = server)
