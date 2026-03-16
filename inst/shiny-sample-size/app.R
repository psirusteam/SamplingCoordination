library(shiny)
source("modules.R", local = TRUE)

ui <- fluidPage(
  titlePanel("Calculadora de tamaño de muestra"),
  tags$style(HTML(".panel-step {padding: 16px; border: 1px solid #d9d9d9; border-radius: 8px; margin-bottom: 15px;}")),
  sidebarLayout(
    sidebarPanel(
      h4("Navegación"),
      textOutput("dominio_actual"),
      uiOutput("paso_actual"),
      tags$hr(),
      actionButton("anterior", "◀ Anterior"),
      actionButton("siguiente", "Siguiente ▶", class = "btn-primary"),
      tags$br(), tags$br(),
      actionButton("reiniciar", "Reiniciar flujo")
    ),
    mainPanel(
      div(class = "panel-step", uiOutput("contenido_paso")),
      tableOutput("tabla_muestreo"),
      tags$hr(),
      h3("Consolidado"),
      tableOutput("consolidado")
    )
  )
)

server <- function(input, output, session) {
  paso <- reactiveVal(1)
  dominio_actual <- reactiveVal(1)
  dominios_total <- reactiveVal(1)
  resultados_dom <- reactiveVal(list())

  parametro <- mod_parametro_server("parametro")
  unidad <- mod_unidad_server("unidad")
  precision_mod <- mod_precision_server("precision", parametro = parametro)
  diseno <- mod_diseno_server(
    "diseno",
    parametro = parametro,
    unidad = unidad,
    precision = reactive(precision_mod$precision())
  )
  dominios <- mod_dominios_server("dominios")

  n_total_sel <- reactive({
    sel <- diseno$seleccion()
    as.numeric(sel$n_hogares)
  })

  asignacion <- mod_asignacion_server("asignacion", n_total = n_total_sel)

  output$paso_actual <- renderUI({
    tags$strong(sprintf("Paso %s de 7", paso()))
  })

  output$dominio_actual <- renderText({
    sprintf("Dominio actual: %s / %s", dominio_actual(), dominios_total())
  })

  output$contenido_paso <- renderUI({
    switch(
      as.character(paso()),
      "1" = mod_parametro_ui("parametro"),
      "2" = mod_unidad_ui("unidad"),
      "3" = tagList(mod_precision_ui("precision"), mod_precision_resumen_ui("precision")),
      "4" = mod_diseno_ui("diseno"),
      "5" = mod_dominios_ui("dominios"),
      "6" = mod_asignacion_ui("asignacion"),
      "7" = tagList(
        h3("Módulo 7. Resultados y exportación"),
        p("Use 'Anterior' para ajustar valores o continúe al siguiente dominio desde el botón Siguiente."),
        p("Al finalizar los dominios se muestra la consolidación completa en la tabla inferior.")
      )
    )
  })

  output$tabla_muestreo <- renderTable({
    req(paso() >= 4)
    diseno$tabla()
  }, striped = TRUE, bordered = TRUE)

  observeEvent(input$anterior, {
    paso(max(1, paso() - 1))
  })

  observeEvent(input$siguiente, {
    p <- paso()

    if (p == 5) {
      dom <- dominios()
      dominios_total(dom$n_dominios)
    }

    if (p < 7) {
      paso(p + 1)
    } else {
      dom_id <- dominio_actual()
      registro <- list(
        dominio = paste("Dominio", dom_id),
        tipo_parametro = parametro()$tipo,
        unidad = unidad()$unidad,
        m = diseno$parametros()$m,
        n_hogares = as.numeric(diseno$seleccion()$n_hogares),
        upm = as.numeric(diseno$seleccion()$upm),
        costo_total = as.numeric(diseno$seleccion()$costo_total),
        asignacion_area = asignacion$asignar()
      )

      if (isTRUE(asignacion$asignar()) && !is.null(asignacion$tabla())) {
        registro$detalle_areas <- asignacion$tabla()
      }

      res <- resultados_dom()
      res[[dom_id]] <- registro
      resultados_dom(res)

      if (dominio_actual() < dominios_total()) {
        dominio_actual(dominio_actual() + 1)
        paso(1)
      } else {
        showNotification("Consolidación final completada", type = "message")
      }
    }
  })

  observeEvent(input$reiniciar, {
    paso(1)
    dominio_actual(1)
    dominios_total(1)
    resultados_dom(list())
  })

  output$consolidado <- renderTable({
    res <- resultados_dom()
    if (length(res) == 0) return(NULL)
    do.call(rbind, lapply(res, function(x) {
      data.frame(
        dominio = x$dominio,
        tipo_parametro = x$tipo_parametro,
        unidad = x$unidad,
        m = x$m,
        n_hogares = x$n_hogares,
        upm = x$upm,
        costo_total = x$costo_total,
        asignacion_area = ifelse(isTRUE(x$asignacion_area), "Sí", "No")
      )
    }))
  }, striped = TRUE, bordered = TRUE)
}

shinyApp(ui, server)
