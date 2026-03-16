library(shiny)

mod_parametro_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Módulo 1. Parámetro de interés"),
    radioButtons(
      ns("tipo_parametro"),
      "Seleccione parámetro de interés",
      choices = c("Media" = "media", "Proporción" = "proporcion"),
      selected = "media",
      inline = TRUE
    ),
    conditionalPanel(
      sprintf("input['%s'] == 'media'", ns("tipo_parametro")),
      numericInput(ns("xbar"), "Media esperada (x̄)", value = 100, min = 0),
      numericInput(ns("s"), "Desviación estándar (s)", value = 20, min = 0)
    ),
    conditionalPanel(
      sprintf("input['%s'] == 'proporcion'", ns("tipo_parametro")),
      numericInput(ns("p"), "Proporción esperada (p)", value = 0.5, min = 0, max = 1, step = 0.01)
    )
  )
}

mod_parametro_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive({
      list(
        tipo = input$tipo_parametro,
        xbar = input$xbar,
        s = input$s,
        p = input$p
      )
    })
  })
}

mod_unidad_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Módulo 2. Unidad de análisis"),
    radioButtons(
      ns("unidad"),
      "Seleccionar unidad de análisis",
      choices = c("Hogares" = "hogares", "Personas" = "personas"),
      selected = "hogares",
      inline = TRUE
    ),
    conditionalPanel(
      sprintf("input['%s'] == 'personas'", ns("unidad")),
      numericInput(ns("r"), "r = personas promedio por hogar", value = 1.2, min = 0.01, step = 0.01),
      numericInput(ns("b"), "b = proporción elegible", value = 0.8, min = 0.01, max = 1, step = 0.01)
    ),
    conditionalPanel(
      sprintf("input['%s'] == 'hogares'", ns("unidad")),
      helpText("Para indicadores en hogares se usa r = 1 y b = 1.")
    )
  )
}

mod_unidad_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive({
      if (identical(input$unidad, "hogares")) {
        list(unidad = "hogares", r = 1, b = 1)
      } else {
        list(unidad = "personas", r = input$r, b = input$b)
      }
    })
  })
}

mod_precision_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Módulo 3. Precisión"),
    numericInput(ns("amplitud"), "Amplitud del intervalo de confianza", value = 10, min = 0.0001),
    numericInput(ns("confianza"), "Nivel de confianza", value = 0.95, min = 0.5, max = 0.999, step = 0.001),
    actionButton(ns("aceptar"), "Estoy de acuerdo con estos valores", class = "btn-primary")
  )
}

mod_precision_server <- function(id, parametro) {
  moduleServer(id, function(input, output, session) {
    calculos <- reactive({
      req(input$amplitud, input$confianza)
      z <- qnorm((1 + input$confianza) / 2)
      se <- input$amplitud / (2 * z)

      prm <- parametro()
      valor_parametro <- if (identical(prm$tipo, "media")) prm$xbar else prm$p
      delta <- z * se
      cve <- if (!isTRUE(all.equal(valor_parametro, 0))) se / valor_parametro else NA_real_
      ic_inf <- valor_parametro - z * se
      ic_sup <- valor_parametro + z * se
      mr <- if (!isTRUE(all.equal(valor_parametro, 0))) delta / abs(valor_parametro) else NA_real_

      list(
        z = z,
        se = se,
        delta = delta,
        cve = cve,
        ic = c(ic_inf, ic_sup),
        mr = mr,
        amplitud = input$amplitud,
        confianza = input$confianza
      )
    })

    output$resumen_precision <- renderTable({
      cals <- calculos()
      data.frame(
        Métrica = c("Error estándar (EE)", "Margen de error (ME)", "Coeficiente de variación (CV)", "Margen de error relativo (MR)", "IC inferior", "IC superior"),
        Valor = round(c(cals$se, cals$delta, cals$cve, cals$mr, cals$ic[1], cals$ic[2]), 6)
      )
    }, striped = TRUE, bordered = TRUE)

    list(
      precision = calculos,
      aceptado = reactive(input$aceptar)
    )
  })
}

mod_precision_resumen_ui <- function(id) {
  ns <- NS(id)
  tableOutput(ns("resumen_precision"))
}

mod_diseno_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Módulo 4. Parámetros de diseño y presupuesto"),
    numericInput(ns("N"), "N = población", value = 500000, min = 1),
    numericInput(ns("M"), "M = UPM en el marco", value = 10000, min = 1),
    numericInput(ns("m_min"), "m mínimo", value = 8, min = 1),
    numericInput(ns("m_max"), "m máximo", value = 18, min = 1),
    numericInput(ns("m"), "m seleccionado", value = 12, min = 1),
    numericInput(ns("rho"), "rho = ICC", value = 0.05, min = 0, max = 1, step = 0.001),
    tags$hr(),
    numericInput(ns("c_h"), "c_h = costo por encuesta", value = 1, min = 0, step = 0.01),
    numericInput(ns("c_upm"), "c_UPM = costo fijo por UPM", value = 40, min = 0, step = 0.01)
  )
}

calcular_tabla_muestreo <- function(tipo, valor, s, p, z, delta, N, r, b, m_seq, rho, c_h, c_upm) {
  do.call(rbind, lapply(m_seq, function(m) {
    deff <- 1 + (m - 1) * rho
    n0 <- if (identical(tipo, "media")) {
      (z^2 * s^2 * deff) / (delta^2)
    } else {
      (z^2 * p * (1 - p) * deff) / (delta^2)
    }
    n <- n0 / (1 + ((n0 - 1) / N))
    n_h <- ceiling(n / (r * b))
    upm <- ceiling(n_h / m)
    costo <- n_h * c_h + upm * c_upm

    data.frame(
      m = m,
      deff = round(deff, 5),
      n_objetivo = ceiling(n),
      n_hogares = n_h,
      upm = upm,
      costo_total = round(costo, 2)
    )
  }))
}

mod_diseno_server <- function(id, parametro, unidad, precision) {
  moduleServer(id, function(input, output, session) {
    tabla <- reactive({
      prm <- parametro()
      uni <- unidad()
      pre <- precision()
      req(prm, uni, pre, input$m_min, input$m_max, input$N, input$rho)

      m_min <- min(input$m_min, input$m_max)
      m_max <- max(input$m_min, input$m_max)
      m_seq <- seq(m_min, m_max)

      calcular_tabla_muestreo(
        tipo = prm$tipo,
        valor = if (identical(prm$tipo, "media")) prm$xbar else prm$p,
        s = prm$s,
        p = prm$p,
        z = pre$z,
        delta = pre$delta,
        N = input$N,
        r = uni$r,
        b = uni$b,
        m_seq = m_seq,
        rho = input$rho,
        c_h = input$c_h,
        c_upm = input$c_upm
      )
    })

    seleccion <- reactive({
      req(tabla(), input$m)
      tb <- tabla()
      idx <- which(tb$m == input$m)
      if (length(idx) == 0) idx <- 1
      as.list(tb[idx[1], , drop = FALSE])
    })

    list(
      tabla = tabla,
      seleccion = seleccion,
      parametros = reactive({
        list(
          N = input$N,
          M = input$M,
          m = input$m,
          rho = input$rho,
          c_h = input$c_h,
          c_upm = input$c_upm,
          m_min = input$m_min,
          m_max = input$m_max,
          n_tilde = input$m * unidad()$r * unidad()$b
        )
      })
    )
  })
}

mod_dominios_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Módulo 5. Dominios"),
    radioButtons(
      ns("usa_dominios"),
      "¿Necesita representatividad por división administrativa?",
      choices = c("No" = "no", "Sí" = "si"),
      selected = "no",
      inline = TRUE
    ),
    conditionalPanel(
      sprintf("input['%s'] == 'si'", ns("usa_dominios")),
      numericInput(ns("n_dominios"), "Ingrese número de dominios", value = 2, min = 1)
    )
  )
}

mod_dominios_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive({
      list(
        usa_dominios = identical(input$usa_dominios, "si"),
        n_dominios = if (identical(input$usa_dominios, "si")) input$n_dominios else 1
      )
    })
  })
}

mod_asignacion_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Módulo 6. Asignación por área"),
    radioButtons(
      ns("asignar_area"),
      "¿Desea hacer la asignación por área?",
      choices = c("No" = "no", "Sí" = "si"),
      selected = "no",
      inline = TRUE
    ),
    conditionalPanel(
      sprintf("input['%s'] == 'si'", ns("asignar_area")),
      numericInput(ns("n_areas"), "Ingrese número de áreas", value = 3, min = 1),
      uiOutput(ns("proporciones_ui"))
    )
  )
}

mod_asignacion_server <- function(id, n_total) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$proporciones_ui <- renderUI({
      req(input$n_areas)
      lapply(seq_len(input$n_areas), function(i) {
        numericInput(ns(paste0("prop_", i)), paste("Proporción censal área", i), value = round(1 / input$n_areas, 3), min = 0, max = 1, step = 0.001)
      })
    })

    tabla_area <- reactive({
      req(input$asignar_area)
      if (!identical(input$asignar_area, "si")) return(NULL)
      req(input$n_areas)
      props <- sapply(seq_len(input$n_areas), function(i) input[[paste0("prop_", i)]])
      if (sum(props, na.rm = TRUE) <= 0) return(NULL)
      props <- props / sum(props, na.rm = TRUE)
      total <- n_total()
      n_area <- round(total * props)
      dif <- total - sum(n_area)
      if (dif != 0 && length(n_area) > 0) n_area[1] <- n_area[1] + dif

      data.frame(area = paste("Área", seq_len(input$n_areas)), proporcion = round(props, 5), n_asignado = n_area)
    })

    list(
      asignar = reactive(identical(input$asignar_area, "si")),
      tabla = tabla_area
    )
  })
}
