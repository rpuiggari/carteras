## Calculadora de retorno de carteras
## Shiny app – Ricardo Puiggari (versión con objetivo de IVA y números formateados)
# --------------------------------------------------------------------
# Paquetes ------------------------------------------------------------
library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(scales)

# --------------------------------------------------------------------
# Funciones auxiliares ------------------------------------------------

pmt <- function(rate, nper, pv) -(pv * rate) / (1 - (1 + rate)^(-nper))

amort_tbl <- function(capital, tasa_anual, n_cuotas, iva_rate) {
  tasa_m <- tasa_anual / 12
  cuota  <- abs(pmt(tasa_m, n_cuotas, -capital))
  saldo  <- capital
  out    <- vector("list", n_cuotas)
  for (p in seq_len(n_cuotas)) {
    int  <- saldo * tasa_m
    prin <- cuota - int
    iva  <- int * iva_rate
    out[[p]] <- data.frame(
      Periodo        = p,
      Saldo_inicial  = saldo,
      Pago_principal = prin,
      Pago_interes   = int,
      IVA            = iva,
      Cuota          = cuota
    )
    saldo <- saldo - prin
  }
  bind_rows(out)
}

a_npv <- function(tabla, tasa_desc) {
  tasa_m <- tasa_desc / 12
  flujos <- tabla$Cuota - tabla$IVA
  sum(flujos / (1 + tasa_m) ^ tabla$Periodo)
}

a_calc <- function(capital, tasa_anual, n_cuotas, iva_rate, tasa_desc) {
  tab <- amort_tbl(capital, tasa_anual, n_cuotas, iva_rate)
  sum_int <- sum(tab$Pago_interes)
  sum_iva <- sum(tab$IVA)
  cap_int <- capital + sum_int
  pv_comp <- a_npv(tab, tasa_desc)
  iva_comp <- pmax(cap_int - pv_comp, 0) * iva_rate
  tot_full <- cap_int + sum_iva + iva_comp
  list(
    tab      = tab,
    pv       = pv_comp,
    int      = sum_int,
    iva_int  = sum_iva,
    iva_cmp  = iva_comp,
    iva_tot  = sum_iva + iva_comp,
    iva_rat  = (sum_iva + iva_comp) / pv_comp,
    roi_ci   = (cap_int - pv_comp) / pv_comp,
    roi_all  = (tot_full - pv_comp) / pv_comp
  )
}

capital_from_iva <- function(iva_objetivo, tasa_anual, n_cuotas, iva_rate, tasa_desc, tol = 1e-2, max_iter = 100) {
  f <- function(cap) a_calc(cap, tasa_anual, n_cuotas, iva_rate, tasa_desc)$iva_tot - iva_objetivo
  low <- 0; high <- iva_objetivo * 10 + 1
  for (i in seq_len(max_iter)) {
    mid <- (low + high) / 2
    val <- f(mid)
    if (abs(val) < tol) return(mid)
    if (val > 0) high <- mid else low <- mid
  }
  mid
}

# --------------------------------------------------------------------
# UI ------------------------------------------------------------------

tema <- bs_theme(version = 5, bootswatch = "flatly")

ui <- fluidPage(
  theme = tema,
  titlePanel("Calculadora de retorno de carteras"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("modo", "Modo de entrada:",
                   choices = c("Capital de la cartera" = "cap",
                               "IVA a recuperar"      = "iva"),
                   selected = "cap"),
      conditionalPanel("input.modo == 'cap'",
                       numericInput("capital", "Capital cartera ($):", 1000, min = 0)
      ),
      conditionalPanel("input.modo == 'iva'",
                       numericInput("iva_obj", "IVA a recuperar ($):", 200, min = 0)
      ),
      numericInput("tasa_anual", "Tasa anual crédito (%):",   120, min = 0),
      numericInput("n_cuotas",  "Número de cuotas:",          12,  min = 1),
      numericInput("iva_rate",  "Tasa IVA (%):",             21,  min = 0),
      numericInput("tasa_desc", "Tasa descuento compra (%):", 55,  min = 0),
      actionButton("calcular", "Calcular", class = "btn-primary")
    ),
    mainPanel(
      fluidRow(uiOutput("metrics_ui")),
      hr(),
      DTOutput("tabla")
    )
  )
)

# --------------------------------------------------------------------
# Server ---------------------------------------------------------------

server <- function(input, output, session) {
  
  resultados <- eventReactive(input$calcular, {
    conv <- function(x) ifelse(x > 1, x / 100, x)
    t_cred <- conv(input$tasa_anual)
    t_desc <- conv(input$tasa_desc)
    n      <- input$n_cuotas
    iva_r  <- input$iva_rate / 100
    
    cap <- if (input$modo == "cap") {
      input$capital
    } else {
      capital_from_iva(input$iva_obj, t_cred, n, iva_r, t_desc)
    }
    
    calc <- a_calc(cap, t_cred, n, iva_r, t_desc)
    calc$capital <- cap
    calc
  })
  
  output$metrics_ui <- renderUI({
    m <- req(resultados())
    tagList(
      valueBox(format_number(m$pv),        "Precio compra (VAN)",  "#f1c40f"),
      valueBox(format_number(m$capital),   "Capital cartera",      "#34495e"),
      valueBox(format_number(m$int),       "Suma intereses",       "#2c3e50"),
      valueBox(format_number(m$iva_int),   "IVA intereses",        "#18bc9c"),
      valueBox(format_number(m$iva_cmp),   "IVA compra",           "#9b59b6"),
      valueBox(format_number(m$iva_tot),   "IVA total",            "#8e44ad"),
      valueBox(percent(m$iva_rat, 0.01),   "IVA / inversión",      "#1abc9c"),
      valueBox(percent(m$roi_ci,  0.01),   "ROI capital+int",      "#2980b9"),
      valueBox(percent(m$roi_all, 0.01),   "ROI total c/IVA",      "#e74c3c")
    )
  })
  
  output$tabla <- renderDT({
    m   <- req(resultados())
    tab <- m$tab
    
    tot <- data.frame(
      Periodo        = "Total",
      Saldo_inicial  = NA_real_,
      Pago_principal = sum(tab$Pago_principal),
      Pago_interes   = sum(tab$Pago_interes),
      IVA            = sum(tab$IVA),
      Cuota          = sum(tab$Cuota)
    )
    
    tab_full <- bind_rows(tab %>% mutate(Periodo = as.character(Periodo)), tot)
    rownames(tab_full) <- NULL
    
    datatable(
      tab_full,
      rownames = FALSE,
      options = list(
        paging    = FALSE,
        searching = FALSE,
        dom       = "t",
        ordering  = FALSE,
        rowCallback = DT::JS("function(row, data){ if(data[0] === 'Total'){ $('td', row).css({'font-weight':'bold'}); } }")
      )
    ) %>%
      formatCurrency(c("Saldo_inicial", "Pago_principal", "Pago_interes", "IVA", "Cuota"),
                     currency = "", digits = 2, interval = 3, mark = ".", dec.mark = ",")
  })
}

# --------------------------------------------------------------------
# Helpers --------------------------------------------------------------

valueBox <- function(value, subtitle, color) {
  htmltools::div(class = "card shadow-sm mb-3",
                 htmltools::div(style = paste0("background-color:", color, ";"), class = "card-header text-white", subtitle),
                 htmltools::div(class = "card-body h3", value, style = "text-align:center;")
  )
}

format_number <- function(x) comma(x, 0.01, locale = "es")

# --------------------------------------------------------------------
# Run -----------------------------------------------------------------

shinyApp(ui, server)
