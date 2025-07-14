# --------------------------------------------------------------------
# Paquetes ------------------------------------------------------------
library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(scales)
library(ggplot2)
library(plotly)

# --------------------------------------------------------------------
# Funciones auxiliares ------------------------------------------------

# Función para crear value boxes personalizados con mejores estilos
customValueBox <- function(value, subtitle, color, icon = NULL){
  icon_html <- if(!is.null(icon)) {
    paste0('<i class="fas fa-', icon, '" style="font-size: 1.2rem; margin-right: 8px;"></i>')
  } else ""
  
  htmltools::div(
    class="card shadow-sm mb-3 h-100",
    style="border-left: 4px solid var(--bs-primary); transition: transform 0.2s ease;",
    onmouseover="this.style.transform='translateY(-2px)'",
    onmouseout="this.style.transform='translateY(0)'",
    htmltools::div(
      style=paste0("background: linear-gradient(135deg,", color, ", ", color, "dd); border-radius: 0.375rem 0.375rem 0 0;"),
      class="card-header text-white py-2",
      HTML(paste0(icon_html, subtitle))
    ),
    htmltools::div(
      class="card-body text-center py-3",
      htmltools::h4(value, class="mb-0 fw-bold", style="color: #2c3e50;")
    )
  )
}

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
      Cuota          = cuota,
      Flujo_neto     = cuota - iva,
      `Cuota+IVA`    = cuota + iva,
      check.names = FALSE
    )
    saldo <- saldo - prin
  }
  bind_rows(out)
}

a_npv <- function(f, r) sum(f / (1 + r) ^ seq_along(f))

calc_kpis <- function(cap, tabla, tasa_desc, iva_rate) {
  sum_int <- sum(tabla$Pago_interes)
  sum_iva <- sum(tabla$IVA)
  cap_int <- cap + sum_int
  
  # CAMBIO: Usar cuota francesa constante sin IVA para el VAN
  cuota_francesa <- tabla$Cuota[1] # Cuota francesa base (constante)
  flujo_constante_sin_iva <- cuota_francesa # $146.76
  
  # Calcular VAN con flujo constante (como Excel PV)
  tasa_desc_mensual <- tasa_desc / 12
  n_periodos <- nrow(tabla)
  pv_comp <- flujo_constante_sin_iva * (1 - (1 + tasa_desc_mensual)^(-n_periodos)) / tasa_desc_mensual
  
  iva_comp <- pmax(cap_int - pv_comp, 0) * iva_rate
  total_full <- cap_int + sum_iva + iva_comp
  
  flujo_tot <- tabla$Flujo_neto + tabla$IVA
  acum <- cumsum(flujo_tot)
  payback <- which(acum >= pv_comp)[1]
  if (is.na(payback)) payback <- "> horizonte"
  
  dur <- with(tabla, sum((Flujo_neto / sum(Flujo_neto)) * Periodo))
  
  irr_fun <- function(r) {
    # IRR con flujo constante también
    if (r == 0) return(flujo_constante_sin_iva * n_periodos - pv_comp)
    factor_irr <- (1 - (1 + r)^(-n_periodos)) / r
    return(flujo_constante_sin_iva * factor_irr - pv_comp)
  }
  irr <- tryCatch(uniroot(irr_fun, c(0, 2))$root, error = function(e) NA)
  
  list(tabla=tabla, cap=cap, pv=pv_comp, sum_int=sum_int, sum_iva=sum_iva,
       iva_comp=iva_comp, iva_total=sum_iva+iva_comp,
       roi_cap_int=(cap_int-pv_comp)/pv_comp,
       roi_total=(total_full-pv_comp)/pv_comp,
       iva_ratio=(sum_iva+iva_comp)/pv_comp,
       payback=payback, duration=dur, irr=irr, acum=acum,
       flujo_constante=flujo_constante_sin_iva)
}

capital_from_iva <- function(obj, tasa, n, iva_r, tasa_desc) {
  f <- function(c) calc_kpis(c, amort_tbl(c, tasa, n, iva_r), tasa_desc, iva_r)$iva_total - obj
  uniroot(f, c(0, obj*20))$root
}

# --------------------------------------------------------------------
# UI ------------------------------------------------------------------

tema <- bs_theme(
  version = 5, 
  bootswatch = "flatly",
  primary = "#3498db",
  secondary = "#95a5a6",
  success = "#2ecc71",
  info = "#17a2b8",
  warning = "#f39c12",
  danger = "#e74c3c"
)

ui <- fluidPage(
  theme = tema,
  
  # CSS personalizado
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css"),
    tags$style(HTML("
      .main-header {
        background: #f8f9fa;
        color: #2c3e50;
        padding: 1.5rem 0;
        margin-bottom: 2rem;
        border-bottom: 3px solid #e9ecef;
        box-shadow: 0 2px 4px rgba(0,0,0,0.05);
      }
      .sidebar-panel {
        background: #f8f9fa;
        border-radius: 10px;
        padding: 1.5rem;
        box-shadow: 0 2px 10px rgba(0,0,0,0.1);
      }
      .form-group {
        margin-bottom: 1.5rem;
      }
      .form-label {
        font-weight: 600;
        color: #2c3e50;
        margin-bottom: 0.5rem;
      }
      .form-control, .form-select {
        border-radius: 8px;
        border: 2px solid #e9ecef;
        transition: all 0.3s ease;
      }
      .form-control:focus, .form-select:focus {
        border-color: #3498db;
        box-shadow: 0 0 0 0.2rem rgba(52, 152, 219, 0.25);
      }
      .btn-primary {
        background: linear-gradient(45deg, #3498db, #2980b9);
        border: none;
        border-radius: 25px;
        padding: 0.75rem 2rem;
        font-weight: 600;
        transition: all 0.3s ease;
      }
      .btn-primary:hover {
        transform: translateY(-2px);
        box-shadow: 0 4px 15px rgba(52, 152, 219, 0.4);
      }
      .nav-tabs .nav-link {
        border-radius: 10px 10px 0 0;
        font-weight: 600;
      }
      .nav-tabs .nav-link.active {
        background: linear-gradient(135deg, #3498db, #2980b9);
        color: white;
        border-color: transparent;
      }
      .table {
        border-radius: 10px;
        overflow: hidden;
        box-shadow: 0 2px 10px rgba(0,0,0,0.1);
      }
      .dataTables_wrapper .dataTables_scrollHead table {
        border-radius: 10px 10px 0 0;
      }
      .radio-group {
        background: white;
        border-radius: 10px;
        padding: 1rem;
        border: 2px solid #e9ecef;
      }
    "))
  ),
  
  # Header principal
  div(class = "main-header text-center",
      h1("Retorno carteras performing", 
         class = "display-5 fw-bold mb-0", style = "color: #2c3e50;")
  ),
  
  div(class = "container-fluid",
      sidebarLayout(
        sidebarPanel(
          div(class = "sidebar-panel",
              h4(HTML('<i class="fas fa-cog me-2"></i>Configuración'), class = "mb-4 text-primary"),
              
              # Modo de entrada mejorado
              div(class = "radio-group mb-4",
                  h5("Modo de entrada:", class = "form-label"),
                  radioButtons("modo", NULL,
                               choices = c("💰 Capital" = "cap", "📊 IVA a recuperar" = "iva"),
                               selected = "cap",
                               inline = FALSE
                  )
              ),
              
              conditionalPanel("input.modo=='cap'", 
                               div(class = "form-group",
                                   numericInput("capital", "💰 Capital cartera ($):", 
                                                value = 1000, min = 0, step = 100)
                               )
              ),
              
              conditionalPanel("input.modo=='iva'", 
                               div(class = "form-group",
                                   numericInput("iva_obj", "📊 IVA a recuperar ($):", 
                                                value = 200, min = 0, step = 10)
                               )
              ),
              
              div(class = "form-group",
                  numericInput("tasa_anual", "📈 Tasa anual crédito (%):", 
                               value = 120, min = 0, step = 1)
              ),
              
              div(class = "form-group",
                  numericInput("n_cuotas", "📅 Número de cuotas:", 
                               value = 12, min = 1, step = 1)
              ),
              
              div(class = "form-group",
                  numericInput("iva_rate", "🏛️ Tasa IVA (%):", 
                               value = 21, min = 0, step = 0.1)
              ),
              
              div(class = "form-group",
                  numericInput("mora", "⚠️ Mora esperada (%):", 
                               value = 3, min = 0, max = 100, step = 0.1)
              ),
              
              div(class = "form-group",
                  numericInput("tasa_desc", "💸 Tasa descuento compra (%):", 
                               value = 55, min = 0, step = 1)
              ),
              
              div(class = "text-center mt-4",
                  actionButton("calcular", HTML('<i class="fas fa-calculator me-2"></i>Calcular'), 
                               class = "btn-primary btn-lg")
              )
          )
        ),
        
        mainPanel(
          tabsetPanel(id = "tabs", type = "tabs",
                      tabPanel(HTML('<i class="fas fa-chart-bar me-2"></i>Resumen'),
                               br(),
                               conditionalPanel("input.calcular > 0",
                                                fluidRow(id = "metrics-row", uiOutput("metrics_ui")),
                                                hr(),
                                                div(class = "card shadow-sm",
                                                    div(class = "card-header bg-primary text-white",
                                                        h5(HTML('<i class="fas fa-table me-2"></i>Tabla de Amortización'), class = "mb-0")
                                                    ),
                                                    div(class = "card-body p-0",
                                                        DTOutput("tabla")
                                                    )
                                                )
                               ),
                               conditionalPanel("input.calcular == 0",
                                                div(class = "text-center py-5",
                                                    div(class = "alert alert-info",
                                                        HTML('<i class="fas fa-info-circle me-2"></i>Presiona "Calcular" para ver los resultados')
                                                    )
                                                )
                               )
                      ),
                      
                      tabPanel(HTML('<i class="fas fa-chart-line me-2"></i>Análisis Avanzado'),
                               br(),
                               conditionalPanel("input.calcular > 0",
                                                fluidRow(
                                                  column(3, customValueBox(textOutput("payback_txt"), "Payback (mes)", "#7f8c8d", "clock")),
                                                  column(3, customValueBox(textOutput("duration_txt"), "Duración media", "#95a5a6", "hourglass-half")),
                                                  column(3, customValueBox(textOutput("irr_mes_txt"), "TIR mensual", "#16a085", "percentage")),
                                                  column(3, customValueBox(textOutput("irr_anual_txt"), "TIR anual", "#27ae60", "chart-line"))
                                                ),
                                                hr(),
                                                
                                                # Gráfico de breakdown por componentes
                                                div(class = "card shadow-sm mb-4",
                                                    div(class = "card-header bg-info text-white",
                                                        h5(HTML('<i class="fas fa-chart-area me-2"></i>Breakdown por Componentes - Performance en el Tiempo'), class = "mb-0")
                                                    ),
                                                    div(class = "card-body",
                                                        plotlyOutput("plot_breakdown", height = "400px")
                                                    )
                                                ),
                                                
                                                div(class = "card shadow-sm",
                                                    div(class = "card-header bg-success text-white",
                                                        h5(HTML('<i class="fas fa-chart-area me-2"></i>Flujo Acumulado vs. Precio Compra'), class = "mb-0")
                                                    ),
                                                    div(class = "card-body",
                                                        plotlyOutput("plot_acum", height = "400px")
                                                    )
                                                )
                               ),
                               conditionalPanel("input.calcular == 0",
                                                div(class = "text-center py-5",
                                                    div(class = "alert alert-info",
                                                        HTML('<i class="fas fa-info-circle me-2"></i>Presiona "Calcular" para ver el análisis avanzado')
                                                    )
                                                )
                               )
                      )
          )
        )
      )
  )
)

# --------------------------------------------------------------------
# Server ---------------------------------------------------------------

server <- function(input, output, session){
  datos <- eventReactive(input$calcular,{
    cnv <- function(x) ifelse(x>1,x/100,x)
    t_credito <- cnv(input$tasa_anual)
    t_desc <- cnv(input$tasa_desc)
    n <- input$n_cuotas
    iva_r <- input$iva_rate/100
    cap <- if(input$modo=='cap') input$capital else capital_from_iva(input$iva_obj,t_credito,n,iva_r,t_desc)
    
    # Crear tabla original (sin mora)
    tabla_original <- amort_tbl(cap, t_credito, n, iva_r)
    
    # Calcular KPIs con tabla original (VAN sin mora)
    k_original <- calc_kpis(cap, tabla_original, t_desc, iva_r)
    
    # Aplicar mora solo a los flujos reales
    mora <- input$mora / 100
    tabla_con_mora <- tabla_original
    if (mora > 0) {
      tabla_con_mora <- tabla_original %>% 
        mutate(across(c(Pago_principal, Pago_interes, IVA, Cuota, Flujo_neto, `Cuota+IVA`), ~ .x * (1 - mora)))
    }
    
    # Recalcular solo métricas que dependen de flujos reales
    flujo_tot_real <- tabla_con_mora$Flujo_neto + tabla_con_mora$IVA
    acum_real <- cumsum(flujo_tot_real)
    payback_real <- which(acum_real >= k_original$pv)[1]
    if (is.na(payback_real)) payback_real <- "> horizonte"
    
    # TIR con flujos reales
    irr_fun_real <- function(r) a_npv(flujo_tot_real, r) - k_original$pv
    irr_real <- tryCatch(uniroot(irr_fun_real, c(0, 2))$root, error = function(e) NA)
    
    # ROI real
    monto_real_cobrado <- sum(tabla_con_mora$Pago_principal) + sum(tabla_con_mora$Pago_interes) + sum(tabla_con_mora$IVA) + k_original$iva_comp
    roi_real <- (monto_real_cobrado - k_original$pv) / k_original$pv
    
    # Combinar resultados: VAN original + flujos reales
    k_final <- k_original
    k_final$tabla <- tabla_con_mora  # Tabla para mostrar (con mora)
    k_final$payback <- payback_real
    k_final$irr <- irr_real
    k_final$acum <- acum_real
    k_final$roi_total <- roi_real
    k_final$sum_int_real <- sum(tabla_con_mora$Pago_interes)
    k_final$sum_iva_real <- sum(tabla_con_mora$IVA)
    
    k_final
  })
  
  # Resumen tarjetas con iconos
  output$metrics_ui <- renderUI({
    m <- req(datos())
    # Usar valores reales para lo que efectivamente cobraste
    monto_total_cobrado <- m$cap + m$sum_int_real + m$sum_iva_real + m$iva_comp
    
    fluidRow(
      column(4, customValueBox(format_number(m$pv), "Precio compra (VAN)", "#f1c40f", "dollar-sign")),
      column(4, customValueBox(format_number(m$cap), "Capital cartera", "#34495e", "wallet")),
      column(4, customValueBox(format_number(m$sum_int_real), "Suma intereses", "#2c3e50", "percent")),
      column(4, customValueBox(format_number(m$sum_iva_real), "IVA intereses", "#18bc9c", "receipt")),
      column(4, customValueBox(format_number(m$iva_comp), "IVA compra", "#9b59b6", "file-invoice")),
      column(4, customValueBox(format_number(m$iva_total), "IVA total", "#8e44ad", "calculator")),
      column(4, customValueBox(format_number(monto_total_cobrado), "Monto total cobrado", "#e67e22", "coins")),
      column(4, customValueBox(percent(m$iva_ratio, 0.01), "IVA / inversión", "#1abc9c", "chart-pie")),
      column(4, customValueBox(percent(m$roi_cap_int, 0.01), "ROI capital+int", "#2980b9", "trending-up")),
      column(4, customValueBox(percent(m$roi_total, 0.01), "ROI total c/IVA", "#e74c3c", "chart-bar"))
    )
  })
  
  # Tabla mejorada
  output$tabla <- renderDT({
    tab <- req(datos())$tabla
    
    if(!"Cuota+IVA" %in% names(tab)) {
      tab$`Cuota+IVA` <- tab$Cuota + tab$IVA
    }
    
    tot <- data.frame(
      Periodo = "Total",
      Saldo_inicial = NA_real_,
      Pago_principal = sum(tab$Pago_principal),
      Pago_interes = sum(tab$Pago_interes),
      IVA = sum(tab$IVA),
      Cuota = sum(tab$Cuota),
      `Cuota+IVA` = sum(tab$`Cuota+IVA`),
      check.names = FALSE
    )
    
    full <- bind_rows(
      tab %>% select(Periodo, Saldo_inicial, Pago_principal, Pago_interes, IVA, Cuota, `Cuota+IVA`) %>%
        mutate(Periodo = as.character(Periodo)),
      tot
    )
    rownames(full) <- NULL
    
    datatable(
      full,
      rownames = FALSE,
      options = list(
        paging = FALSE,
        searching = FALSE,
        dom = "t",
        ordering = FALSE,
        scrollX = TRUE,
        columnDefs = list(
          list(className = 'dt-center', targets = 0),
          list(className = 'dt-right', targets = 1:6)
        ),
        rowCallback = DT::JS("
          function(row, data) {
            if(data[0] === 'Total') {
              $('td', row).css({
                'font-weight': 'bold',
                'background-color': '#f8f9fa',
                'border-top': '2px solid #dee2e6'
              });
            }
          }")
      )
    ) %>%
      formatCurrency(c("Saldo_inicial", "Pago_principal", "Pago_interes", "IVA", "Cuota", "Cuota+IVA"),
                     currency = "", digits = 2, interval = 3, mark = ".", dec.mark = ",")
  })
  
  # Indicadores avanzados
  output$payback_txt <- renderText({req(datos())$payback})
  output$duration_txt <- renderText({round(req(datos())$duration, 2)})
  output$irr_mes_txt <- renderText({percent(req(datos())$irr, 0.01)})
  output$irr_anual_txt <- renderText({
    irr <- req(datos())$irr
    if(is.na(irr)) "NA" else percent((1+irr)^12-1, 0.01)
  })
  
  # Gráfico de breakdown por componentes
  output$plot_breakdown <- renderPlotly({
    d <- req(datos())
    tabla <- d$tabla
    
    # Usar flujo constante para el breakdown también
    flujo_constante <- d$flujo_constante
    
    # Crear datos para el breakdown acumulativo con flujo constante
    breakdown_data <- data.frame(
      Periodo = tabla$Periodo,
      Capital_acum = cumsum(tabla$Pago_principal),
      Intereses_acum = cumsum(tabla$Pago_interes),
      IVA_acum = cumsum(tabla$IVA),
      Flujo_constante_acum = cumsum(rep(flujo_constante, nrow(tabla)))
    )
    
    # Calcular porcentajes de vida útil
    vida_util_pct <- (breakdown_data$Periodo / max(tabla$Periodo)) * 100
    
    # Crear data frame para plotly con formato largo (incluyendo flujo constante)
    breakdown_long <- data.frame(
      Periodo = rep(breakdown_data$Periodo, 4),
      Vida_util_pct = rep(vida_util_pct, 4),
      Componente = rep(c("Capital", "Intereses", "IVA", "Flujo Constante (sin IVA)"), each = nrow(breakdown_data)),
      Valor_acum = c(breakdown_data$Capital_acum, 
                     breakdown_data$Intereses_acum, 
                     breakdown_data$IVA_acum,
                     breakdown_data$Flujo_constante_acum),
      stringsAsFactors = FALSE
    )
    
    # Crear el gráfico
    p <- plot_ly(data = breakdown_long, 
                 x = ~Vida_util_pct, 
                 y = ~Valor_acum, 
                 color = ~Componente,
                 type = 'scatter', 
                 mode = 'lines+markers',
                 line = list(width = 3),
                 marker = list(size = 6),
                 colors = c("Capital" = "#34495e", "Intereses" = "#e74c3c", "IVA" = "#f39c12", "Flujo Constante (sin IVA)" = "#2980b9"),
                 hovertemplate = paste(
                   "<b>%{fullData.name}</b><br>",
                   "Período: %{customdata[0]}<br>",
                   "% Vida útil: %{x:.1f}%<br>",
                   "Acumulado: $%{y:,.0f}<extra></extra>"
                 ),
                 customdata = ~cbind(Periodo)) %>%
      layout(
        title = list(
          text = "Performance por Componente - Retorno Acumulado",
          font = list(size = 16, color = "#2c3e50")
        ),
        xaxis = list(
          title = "% de Vida Útil de la Cartera",
          titlefont = list(size = 14),
          tickfont = list(size = 12),
          showgrid = TRUE,
          gridcolor = "#ecf0f1"
        ),
        yaxis = list(
          title = "Retorno Acumulado ($)",
          titlefont = list(size = 14),
          tickfont = list(size = 12),
          showgrid = TRUE,
          gridcolor = "#ecf0f1"
        ),
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = 'center',
          y = -0.15
        ),
        hovermode = "x unified",
        plot_bgcolor = "white",
        paper_bgcolor = "white",
        shapes = list(
          list(
            type = "line",
            x0 = 20, x1 = 20,
            y0 = 0, y1 = 1,
            yref = "paper",
            line = list(color = "red", width = 2, dash = "dash")
          )
        ),
        annotations = list(
          list(
            x = 20,
            y = 0.95,
            yref = "paper",
            text = "20% vida útil",
            showarrow = TRUE,
            arrowhead = 2,
            arrowsize = 1,
            arrowwidth = 2,
            arrowcolor = "red",
            ax = 20,
            ay = -30,
            font = list(color = "red", size = 12)
          )
        )
      )
    
    p
  })
  
  # Gráfico interactivo mejorado
  output$plot_acum <- renderPlotly({
    d <- req(datos())
    df <- data.frame(Periodo = d$tabla$Periodo, Acumulado = d$acum)
    
    p <- ggplot(df, aes(x = Periodo, y = Acumulado)) +
      geom_line(size = 1.5, color = "#3498db") +
      geom_point(size = 3, color = "#2980b9", alpha = 0.7) +
      geom_hline(yintercept = d$pv, linetype = "dashed", color = "#e74c3c", size = 1.2) +
      geom_ribbon(aes(ymin = 0, ymax = Acumulado), alpha = 0.1, fill = "#3498db") +
      labs(
        title = "Flujo Acumulado vs. Precio Compra",
        y = "$ Acumulado",
        x = "Período (Meses)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold", color = "#2c3e50"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "#ecf0f1", size = 0.5)
      ) +
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(breaks = pretty(df$Periodo, n = 10))
    
    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(
        hovermode = "x unified",
        title = list(text = "Flujo Acumulado vs. Precio Compra", font = list(size = 16))
      )
  })
}

# --------------------------------------------------------------------
# Helpers --------------------------------------------------------------

format_number <- function(x) comma(x, 0.01, locale = "es")

# --------------------------------------------------------------------
# Run -----------------------------------------------------------------
shinyApp(ui, server)