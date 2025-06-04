## Calculadora de retorno de carteras – v2 (fix)
## Shiny app – Ricardo Puiggari

```r
# --------------------------------------------------------------------
# Paquetes ------------------------------------------------------------
library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(scales)
library(ggplot2)

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
      Cuota          = cuota,
      Flujo_neto     = cuota - iva          # capital + interés (sin IVA)
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
  
  pv_comp <- a_npv(tabla$Flujo_neto, tasa_desc / 12)
  iva_comp <- pmax(cap_int - pv_comp, 0) * iva_rate
  total_full <- cap_int + sum_iva + iva_comp
  
  flujo_tot <- tabla$Flujo_neto + tabla$IVA
  acum <- cumsum(flujo_tot)
  payback <- which(acum >= pv_comp)[1]
  if (is.na(payback)) payback <- "> horizonte"
  
  dur <- with(tabla, sum((Flujo_neto / sum(Flujo_neto)) * Periodo))
  
  irr_fun <- function(r) a_npv(flujo_tot, r) - pv_comp
  irr <- tryCatch(uniroot(irr_fun, c(0, 2))$root, error = function(e) NA)
  
  list(tabla=tabla, cap=cap, pv=pv_comp, sum_int=sum_int, sum_iva=sum_iva,
       iva_comp=iva_comp, iva_total=sum_iva+iva_comp,
       roi_cap_int=(cap_int-pv_comp)/pv_comp,
       roi_total=(total_full-pv_comp)/pv_comp,
       iva_ratio=(sum_iva+iva_comp)/pv_comp,
       payback=payback, duration=dur, irr=irr, acum=acum)
}

capital_from_iva <- function(obj, tasa, n, iva_r, tasa_desc) {
  f <- function(c) calc_kpis(c, amort_tbl(c, tasa, n, iva_r), tasa_desc, iva_r)$iva_total - obj
  uniroot(f, c(0, obj*20))$root
}

# --------------------------------------------------------------------
# UI ------------------------------------------------------------------

tema <- bs_theme(version = 5, bootswatch = "flatly")

ui <- fluidPage(
  theme=tema,
  titlePanel("Calculadora de retorno de carteras"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("modo","Modo de entrada:",c("Capital"="cap","IVA a recuperar"="iva"),"cap"),
      conditionalPanel("input.modo=='cap'", numericInput("capital","Capital cartera ($):",1000,0)),
      conditionalPanel("input.modo=='iva'", numericInput("iva_obj","IVA a recuperar ($):",200,0)),
      numericInput("tasa_anual","Tasa anual crédito (%):",120,0),
      numericInput("n_cuotas","Número de cuotas:",12,1),
      numericInput("iva_rate",  "Tasa IVA (%):",             21,  min = 0),
      sliderInput("mora", "Mora esperada (%):", min = 0, max = 10, value = 3, step = 0.1),
      numericInput("tasa_desc", "Tasa descuento compra (%):", 55,  min = 0),"Tasa descuento compra (%):",55,0),
    actionButton("calcular","Calcular",class="btn-primary")
  ),
  mainPanel(
    tabsetPanel(id="tabs",
                tabPanel("Resumen",
                         fluidRow(uiOutput("metrics_ui")), hr(), DTOutput("tabla")),
                tabPanel("Análisis avanzado",
                         br(),
                         fluidRow(
                           column(3,valueBox(textOutput("payback_txt"),"Payback (mes)","#7f8c8d")),
                           column(3,valueBox(textOutput("duration_txt"),"Duración media","#95a5a6")),
                           column(3,valueBox(textOutput("irr_mes_txt"),"TIR mensual","#16a085")),
                           column(3,valueBox(textOutput("irr_anual_txt"),"TIR anual","#27ae60"))
                         ),
                         hr(),
                         plotOutput("plot_acum",height="300px")
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
    tabla <- amort_tbl(cap, t_credito, n, iva_r)
    # aplicar mora (stress)
    mora <- input$mora / 100
    if (mora > 0) {
      tabla <- tabla %>% mutate(across(c(Pago_principal, Pago_interes, IVA, Cuota, Flujo_neto), ~ .x * (1 - mora)))
    }
    k <- calc_kpis(cap, tabla, t_desc, iva_r)
    k
  })
  
  # Resumen tarjetas
  output$metrics_ui <- renderUI({
    m<-req(datos())
    tagList(
      valueBox(format_number(m$pv),"Precio compra (VAN)","#f1c40f"),
      valueBox(format_number(m$cap),"Capital cartera","#34495e"),
      valueBox(format_number(m$sum_int),"Suma intereses","#2c3e50"),
      valueBox(format_number(m$sum_iva),"IVA intereses","#18bc9c"),
      valueBox(format_number(m$iva_comp),"IVA compra","#9b59b6"),
      valueBox(format_number(m$iva_total),"IVA total","#8e44ad"),
      valueBox(percent(m$iva_ratio,0.01),"IVA / inversión","#1abc9c"),
      valueBox(percent(m$roi_cap_int,0.01),"ROI capital+int","#2980b9"),
      valueBox(percent(m$roi_total,0.01),"ROI total c/IVA","#e74c3c")
    )
  })
  
  # Tabla
  output$tabla <- renderDT({
    tab <- req(datos())$tabla
    
    tot <- data.frame(
      Periodo        = "Total",
      Saldo_inicial  = NA_real_,
      Pago_principal = sum(tab$Pago_principal),
      Pago_interes   = sum(tab$Pago_interes),
      IVA            = sum(tab$IVA),
      Cuota          = sum(tab$Cuota)
    )
    
    full <- bind_rows(
      tab %>% select(Periodo, Saldo_inicial, Pago_principal, Pago_interes, IVA, Cuota) %>%
        mutate(Periodo = as.character(Periodo)),
      tot
    )
    rownames(full) <- NULL
    
    datatable(
      full,
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
  
  # Indicadores avanzados
  output$payback_txt<-renderText({req(datos())$payback})
  output$duration_txt<-renderText({round(req(datos())$duration,2)})
  output$irr_mes_txt<-renderText({percent(req(datos())$irr,0.01)})
  output$irr_anual_txt<-renderText({
    irr<-req(datos())$irr
    if(is.na(irr))"NA" else percent((1+irr)^12-1,0.01)
  })
  
  output$plot_acum<-renderPlot({
    d<-req(datos())
    df<-data.frame(Periodo=d$tabla$Periodo,Acumulado=d$acum)
    ggplot(df,aes(Periodo,Acumulado))+geom_line(size=1.2)+geom_hline(yintercept=d$pv,lty="dashed")+
      labs(title="Flujo acumulado vs. Precio compra",y="$ acumulado",x="Mes")+theme_minimal()
  })
}

# --------------------------------------------------------------------
# Helpers --------------------------------------------------------------

valueBox <- function(value, subtitle, color){
  htmltools::div(class="card shadow-sm mb-3",
                 htmltools::div(style=paste0("background-color:",color,";"),class="card-header text-white",subtitle),
                 htmltools::div(class="card-body h3",value,style="text-align:center;")
  )
}

format_number <- function(x) comma(x,0.01,locale="es")

# --------------------------------------------------------------------
# Run -----------------------------------------------------------------
shinyApp(ui,server)
```
