rm(list = ls())

library(shiny)
library(ggvis)
library(dplyr)
library(ggplot2)
library(plotly)

ui <- fluidPage(
  headerPanel('Sample Size Calculator'),
  sidebarLayout(
    sidebarPanel(
      
  ## Step 1 choose test type
  radioButtons("test_type",
                tags$h4("Step 1. Select Test Type"),
                choices = c("t-test to compare means", 
                            "z-test to compare proportions"),
                selected = c("t-test to compare means")
               ),
  ## t- test
  conditionalPanel(condition = "input.test_type == 't-test to compare means'",
                    selectInput('axis_val_t',
                                tags$h4('Step 2. Select Axis Variable:'),
                                   choices = c("significance level",
                                               "power",
                                               "effect_size"
                                               ),
                                   selected = c("significance level")
                                ),
                    
                   ## Axis variable = alpha
                    conditionalPanel(condition = "input.axis_val_t == 'significance level'",
                                      
                                      ## alpha : range
                                      sliderInput(inputId = "t.val_sig.level", 
                                                  label = "2.1 Choose a range of significance level (step = 0.01)",
                                                  min=0, max=0.1, 
                                                  value=c(0.05,0.1),
                                                  step=0.01),
                                      
                                     ## power : fixed
                                      sliderInput(inputId = "t.power_sig.level",
                                                 label = "2.2 Specify Power",
                                                 min=0, max=0.99, 
                                                 value=0.95, 
                                                 step=0.01),
                                      
                                     ## delta: fixed
                                      sliderInput(inputId = "t.effectsize_sig.level",
                                                 label = "2.3 Specify Effect Size",
                                                 min=0.1,max=1,
                                                 value = 0.5)
                                      ),
                    ## Axis variable = power
                    conditionalPanel(condition = "input.axis_val_t == 'power'",
                                      ## power : range
                                      sliderInput(inputId = "t.val_power", 
                                                  label = "2.1 Choose a range of accepted power",
                                                  min=0, max=1, 
                                                  value=c(0.9,0.95),
                                                  step=0.01),
                                      ## alpha : fixed
                                      sliderInput(inputId = "t.sig.level_power",
                                                 label = "2.2 Specify Significance Level",
                                                 min = 0, max = 0.4, 
                                                 value = 0.2),
                                      ## delta : fixed
                                      sliderInput(inputId = "t.effectsize_power",
                                                 label = "2.3 Specify Effect Size",
                                                 min=0.1, max=1,
                                                 value = 0.5)
                                      ),
                    ## Axis variable = delta
                    conditionalPanel(condition = "input.axis_val_t == 'effect_size'",
                                      ## delta : range
                                      sliderInput(inputId = "t.val_effect_size", 
                                                  label = "2.1 Choose a range of effect size",
                                                  min=0.1, max=1, 
                                                  value=c(0.1,0.3),
                                                  step=0.05),
                                      ## alpha : fixed
                                      sliderInput(inputId = "t.sig.level_effect_size",
                                                 label = "2.2 Specify Significance Level",
                                                 min = 0, max = 0.4, 
                                                 value = 0.2),
                                      ## power : fixed
                                      sliderInput(inputId = "t.power_effect_size",
                                                 label = "2.3 Specify Power",
                                                 min=0, max=0.99, 
                                                 value=0.95)
                                      )
                    ),
  
  ## z-test
  conditionalPanel(condition = "input.test_type == 'z-test to compare proportions'",
                    selectInput('axis_val_z',
                                tags$h4('Step 2. Select Axis Variable:'),
                                choices = c("significance level",
                                            "power",
                                            "p1",
                                            "p2"),
                                selected = c("power")
                                ),
                    ## Axis variable = alpha 
                    conditionalPanel(condition = "input.axis_val_z == 'significance level'",
                                      ## alpha : range
                                      sliderInput(inputId = "z.val_sig.level", 
                                                   label = "2.1 Choose a range of significance level",
                                                   min=0, max=0.4, 
                                                  value=c(0.04,0.07),step=0.01),
                                      ## power : fixed
                                      sliderInput(inputId = "z.power_sig.level",
                                                 label = "2.2 Specify Power",
                                                 min=0.5, max=0.99, value=0.9, step=0.01),
                                      ## p1: fixed
                                      sliderInput(inputId = "z.p1_sig.level",
                                                 label = "2.3 Specify p1",
                                                 min=0, max=1, value=0.5, step=0.01),
                                      ## p2: fixed
                                      sliderInput(inputId = "z.p2_sig.level",
                                                 label = "2.4 Specify p2",
                                                 min=0, max=1, value=0.6, step=0.01)
                                      ),
                    ## Axis variable = power
                    conditionalPanel(condition = "input.axis_val_z == 'power'",
                                      ## power : range
                                      sliderInput(inputId = "z.val_power", 
                                                  label = "2.1 Choose a range of accepted power",
                                                  min=0, max=1, value=c(0.9,0.95),
                                                  step=0.01)
                                      ## alpha : fixed
                                      ,sliderInput(inputId = "z.sig.level_power",
                                                 label = "2.2 Specify Significance Level",
                                                 min = 0, max = 0.1, 
                                                 value = 0.01)
                                      ## p1: fixed
                                      ,sliderInput(inputId = "z.p1_power",
                                                 label = "2.3 Specify p1",
                                                 min=0, max=1, value=0.5, 
                                                 step=0.01)
                                      ## p2: fixed
                                      ,sliderInput(inputId = "z.p2_power",
                                                 label = "2.4 Specify p2",
                                                 min=0, max=1, value=0.7,
                                                 step=0.01)
                                      )
                    ## Axis variable = p1
                    ,conditionalPanel(condition = "input.axis_val_z == 'p1'",
                                      ## p1: range
                                      sliderInput(inputId = "z.val_p1", 
                                                  label = "2.1 Choose a range of p1",
                                                  min=0, max=1, value=c(0.5,0.6),
                                                  step=0.01)
                                      ## alpha : fixed
                                      ,sliderInput(inputId = "z.sig.level_p1",
                                                 label = "2.2 Specify Significance Level",
                                                 min = 0, max = 0.4, value = 0.2)
                                      ## power : fixed
                                      ,sliderInput(inputId = "z.power_p1",
                                                 label = "2.3 Specify Power",
                                                 min=0, max=0.99, value=0.95, 
                                                 step=0.01)
                                      ## p2: fixed
                                      ,sliderInput(inputId = "z.p2_p1",
                                                 label = "2.4 Specify p2",
                                                 min=0, max=1, value=0.7, 
                                                 step=0.01)
                                      )
                    ## Axis variable = p2
                    ,conditionalPanel(condition = "input.axis_val_z == 'p2'",
                                      ## p2: range
                                      sliderInput(inputId = "z.val_p2", 
                                                  label = "2.1 Choose a range of p2",
                                                  min=0, max=1, value=c(0.5,0.6),
                                                  step=0.01),
                                      ## alpha : fixed
                                      sliderInput(inputId = "z.sig.level_p2",
                                                 label = "2.2 Specify Significance Level",
                                                 min = 0, max = 0.4, value = 0.2),
                                      ## power : fixed
                                      sliderInput(inputId = "z.power_p2",
                                                 label = "2.3 Specify Power",
                                                 min=0, max=0.99, value=0.95, 
                                                 step=0.01),
                                      ## p1: fixed
                                      sliderInput(inputId = "z.p1_p2",
                                                 label = "2.4 Specify p1",
                                                 min=0, max=1, value=0.8, 
                                                 step=0.01)
                                      )
                    ),
  selectInput('axis_val_t_side',
               tags$h4('Step 3. Select Tail:'),
              choices = c("One Tail Test"
                          ,"Two Tail Test"),
              selected = c("Two Tail Test"))
    ),
  mainPanel(
    plotlyOutput("plot")
  )
  )
)

ttest_methods <- function(input, alt) {
  figure = NULL

  ## Axis variable = alpha
  ## getting variable values
  if (input$axis_val_t == "significance level") {
    power <- reactive({input$t.power_sig.level})
    delta <- reactive({input$t.effectsize_sig.level})
    
    alpha_minmax <- reactive({
      cbind(input$t.val_sig.level[1],
            input$t.val_sig.level[2])})
    
    ## calculating n for entire range
    n = c()
    alpha = seq(alpha_minmax()[1], 
                alpha_minmax()[2], 
                by=0.005)
    
    for (i in alpha) {
      n_calc = power.t.test(
        n = NULL,
        delta = delta(), 
        power = power(), 
        sig.level = i,
        alternative= alt)
      n_calc = n_calc$n
      n = append(n, n_calc)
    }
    
    df = data.frame(alpha, n)
    figure = ggplot(df, aes(x = alpha, y = n)) + 
      geom_point(colour = "blue")+ 
      xlab("\n Significance Level") + 
      ylab("n (Sample size in each group) \n") +
      scale_y_continuous(breaks = round(seq(min(n),max(n+50),by = 2)))
    ggplotly(figure)
  }
  
  ## Axis variable = power
  ## getting variable values
  if (input$axis_val_t == "power") {
    alpha = reactive({input$t.sig.level_power})
    power_minmax <- reactive({
      cbind(input$t.val_power[1],
            input$t.val_power[2])})
    delta<-reactive({input$t.effectsize_power})
    
    ## calculating n for entire range
    n = c()
    power = seq(power_minmax()[1], 
                power_minmax()[2], by=0.005)
    for (i in power) {
      n_calc = power.t.test(
        n = NULL, 
        delta = delta(), 
        power = i, 
        sig.level = alpha(),
        alternative = alt)
      n_calc = n_calc$n
      n = append(n, n_calc)
    }
    
    df = data.frame(power, n)
    figure =
      ggplot(df, aes(x = power, y = n)) + 
      geom_point(colour = "blue")+ 
      xlab("Power") + 
      ylab("n (Sample size in each group)")+
      scale_y_continuous(breaks = round(seq(min(n),max(n+50),by = 2)))
  } 
  
  ## Axis variable = effect size
  ## getting variable values
  if (input$axis_val_t == "effect_size") {
    alpha = reactive({input$t.sig.level_effect_size})
    power = reactive({input$t.power_effect_size})
    delta_minmax <- reactive({
      cbind(input$t.val_effect_size[1],
            input$t.val_effect_size[2])})
    
    ## calculating n for entire range
    n = c()
    delta = seq(delta_minmax()[1], 
                delta_minmax()[2], 
                by=0.005)
    for (i in delta) {
      n_calc = power.t.test(
        n = NULL, 
        delta = i, 
        power = power(), 
        sig.level = alpha(),
        alternative = alt
      )
      n_calc = n_calc$n
      n = append(n, n_calc)
    }
    
    df = data.frame(delta, n)
    figure =
      ggplot(df, aes(x = delta, y = n)) + 
      geom_point(colour = "blue")+ 
      xlab(" Effect Size") + 
      ylab("n (Sample size in each group)")+
      scale_y_continuous(breaks = round(seq(min(n),max(n+20),by = max(n)/10),-1))
  }
  
  return (figure)
}

ztest_methods <- function(input, alt) {
  figure = NULL

  if (input$axis_val_z == "significance level") {
    power <- reactive({input$z.power_sig.level})
    p1 <- reactive({input$z.p1_sig.level})
    p2<-reactive(input$z.p2_sig.level)
    
    alpha_minmax <- reactive({
      cbind(input$z.val_sig.level[1],
            input$z.val_sig.level[2])})
    
    ## calculating n for entire range
    n = c()
    alpha = seq(alpha_minmax()[1], 
                alpha_minmax()[2], 
                by=0.005)
    
    for (i in alpha) {
      n_calc = power.prop.test(
        n = NULL,
        p1 = p1(),
        p2 = p2(),
        power = power(),
        sig.level = i,
        alternative = alt)
      n_calc = n_calc$n
      n = append(n, n_calc)
    }
    
    df = data.frame(alpha, n)
    
    figure =
      ggplot(df, aes(x = alpha, y = n)) + 
      geom_point(colour = "blue")+ 
      xlab("Significance Level") + 
      ylab("n (Sample size in each group)")
  }
    
  ## Axis variable = power
  ## getting variable values
  if (input$axis_val_z == "power"){
      alpha = reactive({input$z.sig.level_power})
      p1 <- reactive({input$z.p1_power})
      p2<-reactive({input$z.p2_power})
      
      power_minmax <- reactive({
        cbind(input$z.val_power[1],
              input$z.val_power[2])})
      
      ## calculating n for entire range
      n = c()
      power = seq(power_minmax()[1], 
                  power_minmax()[2], 
                  by=0.005)
      
      for (i in power) {
        n_calc = power.prop.test(
          n = NULL, 
          power = i, 
          p1=p1(),
          p2=p2(),
          sig.level = alpha(),
          alternative = alt)
        n_calc = n_calc$n
        n = append(n, n_calc)
      }
      
      df = data.frame(power, n)
      figure =
        ggplot(df, aes(x = power, y = n)) + 
        geom_point(colour = "blue")+ 
        xlab("Power") + 
        ylab(" n (Sample size in each group)")
  }
  
  ## Axis variable = p1
  ## getting variable values
  if (input$axis_val_z == "p1") {
    alpha = reactive({input$z.sig.level_p1})
    power = reactive({input$z.power_p1})
    p2=reactive({input$z.p2_p1})
        
    p1_minmax <- reactive({
      cbind(input$z.val_p1[1],
            input$z.val_p1[2])})
        
    ## calculating n for entire range
    n = c()
    p1_range = seq(p1_minmax()[1], 
                   p1_minmax()[2], by=0.002)

    for (i in p1_range) {
      n_calc = power.prop.test(
        n = NULL, 
        p1 = i,
        p2 = p2(),
        power = power(),
        sig.level = alpha(),
        alternative = alt)
      n_calc = n_calc$n
      n = append(n, n_calc)
    }
        
    df1 = data.frame(p1_range, n)
    figure =
      ggplot(df1, aes(x = p1_range, y = n)) + 
      geom_point(colour = "blue")+ 
      xlab(" P1 ") + 
      ylab("n (Sample size in each group)")
  }
    
  ## Axis variable = p2
  ## getting variable values
  if (input$axis_val_z == "p2") {
    alpha = reactive({input$z.sig.level_p2})
    power = reactive({input$z.power_p2})
    p1=reactive({input$z.p1_p2})
      
    p2_minmax <- reactive({
      cbind(input$z.val_p2[1],
            input$z.val_p2[2])
    })
      
    ## calculating n for entire range
    n = c()
    p2_range = seq(p2_minmax()[1], 
                   p2_minmax()[2], 
                   by=0.002)
    for (i in p2_range) {
      n_calc = power.prop.test(
        n = NULL, 
        p2 = i, 
        p1=p1(),
        power = power(), 
        sig.level = alpha(),
        alternative = alt)
      n_calc = n_calc$n
      n = append(n, n_calc)
    }
      
    df = data.frame(p2_range, n)
    figure =
      ggplot(df, aes(x = p2_range, y = n)) + 
      geom_point(colour = "blue")+ 
      xlab(" P2 ") + 
      ylab("n (Sample size in each group)")
  }
  
  return (figure)
}

server <- function(input, output) {
  test_type <- reactive ({input$test_type})
  one_two_sided <- reactive({input$axis_val_t_side})
  
  aniGraph <- reactive({
    alt = ifelse(one_two_sided() == 'Two Tail Test', 'two.sided', 'one.sided')
    if (test_type() == "t-test to compare means") {ggplotly(ttest_methods(input, alt))}  
    else {ggplotly(ztest_methods(input, alt))}
  })
  
  output$plot <- renderPlotly({aniGraph()})
}

shinyApp(ui = ui, server = server)
