{
  # ====================================================================
  # app.R – interactive demo for the DescriptiveRepresentation calculator
  # Upload this single file to a Shiny‑typed Hugging Face Space.
  # ====================================================================
  
  # install.packages( "~/Documents/DescriptiveRepresentationCalculator-software/DescriptiveRepresentationCalculator", repos = NULL, type = "source",force = F)
  options(error = NULL)

  # ====================================================================
  # app.R – interactive demo for the Descriptive Representation Viewer
  # Upload this single file to a Shiny‑typed Hugging Face Space.
  # ====================================================================
  
  options(error = NULL)
  suppressPackageStartupMessages({
    library(shiny)
    library(ggplot2)
    library(tidyr)
    library(scales)
  })
  
  # --------------------------------------------------------------------
  # 1.  Calculator functions (verbatim from Gerring, Jerzak & Öncel 2024)
  # --------------------------------------------------------------------
  library( DescriptiveRepresentationCalculator )
  
  # --------------------------------------------------------------------
  # 2.  Shiny user interface
  # --------------------------------------------------------------------
  ui <- fluidPage(
    # ---- Custom title block ---------------------------------------------------
    tags$div(
      style = "margin-top: 20px;",
      tags$h2("Descriptive Representation Viewer"),
      tags$p(
        tags$a(href = "https://globalleadershipproject.net/",
               tags$span("🔗 GlobalLeadershipProject.net"))
      )
    ),
    
    sidebarLayout(
      sidebarPanel(
        numericInput("bodyN", "Size of the political body (N):",
                     value = 50, min = 1, step = 1),
        
        sliderInput("ngroups", "Number of population groups (K):",
                    min = 2, max = 5, value = 3, step = 1),
        
        uiOutput("popShareInputs"),   # K – 1 numeric inputs + note
        
        checkboxInput("hasBody", "I have the body’s member counts", FALSE),
        
        conditionalPanel(
          "input.hasBody == true",
          uiOutput("bodyCountInputs")
        ),
        
        actionButton("go", "Compute", class = "btn-primary")
      ),
      
      mainPanel(
        fluidRow(
          column(4, verbatimTextOutput("expBox")),
          column(4, verbatimTextOutput("obsBox")),
          column(4, verbatimTextOutput("relBox"))
        ),
        hr(),
        plotOutput("sharePlot", height = "400px"),
        hr(),
        helpText(
          "Indices are based on the Rose Index (a = –0.5, b = 1). ",
          "Expected values follow Gerring, Jerzak & Öncel (2024) ",
          tags$a("[PDF]",
                 href = "https://www.cambridge.org/core/services/aop-cambridge-core/"
                 |> paste0("content/view/7EAEA1CA4C553AB9D76054D1FA9C0840/",
                           "S0003055423000680a.pdf/the-composition-of-",
                           "descriptive-representation.pdf"),
                 target = "_blank")
        )
      )
    )
  )
  
  # --------------------------------------------------------------------
  # 3.  Server logic
  # --------------------------------------------------------------------
  server <- function(input, output, session) {
    
    # ----- Dynamic numericInputs for the first K‑1 shares ----------------------
    output$popShareInputs <- renderUI({
      n <- input$ngroups
      if (n < 2) return()
      tagList(
        lapply(seq_len(n - 1), function(i) {
          numericInput(
            inputId  = paste0("share", i),
            label    = paste("Share of group", LETTERS[i]),
            value    = round(1 / n, 3),
            min      = 0, max = 1, step = 0.001
          )
        }),
        tags$p(
          sprintf("The share of group %s is automatically set to 1 − (sum of the others).",
                  LETTERS[n]),
          style = "font-style: italic;"
        )
      )
    })
    
    # ----- Dynamic numericInputs for group‑wise body counts --------------------
    output$bodyCountInputs <- renderUI({
      n <- input$ngroups
      lapply(seq_len(n), function(i){
        numericInput(
          inputId = paste0("count", i),
          label   = paste("Members of group", LETTERS[i], "in body"),
          value   = 0, min = 0, step = 1
        )
      })
    })
    
    # ----- Helper producing the full K‑vector of population shares -------------
    popShares <- eventReactive(input$go, {
      n <- input$ngroups
      shares_first <- sapply(seq_len(n - 1),
                             function(i) input[[paste0("share", i)]])
      if (anyNA(shares_first) || any(shares_first < 0)) {
        showNotification("All shares must be non‑negative numbers.", type = "error")
        return(NULL)
      }
      remainder <- 1 - sum(shares_first)
      if (remainder < -1e-6) {
        showNotification("The entered shares exceed 1. Reduce them so that K‑1 shares "
                         |> paste("sum to at most 1."), type = "error")
        return(NULL)
      }
      shares <- c(shares_first, max(remainder, 0))
      setNames(shares, LETTERS[seq_len(n)])
    })
    
    # ----- Helper reading body counts into a named vector ----------------------
    bodyCounts <- reactive({
      req(input$hasBody)
      n <- input$ngroups
      counts <- sapply(seq_len(n), function(i) input[[paste0("count", i)]])
      if (anyNA(counts) || any(counts < 0)) {
        showNotification("Body counts must be non‑negative integers.", type = "error")
        return(NULL)
      }
      setNames(counts, LETTERS[seq_len(n)])
    })
    
    # ----- Main computation on “Compute” --------------------------------------
    observeEvent(input$go, {
      validate(need(!is.null(popShares()), "Fix population shares first."))
      
      # Expected representation
      expVal <- ExpectedRepresentation(popShares(), input$bodyN)
      output$expBox <- renderText(sprintf("Expected representation:\n%.3f", expVal))
      
      # Observed / relative representation (if counts supplied)
      if (input$hasBody) {
        validate(need(!is.null(bodyCounts()), "Enter valid body counts."))
        counts <- bodyCounts()
        bodyTotal <- sum(counts)
        if (bodyTotal == 0) {
          showNotification("Total body member count cannot be zero.", type = "error")
          output$obsBox <- renderText("Observed representation:\n—")
          output$relBox <- renderText("Relative representation:\n—")
          return()
        }
        if (bodyTotal != input$bodyN) {
          showNotification(
            sprintf("Sum of counts (%d) differs from N (%d). We use the counts.",
                    bodyTotal, input$bodyN),
            type = "warning", duration = 7
          )
        }
        bodyShares <- counts / bodyTotal
        obsVal <- ObservedRepresentation(NULL, popShares(),
                                         BodyShares = bodyShares)
        relVal <- RelativeRepresentation(obsVal, expVal)
        output$obsBox <- renderText(sprintf("Observed representation:\n%.3f", obsVal))
        output$relBox <- renderText(sprintf("Relative representation:\n%.3f", relVal))
      } else {
        output$obsBox <- renderText("Observed representation:\n—")
        output$relBox <- renderText("Relative representation:\n—")
      }
    }, ignoreNULL = TRUE)
    
    # ----- Bar plot ------------------------------------------------------------
    output$sharePlot <- renderPlot({
      req(popShares())
      
      # Body shares (only if counts provided)
      if (input$hasBody && !is.null(bodyCounts())) {
        counts <- bodyCounts()
        bodyShares <- counts / sum(counts)
      } else {
        bodyShares <- rep(NA_real_, length(popShares()))
      }
      
      df <- data.frame(
        Group      = factor(names(popShares()), levels = names(popShares())),
        Population = as.numeric(popShares()),
        Body       = as.numeric(bodyShares)
      )
      
      df_long <- pivot_longer(df, -Group, names_to = "Type", values_to = "Share")
      
      ggplot(df_long, aes(Group, Share, fill = Type)) +
        geom_col(position = position_dodge(width = 0.6),
                 width = 0.55, na.rm = TRUE) +
        scale_y_continuous(labels = percent_format(accuracy = 1)) +
        scale_fill_manual(values = c(Population = "grey60", Body = "steelblue")) +
        labs(x = NULL, y = "Share", fill = NULL) +
        theme_minimal(base_size = 14) +
        theme(legend.position = "bottom")
    })
  }
  
  # --------------------------------------------------------------------
  # 4.  Launch the app
  # --------------------------------------------------------------------
  shinyApp(ui, server)
}