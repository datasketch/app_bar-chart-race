library(shinypanels)
library(parmesan)
library(shinyinvoer)
library(shi18ny)
library(V8)
library(dsmodules)
library(hotr)
library(tidyverse)
library(homodatum)
library(gganimate)
library(gifski)
library(ggmagic)

ui <- panelsPage(includeScript(paste0(system.file("aux/", package = "dsmodules"), "downloadGen.js")),
                 useShi18ny(),
                 panel(title = ui_("upload_data"),
                       width = 200,
                       body = uiOutput("table_input")),
                 panel(title = ui_("dataset"), 
                       width = 300,
                       body = uiOutput("data_preview")),
                 panel(title = ui_("options"),
                       width = 250,
                       color = "chardonnay",
                       body = uiOutput("controls"),
                       footer =  div(style = "text-align: center; display: flex; align-items: baseline;",
                                     `data-for-btn` = "generate",
                                     # uiOutput("generate_bt"),
                                     uiOutput("generate_bt"),
                                     span(class = "btn-loading-container",
                                          img(style = "display: none; margin-left: 18px;",
                                              class = "btn-loading-indicator",
                                              src = "data:image/gif;base64,R0lGODlhEAALAPQAAP///wAAANra2tDQ0Orq6gYGBgAAAC4uLoKCgmBgYLq6uiIiIkpKSoqKimRkZL6+viYmJgQEBE5OTubm5tjY2PT09Dg4ONzc3PLy8ra2tqCgoMrKyu7u7gAAAAAAAAAAACH/C05FVFNDQVBFMi4wAwEAAAAh/hpDcmVhdGVkIHdpdGggYWpheGxvYWQuaW5mbwAh+QQJCwAAACwAAAAAEAALAAAFLSAgjmRpnqSgCuLKAq5AEIM4zDVw03ve27ifDgfkEYe04kDIDC5zrtYKRa2WQgAh+QQJCwAAACwAAAAAEAALAAAFJGBhGAVgnqhpHIeRvsDawqns0qeN5+y967tYLyicBYE7EYkYAgAh+QQJCwAAACwAAAAAEAALAAAFNiAgjothLOOIJAkiGgxjpGKiKMkbz7SN6zIawJcDwIK9W/HISxGBzdHTuBNOmcJVCyoUlk7CEAAh+QQJCwAAACwAAAAAEAALAAAFNSAgjqQIRRFUAo3jNGIkSdHqPI8Tz3V55zuaDacDyIQ+YrBH+hWPzJFzOQQaeavWi7oqnVIhACH5BAkLAAAALAAAAAAQAAsAAAUyICCOZGme1rJY5kRRk7hI0mJSVUXJtF3iOl7tltsBZsNfUegjAY3I5sgFY55KqdX1GgIAIfkECQsAAAAsAAAAABAACwAABTcgII5kaZ4kcV2EqLJipmnZhWGXaOOitm2aXQ4g7P2Ct2ER4AMul00kj5g0Al8tADY2y6C+4FIIACH5BAkLAAAALAAAAAAQAAsAAAUvICCOZGme5ERRk6iy7qpyHCVStA3gNa/7txxwlwv2isSacYUc+l4tADQGQ1mvpBAAIfkECQsAAAAsAAAAABAACwAABS8gII5kaZ7kRFGTqLLuqnIcJVK0DeA1r/u3HHCXC/aKxJpxhRz6Xi0ANAZDWa+kEAA7AAAAAAAAAAAA"),
                                          HTML("<i class = 'btn-done-indicator fa fa-check' style = 'display: none; margin-left: 18px;'> </i>")))),
                 panel(title = ui_("viz"),
                       color = "chardonnay",
                       can_collapse = FALSE,
                       body = div(langSelectorInput("lang", position = "fixed"),
                                  imageOutput("result"),
                                  shinypanels::modal(id = "download",
                                                     title = ui_("download_plot"),
                                                     uiOutput("modal"))),
                       footer = shinypanels::modalButton(label = "Download plot", modal_id = "download")))


server <- function(input, output, session) {
  
  i18n <- list(defaultLang = "en", availableLangs = c("es", "en", "pt"))
  lang <- callModule(langSelector, "lang", i18n = i18n, showSelector = TRUE)
  observeEvent(lang(), {
    uiLangUpdate(input$shi18ny_ui_classes, lang())
  })  
  
  output$table_input <- renderUI({
    choices <- c("sampleData", "pasted", "fileUpload", "googleSheets")
    names(choices) <- i_(c("sample", "paste", "upload", "google"), lang = lang())
    tableInputUI("initial_data",
                 choices = choices,
                 selected = ifelse(is.null(input$`initial_data-tableInput`), "sampleData", input$`initial_data-tableInput`))
  })
  
  brchr <- reactiveValues(pth = NULL,
                          an = NULL)
  
  path <- "parmesan"
  parmesan <- parmesan_load(path)
  parmesan_input <- parmesan_watch(input, parmesan)
  parmesan_alert(parmesan, env = environment())
  parmesan_lang <- reactive({i_(parmesan, lang(), keys = c("label", "choices", "text"))})
  output_parmesan("controls", 
                  parmesan = parmesan_lang,
                  input = input,
                  output = output, 
                  env = environment())
  
  output$generate_bt <- renderUI({
    gn <- i_("generate", lang())
    actionButton("generate", gn, style = "margin: 0;")
  }) 
  
  output$modal <- renderUI({
    dw <- i_("download", lang())
    loadingGif <- "data:image/gif;base64,R0lGODlhEAALAPQAAP///wAAANra2tDQ0Orq6gYGBgAAAC4uLoKCgmBgYLq6uiIiIkpKSoqKimRkZL6+viYmJgQEBE5OTubm5tjY2PT09Dg4ONzc3PLy8ra2tqCgoMrKyu7u7gAAAAAAAAAAACH/C05FVFNDQVBFMi4wAwEAAAAh/hpDcmVhdGVkIHdpdGggYWpheGxvYWQuaW5mbwAh+QQJCwAAACwAAAAAEAALAAAFLSAgjmRpnqSgCuLKAq5AEIM4zDVw03ve27ifDgfkEYe04kDIDC5zrtYKRa2WQgAh+QQJCwAAACwAAAAAEAALAAAFJGBhGAVgnqhpHIeRvsDawqns0qeN5+y967tYLyicBYE7EYkYAgAh+QQJCwAAACwAAAAAEAALAAAFNiAgjothLOOIJAkiGgxjpGKiKMkbz7SN6zIawJcDwIK9W/HISxGBzdHTuBNOmcJVCyoUlk7CEAAh+QQJCwAAACwAAAAAEAALAAAFNSAgjqQIRRFUAo3jNGIkSdHqPI8Tz3V55zuaDacDyIQ+YrBH+hWPzJFzOQQaeavWi7oqnVIhACH5BAkLAAAALAAAAAAQAAsAAAUyICCOZGme1rJY5kRRk7hI0mJSVUXJtF3iOl7tltsBZsNfUegjAY3I5sgFY55KqdX1GgIAIfkECQsAAAAsAAAAABAACwAABTcgII5kaZ4kcV2EqLJipmnZhWGXaOOitm2aXQ4g7P2Ct2ER4AMul00kj5g0Al8tADY2y6C+4FIIACH5BAkLAAAALAAAAAAQAAsAAAUvICCOZGme5ERRk6iy7qpyHCVStA3gNa/7txxwlwv2isSacYUc+l4tADQGQ1mvpBAAIfkECQsAAAAsAAAAABAACwAABS8gII5kaZ7kRFGTqLLuqnIcJVK0DeA1r/u3HHCXC/aKxJpxhRz6Xi0ANAZDWa+kEAA7AAAAAAAAAAAA"
    div(style = "text-align: center;",
        downloadButton("downloadGif", paste0(dw, " GIF"), style = "width: 200px; display: inline-block;"),
        span(class = "btn-loading-container",
             img(src = loadingGif, class = "btn-loading-indicator", style = "display: none"),
             shiny::HTML("<i class = 'btn-done-indicator fa fa-check' style='display: none'> </i>")))
  })
  
  labels <- reactive({
    list(sampleLabel = i_("sample_lb", lang()), 
         sampleFiles = list("Emission per capita C02" = "data/sampleData/emisiones_c02.csv", 
                            "Tennis grand slams" = "data/sampleData/tennis_grand_slams.csv"),
         pasteLabel = i_("paste", lang()), pasteValue = "", pastePlaceholder = i_("paste_pl", lang()), pasteRows = 5, 
         uploadLabel = i_("upload_lb", lang()), uploadButtonLabel = i_("upload_bt_lb", lang()), uploadPlaceholder = i_("upload_pl", lang()),
         googleSheetLabel = i_("google_sh_lb", lang()), googleSheetValue = "", googleSheetPlaceholder = i_("google_sh_pl", lang()),
         googleSheetPageLabel = i_("google_sh_pg_lb", lang())
         
         # infoList = list("pasted" = ("Esto es información sobre el paste"),
         #                 "fileUpload" = HTML("Esto es información sobre el fileUpload"),
         #                 "sampleData" = HTML("Info sample Data"),
         #                 "googleSheets" = HTML("IFO GGO"))
    )
  })
  
  observeEvent(lang(), {
    ch <- as.character(parmesan$styles$inputs[[3]]$input_params$choices)
    names(ch) <- i_(ch, lang())
    updateSelectInput(session, "background", choices = ch, selected = input$background)
  })
  
  inputData <- eventReactive(labels(), {
    do.call(callModule, c(tableInput, "initial_data", labels()))
  })
  
  output$data_preview <- renderUI({
    req(inputData())
    suppressWarnings(hotr("hotr_input", data = inputData(), order = NULL, options = list(height = 470), enableCTypes = FALSE))
  })
  
  dt <- reactive({
    req(input$hotr_input)
    hotr_table(input$hotr_input)
  })
  
  # poner sólo categóricas
  ids <- reactive({
    dt0 <- input$hotr_input$dic
    dt0$label[dt0$ctype %in% "Cat"]
  })
  
  # excluir la escogida en ids
  states <- reactive({
    # setdiff(names(dt()), c(input$ids, input$values))
    dt0 <- input$hotr_input$dic
    dt0$label[dt0$ctype %in% "Yea"]
    # que uno pueda ponde input__ids para selected y no lo tenga que poner en un reactivo independiente
    # setdiff(names(dt()), input$ids)
  })
  
  values <- reactive({
    # setdiff(names(dt()), c(input$states, input$ids))
    dt0 <- input$hotr_input$dic
    dt0$label[dt0$ctype %in% "Num"]
  })
  
  dt_ready <- reactive({
    req(input$ids, input$states, input$values)
    nm0 <- c(input$ids, input$states, input$values)
    if (all(nm0 %in% names(dt()))) {
      if (n_distinct(nm0) == length(nm0) ) {
        d0 <- dt()[, nm0]
        names(d0) <- c("a", "b", "c")
        
        d <- d0 %>%
          group_by(a, b) %>%
          summarise(c = sum(as.numeric(c), na.rm = TRUE)) %>%
          ungroup() %>%
          group_by(b) %>%
          arrange(desc(c)) %>% 
          slice(1:10) %>%
          mutate(rk = n():1) %>%
          ungroup()
        
        # d <- d0 %>%
        #   group_by(b) %>%
        #   arrange(desc(c))
        #   mutate(rk = rank(-c)) %>%
        #   # Value_rel = values/values[rank == 1],
        #   # Value_lbl = paste0(" ", round(values / 1e9))) %>%
        #   group_by(a) %>% 
        #   filter(rk <= 10) %>%
        #   ungroup()
        # assign("dt1", d, envir = globalenv())
      }
    }
  })
  
  observeEvent(input$generate, {
    session$sendCustomMessage("setButtonState", c("none", "downloadGif"))
    session$sendCustomMessage("setButtonState", c("loading", "generate_bt"))
    vl <- rep(c("#3DB26F", "#FECA84", "#74D1F7", "#F75E64", "#8097A4", "#B70F7F", "#5D6AE9", "#53255E", "#BDCAD1"), 3)[seq_along(unique(dt1$a))]
    names(vl) <- unique(dt1$a)
    cl <- ifelse(input$text_show, "black", "transparent")
    tl <- gsub("\\n", "\n", input$title, fixed = TRUE)
    st <- input$states_text %||% input$states
    g0 <- ggplot(dt_ready(), aes(x = rk, y = c, fill = a)) +
      geom_bar(stat = "identity") +
      geom_bar(stat = "identity") +
      geom_text(aes(y = 0, label = a), vjust = 0.2, hjust = 1.1, size = 4) +
      geom_text(aes(y = c, label = paste0(input$prefix, format(c, digits = input$n_digits), input$suffix), hjust = -0.1), size = 4, color = cl) +
      coord_flip(clip = "off", expand = FALSE) +
      labs(title = tl, subtitle = paste0(st, ": {closest_state}"), caption = input$caption) +
      # geom_text(aes(x = 1, y = 18.75, label = "Month: {closest_state}")) +
      # scale_y_continuous(labels = scales::comma) +
      # scale_x_reverse() +
      guides(color = FALSE, fill = FALSE) +
      do.call(paste0("theme_", input$theme), list()) +
      theme(#axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        # legend.position = "none",
        plot.title = element_text(size = 16, hjust = 1, vjust = 6),
        plot.subtitle = element_text(size = 16, hjust = 1, vjust = 5, color = "grey"),
        plot.caption = element_text(size = 13, hjust = 1, color = "gray"),
        plot.margin = margin(2, 2, 2, 4, "cm"))
    if (input$theme == "ds") {
      g0 <- g0 + 
        scale_fill_manual("legend", values = vl) +
        theme(plot.background = element_rect(colour = input$background, fill = input$background),
              panel.background = element_rect(fill = input$background))
    }
    if (!input$y_labels_show) {
      g0 <- g0 + theme(axis.text.x = element_blank())
    }
    
    # if (input$orientation == "ver") {
    #   g0 <- g0 + coord_flip()
    # }
    g0 <- g0 +
      transition_states(b, transition_length = 4, state_length = 1) +
      # view_follow() +
      # view_follow(fixed_x = TRUE)
      ease_aes('sine-in-out')
    
    # g1 <- animate(g0,  nframes = 10, fps = 7, rewind = FALSE)
    g1 <- animate(g0,  nframes = input$nframes, #fps = input$fps,
                  duration = input$duration, rewind = FALSE)
    gif_path <- tempfile(fileext = ".gif")
    # assign("g1", g1, envir = globalenv())
    brchr$an <- g1
    anim_save(gif_path, g1)
    session$sendCustomMessage("setButtonState", c("done", "generate_bt"))
    brchr$pth <- gif_path
  })
  
  output$result <- renderImage({
    if (!is.null(brchr$pth)) {
      pth <- brchr$pth
    } else {
      pth <- "data/sampleData/prim.gif"
    }
    list(src = pth, contentType = "image/gif")
  }, deleteFile = FALSE)
  
  observeEvent(list(parmesan_input(), inputData()), {
    session$sendCustomMessage("setButtonState", c("none", "generate_bt"))
  })
  
  output$downloadGif <- downloadHandler(filename = function() { 
    session$sendCustomMessage("setButtonState", c("loading", "downloadGif"))
    paste0("bar_chart_race", "-", gsub(" ", "_", substr(as.POSIXct(Sys.time()), 1, 19)), ".gif")
    },
    content = function(file) {
      anim_save(file, brchr$an)
      session$sendCustomMessage("setButtonState", c("done", "downloadGif"))
    })
  
}



shinyApp(ui, server)