library(shinypanels)
library(parmesan)
library(shinyinvoer)
library(shi18ny)
library(V8)
library(dsmodules)
library(dspins)
library(hotr)
library(tidyverse)
library(homodatum)
library(gganimate)
library(gifski)
library(magick)
library(makeup)
library(shinycustomloader)



ui <- panelsPage(styles = ".load-container {
                  top: 22%;
                 }",
                 includeScript(paste0(system.file("js/", package = "dsmodules"), "downloadGen.js")),
                 useShi18ny(),
                 showDebug(), 
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
                                     uiOutput("generate_bt"),
                                     span(class = "btn-loading-container",
                                          img(style = "display: none; margin-left: 18px;",
                                              class = "btn-loading-indicator",
                                              src = dsmodules:::loadingGif()),
                                          HTML("<i class = 'btn-done-indicator fa fa-check' style = 'display: none; margin-left: 18px;'> </i>")))),
                 panel(title = ui_("viz"),
                       title_plugin = uiOutput("download"),
                       color = "chardonnay",
                       can_collapse = FALSE,
                       body = div(langSelectorInput("lang", position = "fixed"),
                                  withLoader(imageOutput("result", height = "86vh"), type = "image", loader = "loading_gris.gif"))))



server <- function(input, output, session) {
  
  i18n <- list(defaultLang = "en", availableLangs = c("es", "en", "pt_BR"))
  lang <- callModule(langSelector, "lang", i18n = i18n, showSelector = FALSE)
  observeEvent(lang(), {uiLangUpdate(input$shi18ny_ui_classes, lang())})  
  
  output$table_input <- renderUI({
    choices <- c("sampleData", "pasted", "fileUpload", "googleSheets")
    names(choices) <- i_(c("sample", "paste", "upload", "google"), lang = lang())
    tableInputUI("initial_data",
                 label = "",
                 choices = choices,
                 selected = ifelse(is.null(input$`initial_data-tableInput`), "sampleData", input$`initial_data-tableInput`))
  })
  
  labels <- reactive({
    sm_f <- i_(c("sample_ch_0", "sample_ch_1"), lang())
    names(sm_f) <- i_(c("sample_ch_nm_0", "sample_ch_nm_1"), lang())
    
    list(sampleLabel = i_("sample_lb", lang()), 
         sampleFiles = sm_f,
         
         pasteLabel = i_("paste", lang()),
         pasteValue = "", 
         pastePlaceholder = i_("paste_pl", lang()), 
         pasteRows = 5, 
         
         uploadLabel = i_("upload_lb", lang()), 
         uploadButtonLabel = i_("upload_bt_lb", lang()), 
         uploadPlaceholder = i_("upload_pl", lang()),
         
         googleSheetLabel = i_("google_sh_lb", lang()), 
         googleSheetValue = "",
         googleSheetPlaceholder = i_("google_sh_pl", lang()),
         googleSheetPageLabel = i_("google_sh_pg_lb", lang())
    )
  })
  
  # inputData <- eventReactive(list(labels(), input$`initial_data-tableInput`), {
  inputData <- eventReactive(labels(), {
    do.call(tableInputServer, c("initial_data", labels()))
  })
  
  output$data_preview <- renderUI({
    req(inputData())
    suppressWarnings(hotr("hotr_input", data = inputData(), order = NULL, options = list(height = "86vh"), enableCTypes = FALSE))
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
  
  dt <- reactive({
    req(input$hotr_input)
    hotr_table(input$hotr_input)
  })
  
  # poner sólo categóricas
  ids <- reactive({
    dt0 <- hotr_fringe(input$hotr_input)$dic
    dt0$label[dt0$hdType %in% "Cat"]
  })
  
  # excluir la escogida en ids
  states <- reactive({
    # setdiff(names(dt()), c(input$ids, input$values))
    dt0 <- hotr_fringe(input$hotr_input)$dic
    dt0$label[dt0$hdType %in% c("Yea", "Dat")]
    # setdiff(names(dt()), input$ids)
  })
  
  values <- reactive({
    # setdiff(names(dt()), c(input$states, input$ids))
    dt0 <- hotr_fringe(input$hotr_input)$dic
    dt0$label[dt0$hdType %in% c("Num", "Pct")]
  })
  
  dt_ready <- reactive({
    req(input$ids, input$states)
    if (nzchar(input$values)) {
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
    } else {
      nm0 <- c(input$ids, input$states)
      if (all(nm0 %in% names(dt()))) {
        if (n_distinct(nm0) == length(nm0) ) {
          d0 <- dt()[, nm0]
          names(d0) <- c("a", "b")
          
          d <- d0 %>%
            group_by(a, b) %>%
            summarise(c = n()) %>%
            ungroup() %>%
            group_by(b) %>%
            arrange(desc(c)) %>% 
            slice(1:10) %>%
            mutate(rk = n():1) %>%
            ungroup()
        }
      }
    }
    
  })
  
  observeEvent(input$generate, {
    session$sendCustomMessage("setButtonState", c("none", "downloadGif"))
    session$sendCustomMessage("setButtonState", c("loading", "generate_bt"))
    vl <- rep(c("#3DB26F", "#FECA84", "#74D1F7", "#F75E64", "#8097A4", "#B70F7F", "#5D6AE9", "#53255E", "#BDCAD1"), 3)[seq_along(unique(dt_ready()$a))]
    names(vl) <- unique(dt_ready()$a)
    cl <- ifelse(input$text_show, "black", "transparent")
    tl <- gsub("\\n", "\n", input$title, fixed = TRUE)
    st <- input$states_text %||% input$states
    st <- ifelse(nzchar(st), paste0(st, ": "), st)
    lb <- makeup_num(dt_ready()$c, input$marks, prefix = input$prefix, suffix = input$suffix)
    g0 <- ggplot(dt_ready(), aes(x = rk, y = c, fill = a)) +
      geom_bar(stat = "identity") +
      geom_bar(stat = "identity") +
      geom_text(aes(y = 0, label = a), vjust = 0.2, hjust = 1.1, size = 4) +
      # geom_text(aes(y = c, label = paste0(input$prefix, format(c, digits = input$n_digits), input$suffix), hjust = -0.1), size = 4, color = cl) +
      geom_text(aes(y = c, hjust = -0.1), label = lb, size = 4, color = cl) +
      coord_flip(clip = "off", expand = FALSE) +
      labs(title = tl, subtitle = paste0(st, "{closest_state}"), caption = input$caption) +
      # geom_text(aes(x = 1, y = 18.75, label = "Month: {closest_state}")) +
      # scale_y_continuous(labels = scales::comma) +
      # scale_x_reverse() +
      guides(color = FALSE, fill = FALSE) 
    which_num_format
    if (input$theme == "ds") {
      type <- "outer"
      inner <- type == "inner"
      palette <- list(background = "#ffffff", 
                      text = list(inner = "#555555", outer = "#111111"), 
                      line = list(inner = "#826A50", outer = "#362C21"), 
                      gridline = "#c9c7d3", 
                      swatch = c("#111111", "#65ADC2", "#233B43", "#E84646", "#C29365",
                                 "#362C21", "#316675", "#168E7F", "#109B37"),
                      gradient = list(low = "#65ADC2", high = "#362C21"))
      spacing <- 0.5
      line_colour <- "#1d1d1d"
      text_colour <- "#555555"
      text_size <- 12
      line_weight <- 0.5
      x_title_spacing <- function(spacing) max(-1.2, -(spacing/1.25) + 0.5)
      y_title_spacing <- function(spacing) max(0.8, min(2.4, spacing))
      g0 <- g0 +
        theme(line = element_line(colour = line_colour, size = line_weight, linetype = 1, lineend = "butt"), 
              rect = element_rect(fill = "white", colour = text_colour, size = 0.5, linetype = 1), 
              text = element_text(debug = FALSE, margin = margin(), family = "", face = "plain", colour = text_colour, 
                                  size = text_size, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9), 
              axis.text = element_text(debug = FALSE, margin = margin(), size = rel(0.8), colour = text_colour), 
              strip.text = element_text(debug = FALSE, margin = margin(), size = rel(0.8)), axis.line = element_line(colour = line_colour), 
              axis.line.x = element_line(colour = line_colour), axis.line.y = element_line(colour = line_colour), 
              axis.text.x = element_text(debug = FALSE, margin = margin(0.1 * spacing, 0.1 * spacing, 0.1 * spacing, 0.1 * spacing, unit = "cm"), 
                                         vjust = 1, colour = text_colour, face = "bold"), 
              axis.text.y = element_text(debug = FALSE, margin = margin(0.1 * spacing, 0.1 * spacing, 0.1 * spacing, 0.1 * spacing, unit = "cm"), 
                                         hjust = 1, colour = text_colour, face = "bold"), 
              axis.ticks = element_line(colour = line_colour), axis.title = element_text(face = "bold", colour = text_colour), 
              axis.title.x = element_text(debug = FALSE, margin = margin(), vjust = x_title_spacing(spacing)), 
              axis.title.y = element_text(debug = FALSE, margin = margin(), angle = 90, vjust = y_title_spacing(spacing)),
              axis.ticks.length = grid::unit(0.15, "cm"), 
              axis.ticks.length.x.bottom = grid::unit(0.15, "cm"), 
              axis.ticks.length.x.top = grid::unit(0.15, "cm"),
              axis.ticks.length.y.left = grid::unit(0.15, "cm"), 
              axis.ticks.length.y.right = grid::unit(0.15, "cm"),
              legend.background = element_rect(colour = ifelse(inner, "white", palette$background), fill = ifelse(inner, "white", palette$background)),
              legend.margin = grid::unit(0.2 * spacing, "cm"), 
              legend.key = element_rect(colour = ifelse(inner, "white", palette$background), fill = palette$background), 
              legend.key.size = grid::unit(1.2, "lines"), 
              legend.key.height = NULL, 
              legend.key.width = NULL, 
              legend.text = element_text(debug = FALSE, margin = margin(), size = rel(0.8)),
              legend.position = "right", 
              legend.direction = NULL,
              legend.justification = "center", 
              legend.box = NULL, 
              panel.background = element_rect(fill = palette$background, colour = NA),
              panel.border = element_blank(), 
              panel.grid.major = element_line(linetype = "dashed", colour = palette$gridline), 
              panel.grid.minor = element_blank(), 
              panel.margin = grid::unit(0.5 * spacing, "cm"), 
              panel.margin.x = NULL, 
              panel.margin.y = NULL,
              panel.ontop = FALSE,
              strip.background = element_rect(fill = ifelse(inner, "white", palette$background), colour = NA),
              strip.text.x = element_text(debug = FALSE, margin = margin(), size = rel(1.1), face = "bold"), 
              strip.text.y = element_text(debug = FALSE, margin = margin(), angle = -90, face = "bold", size = rel(1.1)),
              strip.switch.pad.grid = grid::unit(0, "cm"), 
              strip.switch.pad.wrap = grid::unit(0, "cm"), 
              plot.background = element_rect(colour = ifelse(inner, "white", palette$background), fill = ifelse(inner, "white", palette$background)),
              plot.title = element_text(debug = FALSE, margin = margin(0, 0, 6.6, 0), size = rel(1.2), vjust = spacing, face = "bold"), 
              plot.margin = grid::unit(c(0.625, 0.625, 0.625, 0.625) * spacing, "cm"), complete = TRUE)
      g0 <- g0 + 
        scale_fill_manual("legend", values = vl)
    } else {
      g0 <- g0 +
        do.call(paste0("theme_", input$theme), list()) 
    }
    g0 <- g0 +
      theme(#axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        # legend.position = "none",
        plot.background = element_rect(colour = input$background, fill = input$background),
        panel.background = element_rect(fill = input$background),
        
        plot.title = element_text(size = 16, hjust = 1, vjust = 6),
        plot.subtitle = element_text(size = 16, hjust = 1, vjust = 5, color = "grey"),
        plot.caption = element_text(size = 13, hjust = 1, color = "gray"),
        plot.margin = margin(2, 2, 2, 4, "cm"))
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
  
  # output$download <- renderUI({
  #   lb <- i_("download_plot", lang())
  #   dw <- i_("download", lang())
  #   gl <- i_("get_link", lang())
  #   mb <- list(textInput("name", i_("gl_name", lang())),
  #              textInput("description", i_("gl_description", lang())),
  #              selectInput("license", i_("gl_license", lang()), choices = c("CC0", "CC-BY")),
  #              selectizeInput("tags", i_("gl_tags", lang()), choices = list("No tag" = "no-tag"), multiple = TRUE, options = list(plugins= list('remove_button', 'drag_drop'))),
  #              selectizeInput("category", i_("gl_category", lang()), choices = list("No category" = "no-category")))
  #   downloadDsUI("download_data_button", dropdownLabel = lb, text = dw, formats = "png",
  #                display = "dropdown", dropdownWidth = 170, getLinkLabel = gl, modalTitle = gl, modalBody = mb,
  #                modalButtonLabel = i_("gl_save", lang()), modalLinkLabel = i_("gl_url", lang()), modalIframeLabel = i_("gl_iframe", lang()),
  #                modalFormatChoices = c("HTML" = "html", "PNG" = "png"))
  # })
  
  output$download <- renderUI({
    lb <- i_("download_plot", lang())
    dw <- i_("download", lang())
    downloadTableUI("download_data_button", dropdownLabel = lb, text = dw, formats = "gif", display = "dropdown", dropdownWidth = 164)
  })
  
  
  output$result <- renderImage({
    # if (!is.null(brchr$pth)) {
    #   pth <- brchr$pth
    # } else {
    #   pth <- "data/sampleData/prim.gif"
    # }
    # if (input$`initial_data-imageInput` == "sampleData") {
    #   img(src = ls[[plot_lego$dtin()$src]])
    if (input$`initial_data-tableInput` == "sampleData" & input$generate == 0) {
      ls <- list("data/sampleData/emisiones_c02_en.csv" = "data/sampleData/em_en.gif",
                 "data/sampleData/emisiones_c02_es.csv" = "data/sampleData/em_es.gif",
                 "data/sampleData/emisiones_c02_pt.csv" = "data/sampleData/em_pt.gif",
                 "data/sampleData/tennis_grand_slams_en.csv" = "data/sampleData/tn_en.gif",
                 "data/sampleData/tennis_grand_slams_es.csv" = "data/sampleData/tn_es.gif",
                 "data/sampleData/tennis_grand_slams_pt.csv" = "data/sampleData/tn_pt.gif")
      pth <- ls[[input$`initial_data-inputDataSample`]]
    } else {
      pth <- brchr$pth
    }
    list(src = pth, contentType = "image/gif", style = "height: 60vh !important; margin: 3% 0 0 17%; width: 60%;")
  }, deleteFile = FALSE)
  
  observeEvent(list(parmesan_input(), inputData()), {
    session$sendCustomMessage("setButtonState", c("none", "generate_bt"))
  })
  
  output$`download_data_button-DownloadTblgif` <- downloadHandler(filename = function() { 
    session$sendCustomMessage("setButtonState", c("loading", "downloadGif"))
    paste0("bar_chart_race", "-", gsub(" ", "_", substr(as.POSIXct(Sys.time()), 1, 19)), ".gif")
  },
  content = function(file) {
    if (input$`initial_data-tableInput` == "sampleData" & input$generate == 0) {
      ls <- list("data/sampleData/emisiones_c02_en.csv" = "data/sampleData/em_en.gif",
                 "data/sampleData/emisiones_c02_es.csv" = "data/sampleData/em_es.gif",
                 "data/sampleData/emisiones_c02_pt.csv" = "data/sampleData/em_pt.gif",
                 "data/sampleData/tennis_grand_slams_en.csv" = "data/sampleData/tn_en.gif",
                 "data/sampleData/tennis_grand_slams_es.csv" = "data/sampleData/tn_es.gif",
                 "data/sampleData/tennis_grand_slams_pt.csv" = "data/sampleData/tn_pt.gif")
      image_write(image_read(ls[[input$`initial_data-inputDataSample`]]), file)
    } else {
      anim_save(file, brchr$an)
    }
    session$sendCustomMessage("setButtonState", c("done", "downloadGif"))
  })
  
}



shinyApp(ui, server)