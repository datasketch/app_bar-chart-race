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

ui <- panelsPage(useShi18ny(),
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
                                     uiOutput("neg"),
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
                                                     # dsmodules::downloadHtmlwidgetUI("download_data_button", "Download"))),
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
  
  # gif_path <- tempfile(fileext = ".gif")
  brchr <- reactiveValues(pth = NULL)
  
  path <- "parmesan"
  parmesan <- parmesan_load(path)
  parmesan_input <- parmesan_watch(input, parmesan)
  parmesan_alert(parmesan, env = environment())
  parmesan_lang <- reactive({i_(parmesan, lang(), keys = c("label", "choices"))})
  output_parmesan("controls", 
                  parmesan = parmesan_lang,
                  input = input,
                  output = output, 
                  env = environment())
  
  # output$generate_bt <- renderUI({
  output$neg <- renderUI({
    gn <- i_("generate", lang())
    actionButton("generate", gn, style = "margin: 0;")
  }) 
  
  output$modal <- renderUI({
    dw <- i_("download", lang())
    # pner dowloader de gifs...
    # dsmodules::downloadHtmlwidgetUI("download_data_button", pastedw))),
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
  
  inputData <- eventReactive(labels(), {
    do.call(callModule, c(tableInput, "initial_data", labels()))
  })
  # callModule(tableInput, 
  #                       "initial_data",
  #                       sampleFile = list("Emission per capita C02" = "data/sampleData/emisiones_c02.csv", 
  #                                         "Tennis grand slams" = "data/sampleData/tennis_grand_slams.csv"))
  
  
  output$data_preview <- renderUI({
    req(inputData())
    suppressWarnings(hotr("hotr_input", data = inputData(), order = NULL, options = list(height = 470), enableCTypes = FALSE))
    # print(input$ids)
    # suppressWarnings(hotr("hotr_input", data = inputData(), order = c(input$ids, input$states, input$values), options = list(height = 470), enableCTypes = FALSE))
  })
  
  observe({
    assign("dt0", input$hotr_input, envir = globalenv())
    #   c(input$ids, input$states, input$values)
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
    # session$sendCustomMessage("setButtonState", c("loading", "generate_bt"))
    session$sendCustomMessage("setButtonState", c("loading", "neg"))
    
    # session$sendCustomMessage("setButtonState", c("loading", "generate"))
    print("w")
    assign("dt1", dt_ready(), envir = globalenv())
    vl <- c("#3DB26F", "#FECA84", "#74D1F7", "#F75E64", "#8097A4", "#B70F7F", "#5D6AE9", "#53255E", "#BDCAD1")[seq_along(unique(dt1$a))]
    names(vl) <- unique(dt1$a)
    cl <- ifelse(input$text_show, "black", "transparent")
    # g0 <- ggplot(dt1, aes(x = rk, y = c, fill = a)) +
    print(input$title)
    assign("t0", input$title, envir = globalenv())
    tl <- gsub("\\n", "\n", input$title, fixed = TRUE)
    print(tl)
    g0 <- ggplot(dt_ready(), aes(x = rk, y = c, fill = a)) +
      geom_bar(stat = "identity") +
      geom_bar(stat = "identity") +
      geom_text(aes(y = 0, label = a), vjust = 0.2, hjust = 1.1, size = 4) +
      geom_text(aes(y = c, label = paste0(input$prefix, c, input$suffix), hjust = -0.1), size = 4, color = cl) +
      coord_flip(clip = "off", expand = FALSE) +
      labs(title = tl, subtitle = paste0(input$states, ": {closest_state}"), caption = input$caption) +
      # geom_text(aes(x = 1, y = 18.75, label = "Month: {closest_state}")) +
      # scale_y_continuous(labels = scales::comma) +
      # scale_x_reverse() +
      scale_fill_manual("legend", values = vl) +
      guides(color = FALSE, fill = FALSE) +
      do.call(paste0("theme_", input$theme), list()) +
      # ggmagic::theme_ds() +
      theme(#axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            # legend.position = "none",
            plot.title = element_text(size = 16, hjust = 1, vjust = 6),
            plot.subtitle = element_text(size = 16, hjust = 1, vjust = 5, color = "grey"),
            plot.caption = element_text(size = 13, hjust = 1, color = "gray"),
            plot.background = element_rect(colour = input$background, fill = input$background),
            panel.background = element_rect(fill = input$background),
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
    g1 <- animate(g0,  nframes = input$nframes, fps = input$fps, duration = input$duration, rewind = FALSE)
    gif_path <- tempfile(fileext = ".gif")
    assign("g1", g1, envir = globalenv())
    anim_save(gif_path, g1)
    session$sendCustomMessage("setButtonState", c("done", "neg"))
    brchr$pth <- gif_path
  })
  
  # observeEvent(input$generate, {
  #   # session$sendCustomMessage("setButtonState", c("none", buttonId)) 
  #   session$sendCustomMessage("setButtonState", c("loading", "generate"))
  #   
  #   
  #   # output$result <- renderImage({
  #     # req(dt_ready())
  #     
  #     # ggplot(dt1, aes(x = rk, y = c, fill = a)) +
  #     g0 <- ggplot(dt_ready(), aes(x = rk, y = c, fill = a)) +
  #       geom_bar(stat = "identity") +
  #       geom_text(aes(y = 0, label = a), vjust = 0.2, hjust = 1) +
  #       geom_text(aes(y = c, label = c, hjust = 0)) +
  #       coord_flip(clip = "off", expand = FALSE) +
  #       labs(title = paste0(input$states,": {closest_state}")) +
  #       # geom_text(aes(x = 1, y = 18.75, label = "Month: {closest_state}")) +
  #       # scale_y_continuous(labels = scales::comma) +
  #       # scale_x_reverse() +
  #       guides(color = FALSE, fill = FALSE) +
  #       theme(axis.line = element_blank(),
  #             axis.text.x = element_blank(),
  #             axis.text.y = element_blank(),
  #             axis.ticks = element_blank(),
  #             axis.title.x = element_blank(),
  #             axis.title.y = element_blank(),
  #             # legend.position = "none",
  #             panel.background = element_blank(),
  #             # panel.border=element_blank(),
  #             # panel.grid.major=element_blank(),
  #             # panel.grid.minor=element_blank(),
  #             panel.grid.major.x = element_line(size = .1, color = "grey"),
  #             panel.grid.minor.x = element_line(size = .1, color = "grey"),
  #             # plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
  #             # plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
  #             # plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
  #             # plot.background=element_blank(),
  #             plot.margin = margin(2, 2, 2, 4, "cm")) +
  #       
  #       # gganimate specific bits:
  #       transition_states(b, transition_length = 4, state_length = 1) +
  #       # view_follow() +
  #       # view_follow(fixed_x = TRUE)
  #       ease_aes('sine-in-out')
  #     
  #     g1 <- animate(g0,  nframes = 10, fps = 7, rewind = FALSE)
  #     
  #     # mr <- strsplit(input$marks_viz, "&")[[1]]
  #     # 
  #     # b0 <- ggplot(aes(ordering, # orden
  #     #                  group = a), # id, nombre
  #     #              data = dt()) + # datos
  #     #   geom_tile(aes(y = c / 2,
  #     #                 height = c, # valor
  #     #                 width = 0.9), 
  #     #             # fill = ), # categoría color
  #     #             alpha = 0.9) +
  #     #   scale_fill_manual(values = c('#F8AFA8','#74A089')) +
  #     #   geom_text(aes(y = c, label = a), family = 'Quicksand', nudge_y = -2, size = 3, 
  #     #             color = ifelse(input$text_show_viz, "black", "transparent")) +
  #     #   #convert to character to get rid of blurry trailing decimals
  #     #   geom_text(aes(y = c, label = paste0(input$prefix_viz, 
  #     #                                       format(c, 
  #     #                                              big.mark = mr[1], 
  #     #                                              decimal.mark = mr[2], 
  #     #                                              digits = input$nDigits_viz,
  #     #                                              nsmall = input$nDigits_viz), 
  #     #                                       input$suffix_viz)), family = 'Quicksand', nudge_y = 0.5,
  #     #             color = ifelse(input$text_show_viz, "black", "transparent")) +
  #     #   geom_text(aes(x = 1, y = 18.75, label = paste0(b)), family = 'Quicksand', size = 8, color = 'gray45',
  #     #             color = ifelse(input$text_show_viz, "black", "transparent")) +
  #     #   coord_cartesian(clip = "off", expand = FALSE)
  #     # 
  #     # if (input$orientation_viz == "hor") {
  #     #   b0 <- b0 +
  #     #     coord_flip()
  #     # }
  #     # 
  #     # b0 <- b0 +
  #     #   labs(title = input$title_viz,
  #     #        subtitle = input$subtitle_viz,
  #     #        caption = input$caption_viz,
  #     #        x = input$horLabel_viz,
  #     #        y = input$verLabel_viz) +
  #     #   theme(rect = element_rect(fill = input$background_viz),
  #     #         plot.background = element_rect(fill = input$background_viz, color = NA),
  #     #         panel.background = element_rect(fill = input$background_viz, color = NA)) +
  #     #   transition_states(frame_id) +
  #     #   # transition_length = 4, state_length = 3) +
  #     #   ease_aes('cubic-in-out')
  #     # 
  #     # b0 <- animate(b0, 
  #     #               nframes = input$nframes_viz, 
  #     #               duration = input$duration_viz, 
  #     #               fps = input$fps_viz, 
  #     #               end_pause = 50, 
  #     #               width = input$width_viz, 
  #     #               height = input$height_viz)
  #     anim_save(gif_path, g1)
  #     # # 
  #     session$sendCustomMessage("setButtonState", c("done", "generate"))
  #     # list(src = gif_path,
  #     #      contentType = "image/gif")
  #     # a0 <- animate(barplot_race_blur, nframes = 176) # number of years
  #     # anim_save("gran-slam.gif", a0)
  #     # animate(b0, nframes = 100, fps = 25, end_pause = 50, width = 1200, height = 900)
  #     
  #     
  #   # }, deleteFile = FALSE)
  
  # })
  
  # observe({
  #   print("dasdfs")
  #   print(nchar(brchr()))
  # })
  
  # observeEvent(brchr(), {
  output$result <- renderImage({
    if (!is.null(brchr$pth)) {
      pth <- brchr$pth
    } else {
      pth <- "data/sampleData/prim.gif"
    }
    list(src = pth, contentType = "image/gif")
    
    # anim_save(gif_path, brchr())
    # list(src = gif_path,
  }, deleteFile = FALSE)
  # })
  
  
  observeEvent(list(parmesan_input(), inputData()), {
    session$sendCustomMessage("setButtonState", c("none", "generate_bt"))
  })
  
  
  
  
  
}
ggmagic::gg_bar_CatCatNum

ggmagic::theme_ds

shinyApp(ui, server)