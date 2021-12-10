#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggridges)
library(DT)
source(here::here("R", "scales.R")) #library(colorblindr)
library(shinydashboardPlus)

dat <- readRDS(here::here("data", "data.rds"))
wwmx_dir <- here::here("data", "wwmx")
lemmas <- dir(wwmx_dir)

shinyServer(function(input, output, session) {
  
  medoidname <- reactive(names(dat[[input$lemma]]$medoidCoords)[[as.integer(input$medoid)]])
  
  output$medoid <- renderUI({
    a(medoidname(),
      href=glue::glue("https://qlvl.github.io/NephoVis/level3.html?type={input$lemma}&model={medoidname()}"),
      style="color:white;")
  })
  
  medoid <- reactive(dat[[input$lemma]]$medoidCoords[[medoidname()]])
  
  observeEvent(medoid(), {
    if (length(unique(medoid()$coords$cluster)) == 1 & input$noise) {
      ask_confirmation(
        "allnoise",
        title = "All the tokens are noise!",
        text = "Would you like to stop ignoring noise?",
        btn_labels = c("Show noise", "Keep ignoring")
      )
    }
  })
  observeEvent(input$allnoise, {
    updateMaterialSwitch(session, "noise", value = input$allnoise)
  })
  
  observeEvent(input$previous, {
    nmedoid <- as.integer(input$medoid)
    if (nmedoid == 1) {
      current_lemma <- which(lemmas == input$lemma)
      if (current_lemma != 1) {
        updateSelectInput("lemma", session = session, selected = lemmas[[current_lemma-1]])
        updateSelectInput("medoid", session = session, selected = 8)
      }
    } else {
      updateSelectInput("medoid", session = session, selected = nmedoid - 1)
    }
  })
  
  observeEvent(input$nextmodel, {
    nmedoid <- as.integer(input$medoid)
    if (nmedoid == 8) {
      current_lemma <- which(lemmas == input$lemma)
      if (current_lemma != length(lemmas)) {
        updateSelectInput("lemma", session = session, selected = lemmas[[current_lemma+1]])
        updateSelectInput("medoid", session = session, selected = 1)
      }
    } else {
      updateSelectInput("medoid", session = session, selected = nmedoid + 1)
    }
  })
  # Load distance matrix
  focdists <- reactive(focdists_from_csv(here::here("data", "wwmx", input$lemma), paste0(medoidname(), ".wwmx.dist.csv")))
  
  # Select relevant context words ----
  relevantcws <- reactive({
    pr_filter <- parse(text = glue::glue("recall >= {input$recall} {if (input$operator) '&' else '|'} precision >= {input$precision}"))
    d <- filter(medoid()$cws, TP > input$minfreq, eval(pr_filter)) %>%
      arrange(desc(Fscore)) %>% 
      group_by(cw) %>% mutate(reps = seq(n()))
  })
  
  # If a word is "relevant" for more than one cluster choose highest Fscore
  relevantcws_filtered <- reactive({
    relevantcws() %>% 
      filter(reps == 1) %>% 
      select(-reps)
  })
  
  # Plot tokens ----
  output$tokens <- renderPlotly({
    
    g <- medoid()$coords %>% 
      mutate(
        alpha = map2_dbl(cws, eps, function(cwlist, mp) {
          if (is.null(cor_click())) mp else as.numeric(cor_click() %in% cwlist)
        }),
        cws = map2_chr(cws, cluster, format_cws, relevantcws_filtered()),
        cluster = if_else(cluster == "0", NA_character_, as.character(cluster)),
        cluster = fct_reorder(cluster, as.numeric(cluster))
      ) %>%
      ggplot() +
      geom_point(aes(x = model.x, y = model.y, color = cluster, alpha = alpha,
                     text = cws)) +
      theme_void() +
      scale_color_OkabeIto(darken = 0.1, use_black = TRUE, na.value = "#9b9c9f") +
      coord_fixed()
    if (!is.null(cor_click())) {
      g <- g + scale_alpha(range = c(0.25, 1))
      # } else if (!input$alpha) {
      #     g <- g + scale_alpha_identity()
    } else {
      g <- g + scale_alpha(range = c(1, 0))
    }
    ggplotly(g, tooltip="text")
  })
  
  output$contexts <- renderDT({
    dt <- medoid()$coords %>% select(ctxt, cluster
                                     # , sense
                                     , cws) %>% 
      mutate(
        # sense = str_remove(sense, paste0(input$lemma, "_")),
        cluster = if_else(cluster == "0", "noise", as.character(cluster)))
    if (!is.null(cor_click())) {
      dt <- dt %>% mutate(selected = map_lgl(cws, ~cor_click() %in% .x)) %>% filter(selected) %>% 
        select(-selected)
    } else {
      only_noise <- length(unique(dt$cluster)) == 1
      dt <- dt %>% filter(!(cluster == "noise" & input$noise & !only_noise))
    }
    available_clusters <- sort(as.numeric(unique(dt$cluster)), na.last = TRUE)
    cluster_colors <- paste0(palette_OkabeIto_black[available_clusters], "66")
    if ("noise" %in% dt$cluster) {
      cluster_colors[length(cluster_colors)] <- '#9b9c9f66'
    }
    cws_col <- which(colnames(dt) == 'cws')
    dt %>% rename(Context = ctxt) %>% 
      mutate(cws = map_chr(cws, paste, collapse = "; ")) %>%
      datatable(
        escape = F, filter = "top",
        options = list(
          pageLength = 4,
          columnDefs = list(
            list(visible = FALSE, searchable = TRUE, targets = c(cws_col)),
            list(searchable = TRUE, targets = seq(cws_col-1))
          )
        )
      ) %>% 
      formatStyle(
        "cluster",
        target="row",
        backgroundColor = styleEqual(available_clusters, cluster_colors))
  })
  
  # Plot context words ----
  output$focs <- renderPlotly({
    
    clustermapping <- select(relevantcws_filtered(), cw, cluster) %>%
      deframe
    
    # From distances to coordinates
    ncluster <- medoid()$coords %>% pull(cluster) %>% as.character() %>% as.numeric() %>% max
    tsne <- medoid()$cws %>% 
      filter(!is.na(x), !is.na(y)) %>% 
      mutate(is_relevant = map_lgl(cw, ~.x %in% relevantcws_filtered()$cw),
             cluster = map2_chr(cw, is_relevant, function(c, b) {
               if (b) clustermapping[[c]] else NA_character_
             }),
             text = map2_chr(cw, is_relevant, info, cwinfo = relevantcws_filtered(), focdists = focdists()),
             cluster = map2_chr(cluster, is_relevant, function(clus, isr) {
               if (isr & clus != "0" & as.integer(clus) <= length(palette_OkabeIto_black)) {
                 palette_OkabeIto_black[[as.integer(clus)]]
               } else if (!isr | (input$noise & clus == "0") ) {
                 "#9b9c9f26"
               } else {
                 "#9b9c9f"
               }
             })
             # cluster = if_else(!is.na(cluster) & cluster == "0", as.character(min(ncluster+1, 9)), cluster)
      )
    cluster_order <- sort(as.numeric(unique(tsne$cluster)))
    
    g <- ggplot(tsne) +
      geom_text(aes(x = x, y = y, label = cw,
                    # alpha = is_relevant,
                    color = cluster,
                    text = text)) +
      theme_void() +
      scale_color_identity() +
      # scale_color_OkabeIto(na.value = "#9b9c9f", order = cluster_order, use_black = TRUE) +
      coord_fixed()
    ggplotly(g, tooltip = "text") %>% 
      hide_legend()
  })
  
  observeEvent(medoid(), {
    if (max(medoid()$cws$x, na.rm=T) == 0) {
      show_alert(
        title = "No type-level t-SNE available!",
        text = tags$span(
          sprintf("There are only %d context words, which is too low for perplexity of 30.",
                  nrow(medoid()$cws)),
          br(),
          "But don't panic, the rest of the app will work!"
        ),
        type = "warning"
      )
      if (!input$focs_box$collapsed) {
        updateBox("focs_box", action = "toggle", options =list(collapsed = TRUE))    
      }
      
    } else {
      if (input$focs_box$collapsed) {
        updateBox("focs_box", action = "toggle", options =list(collapsed = FALSE))    
      }
    }
  })
  
  output$cor <- renderPlotly({
    d <- medoid()$cws %>% 
      filter(cluster != "0" | !input$noise) %>% 
      mutate(cluster = if_else(cluster == "0", NA_character_, cluster),
             cluster = fct_reorder(cluster, as.numeric(cluster)))
    g <- ggplot(data = d, aes(x = recall, y = precision, color = cluster,
                              size = TP, customdata = cw, text = glue::glue("<b>{cw}</b><br>F: {TP}"))) +
      geom_point(alpha = 0.7) +
      geom_hline(yintercept = input$precision, size = 0.1) +
      geom_vline(xintercept = input$recall, size = 0.1) +
      scale_color_OkabeIto(na.value = "#9b9c9f", use_black = TRUE, order = sort(unique(as.numeric(d$cluster)))) +
      theme_bw()
    ggplotly(g, source = "cor", tooltip = "text")
  })
  
  cor_click <- reactive({
    
    d <- event_data("plotly_click", source = "cor")
    if (is.null(d)) NULL else d$customdata
  })
  
  # Plot heatmap of distances ----
  
  output$heatmap_or <- renderPlotly({
    tooltip_template <- "<b>x:</b> <em>%s</em><br><b>y:</b> <em>%s</em>; <b>distance: </b> %s"
    
    g <- matrix2heatmap(relevantcws_filtered(), input$noise, focdists()) %>% 
      mutate(distance = orig_distance, text = sprintf(tooltip_template, x, y, round(distance, 2))) %>% 
      ggplot(aes(x = x_label, y = y_label, fill = distance, text = text)) +
      geom_tile() +
      labs(x = "", y = "", fill = "Cosine") 
    
    ggplotly(g, tooltip = "text", height = 500) %>% 
      layout(xaxis = list(tickangle = 90))
  })
  
  # Membership probabilities  // EPS ----
  
  output$eps <- renderPlot({
    medoid()$coords %>% 
      # ggplot(aes(x = sense, y = eps, fill = sense, color = sense)) +
      ggplot(aes(x = cluster, y = eps)) + 
      geom_violin(alpha = 0.3) +
      geom_jitter(size = 3, height = 0) +
      facet_grid(~cluster, scales = "free_x", labeller = labeller(cluster = label_both)) +
      theme_bw() +
      theme(axis.text.y = element_text(size = 18),
            axis.text.x = element_text(size = 16, angle = 45, vjust = 1, hjust = 1),
            axis.title = element_text(size = 15),
            strip.text = element_text(size = 20),
            legend.position = "none")
    
  })
  output$mp <- renderPlot({
    medoid()$coords %>%
      filter(cluster != "0") %>%
      ggplot(aes(x = eps, y = cluster
                 # , fill = sense, color = sense
                 )) +
      geom_density_ridges(
        # aes(point_color = sense, point_fill = sense),
        alpha = 0.3, point_alpha = 1, jittered_points = TRUE, scale = 0.9) +
      theme_ridges() +
      labs(y = "Cluster", x = "Membership probability") +
      xlim(c(0, 1))
  })
  
  output$bars <- renderPlot({
    imap_dfr(dat[[input$lemma]]$medoidCoords, function(medoidmodel, medoidname) {
      mutate(medoidmodel$coords, isnoise = cluster == "0") %>% 
        count(isnoise) %>% 
        mutate(medoid = as.character(which(names(dat[[input$lemma]]$medoidCoords) == medoidname)))
    }) %>% 
      mutate(isnoise = if_else(isnoise, "Noise", "Actual clusters")) %>%
      ggplot() +
      geom_col(aes(x = medoid, y = n, fill = isnoise)) +
      labs(x = "Medoid number", y = "Number of tokens", fill = "HDBSCAN noise") +
      scale_fill_manual(values = c("gray60", "gray30")) +
      theme_bw() +
      theme(axis.text = element_text(size = 20),
            axis.title = element_text(size = 15),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 15))
  })
  
  # Report double relevance or missing plot ----
  output$splitrelevance <- renderUI({
    if (nrow(relevantcws_filtered()) < nrow(relevantcws())) {
      repeated <- relevantcws() %>% filter(reps > 1) %>% 
        pull(cw) %>% unique()
      desc <- map(repeated, function(r) {
        rrow <- relevantcws() %>% filter(cw == r) %>% 
          mutate(
            desc = glue::glue("cluster {cluster} with F: {TP}, R: {round(recall, 2)} and P: {round(precision, 2)}")
          ) %>% 
          pull(desc) %>% map(tags$li)
        div(span(em(r), "for:"), tags$ul(rrow))
      }) %>% div()
      div(
        p("Some context words are 'relevant' for more than one cluster:"),
        desc)
    } else {
      p("All relevant context words are only relevant for one cluster.")
    }
  })
  
  
  # Extra info
  output$tokendata <- renderMenu({
    surviving_tokens <- nrow(medoid()$coords)
    total_tokens <- nrow(dat[[input$lemma]]$senses)
    noise_tokens <- nrow(filter(medoid()$coords, cluster == "0"))
    n_FOC <- nrow(medoid()$cws)
    
    dropdownMenu(
      headerText = "Some more info",
      type = "notifications", badgeStatus = "success",
      icon = icon("info-circle"),
      notificationItem(text = glue::glue("{surviving_tokens} of {total_tokens} tokens plotted."),
                       icon = icon("cloud"),status = "primary"),
      notificationItem(text = glue::glue("{noise_tokens} tokens are noise."),
                       icon = icon("ban"),status = "danger"),
      notificationItem(text = glue::glue("{n_FOC} context words plotted."),
                       icon = icon("cloud"),status = "success")
    )
  })
})
