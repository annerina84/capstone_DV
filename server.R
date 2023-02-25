shinyServer(function(input, output) {
  
  # ---- kode untuk (peta_gempa) ---
  output$peta_gempa <- renderLeaflet({
    
    map1 <- leaflet()
    map1 <- addTiles(map1)
    map1 <- addMarkers(map = map1 , 
                       data = remark_pos10, 
                       popup = glue("Area : {remark_pos10$remark}, Frekuensi gempa : {remark_pos10$frek}"))
    
    map1
    
  })
  
  
  # ---- Kode untuk Barplot (plot1) ---- 
  output$plot1_2 <- renderPlotly({
    
    # Data Wrangling
    gempa_tahunan2 <- data_cleans %>% 
      group_by(tahun) %>% 
      summarise(frek=length(mag>=6)) %>% 
      ungroup() %>% 
      mutate(label=glue("Banyaknya Gempa : {frek}" ), tahun=as.numeric(as.character(tahun))) 
    
    # Visualization
    plotfrek <-
      ggplot(gempa_tahunan2, aes(x=tahun, y= frek))+
      geom_line(col="red") +
      geom_point(aes(text= label), col="black") +
      scale_x_continuous(breaks = seq(from = 2009, to = 2023, by = 1.0))+
      labs(
        title = "Aktivitas Gempa di Indonesia Periode Tahun 2009-2022",
        x = "Tahun",
        y = "Kejadian Gempa"
      ) +
      theme_minimal()
    
    ggplotly(plotfrek, tooltip = "text")
    
  })
  
  output$plot1_3 <- renderPlotly({
    lokasi_terbanyak <- data_cleans %>% 
      group_by(remark) %>% 
      summarise(frekuensi_gempa=length(remark)) %>% 
      ungroup() %>% 
      arrange(-frekuensi_gempa) %>% 
      mutate(label2 = glue("Banyaknya Gempa : {frekuensi_gempa}")) 
    lokasi_terbanyak10 <- head(lokasi_terbanyak, 10)
    
    # Visualization
    plot_region <- ggplot(data = lokasi_terbanyak10, aes(x = frekuensi_gempa, 
                                                         y = reorder(remark, frekuensi_gempa), text=label2)) + # reorder(A, berdasarkan B)
      geom_col(aes(fill = frekuensi_gempa)) +
      scale_fill_gradient(low="red", high="black") +
      labs(title = "10 Daerah Kejadian Gempa Terbanyak di Indonesia", subtitle = "Kurun Waktu 2009-2022", 
           x = "Frekuensi Terjadinya Gempa",
           y = NULL) +
      theme_minimal() +
      theme(legend.position = "none") 
    
    ggplotly(plot_region, tooltip = "text")
    
  })
  
  output$plot1_4 <- renderPlotly({
    dept_terbanyak <- data_cleans %>% 
      group_by(remark) %>% 
      summarise(mag_avr=mean(mag)) %>% 
      ungroup() %>% 
      arrange(-mag_avr) %>% 
      mutate(label2 = glue("Rata-rata mag : {mag_avr}"))
    dept_terbanyak <- head( dept_terbanyak, 10)
    
    # Visualization
    plot_region <- ggplot(data =  dept_terbanyak, aes(x = mag_avr, 
                                                      y = reorder(remark, mag_avr), text=label2)) + # reorder(A, berdasarkan B)
      
      
      geom_col(aes(fill = mag_avr)) +
      scale_fill_gradient(low="red", high="black") +
      labs(title = "Rata-rata Magnitude Gempa Terbesar di Indonesia", subtitle = "Kurun Waktu 2009-2022",
           x = "Magnitude (SR)",
           y = NULL) +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(plot_region, tooltip = "text")
    
  })
  
  
  output$plot2_1 <- renderPlotly({
    # Data wrangling
    gempa_banda <- data_cleans %>% 
      filter(remark==input$input_category & tahun == input$input_category2) %>% 
      group_by(bulan) %>% 
      summarise(frekuensi_gempa=length(bulan)) %>% 
      ungroup() %>% 
      mutate(label=glue("Banyaknya Gempa : {frekuensi_gempa}" ))
    gempa_banda$bulan <- as.numeric(gempa_banda$bulan)
    # Visualization
    
    plot4 <- ggplot(gempa_banda, aes(x=bulan, y=frekuensi_gempa))+
      geom_line(col="red") +
      geom_point(aes(), col="black") +
      scale_x_continuous(breaks = seq(from = 1, to = 12, by = 1.0))+
      labs(
        title = glue("Frekuensi Gempa di {input$input_category} pada Tahun {input$input_category2}"),
        x = "Bulan ke-",
        y = "Kejadian Gempa"
      ) +
      theme_minimal()
    
    ggplotly(plot4, tooltip = "text")
  })
  
  #----page 2 tampilan peta----------
  
  
  # gempa_hari <- gempa_banda_hari %>% 
  #     filter( gempa_banda_hari$tgl == input$input_category3)
  #    
  #   
  # output$peta_gempa2 <- renderLeaflet({
  #   date
  #   map2 <- leaflet()
  #   map2 <- addTiles(map)
  #   map2 <- addMarkers(map = map2 , 
  #                      data = gempa_hari, 
  #                      popup = glue("Daerah : {gempa_hari$remark}, Mag : {gempa_hari$mag}, Kedalaman: {gempa_banda_hari$depth}"))
  #   map2
  # })
  
  
  
  
  #---page 3-----
  #---Optional code untuk menampilkan dataset tabel
  output$dataset <- renderDataTable(
    data_cleans,
    options = list(scrollX=TRUE,
                   scrollY=TRUE)
  )
  
})



