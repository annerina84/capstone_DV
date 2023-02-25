library(shiny)
library(shinydashboard)

dashboardPage(
  skin="red",
  
  # ------ Bagian header ------
  dashboardHeader(
    title = "Aktivitas Gempa di Indonesia", titleWidth = 250
  ),
  
  # ----- Bagian Sidebar ------
  dashboardSidebar(width = 250,
                   sidebarMenu(
                     
                     menuItem("Aktifitas Gempa", tabName = "page1", icon = icon("volcano")),
                     
                     menuItem("Analisis Gempa", tabName = "page2", icon = icon("chart-simple")),
                     
                     menuItem("Dataset", tabName = "page3", icon = icon("server"))
                     
                   )
  ),
  
  # ----- Bagian Main / Body ------
  dashboardBody(
    tabItems(
      #-----Page 1
      tabItem(
        tabName="page1",
        h1("Overview Gempa Indonesia Berkekuatan > 6 SR dari Tahun 2009-2022"),
        fluidRow(
          box(
            h3("10 Daerah dengan Frekuensi Gempa Terbanyak dalam Kurun Waktu 2009-2022 "),
            width = 12,
            leafletOutput("peta_gempa")
          )
        ),
        
        fluidRow(
          box(
            width = 6,
            plotlyOutput("plot1_2")
            
          ),
          box(
            width = 6,
            plotlyOutput("plot1_3")
            
          )
        ),
        fluidRow(
          infoBox(
            width = 4,
            title = "Total gempa (2009-2022) ",
            value = nrow(data_cleans),
            color = "red"
              
          ),
          infoBox(
            width = 4,
            title = "Daerah dengan gempa terbanyak",
            value = lokasi_terbanyak2 <- data_cleans %>% 
              group_by(remark) %>% 
              summarise(frekuensi_gempa=length(remark)) %>% 
              ungroup() %>% 
              arrange(-frekuensi_gempa) %>% 
              select(-frekuensi_gempa) %>% 
              head(1),
            color = "red"
          ),
          infoBox(
            width = 4,
            title = "Magnitude terbesar ",
            value =data_cleans2 <- data_cleans %>% 
              arrange(-mag) %>% select(mag) %>% 
              head(1),
            color = "red"
              
          ),
        ),
        fluidRow(
          box(
            width = 12,
            plotlyOutput("plot1_4")
            
          )
        )
      ),
      
      
      #-----Page 2
      tabItem(
        tabName="page2",
        #----Page 2 Row 1
        fluidRow(
          box(
            width = 6,
            selectInput(
              inputId="input_category",
              label="Pilih Area",
              choices = unique(data_cleans$remark),
              selected = "Banda Sea"
            )
            
          ),
          box(
            width = 6,
            sliderInput(
              inputId="input_category2",
              label = "Pilih Tahun",
              min = 2009 ,
              max = 2022 ,
              value = 2019,
              step = 1
            )
          )
        ),
        
        #----Page 2 Row 2
        fluidRow(
          box(
            width = 12,
            plotlyOutput( outputId = "plot2_1")
          )
        ),
        
        #---page 2 row 3
       
        
        # fluidRow(
        #   box(
        #     width = 12,
        #     leafletOutput("peta_gempa2")
        #   )
        # )
        
      ),
      #----Page3
      tabItem(
        tabName="page3",
        fluidRow(
          box(
            width = 12,
            title = "Data Gempa di Indonesia Tahun 2009-2022",
            dataTableOutput(outputId="dataset")
          )
        )
      )
    )
  )
)