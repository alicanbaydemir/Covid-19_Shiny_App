# Press Ctrl + a and Ctrl + Enter 


library(dplyr)
library(tidyr)
library(magrittr)
library(tidyverse)
library(plotly)
library(ggplot2)
library(DT)



# verinin düzenlenmesi ----------------------------------------------------

# vaka sayılarının bulunduğu veri setinin okutulması

confirmed_global <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
# geniş formattan uzun formata çevirme
long_confirmed<- gather(confirmed_global, Date, num_of_confirmed, 5:ncol(confirmed_global), convert =TRUE)
# tarih değişkenin düzeltilmesi
long_confirmed$Date<- as.Date(long_confirmed$Date,format='%m/%d/%y')



# vefat sayılarının bulunduğu veri setinin okutulması düzenlenmesi

deaths_global <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")

long_deaths<- gather(deaths_global, Date, num_of_dead, 5:ncol(deaths_global), convert =TRUE)

long_deaths$Date <- as.Date(long_deaths$Date,format='%m/%d/%y') 



# iyileşen sayılarının bulunduğu veri setinin okutulması düzenlenmesi

recovered_global <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")

long_recovered<- gather(recovered_global, Date, num_of_recovered, 5:ncol(recovered_global), convert =TRUE)

long_recovered$Date <- as.Date(long_recovered$Date,format='%m/%d/%y') 




# dataframeleri birleştirme 
data_all <- long_confirmed %>% merge(long_deaths, all=T) %>% merge(long_recovered, all=T)

# enlem ve boylam koordinatları çıkarıldı
data_all <- data_all %>% select(-c( Lat, Long)) 



# birleştirme sonucunda NA değerler oluştu bunun sebebi üç veri setindei gözlem saylarnn eşit olmaması
# bir ülke için ölüm sayısı ölçülmüş fakat kurtarılan hasta sayısı ölçülmemiş olabilir

# dünya sayılarının hesaplanması- tüm ülkelerin tarihe göre toplamları
World_daily<- data_all %>% group_by(Date) %>% summarize("Country/Region"="World",
                                                        num_of_confirmed = sum(num_of_confirmed, na.rm = TRUE),
                                                        num_of_dead = sum(num_of_dead, na.rm = TRUE),
                                                        num_of_recovered = sum(num_of_recovered, na.rm = TRUE))


# dünya sayılarının ana veriye eklenmesi
all_daily <- bind_rows(data_all,World_daily)


# aktif vaka sayısının hesaplanması
all_daily%<>% mutate(active_confirmed=num_of_confirmed-num_of_dead-num_of_recovered) 

# veriyi her bir ülkenin tarihe göre sıralama 
all_daily %<>% arrange( "Country/Region", Date) 



# yeni vaka vefat iyileşen sayılarının hesaplanması 
first_day <- min(all_daily$Date)

all_daily %<>%
    mutate(new_confirmed= ifelse(Date == first_day, NA, num_of_confirmed - lag(num_of_confirmed, n=1)),
           new_dead= ifelse(Date == first_day, NA, num_of_dead - lag(num_of_dead, n=1)),
           new_recovered= ifelse(Date == first_day, NA, num_of_recovered - lag(num_of_recovered, n=1))
    )


# mortalite oranlarının hesaplanması
all_daily %<>%
    mutate(top_mortality_rate=100*(num_of_dead)/(num_of_dead+num_of_recovered)) %>%
    mutate(low_mortality_rate=100*num_of_dead/num_of_confirmed) %>%
    mutate(daily_mortality_rate=100*(new_dead)/(new_dead+new_recovered))



# veri okuma
MyData <- all_daily


# ülke ve bölge isimlerini birleştirip tek değişken yaptık
MyData <-MyData%>% 
    unite(Country, "Country/Region", "Province/State", sep = " " ,na.rm = TRUE,remove = T)


# sütun isimlerini aldık Checkbox ta kullanmak için
column_names<- as.list(colnames(MyData))

# ülke isimlerini liste yaptık ki select inputta kullanalım
Country_names <- as.list(levels(as.factor(MyData$Country)))


# ilk ve son gün aldık Date_range için
first_day <- min(MyData$Date)
last_day <- max(MyData$Date)




# shiny uygulaması --------------------------------------------------------


if (interactive()) {
    
    library(shiny)
    
    
    ui <- fluidPage(
        
        
        titlePanel(h1("Covid-19 Shiny App", align = "center")),
        
        sidebarLayout(
            
            sidebarPanel (
                
                # ülke seçtirme
                selectInput(inputId = "Countryslct", label = "Select Countries", 
                            choices = Country_names, multiple = T, selected = "Turkey"),
                
                
                # oran mı sayı mı seçtirme
                radioButtons(inputId = "radiobutton", label = "Choose Number or Rate",
                             choices = list("Covid-19 number" = 1, "Covid-19 mortality rate" = 2),
                             selected = 1),
                
                # sütun seçtirme radio buton a göre güncelleniyor
                checkboxGroupInput(inputId ="checkGroup",label = "First Choose above"),
                
                # tarih aralık seçtirme serverdan çekiyoruz
                uiOutput("dateRange"),
                
                
                checkboxInput(inputId = "scale", label = "Log Scale"),
                
                helpText("Note: Line colors may change as selections change!!!")
            ),
            
            mainPanel(
                
                #  sekmeler
                tabsetPanel( type = "tab",
                             
                             # ilk sekme plotly grafiği
                             tabPanel(title = "Interactive Graphic", plotlyOutput("plotlygraph"), icon = icon("chart-line")),
                             
                             # ikinci sekme interaktif tablo
                             tabPanel(title = "Data", dataTableOutput("table"), icon = icon("table")),
                             
                             tabPanel(title = "Data Summary", verbatimTextOutput("Summary"), icon = icon("eye")),
                             
                             tabPanel(title = "Data Structer", verbatimTextOutput("Structer"), icon = icon("code"))
                             
                             
                )
                
            )
        )
    )
    
    server <- function(input, output, session) {
        
        # veriyi inputlara göre filtreleme outputlarda kullanmak için
        dat <- reactive({
            dat <- MyData %>%
                filter(Country == input$Countryslct & Date >= input$Date[1] & Date <= input$Date[2]) %>%
                select(Country, Date, input$checkGroup)
            dat
        })
        
        # seçilen radyo buton a göre checkbox ları update etme
        observe({
            
            updateCheckboxGroupInput(session, "checkGroup",
                                     label = "Choose Columns",
                                     
                                     
                                     if (input$radiobutton==2) {
                                         
                                         chocies =c("Top Mortality Rate" = column_names[10],
                                                    "Low Mortality Rate" = column_names[11],
                                                    "Daily Mortality Rate" = column_names[12])
                                         
                                         
                                         
                                     } else { 
                                         choices = c("Number of Confirmed" = column_names[3],
                                                     "Number of Dead" = column_names[4],
                                                     "Number of Recovered" = column_names[5],
                                                     "Active Confirmed" = column_names[6],
                                                     "New Confirmed" = column_names[7],
                                                     "New Dead" = column_names[8],
                                                     "New Recovered" = column_names[9])
                                         
                                     },
                                     
                                     if (input$radiobutton==2) selected = column_names[10]
                                     else selected = column_names[3]
                                     
            )
            
        })
        
        # date range i grafiklerde kullanmak için serverdan çekiyoruz
        output$dateRange <- renderUI({
            
            dateRangeInput(inputId = "Date", label = "Date range", 
                           min = first_day, max = last_day,
                           start = first_day, end = last_day )
        })
        
        ### summary sekmesi
        output$Summary <- renderPrint({
            
            summary(dat() )
            
            
        })
        
        ### structer sekmesi
        output$Structer <- renderPrint({
            
            str(dat() )
            
            
        })
        
        ### data sekmesinin verileri seçilen tarih aralığı ve checkbox taki sütunlar ile belirleniyor
        output$table <-  renderDataTable({
            
            dat()
            
        })
        
        ### seçilen değerlere göre grafik çizdirme
        output$plotlygraph <- renderPlotly({
            
            
            
            # grafik tabanı
            p <- plot_ly( mode = "lines") %>% layout(title = list(text = paste0(format(input$Date[2],'%d/%m/%Y')), x= 0.1, y=0.98),
                                                     yaxis=list(title = " "),
                                                     hovermode = "x unified")# bu çok kulanışlı mause un gezdiği yerdeki y değerlerini gösteriyor
            
            # eklenen ülke ve seçilen değişkenler için line çizdirme 
            for(k in c(1:length(input$Countryslct))){
                
                for(i in c(1:length(input$checkGroup))){
                    
                    p <- p%>%add_trace(
                        
                        # veriyi her eklemede güncelliyoruz
                        data=dat() %>%
                            filter(Country == input$Countryslct[k] ),
                        
                        x=~Date,
                        y=as.formula(paste0("~`", input$checkGroup[i],"`")),# checkbox tan geriye string dönuyor ggplot olsa aes_string yapabilirdik ama 
                        # burası için çözüm bu oldu
                        
                        name=paste(input$Countryslct[k],input$checkGroup[i])) # line isimleri
                    
                }
            }
            
            # logaritma scale
            if(input$scale==1){
                p <- p%>%  layout(yaxis = list(type = "log"))
            }else  p <- p%>%  layout(yaxis = list(type = "linear"))
            
            
            p # grafik tamam
            
            
        })
        
        
    }
    
    shinyApp(ui, server)
}

