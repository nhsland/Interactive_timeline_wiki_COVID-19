
library("ggrepel")
library(shiny)
library(ggplot2)
library(ggthemes)
library(plotly)
library(dplyr)
library(lubridate)

dfcr=read.csv("dfcr_annot3_doi_filtered.csv",sep=";")
dfcr$tsc=dmy(gsub("/","-",dfcr$tsc))


dfcr$wiki=paste("http://en.wikipedia.org/?curid=",dfcr$pageid,sep="")

dfcr$label=paste('<a href="',dfcr$wiki,'">',dfcr$art,'</a>',sep="")

#pdf("timeline_HMC_not_colored_types_2.pdf",height=10,width=20)

# ggplot(dfcr,aes(x=tsc,y=0,fill=type))+
#   geom_point(aes(colour=type))+
#   geom_label_repel(aes(label =art),nudge_y= 0.5, #fill = factor(type)
#                    direction = "y",
#                    angle        = 0,
#                    vjust        = 0,segment.alpha =0.2,
#                    size=3,segment.size = .5)+
#   scale_x_date()+theme_minimal()+
#   ylim(0,0.5)+scale_colour_brewer("type", palette="Dark2")+
#   scale_fill_brewer("type", palette="Dark2")+
#   theme(legend.position = "bottom")+facet_wrap(~type,ncol=1)

#dev.off()
#write.table(dfcr,"wiki_articles_nodes_info.csv",quote=F,row.names=F,sep=";")


#############

library(shiny)
library(timevis)

color_pal=c("#fbb4ae",
  "#b3cde3",
  "#ccebc5",
  "#decbe4",
  "#fed9a6",
  "#ffffcc")

data <- data.frame(
  id      = 1:dim(dfcr)[1],
  content = dfcr$label,#dfcr$art,
  start   = dfcr$tsc,
  end     =rep(NA,dim(dfcr)[1]),
  group = as.numeric(dfcr$type),
  style = paste("background-color: ",color_pal[as.numeric(dfcr$type)],";")) #,
 # groups = data.frame(id = 1:6, content = levels(unique(dfcr$type)))

#timevis(data, groups = data.frame(id = 1:6, content = levels(unique(dfcr$type))),zoomFactor=0.1)%>%setWindow("2001-01-01", Sys.Date()) %>%
 # setGroups(data.frame(id = 1:6, content = levels(unique(dfcr$type))))

ui <- fluidPage(
  timevisOutput("timeline")
)

server <- function(input, output, session) {
  output$timeline <- renderTimevis({
    timevis(data,groups = data.frame(id = 1:6, content = levels(unique(dfcr$type))),zoomFactor=0.1)%>%setWindow("2004-04-01","2006-01-01")%>%
      setGroups(data.frame(id = 1:6, content = levels(unique(dfcr$type))))
    
  })
}

shinyApp(ui = ui, server = server)

# ui <- fluidPage(  
#   titlePanel("Interactive circadian field timeline"),
#   # sidebarLayout(
#   #   sidebarPanel(
#   #     checkboxGroupInput("variable", "type of atricles to show:",
#   #                        c("Concept" = "Concept",
#   #                          "Molecule/gene" = "Molecule/gene",
#   #                          "Disease"="Disease",
#   #                          "Model/tissue" = "Model/tissue",
#   #                          "People" = "People",
#   #                          "Other" = "Other"
#   #                        )),
#   #     sliderInput("DatesMerge",
#   #                 "Dates:",
#   #                 min = as.Date("2001-01-01","%Y-%m-%d"),
#   #                 max = as.Date("2019-12-01","%Y-%m-%d"),
#   #                 value=c(as.Date("2010-12-01"),as.Date("2018-12-01")),
#   #                 timeFormat="%Y-%m-%d",dragRange = TRUE),
#   #     sliderInput("yrange",
#   #                 "Y Range:",
#   #                 min = 0,
#   #                 max = max(dfcr$length),
#   #                 value=c(0,max(dfcr$length)),
#   #                 dragRange = TRUE)
#   #   ),
#     mainPanel(
#       plotlyOutput("plot2")))
# 
# server <- function(input, output) {
#   
#   output$plot2 <- renderPlotly({
#     print(
#       ggplotly(
#         #ggplot(dplyr::filter(dfcr,type %in% input$variable ),aes(x=tsc,y=length,fill=type,label=art))+
#           ggplot(dfcr,aes(x=tsc,y=length,fill=type,label=label))+
#           geom_point(aes(colour=type),alpha=0.3)+geom_text(aes(colour=type),size=2,vjust = 0, nudge_y = 2)+
#           #geom_text_repel(aes(label =art),nudge_y= 0.5, #fill = factor(type)
#            #                direction = "y",
#             #               angle        = 0,
#              #              vjust        = 0,segment.alpha =0.2,
#               #             size=3,segment.size = .5)+
#           scale_x_date()+theme_minimal()+#xlim(input$DatesMerge)+
#           #ylim(input$yrange)+
#           scale_colour_brewer("type", palette="Dark2")+
#           scale_fill_brewer("type", palette="Dark2")+
#           theme(legend.position = "bottom")+ labs(x ="time",y="Article size"))%>%layout(autosize = F, width = 1200, height = 500,hovermode="closest"))#
#     #p$data[[1]]$links = links
#     #p
#     #p <- plot_ly()
#     #p <- add_trace(p, uid=trace1$uid, line=trace1$line, mode=trace1$mode, name=trace1$name, type=trace1$type, x=trace1$x, y=trace1$y, marker=trace1$marker, error_x=trace1$error_x, error_y=trace1$error_y, text=trace1$text, textfont=trace1$textfont)
#     #p <- layout(p, font=layout$font, smith=layout$smith, title=layout$title, width=layout$width, xaxis=layout$xaxis, yaxis=layout$yaxis, bargap=layout$bargap, boxgap=layout$boxgap, height=layout$height, legend=layout$legend, margin=layout$margin, barmode=layout$barmode, boxmode=layout$boxmode, autosize=layout$autosize, dragmode=layout$dragmode, hovermode=layout$hovermode, titlefont=layout$titlefont, separators=layout$separators, showlegend=layout$showlegend, bargroupgap=layout$bargroupgap, boxgroupgap=layout$boxgroupgap, hidesources=layout$hidesources, plot_bgcolor=layout$plot_bgcolor, paper_bgcolor=layout$paper_bgcolor)#facet_wrap(~type,ncol=1)+ theme_gdocs()
#     
#   })
# }
# 
# shinyApp(ui, server)

#############



