#library(RColorBrewer)

#ggplot <- function(...) ggplot2::ggplot(...) + scale_fill_brewer(palette="Set2")

theme_wl <- function() {
  
  font <- "sans"
  
  theme_bw() %+replace%
    
    theme(
      
      # Grid elements
      
      panel.grid.minor = element_blank(),    #strip minor gridlines
      axis.ticks = element_line(color="#636363",size = 0.7), 
      
      
      # Border lines
      panel.border = element_rect(color="#636363",fill=NA,size = 0.7),
      panel.background = element_blank(),
            # Text elements
      
      plot.title = element_text(             #title
        family = font,            #set font family
        size = 14,                #set font size
        face = 'bold',            #bold typeface
        hjust = 0,                #left align
        vjust = 3,                #raise slightly
        color = '#636363'),       #color
      
      plot.subtitle = element_text(
        family = font,
        size = 12,
        hjust = 0,
        vjust = 2,
        color = '#636363'),
      
      plot.caption = element_text(           #caption
        family = font,            #font family
        size = 9,                 #font size
        hjust = 0,                #right align
        color = '#bdbdbd'),       #color
      
      axis.title = element_text(             #axis titles
        family = font,            #font family
        size = 10,                #font size
        color = '#636363'),       #color
      
      axis.text = element_text(              #axis text
        family = font,            #axis famuly
        size = 9,                 #font size
        color = '#636363'),       #color
      
      axis.text.x = element_text(            #margin for axis text
        margin=margin(5, b = 10)),
      
      text = element_text(
        family = font,
        color = '#636363')
      
    )
  
}

theme_wl_emissions <- function() {
  
  font <- "sans"
  
  theme_bw() %+replace%
    
    theme(
      
      # Grid elements
      
      panel.grid.minor = element_blank(),                       #strip minor gridlines
      panel.grid.major.x = element_blank(),
      axis.ticks = element_line(color="#636363",size=0.25),     #smaller axis ticks
      axis.line = element_line(color="#636363",size = 0.25),
      strip.background = element_blank(),
      strip.text = element_text(size=12,color='#636363',hjust=0,face = "bold",margin=margin(b=5)),
      
      
      # Border lines
      panel.border = element_blank(),
      panel.background = element_blank(),
      
      
      # Legend
      legend.position="right",
      legend.title=element_blank(),
      legend.key.size = unit(0.5, "cm"),
      
      
      # Text elements
      
      plot.title = element_text( 
        family = font,           
        size = 14,               
        face = 'bold',           
        hjust = 0,               
        vjust = 3,               
        color = '#636363',
        margin = margin(t = 10)),
      
      plot.subtitle = element_text(
        family = font,
        size = 12,
        hjust = 0,
        vjust = 2,
        color = '#636363'),
      
      plot.caption = element_text(           #caption
        family = font,            #font family
        size = 9,                 #font size
        hjust = 0,                #right align
        color = '#bdbdbd'),       #color
      
      
      axis.title = element_text(             #axis titles
        family = font,            #font family
        size = 10,                #font size
        color = '#636363'),       #color
      
      axis.text = element_text(              #axis text
        family = font,            #axis famuly
        size = 9,                 #font size
        color = '#636363'),       #color
      
      axis.text.x = element_text(            #margin for axis text
        margin=margin(5, b = 10)),
      
      text = element_text(
        family = font,
        color = '#636363')
      
    )
      
}

colours_9_sectors <- c("#365f90ff", "#4672b3ff", "#659cccff", "#a3d3f7ff", "#6ccac8ff", "#e5b82cff", "#4f8455ff", "#6aad75ff", "#818181ff")
colours_9_qual <- c("#ff9055ff", "#659cccff", "#e5b82cff", "#7dd396ff", "#818181ff", "#9467bdff", "#17becfff", "#d45087ff")


# more_set2_colors <- function(number) {
#   
#   colors = colorRampPalette(brewer.pal(8, "Set2"))(number)
#   
# }



