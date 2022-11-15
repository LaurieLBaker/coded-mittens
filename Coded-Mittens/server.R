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
library(raster)
library(kableExtra)
library(DT)
library(patchwork)

# Functions

plot_chart <- function(chart){
  ggplot(chart, aes(y = as.numeric(row_num), x = as.numeric(col_num),)) +
    geom_tile(aes(fill = color), alpha = 0.7, color = "black") +
    labs(y = "Row",
         x = " ") +
    scale_x_continuous(breaks = 5, labels = "") +
    scale_y_continuous(breaks = chart$row_num, labels = as.character(chart$row_num), position = "right") +
    geom_text(aes(label = label),
              label.size = 0.1) +
    scale_fill_identity()
}


# Read in the data template

template <- read_csv("../template/mitten_template.csv")

yarn_table <- read_csv("../template/yarn_table.csv")

template_pattern <- chart_prep(pattern = template)

ruleMap <- function(hat, rule){
  ca_output <- rule%/%(2^hat) %% 2
  return(ca_output)
}

iter_template <- function(pattern, CA_rule){
  
  p_cols <- ncol(pattern) - 3 # Assumes thumb has three stitches
  p_rows <- nrow(pattern) - 7 # Not considering cuff in the pattern for ca purposes
  pattern_nt <- pattern[ , 1:p_cols] 
  
  # Add dummy columns so CA algorithm is not bounded by pattern
  dummyct <- p_rows - (p_cols + 1) / 2
  pattern_nt <- bind_cols(matrix("0", nrow(pattern_nt), dummyct), 
                          pattern_nt, 
                          matrix("0", nrow(pattern_nt), dummyct))
  a_cols <- ncol(pattern_nt)
  
  # Constructing the transition matrix
  A <- 2 * diag(a_cols)
  A[1:(a_cols-1), ] <- A[1:(a_cols-1), ] + 1*diag(a_cols)[2:a_cols, ]
  A[2:a_cols, ] <- A[2:a_cols, ] + 4*diag(a_cols)[1:(a_cols-1), ]
  
  pattern_nt[1, (pattern_nt[1,] == "Blank")[1, ]] <- "C1"
  
  # Iterate over body of matrix based on values of row above. Ignore last 7 rows for cuff
  for(i in 2:p_rows){
    ca_out <- A %*% (pattern_nt[(i-1),]=="C1")[1,]
    element_convert <- as.logical(map2(ca_out, CA_rule, ruleMap))
    pattern_nt[i,element_convert] <- "C1"
  }
  
  pattern_nt <- pattern_nt[, (dummyct+1):(dummyct+p_cols)]
  #Mark original thumb locations that were converted to C1 in previous step
  pattern_nt[array(as.logical((pattern_nt[] == "C1")*(pattern[, 1:p_cols] == "Blank Thumb")), dim(pattern_nt))] <- "C1_Thumb"
  pattern_nt[array(as.logical((pattern_nt[]=="C1")*(pattern[, 1:p_cols]=="K2TOG")), dim(pattern_nt))] <- "K2TOG"
  pattern_nt[array(as.logical((pattern_nt[]=="C1")*(pattern[, 1:p_cols]=="SSK")), dim(pattern_nt))] <- "SSK"
  pattern_nt[array(as.logical((pattern_nt[]=="C1")*(pattern[, 1:p_cols]=="None")), dim(pattern_nt))] <- "None"
  
  pattern[, 1:p_cols] <- pattern_nt[]
  
  #Making the cuff
  pattern[nrow(pattern)-6, ] <- "Blank"
  pattern[nrow(pattern)-c(5,0), ] <- "C1"
  pattern[nrow(pattern)-c(4,1), ] <- "C3"
  
  hat_logi <- as.logical(rep(0, times=24))
  for(i in 1:24){
    hat_logi[i] <- as.logical(ruleMap(((24-i) %% 3), ((24-i) %/% 3)))
  }
  pattern[nrow(pattern)-3, hat_logi] <- "C1"
  
  rule_logi <- as.logical(rep(0, times = 24))
  for(i in 0:7){
    rule_logi[(24-3*i)-1] <- as.logical(ruleMap(i, CA_rule))
  }
  pattern[nrow(pattern)-2, rule_logi] <- "C1"
  pattern[nrow(pattern)-2, as.logical(rep(c(1,0,1),times=8))] <- "C3"
  
  return(pattern)
}


## CA Generator


# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$mittens <- renderPlot({

      CA_rule <- input$rule 
      
      
      chart_prep <- function(pattern){
        pattern %>%
          mutate(row_num = 53:1, .before = "1") %>%
          pivot_longer(cols = !row_num, names_to = "col_num", values_to = "stitch") %>%
          mutate(label = case_when(stitch == "K2TOG" ~ "//", 
                                   stitch == "SSK" ~ "\\\\",
                                   stitch %in% c("Blank_Thumb", "CC_Thumb") ~ "T",
                                   TRUE ~ "")) %>%
          mutate(color = case_when(stitch == "None" ~ "#000000",
                                   stitch == "K2TOG" ~ input$col1,
                                   stitch == "SSK" ~ input$col1,
                                   stitch %in% c("Blank") ~ input$col1,
                                   stitch %in% c("MC") ~ input$col1,
                                   stitch %in% c("C1", "C1_Thumb") ~ input$col2,
                                   stitch == "C2" ~ input$col3,
                                   stitch %in% c("C3") ~ input$col3,                                             
                                   stitch %in% c("C4") ~ "#0000FF",
                                   stitch %in% c("C5") ~ "#0000FF",
                                   
                                   stitch == "Blank_Thumb" ~ "#FFFFFF"))
      }
      
      
      ca_pattern <- iter_template(pattern = template, CA_rule)
      
      ca_chart <- chart_prep(ca_pattern)
      
      ca_chart2 <- chart_prep(ca_pattern) %>%
        mutate(col_num = abs(as.numeric(col_num)-24)) %>%
        mutate(label = case_when(stitch == "K2TOG" ~ "\\\\", 
                                 stitch == "SSK" ~ "//",
                                 stitch %in% c("Blank_Thumb", "C1_Thumb") ~ "T",
                                 TRUE ~ "")) 
      
      p1 <- plot_chart(ca_chart) + scale_fill_identity(guide = "legend", 
                                                 labels = c("None", "Contrast Color", "Main Color", "K2Tog or SSK"),
                                                 name = "Stitch Guide")  +
        labs(title = "Left Mitten")
      
      p2 <- plot_chart(ca_chart2) + scale_fill_identity(guide = "legend", labels = c("None", "Contrast Color", "Main Color", "K2Tog or SSK"),
                                                 name = "Stitch Guide") +
        labs(title = "Right Mitten")
      
      p1 + p2
    })

    
    output$yarn_table <- renderDT(
      
      (yarn_datatable <- yarn_table[ ,c("Name", "Yarn", "Hex Code (M)", "Hex Code (B)", "Hex Code (3)")] %>%
          DT::datatable(escape = FALSE, caption = htmltools::tags$caption(
            style = 'caption-side: top; text-align: Left;',
            htmltools::withTags(
              div(HTML('Choose from the palette or get inspired by the lovely wools by <a href="https://www.jamiesonsofshetland.co.uk/jamiesons-of-shetland-spindrift-1-c.asp">Jamieson\'s of Shetland</a>'))
            )
          ),
          options = list(pageLength = 5)) %>% 
          DT::formatStyle(columns = "Hex Code (M)", 
                          target = 'cell',
                          color = "white",
                          backgroundColor = DT::styleEqual(yarn_table$`Hex Code (M)`,
                                                           yarn_table$`Hex Code (M)`))) %>% 
        DT::formatStyle(columns = "Hex Code (B)", 
                        target = 'cell',
                        color = "white",
                        backgroundColor = DT::styleEqual(yarn_table$`Hex Code (B)`,
                                                         yarn_table$`Hex Code (B)`)) %>% 
        DT::formatStyle(columns = "Hex Code (3)", 
                        target = 'cell',
                        color = "white",
                        backgroundColor = DT::styleEqual(yarn_table$`Hex Code (3)`,
                                                         yarn_table$`Hex Code (3)`))
      
    )
})
