# ---------------------------------------------------------------------------- #
#
# tab.freq
#
# ---------------------------------------------------------------------------- #

# librairies ----

library(DT)
library(tidyverse)


# function ----

tab_freq <- function(df, 
                     y, 
                     x1, 
                     w = NULL, 
                     colnames = c(levels(df[,y]), "Effectives"), 
                     Total = 'Total', 
                     rownames = c(levels(df[,x1]), Total), 
                     caption = NULL, 
                     style = 1, 
                     cell_colored = c(1e8), 
                     color = '#E0F8E6')
{
  
  if(is.null(w)) weights = rep(1, nrow(df))
  if(!is.null(w)) weights = df[,w] / sum(df[,w]) * nrow(df)
  table <- wtd.table(df[,x1], df[,y], weights = weights)
  tabfreq <- cbind(prop.table(addmargins(table,1),1), Eff = c(margin.table(table,1), sum(table))) %>% 
    as.data.frame()
  
  if(is.null(Total)) tabfreq %<>% slice(-which(rownames(.) == 'Sum'))
  if(style == 1){
    tabfreq %<>% 
      mutate_at(vars(-Eff), funs(ifelse(. == 0, NA, paste0('', round(. * Eff), '<br />', ' <i><small>(', round(. * 100, 1), '%)</small></i>')))) 
  }
  if(style == 2){
    tabfreq %<>% 
      mutate_at(vars(-Eff), funs(ifelse(. == 0, NA, paste0(round(. * 100, 1), '%')))) 
  }
  if(style == 3){
    tabfreq %<>% 
      mutate_at(vars(-Eff), funs(ifelse(. == 0, NA, paste0(round(. * 100, 1), '% <br />', '<i><small>(', round(. * Eff), ')</small></i>')))) 
  }
  
  tabfreq %>% 
    datatable(caption = caption, rownames = rownames, colnames = colnames, escape = F,
              options = list(paging = F, searching = F, info = F, columnDefs = list(list(targets = 1:ncol(.), class = 'dt-right')))) %>% 
    formatStyle(' ', fontWeight = "bold", backgroundColor = "#BDBDBD") %>% 
    formatStyle('Eff', fontStyle = "italic", backgroundColor = "#F2F2F2") %>% 
    formatRound('Eff', digits = 0) %>% 
    formatStyle(1:ncol(tabfreq), backgroundColor = styleEqual(unlist(c(tabfreq))[cell_colored], rep(color, length(cell_colored))))
}
