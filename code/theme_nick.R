theme_nick <- function () { 
  theme_bw(base_size = 12) %+replace% 
    theme(axis.title  = element_text(size = 14, face = 'plain'),
          axis.text = element_text(size = 12, face = 'plain'),
          legend.title = element_text(size = 12, face = 'plain')
          )
}
