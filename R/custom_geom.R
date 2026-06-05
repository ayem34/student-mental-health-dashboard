library(ggplot2)
library(grid)

GeomLollipop <- ggproto(
  
  "GeomLollipop",
  
  Geom,
  
  required_aes = c("x","y"),
  
  draw_panel = function(
    data,
    panel_params,
    coord
  ){
    
    coords <- coord$transform(
      data,
      panel_params
    )
    
    grobTree(
      
      segmentsGrob(
        x0 = coords$x,
        x1 = coords$x,
        y0 = 0,
        y1 = coords$y
      ),
      
      pointsGrob(
        x = coords$x,
        y = coords$y,
        pch = 16,
        size = unit(4,"mm")
      )
      
    )
  }
)

geom_lollipop <- function(...) {
  
  layer(
    geom = GeomLollipop,
    stat = "identity",
    position = "identity",
    params = list(...)
  )
  
}