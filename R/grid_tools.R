library(grid)
library(gridExtra)
library(gtable)
library(ggplot2)

create_grid_dashboard <- function(data){
  
  p1 <- ggplot(
    data,
    aes(age)
  ) +
    geom_histogram()
  
  p2 <- ggplot(
    data,
    aes(gender)
  ) +
    geom_bar()
  
  p3 <- data %>%
    count(course) %>%
    slice_max(n,n=10) %>%
    ggplot(
      aes(
        reorder(course,n),
        n
      )
    ) +
    geom_col() +
    coord_flip()
  
  grid.arrange(
    p1,
    p2,
    p3,
    ncol = 2
  )
  
}


create_grob_plot <- function(data){
  
  p <- ggplot(
    data,
    aes(
      cgpa_level,
      fill = depression
    )
  ) +
    geom_bar()
  
  g <- ggplotGrob(p)
  
  grid.newpage()
  
  grid.draw(g)
  
}

create_annotation_plot <- function(data){
  
  taux <- mean(
    data$treatment=="Yes"
  ) * 100
  
  p <- ggplot(
    data,
    aes(treatment)
  ) +
    geom_bar()
  
  g <- ggplotGrob(p)
  
  grid.newpage()
  
  grid.draw(g)
  
  grid.text(
    
    paste0(
      "Recours spécialiste : ",
      round(taux,1),
      "%"
    ),
    
    x = .7,
    y = .9,
    
    gp = gpar(
      fontsize = 14,
      fontface = "bold",
      col = "red"
    )
    
  )
  
}