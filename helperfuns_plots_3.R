library(dplyr)
library(readr)
library(forcats)
library(scales)
library(labelled)
library(sjlabelled)
library(stringr)
library(ggplot2)

working_directory

### ggplot themes
ggtheme_descriptive_plot <- function(angletext_yaxis=0, angletext_xaxis=0,
                                     striptext_size_x = 9, striptext_size_y = 9){
  theme_set(theme_minimal() +
              theme(
                legend.position="bottom",
                legend.text = element_text(size = 8),
                legend.title = element_text(size = 8, color = "red", face = "bold", hjust = 0.5),
                axis.line.y = element_line(colour = "grey",inherit.blank = FALSE),
                axis.line.x = element_line(colour = "grey",inherit.blank = FALSE),
                axis.ticks.y = element_line(linewidth = 0.5, color="black"),
                axis.ticks.x = element_line(linewidth = 0.5, color="black"),
                axis.text.y = element_text(angle = angletext_yaxis, lineheight = 0.7, hjust = 0.5),
                axis.text.x = element_text(angle = angletext_xaxis, lineheight = 0.7, vjust = 0.5),
                plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
                plot.caption = element_text(angle = 0, size = 10, face = "italic"),
                axis.title.x = element_text(size = 10, face = "bold"),
                axis.title.y = element_text(size = 10, face = "bold"),
                strip.text.x = element_text(size = striptext_size_x),
                strip.text.y = element_text(size = striptext_size_y),
                panel.grid.major.y = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank(),
                panel.grid.minor.y = element_blank()
              )
  )
}

ggtheme_rank_plot <- function() {
  theme_set( theme_bw(base_size=11) +
               theme( 
                 legend.position="right", #default
                 axis.text.x = element_text(angle = 0, lineheight = 0.6),
                 axis.text.y = element_text(angle = 0, lineheight = 0.6),
                 plot.title = element_text(hjust = 0.5),
                 axis.title.x = element_text(size = 12, face = "bold"),
                 axis.title.y = element_text(size = 12, face = "bold"),
                 strip.background = element_blank(),
                 panel.border = element_rect(colour = "grey"
                                             , fill = NA
                                             , linewidth = 0.8
                                             ),
                 strip.text.x = element_text(size = 8
                                             , colour = "black"
                                             , face = "bold"
                                             )
               )
  )
}

ggtheme_sf_plot <- function(){
  theme_set(#theme_minimal() +
    theme(
      rect = element_blank(),
      #plot.background = element_blank(),
      #panel.background = element_rect(fill = "white"),
      #legend.position="bottom",
      #legend.text = element_text(size = 8),
      #legend.title = element_text(size = 8, color = "red", face = "bold", hjust = 0.5),
      #axis.line.y = element_line(colour = "grey",inherit.blank = FALSE),
      #axis.line.x = element_line(colour = "grey",inherit.blank = FALSE),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_blank(),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
      plot.caption = element_text(angle = 0, size = 10, face = "italic"),
      axis.title.x = element_text(size = 10, face = "bold"),
      axis.title.y = element_text(size = 10, face = "bold"),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
    )
  )
}

ggtheme_heat_plot = function(angletext_yaxis=0, angletext_xaxis=0) {
  theme_set(theme_bw(base_size = 10)
            + theme(panel.grid = element_blank()
                    , plot.title = element_text(hjust = 0.5, face = "bold", size = 10)
                    , axis.title.x = element_blank()
                    , axis.title.y = element_blank()
                    , axis.text.y = element_text(size = 8.5, angle = angletext_yaxis, lineheight = 0.7)
                    , axis.text.x = element_text(size = 8.5, angle = angletext_xaxis, lineheight = 0.7)
            )
  )
}

ggtheme_regression_plot = function(angletext_yaxis=0, angletext_xaxis=0) {
  theme_set(theme_bw(base_size = 10)
            + theme(panel.grid = element_blank()
                    , plot.title = element_text(hjust = 0.5, face = "bold", size = 10)
                    , axis.title.x = element_text(size = 10, face = "bold")
                    , axis.title.y = element_text(size = 10, face = "bold")
                    , axis.text.y = element_text(size = 8.5, angle = angletext_yaxis, lineheight = 0.7)
                    , axis.text.x = element_text(size = 8.5, angle = angletext_xaxis, lineheight = 0.7)
            )
  )
}

### simple Bar graphs and histograms without density plots
single_plot <- function(df, variable, rotate_axis=FALSE, percent_yaxis=FALSE, title_label = TRUE, text_label = TRUE,
                        histogram = TRUE, text_label_size = 2.5, y_axis_breaks =10, x_axis_breaks_histogram = 10,
                        expand_yaxis = c(0.01,0.05), y_axis_limits = c(NULL, NULL), histogram_bins = 50,
                        percent_text_accuracy = 0.1, x_axis_label_wrap_width = 10,
                        title_label_wrap_width = 35, bar_width = 0.8) {
  out <- lapply(variable, function(x){
    df <- (df %>%
             mutate(across(where(is.character), sjlabelled::as_factor))
    )
    df <- df[!is.na(df[[x]]),]
    index <- df[[x]]
    label <- if (is.null(labelled::var_label(index))) {x
    } else {
      labelled::var_label(index)
    }
    
    plot <- if (is.factor(index)) {
      p1 <- if (rotate_axis == TRUE) {
        p1_1 <- ggplot(data=df, aes(x= forcats::fct_rev(forcats::fct_infreq(index)))) +
          coord_flip() 
        
        p1_2 <- if (percent_yaxis == TRUE) {
          p1_3 <- if (text_label == TRUE) {
            p1_1 +
              geom_bar(aes(y = after_stat(count)/sum(after_stat(count)), fill = index, group=index),
                       position="stack", stat="count", show.legend = FALSE, width = bar_width) +
              scale_y_continuous(labels = scales::percent, n.breaks = y_axis_breaks, limits = y_axis_limits,
                                 expand = expansion(mult = expand_yaxis)) +
              geom_text(aes(y = after_stat(count)/sum(after_stat(count)),
                            label = scales::percent(after_stat(count)/sum(after_stat(count)),  
                                                    accuracy = percent_text_accuracy),group=index),
                        stat = "count", 
                        hjust = -0.05, 
                        colour = "black",
                        size = text_label_size)
          } else {
            p1_1 +
              geom_bar(aes(y = after_stat(count)/sum(after_stat(count)), fill = index, group=index),
                       position="stack", stat="count", show.legend = FALSE, width = bar_width) +
              scale_y_continuous(labels = scales::percent, n.breaks = y_axis_breaks, limits = y_axis_limits,
                                 expand = expansion(mult = expand_yaxis)) 
          }
          p1_3
        }
        else {
          p1_3 <- if (text_label == TRUE) {
            p1_1 +
              geom_bar(aes(fill = index), position="stack", stat="count", show.legend = FALSE, width = bar_width) +
              scale_y_continuous(n.breaks = y_axis_breaks, limits = y_axis_limits,
                                 expand = expansion(mult = expand_yaxis)) +
              geom_text(aes(label = paste0(after_stat(count), " (",
                                           scales::percent(after_stat(count)/sum(after_stat(count)),
                                                           accuracy = percent_text_accuracy),")" ), group=1 ),
                        stat = "count", 
                        hjust = -0.05, 
                        colour = "black",
                        size = text_label_size)
          } else {
            p1_1 +
              geom_bar(aes(fill = index), position="stack", stat="count", show.legend = FALSE, width = bar_width) +
              scale_y_continuous(n.breaks = y_axis_breaks, limits = y_axis_limits,
                                 expand = expansion(mult = expand_yaxis))
          }
          p1_3
        }
        p1_2
        
      } else { 
        p1_1 <- ggplot(data=df, aes(x=forcats::fct_infreq(index)))
        
        p1_2 <- if (percent_yaxis == TRUE) {
          p1_3 <- if (text_label == TRUE) {
            p1_1 +
              geom_bar(aes(y = after_stat(count)/sum(after_stat(count)), fill = index, group=index),
                       position="stack", stat="count", show.legend = FALSE, width = bar_width) +
              scale_y_continuous(labels = scales::percent, n.breaks = y_axis_breaks, limits = y_axis_limits,
                                 expand = expansion(mult = expand_yaxis)) +
              geom_text(aes(y = after_stat(count)/sum(after_stat(count)),
                            label = scales::percent(after_stat(count)/sum(after_stat(count)),  
                                                    accuracy = percent_text_accuracy), group=index),
                        stat = "count", 
                        vjust = -0.2, 
                        colour = "black",
                        size = text_label_size)
          } else {
            p1_1 +
              geom_bar(aes(y = after_stat(count)/sum(after_stat(count)), fill = index, group=index, width = bar_width),
                       position="stack", stat="count", show.legend = FALSE) +
              scale_y_continuous(labels = scales::percent, n.breaks = y_axis_breaks, limits = y_axis_limits,
                                 expand = expansion(mult = expand_yaxis))
          }
          p1_3
        }
        else {
          p1_3 <- if (text_label == TRUE) {
            p1_1 +
              geom_bar(aes(fill = index), position="stack", stat="count", show.legend = FALSE, width = bar_width) +
              scale_y_continuous(n.breaks = y_axis_breaks, limits = y_axis_limits,
                                 expand = expansion(mult = expand_yaxis)) +
              geom_text(aes(label = paste0(after_stat(count), " (",
                                           scales::percent(after_stat(count)/sum(after_stat(count)),
                                                           accuracy = percent_text_accuracy),  ")" ), group=1 ),
                        stat = "count", 
                        vjust = -0.2, 
                        colour = "black",
                        size = text_label_size)
          } else {
            p1_1 +
              geom_bar(aes(fill = index), position="stack", stat="count", show.legend = FALSE, width = bar_width) +
              scale_y_continuous(n.breaks = y_axis_breaks, limits = y_axis_limits,
                                 expand = expansion(mult = expand_yaxis)) 
          }
          p1_3
        }
        p1_2
      }
      
      p2 <- p1 +
        scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = x_axis_label_wrap_width))
      
      p2
    } else {
      if (histogram == TRUE) {
      ggplot(data=df, aes(x = as.numeric(index))) + 
        geom_histogram(aes(y = after_stat(density)), 
                       bins = histogram_bins, color = "black", fill = "gray") +
        geom_density(linewidth = 0.5, colour = "royalblue",
                       fill = "royalblue", alpha = 0.25) +  
        geom_vline(aes(xintercept = mean(index, na.rm=TRUE)), 
                   linetype = "dashed", linewidth = 0.6) +
        scale_x_continuous(n.breaks = x_axis_breaks_histogram) +
        scale_y_continuous(n.breaks = y_axis_breaks, limits = y_axis_limits,
                           expand = expansion(mult = expand_yaxis)) 
      } else {
        ggplot(data=df, aes(y = as.numeric(index))) + 
          geom_boxplot(outlier.colour = "red", outlier.shape = 1, show.legend = FALSE) +
          scale_x_discrete() +
          scale_y_continuous(n.breaks = y_axis_breaks, limits = y_axis_limits,
                             expand = expansion(mult = expand_yaxis))
      }
    }
    plot1 <- if (title_label == TRUE) {
      plot +
        labs(x=NULL,y=NULL, title = stringr::str_wrap(label, width = title_label_wrap_width))
    } else {
      plot + 
        labs(x=NULL,y=NULL, title = "")
    }
    
    plot1
    
  })
  out
}

### Multiple responses bar graphs
multires_stack_plot <- 
  function(df, variable, reverse_xaxis=FALSE, reverse_fill = FALSE, rotate_axis=TRUE, percent_yaxis=TRUE,
           y_axis_breaks = 10, title_label="", text_label = TRUE, text_angle = 0, text_label_size = 2.5,
           bar_width = 0.8, legend_label = "Response", expand_yaxis = c(0.01,0.05), nrow_legend = 1, ncol_legend = NULL,
           x_axis_label_wrap_width = 20, title_label_wrap_width = 35, percent_text_accuracy = 0.1) {
  df <- (df %>%
           dplyr::select(all_of(variable)) %>%
           sjlabelled:::label_to_colnames() %>% #variable labels to column names.
           mutate(across(where(is.factor), as.character)) %>% 
           pivot_longer(cols = everything(),
                 names_to = "name",
                 values_to = "value") %>%
           drop_na(value) %>%
           group_by(name) %>%
           mutate(total = n()) %>%
           ungroup() %>%
           group_by(name, value, total) %>%
           summarise(count = n(), .groups = 'drop') %>%
           mutate(p = scales::percent(count/total, accuracy = percent_text_accuracy)) %>%
           arrange(desc(count), name, value) %>%
           mutate(across(c(name, value), ~ forcats::as_factor(.x))
                  )
         )
  
  p <- if (reverse_xaxis == TRUE) {
    if (reverse_fill == TRUE) {
      ggplot(df, aes(x= forcats::fct_rev(name), y=count,
                     fill=forcats::fct_rev(forcats::fct_reorder(value, count)))) +
        guides(fill=guide_legend(nrow = nrow_legend, ncol = ncol_legend))
    } else {
      ggplot(df, aes(x= forcats::fct_rev(name), y=count,
                     fill=forcats::fct_reorder(value, count))) +
        guides(fill=guide_legend(nrow = nrow_legend, ncol = ncol_legend))
    }
  } else {
    if (reverse_fill == TRUE) {
      ggplot(df, aes(x= name, y=count, fill=forcats::fct_rev(forcats::fct_reorder(value, count)))) +
        guides(fill=guide_legend(nrow = nrow_legend, ncol = ncol_legend))
    } else {
      ggplot(df, aes(x= name, y=count, fill=forcats::fct_reorder(value, count))) +
        guides(fill=guide_legend(nrow = nrow_legend, ncol = ncol_legend))
      }
  }
  
  p1 <- if (rotate_axis == TRUE) { 
    p + coord_flip()
  } else {
      p
  }
  
  p3 <- if (percent_yaxis == TRUE) {
    p2 <- if (text_label == TRUE) {
      p1 +
        geom_bar(position = "fill", stat = "identity",width=bar_width) + 
        geom_text(aes(label= p),
                  position = position_fill(vjust=0.5), color="black", angle = text_angle,
                  size=text_label_size, fontface = "bold") +
        scale_y_continuous(labels = scales::percent, n.breaks = y_axis_breaks, expand = expansion(mult = expand_yaxis))
    } else {
      p1 +
        geom_bar(position = "fill", stat = "identity",width=bar_width) + 
        scale_y_continuous(labels = scales::percent, n.breaks = y_axis_breaks, expand = expansion(mult = expand_yaxis))
    }
    p2
  } else {
    p2 <- if (text_label == TRUE) {
      p1 +
        geom_bar(stat = "identity",width=bar_width) + 
        geom_text(aes(label= count),
                  position = position_stack(vjust=0.5), color="black", angle = text_angle,
                  size=text_label_size, fontface = "bold") +
        scale_y_continuous(n.breaks = y_axis_breaks, expand = expansion(mult = expand_yaxis))
    } else {
      p1 +
        geom_bar(stat = "identity",width=bar_width) + 
        scale_y_continuous(n.breaks = y_axis_breaks, expand = expansion(mult = expand_yaxis))
    }
    p2
      
  }
  
  p4 <- p3 +
    scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = x_axis_label_wrap_width)) +
    labs(x=NULL, y=NULL, fill = legend_label, title = stringr::str_wrap(title_label, width = title_label_wrap_width))
  
  print(p4)
}


#filter only one level
multires_count_plot <- 
  function(df, variable, filter_level, reverse_xaxis=TRUE, rotate_axis=TRUE, percent_yaxis=FALSE,
           y_axis_breaks = 6, title_label="", text_label = TRUE, text_label_size = 2.5, percent_text_accuracy = 0.1,
           expand_yaxis = c(0.01,0.06), x_axis_label_wrap_width = 20, title_label_wrap_width = 35,
           bar_width = 0.8, y_axis_limits = c(NULL, NULL)) {
    df <- (df %>%
             dplyr::select(all_of(variable)) %>%
             sjlabelled:::label_to_colnames() %>% #variable labels to column names.
             mutate(across(where(is.factor), as.character)) %>% 
             pivot_longer(cols = everything(),
                          names_to = "name",
                          values_to = "value") %>%
             drop_na(value) %>%
             group_by(name) %>%
             mutate(total = n()) %>%
             ungroup() %>%
             group_by(name, value, total) %>%
             summarise(count = n(), .groups = 'drop') %>%
             mutate(prop = round(count/total, 3),
                    perc = scales::percent(prop, accuracy = percent_text_accuracy)) %>%
             dplyr::filter(value == filter_level) %>%
             arrange(desc(count), name, value) %>%
             mutate(across(c(name, value), ~ forcats::as_factor(.x))
             )
           )
    
    p <- if (reverse_xaxis == TRUE) {
      if (percent_yaxis == TRUE) {
        ggplot(df, aes(fill=value, y = prop, x=forcats::fct_rev(name))) +
          scale_y_continuous(labels = scales::percent, n.breaks = y_axis_breaks, limits = y_axis_limits,
                             expand = expansion(mult = expand_yaxis))
      } else {
        ggplot(df, aes(fill=value, y = count, x=forcats::fct_rev(name))) +
          scale_y_continuous( n.breaks = y_axis_breaks, limits = y_axis_limits,
                              expand = expansion(mult = expand_yaxis))
      }
    } else {
      if (percent_yaxis == TRUE) {
        ggplot(df, aes(fill=value, y = prop, x=name)) +
          scale_y_continuous(labels = scales::percent, n.breaks = y_axis_breaks, limits = y_axis_limits,
                             expand = expansion(mult = expand_yaxis))
      } else {
        ggplot(df, aes(fill=value, y = count, x=name)) +
          scale_y_continuous( n.breaks = y_axis_breaks, limits = y_axis_limits,
                              expand = expansion(mult = expand_yaxis))
      }
    }
    
    p1 <- p +
      geom_bar(position = "dodge", stat = "identity", width=bar_width)
    
    p3 <- if (rotate_axis == TRUE) {
      p2 <- if (text_label == TRUE) {
        p1 +
          geom_text(aes(label= paste0(count," (", perc,")" )), vjust=0.4, hjust=-0.1,
                    color="black", size=text_label_size) + 
          coord_flip()
      } else {
        p1 + 
          coord_flip()
      }
      p2
    } else {
      p2 <- if (text_label == TRUE) {
        p1 +
          geom_text(aes(label= paste0(count," (", perc,")" )), vjust=-0.3, hjust=0.5,
                    color="black", size=text_label_size)
      } else {
        p1 
      }
      p2
    }
    
    p4 <- p3 +
      scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = x_axis_label_wrap_width)) +
      labs(x=NULL, y=NULL, fill = NULL, title = stringr::str_wrap(title_label, width = title_label_wrap_width)) +
      guides(fill = "none")
    
    print(p4)
      
  }


#filter only one level with time
multires_count_time_plot <- 
  function(df, variable, timevars, id_vars, filter_level, time_text_xaxis = TRUE, reverse_xaxis=TRUE, rotate_axis=TRUE,
           reverse_fill = TRUE, percent_yaxis=TRUE, y_axis_breaks = 6, title_label="", text_label = TRUE,
           text_label_size = 2.5, y_axis_limits = c(NULL, NULL), percent_text_accuracy = 0.1, bar_width = 0.8,
           expand_yaxis = c(0.01,0.06), x_axis_label_wrap_width = 20, title_label_wrap_width = 35,
           nrow_legend = 1, ncol_legend = NULL) {
    
    df <- (df %>%
             dplyr::select(all_of(c(id_vars, variable))) %>%
             mutate(across(where(is.factor), as.character)) %>% 
             pivot_longer(cols = all_of(variable),
                          names_to = c("name"),
                          values_to = "value") %>%
             mutate(
               number = readr::parse_number(name),
               name = gsub('[[:digit:]]+', '', name)
               ) %>% 
             dplyr::left_join(df %>%
                                dplyr::select(all_of(c(id_vars, timevars))) %>%
                                mutate(across(where(is.factor), as.character)) %>% 
                                pivot_longer(cols = all_of(timevars),
                                             names_to = c("name_time"),
                                             values_to = "time") %>%
                                mutate(
                                  number = readr::parse_number(name_time)
                                  ),
                              by = c(id_vars, "number")
                              ) %>%
             drop_na(value, time) %>%
             group_by(name_time, name) %>%
             mutate(total = n()) %>%
             ungroup() %>%
             group_by(name_time, name, value, total) %>%
             summarise(count = n(), .groups = 'drop') %>%
             mutate(prop = round(count/total, 3),
                    perc = scales::percent(prop, accuracy = percent_text_accuracy)) %>%
             dplyr::filter(value == filter_level) %>%
             arrange(name_time, desc(count), name, value) %>%
             mutate(across(c(name_time, name, value), ~ forcats::as_factor(.x))
             )
           )
    
    p <- if (time_text_xaxis == TRUE) {
      if (reverse_xaxis == TRUE) {
        if (reverse_fill == TRUE) {
          if (percent_yaxis == TRUE) {
            ggplot(df, aes(fill=forcats::fct_rev(name), y = prop, x=forcats::fct_rev(name_time))) +
              scale_y_continuous(labels = scales::percent, n.breaks = y_axis_breaks, limits = y_axis_limits,
                                 expand = expansion(mult = expand_yaxis))
            } else {
              ggplot(df, aes(fill=forcats::fct_rev(name), y = count, x=forcats::fct_rev(name_time))) +
                scale_y_continuous( n.breaks = y_axis_breaks, limits = y_axis_limits,
                                    expand = expansion(mult = expand_yaxis))
              } 
          } else {
            if (percent_yaxis == TRUE) {
              ggplot(df, aes(fill=name, y = prop, x=forcats::fct_rev(name_time))) +
                scale_y_continuous(labels = scales::percent, n.breaks = y_axis_breaks, limits = y_axis_limits,
                                   expand = expansion(mult = expand_yaxis))
            } else {
              ggplot(df, aes(fill=name, y = count, x=forcats::fct_rev(name_time))) +
                scale_y_continuous( n.breaks = y_axis_breaks, limits = y_axis_limits,
                                    expand = expansion(mult = expand_yaxis))
            }
          }
        } else {
          if (reverse_fill == TRUE) {
            if (percent_yaxis == TRUE) {
              ggplot(df, aes(fill=forcats::fct_rev(name), y = prop, x=name_time)) +
                scale_y_continuous(labels = scales::percent, n.breaks = y_axis_breaks, limits = y_axis_limits,
                                   expand = expansion(mult = expand_yaxis))
            } else {
              ggplot(df, aes(fill=forcats::fct_rev(name), y = count, x=name_time)) +
                scale_y_continuous( n.breaks = y_axis_breaks, limits = y_axis_limits,
                                    expand = expansion(mult = expand_yaxis))
            } 
          } else {
            if (percent_yaxis == TRUE) {
              ggplot(df, aes(fill=name, y = prop, x=name_time)) +
                scale_y_continuous(labels = scales::percent, n.breaks = y_axis_breaks, limits = y_axis_limits,
                                   expand = expansion(mult = expand_yaxis))
            } else {
              ggplot(df, aes(fill=name, y = count, x=name_time)) +
                scale_y_continuous( n.breaks = y_axis_breaks, limits = y_axis_limits,
                                    expand = expansion(mult = expand_yaxis))
            }
          }
      }
      
    } else {
      if (reverse_xaxis == TRUE) {
        if (reverse_fill == TRUE) {
          if (percent_yaxis == TRUE) {
            ggplot(df, aes(fill=forcats::fct_rev(name_time), y = prop, x=forcats::fct_rev(name))) +
              scale_y_continuous(labels = scales::percent, n.breaks = y_axis_breaks, limits = y_axis_limits,
                                 expand = expansion(mult = expand_yaxis))
          } else {
            ggplot(df, aes(fill=forcats::fct_rev(name_time), y = count, x=forcats::fct_rev(name))) +
              scale_y_continuous( n.breaks = y_axis_breaks, limits = y_axis_limits,
                                  expand = expansion(mult = expand_yaxis))
          } 
        } else {
          if (percent_yaxis == TRUE) {
            ggplot(df, aes(fill=name_time, y = prop, x=forcats::fct_rev(name))) +
              scale_y_continuous(labels = scales::percent, n.breaks = y_axis_breaks, limits = y_axis_limits,
                                 expand = expansion(mult = expand_yaxis))
          } else {
            ggplot(df, aes(fill=name_time, y = count, x=forcats::fct_rev(name))) +
              scale_y_continuous( n.breaks = y_axis_breaks, limits = y_axis_limits,
                                  expand = expansion(mult = expand_yaxis))
          }
        }
      } else {
        if (reverse_fill == TRUE) {
          if (percent_yaxis == TRUE) {
            ggplot(df, aes(fill=forcats::fct_rev(name_time), y = prop, x=name)) +
              scale_y_continuous(labels = scales::percent, n.breaks = y_axis_breaks, limits = y_axis_limits,
                                 expand = expansion(mult = expand_yaxis))
          } else {
            ggplot(df, aes(fill=forcats::fct_rev(name_time), y = count, x=name)) +
              scale_y_continuous( n.breaks = y_axis_breaks, limits = y_axis_limits,
                                  expand = expansion(mult = expand_yaxis))
          } 
        } else {
          if (percent_yaxis == TRUE) {
            ggplot(df, aes(fill=name_time, y = prop, x=name)) +
              scale_y_continuous(labels = scales::percent, n.breaks = y_axis_breaks, limits = y_axis_limits,
                                 expand = expansion(mult = expand_yaxis))
          } else {
            ggplot(df, aes(fill=name_time, y = count, x=name)) +
              scale_y_continuous( n.breaks = y_axis_breaks, limits = y_axis_limits,
                                  expand = expansion(mult = expand_yaxis))
          }
        }
      }
    }
    
    p1 <- p +
      geom_bar(position = "dodge", stat = "identity", width=bar_width)
    
    p3 <- if (rotate_axis == TRUE) {
      p2 <- if (text_label == TRUE) {
        p1 +
          geom_text(aes(label= paste0(count," (", perc,")" )), position = position_dodge(0.9),
                    vjust=0.4, hjust=-0.1, color="black", size=text_label_size) + 
          coord_flip()
      } else {
        p1 + 
          coord_flip()
      }
      p2
    } else {
      p2 <- if (text_label == TRUE) {
        p1 +
          geom_text(aes(label= paste0(count," (", perc,")" )), position = position_dodge(0.9),
                    vjust=-0.3, hjust=0.5, color="black", size=text_label_size)
      } else {
        p1
      }
      p2
    }
    
    p4 <- p3 +
      scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = x_axis_label_wrap_width)) +
      labs(x=NULL, y=NULL, fill = NULL, title = stringr::str_wrap(title_label, width = title_label_wrap_width)) +
      guides(fill = guide_legend(reverse = reverse_fill, nrow = nrow_legend, ncol = ncol_legend))
    
    print(p4)
    
  }

