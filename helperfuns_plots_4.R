library(dplyr)
library(forcats)
library(scales)
library(labelled)
library(stringr)
library(ggplot2)
library(ggstats)

working_directory

### stacked Bar graphs and density/box plots
stacked_plot <- function(df, variable, fill_vars, facet_vars=NULL, facet_wrap=FALSE, title_label = NULL, 
                         facet_ncol = 4, rotate_axis=FALSE, box_plot=TRUE, legend=TRUE, nrow_legend = 1,
                         ncol_legend = NULL, y_axis_breaks = 10, expand_yaxis = c(0.01,0.05),
                         title_label_wrap_width = 35, text_label = TRUE, text_angle = 0, text_label_size = 3,
                         x_axis_label_wrap_width = 10, x_axis_breaks = 10, bar_width = 0.9) {
  out <- lapply(variable, function(x){
    df <- (df %>%
             mutate(across(where(is.character), sjlabelled::as_factor))
           )
    plot <- if (facet_wrap == TRUE) {
      df <- df[!is.na(df[[x]]) & !is.na(df[[fill_vars]]) & !is.na(df[[facet_vars]]),]
      index_fill <- df[[fill_vars]]
      index <- df[[x]]
      label <- if (is.null(labelled::var_label(index))) {x 
        } else {
      labelled::var_label(index)
          }
      
      p <- if (is.factor(index)| is.logical(index)) {
        p1 <- if (rotate_axis == TRUE) {
          ggplot(data=df, aes(x= forcats::fct_rev(index), fill = forcats::fct_infreq(index_fill),
                              by = forcats::fct_rev(index))
                 ) +
            coord_flip() 
        } else { ggplot(data=df, aes(x=index, fill = forcats::fct_infreq(index_fill),
                                     by = index)
                        )  
        }
        
        p2 <- if (legend == TRUE) {
          if (text_label == TRUE) {
            p1 +
              geom_bar(width=bar_width, position="fill", stat="count", show.legend = TRUE) +
              geom_text(stat = "prop", position = position_fill(.5), color="black", angle = text_angle,
                        size=text_label_size, fontface = "bold") +
              guides(fill=guide_legend(nrow = nrow_legend, ncol = ncol_legend)) 
          } else {
            p1 +
              geom_bar(width=bar_width, position="fill", stat="count", show.legend = TRUE) +
              guides(fill=guide_legend(nrow = nrow_legend, ncol = ncol_legend)) 
          }
        } else {
          if (text_label == TRUE) {
            p1 +
              geom_bar(width=bar_width, position="fill", stat="count", show.legend = FALSE) +
              geom_text(stat = "prop", position = position_fill(.5), color="black", angle = text_angle,
                        size=text_label_size, fontface = "bold")
          } else {
            p1 +
              geom_bar(width=bar_width, position="fill", stat="count", show.legend = FALSE)
          }
        }
        
        p3 <- p2 +
          scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = x_axis_label_wrap_width)) +
          scale_y_continuous(labels = scales::percent, n.breaks = y_axis_breaks, expand = expansion(mult = expand_yaxis))
        p3
          
        } else {
          p4 <- if (box_plot == TRUE) {
            p5 <- if (rotate_axis == TRUE) {
            ggplot(data=df, aes(x = forcats::fct_rev(index_fill), y = index, colour = forcats::fct_rev(index_fill))) +
                coord_flip() 
            } else {
              ggplot(data=df, aes(x = index_fill, y = index, colour = index_fill))
            }
            p6 <- if (legend == TRUE) { 
              p5 +
                geom_boxplot(outlier.colour = "black", outlier.shape = 1, show.legend = TRUE) +
                guides(colour=guide_legend(nrow = nrow_legend, ncol = ncol_legend)) } 
            else {
              p5 +
                geom_boxplot(outlier.colour = "black", outlier.shape = 1, show.legend = FALSE)
            }
            
            } else {
              ggplot(data=df, aes(x = as.numeric(index), fill = index_fill)) + 
                geom_density(alpha=0.4, show.legend = TRUE) +
                scale_x_continuous(n.breaks = x_axis_breaks) +
                guides(fill=guide_legend(nrow = nrow_legend, ncol = ncol_legend))
              }
          p7 <- p4 +
            scale_y_continuous(n.breaks = y_axis_breaks)
       
          p7
        }
      p_ <- p + 
        facet_wrap(as.formula(paste("~", facet_vars)), ncol = facet_ncol)
      
    } else {
      df <- df[!is.na(df[[x]]) & !is.na(df[[fill_vars]]),]
      index_fill <- df[[fill_vars]]
      index <- df[[x]]
      label <- if (is.null(labelled::var_label(index))) {x 
      } else {
        labelled::var_label(index)
      }
      
      p <- if (is.factor(index)| is.logical(index)) {
        p1 <- if (rotate_axis == TRUE) {
          ggplot(data=df, aes(x= forcats::fct_rev(index), fill = forcats::fct_infreq(index_fill),
                              by = forcats::fct_rev(index))
                 ) +
            coord_flip() 
        } else { ggplot(data=df, aes(x=index, fill = forcats::fct_infreq(index_fill),
                                     by = index)
                        )  
        }
        
        p2 <- if (legend == TRUE) {
          if (text_label == TRUE) {
            p1 +
              geom_bar(width=bar_width, position="fill", stat="count", show.legend = TRUE) +
              geom_text(stat = "prop", position = position_fill(.5), color="black", angle = text_angle,
                        size=text_label_size, fontface = "bold") +
              guides(fill=guide_legend(nrow = nrow_legend, ncol = ncol_legend)) 
          } else {
            p1 +
              geom_bar(width=bar_width, position="fill", stat="count", show.legend = TRUE) +
              guides(fill=guide_legend(nrow = nrow_legend, ncol = ncol_legend)) 
          }
        } else {
          if (text_label == TRUE) {
            p1 +
              geom_bar(width=bar_width, position="fill", stat="count", show.legend = FALSE) +
              geom_text(stat = "prop", position = position_fill(.5), color="black", angle = text_angle,
                        size=text_label_size, fontface = "bold")
          } else {
            p1 +
              geom_bar(width=bar_width, position="fill", stat="count", show.legend = FALSE)
          }
        }
        
        p3 <- p2 +
          scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = x_axis_label_wrap_width)) +
          scale_y_continuous(labels = scales::percent, n.breaks = y_axis_breaks, expand = expansion(mult = expand_yaxis))
        p3
        
      } else {
        p4 <- if (box_plot == TRUE) {
          p5 <- if (rotate_axis == TRUE) {
            ggplot(data=df, aes(x = forcats::fct_rev(index_fill), y = index)) +
              coord_flip() 
          } else {
            ggplot(data=df, aes(x = index_fill, y = index))
          }
          p6 <- if (legend == TRUE) { 
            p5 +
              geom_boxplot(aes(colour = index_fill), outlier.colour = "black", 
                           outlier.shape = 1, show.legend = TRUE) +
              guides(colour=guide_legend(nrow = nrow_legend, ncol = ncol_legend)) } 
          else {
            p5 +
              geom_boxplot(aes(colour = index_fill), outlier.colour = "black", 
                           outlier.shape = 1, show.legend = FALSE)
            }
          } else {
          ggplot(data=df, aes(x = as.numeric(index), fill = index_fill)) + 
            geom_density(alpha=0.4, show.legend = TRUE) +
            scale_x_continuous(n.breaks = x_axis_breaks) +
            guides(fill=guide_legend(nrow = nrow_legend, ncol = ncol_legend))
            }
        p7 <- p4 +
          scale_y_continuous(n.breaks = y_axis_breaks)
        p7
      }
     p
    }
    
    plot1 <- if (is.null(title_label)) {
      plot +
        labs(x=NULL,y=NULL, fill = "", colour = "", title = stringr::str_wrap(label, width = title_label_wrap_width))
    } else {
      plot + 
        labs(x=NULL,y=NULL, fill = "", colour = "", title = title_label)
    }
    
    plot1
    
  })
  out
}

### Line graphs
line_count_plot <- function(df, x_vars, x_label=NULL, y_label=NULL, colour_vars, facet_vars=NULL, facet_wrap=FALSE,
                            facet_ncol = 4, title_label = TRUE, legend=TRUE, nrow_legend = 1, ncol_legend = NULL,
                            y_axis_breaks =10, x_axis_date_breaks = "1 month", x_axis_limits = c(NULL, NULL),
                            x_axis_min_max_break_months = 2, facet_scales = "free_y", y_axis_limits = c(NULL, NULL),
                            line_width = 0.5, expand_xaxis = c(0.02,0.02), expand_yaxis = c(0.02,0.05),
                            title_label_wrap_width = 35, x_axis_date_label = "%Y-%m", x_axis_label_wrap_width = 20,
                            add_point = FALSE, point_size = 1, point_shape = 19, add_text = FALSE, text_label_size = 3,
                            text_label_digits = 0, text_label_angle = 0, text_label_fontface = "bold", 
                            text_label_position = "jitter", text_label_overlap = FALSE, text_label_size_unit = "mm") {
  out <- lapply(colour_vars, function(x){
    df <- (df %>%
             mutate(across(where(is.character), sjlabelled::as_factor)) %>%
             mutate(across(where(is.logical), sjlabelled::as_factor))
           )
    plot <- if (facet_wrap == TRUE) {
      df <- df[!is.na(df[[x]]) & !is.na(df[[x_vars]]) & !is.na(df[[facet_vars]]),]
      index_colour <- df[[x]]
      index <- df[[x_vars]]
      label <- if (is.null(labelled::var_label(index_colour))) {x
        } else {
      labelled::var_label(index_colour)
          }
    
      p <- ggplot(data=df, aes(x=index, colour=index_colour, group=index_colour))
      
      p1 <- if (legend == TRUE) {
        if (add_point == TRUE) {
          if (add_text == TRUE) {
            p +
              geom_line(linewidth = line_width, stat="count", show.legend = TRUE) +
              geom_point(stat = "count", size = point_size, shape = point_shape, show.legend = TRUE) +
              geom_text(aes(label = round(after_stat(count),text_label_digits)), stat = "count", size = text_label_size,
                        angle = text_label_angle, fontface = text_label_fontface, position = text_label_position,
                        check_overlap = text_label_overlap, size.unit = text_label_size_unit, show.legend = FALSE ) +
              guides(colour=guide_legend(nrow = nrow_legend, ncol = ncol_legend))
          } else {
            p +
              geom_line(linewidth = line_width, stat="count", show.legend = TRUE) +
              geom_point(stat = "count", size = point_size, shape = point_shape, show.legend = TRUE) +
              guides(colour=guide_legend(nrow = nrow_legend, ncol = ncol_legend))
            }
        } else {
          if (add_text == TRUE) {
            p +
              geom_line(linewidth = line_width, stat="count", show.legend = TRUE) +
              geom_text(aes(label = round(after_stat(count),text_label_digits)), stat = "count", size = text_label_size,
                        angle = text_label_angle, fontface = text_label_fontface, position = text_label_position,
                        check_overlap = text_label_overlap, size.unit = text_label_size_unit, show.legend = FALSE ) +
              guides(colour=guide_legend(nrow = nrow_legend, ncol = ncol_legend))
          } else {
            p +
              geom_line(linewidth = line_width, stat="count", show.legend = TRUE) +
              guides(colour=guide_legend(nrow = nrow_legend, ncol = ncol_legend))
            }
          }
      } else {
        if (add_point == TRUE) {
          if (add_text == TRUE) {
            p +
              geom_line(linewidth = line_width, stat="count", show.legend = FALSE) +
              geom_point(stat = "count", size = point_size, shape = point_shape, show.legend = FALSE) +
              geom_text(aes(label = round(after_stat(count),text_label_digits)), stat = "count", size = text_label_size,
                        angle = text_label_angle, fontface = text_label_fontface, position = text_label_position,
                        check_overlap = text_label_overlap, size.unit = text_label_size_unit, show.legend = FALSE )
          } else {
            p +
              geom_line(linewidth = line_width, stat="count", show.legend = FALSE) +
              geom_point(stat = "count", size = point_size, shape = point_shape, show.legend = FALSE)
            }
        } else {
          if (add_text == TRUE) {
            p +
              geom_line(linewidth = line_width, stat="count", show.legend = FALSE) +
              geom_text(aes(label = round(after_stat(count),text_label_digits)), stat = "count", size = text_label_size,
                        angle = text_label_angle, fontface = text_label_fontface, position = text_label_position,
                        check_overlap = text_label_overlap, size.unit = text_label_size_unit, show.legend = FALSE )
          } else {
            p +
              geom_line(linewidth = line_width, stat="count", show.legend = FALSE)
            }
          }
        }
      
      p2 <- if (class(index) == "Date") {
        p1 +
          scale_x_date(date_breaks = waiver(), date_labels = x_axis_date_label, limits = x_axis_limits,
                       breaks = c(min(as.Date(index)),
                                  seq(from = min(as.Date(index))%m+%months(x_axis_min_max_break_months),
                                      to = max(as.Date(index))%m-%months(x_axis_min_max_break_months),
                                      by = x_axis_date_breaks),
                                  max(as.Date(index))
                                  ),
                       expand = expansion(mult = expand_xaxis)) 
      } else if (class(index)[[1]] == "POSIXct") {
        p1 +
          scale_x_datetime(date_breaks = waiver(), date_labels = x_axis_date_label, limits = x_axis_limits,
                           breaks = c(min(as.Date(index)),
                                      seq(from = min(as.Date(index))%m+%months(x_axis_min_max_break_months),
                                          to = max(as.Date(index))%m-%months(x_axis_min_max_break_months),
                                          by = x_axis_date_breaks),
                                      max(as.Date(index))
                                      ),
                           expand = expansion(mult = expand_xaxis))
      } else if (class(index) == "factor") { 
        p1 +
          scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = x_axis_label_wrap_width))
      } else {
        p1
        }
      
      p3 <- p2 +
        facet_wrap(as.formula(paste("~", facet_vars)), ncol = facet_ncol, scales = facet_scales)
    } else {
      df <- df[!is.na(df[[x]]) & !is.na(df[[x_vars]]),]
      index_colour <- df[[x]]
      index <- df[[x_vars]]
      label <- if (is.null(labelled::var_label(index_colour))) {x
      } else {
        labelled::var_label(index_colour)
      }
      
      p <- ggplot(data=df, aes(x=index, colour=index_colour, group=index_colour))
      
      p1 <- if (legend == TRUE) {
        if (add_point == TRUE) {
          if (add_text == TRUE) {
            p +
              geom_line(linewidth = line_width, stat="count", show.legend = TRUE) +
              geom_point(stat = "count", size = point_size, shape = point_shape, show.legend = TRUE) +
              geom_text(aes(label = round(after_stat(count),text_label_digits)), stat = "count", size = text_label_size,
                        angle = text_label_angle, fontface = text_label_fontface, position = text_label_position,
                        check_overlap = text_label_overlap, size.unit = text_label_size_unit, show.legend = FALSE ) +
              guides(colour=guide_legend(nrow = nrow_legend, ncol = ncol_legend))
          } else {
            p +
              geom_line(linewidth = line_width, stat="count", show.legend = TRUE) +
              geom_point(stat = "count", size = point_size, shape = point_shape, show.legend = TRUE) +
              guides(colour=guide_legend(nrow = nrow_legend, ncol = ncol_legend))
          }
        } else {
          if (add_text == TRUE) {
            p +
              geom_line(linewidth = line_width, stat="count", show.legend = TRUE) +
              geom_text(aes(label = round(after_stat(count),text_label_digits)), stat = "count", size = text_label_size,
                        angle = text_label_angle, fontface = text_label_fontface, position = text_label_position,
                        check_overlap = text_label_overlap, size.unit = text_label_size_unit, show.legend = FALSE ) +
              guides(colour=guide_legend(nrow = nrow_legend, ncol = ncol_legend))
          } else {
            p +
              geom_line(linewidth = line_width, stat="count", show.legend = TRUE) +
              guides(colour=guide_legend(nrow = nrow_legend, ncol = ncol_legend))
          }
        }
      } else {
        if (add_point == TRUE) {
          if (add_text == TRUE) {
            p +
              geom_line(linewidth = line_width, stat="count", show.legend = FALSE) +
              geom_point(stat = "count", size = point_size, shape = point_shape, show.legend = FALSE) +
              geom_text(aes(label = round(after_stat(count),text_label_digits)), stat = "count", size = text_label_size,
                        angle = text_label_angle, fontface = text_label_fontface, position = text_label_position,
                        check_overlap = text_label_overlap, size.unit = text_label_size_unit, show.legend = FALSE )
          } else {
            p +
              geom_line(linewidth = line_width, stat="count", show.legend = FALSE) +
              geom_point(stat = "count", size = point_size, shape = point_shape, show.legend = FALSE)
          }
        } else {
          if (add_text == TRUE) {
            p +
              geom_line(linewidth = line_width, stat="count", show.legend = FALSE) +
              geom_text(aes(label = round(after_stat(count),text_label_digits)), stat = "count", size = text_label_size,
                        angle = text_label_angle, fontface = text_label_fontface, position = text_label_position,
                        check_overlap = text_label_overlap, size.unit = text_label_size_unit, show.legend = FALSE )
          } else {
            p +
              geom_line(linewidth = line_width, stat="count", show.legend = FALSE)
          }
        }
      }
      
      p2 <- if (class(index) == "Date") {
        p1 +
          scale_x_date(date_breaks = waiver(), date_labels = x_axis_date_label, limits = x_axis_limits,
                       breaks = c(min(as.Date(index)),
                                  seq(from = min(as.Date(index))%m+%months(x_axis_min_max_break_months),
                                      to = max(as.Date(index))%m-%months(x_axis_min_max_break_months),
                                      by = x_axis_date_breaks),
                                  max(as.Date(index))
                                  ),
                       expand = expansion(mult = expand_xaxis)) 
      } else if (class(index)[[1]] == "POSIXct") {
        p1 +
          scale_x_datetime(date_breaks = waiver(), date_labels = x_axis_date_label, limits = x_axis_limits,
                           breaks = c(min(as.Date(index)),
                                      seq(from = min(as.Date(index))%m+%months(x_axis_min_max_break_months),
                                          to = max(as.Date(index))%m-%months(x_axis_min_max_break_months),
                                          by = x_axis_date_breaks),
                                      max(as.Date(index))
                                      ),
                           expand = expansion(mult = expand_xaxis))
      } else if (class(index) == "factor") { 
        p1 +
          scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = x_axis_label_wrap_width))
      } else {
        p1
      }
      
      p3 <- p2 
      
    }
    
    plot1 <- plot +
      scale_y_continuous( labels = function(x) base::format(x, scientific= FALSE), limits = y_axis_limits,
                          n.breaks = y_axis_breaks, expand = expansion(mult = expand_yaxis))
    
    plot2 <- if (title_label == TRUE) {
      plot1 +
        labs(x=x_label, y=y_label, colour = "",  title = stringr::str_wrap(label, width = title_label_wrap_width))
    } else {
      plot1 +
        labs(x=x_label, y=y_label, colour = "",  title = "")
    }
    
    plot2
    
  })
  out
}

line_sum_plot <- function(df, x_vars, y_vars, x_label=NULL, y_label=NULL, colour_vars, facet_vars=NULL, stat_fun_sum = TRUE,
                          title_label = TRUE, facet_wrap=FALSE, legend=TRUE, nrow_legend = 1, ncol_legend = NULL,
                          facet_ncol = 4, y_axis_breaks =10, x_axis_date_breaks = "1 month", x_axis_limits = c(NULL, NULL),
                          x_axis_min_max_break_months = 2, y_axis_label_percent = FALSE, y_axis_limits = c(NULL, NULL),
                          facet_scales = "free_y", line_width = 0.5, title_label_wrap_width = 35, expand_xaxis = c(0.02,0.02), 
                          x_axis_date_label = "%Y-%m", x_axis_label_wrap_width = 20, expand_yaxis = c(0.02,0.05),
                          add_point = FALSE, point_size = 1, point_shape = 19, add_text = FALSE, text_label_size = 3, 
                          text_label_digits = 0, text_label_angle = 0, text_label_fontface = "bold",
                          text_label_position = "jitter", text_label_overlap = FALSE, text_label_size_unit = "mm") {
  out <- lapply(colour_vars, function(x){
    df <- (df %>%
             mutate(across(where(is.character), sjlabelled::as_factor)) %>%
             mutate(across(where(is.logical), sjlabelled::as_factor))
    )
    plot <- if (facet_wrap == TRUE) {
      df <- df[!is.na(df[[x]]) & !is.na(df[[x_vars]]) & !is.na(df[[y_vars]]) & !is.na(df[[facet_vars]]),]
      index_colour <- df[[x]]
      index_y <- df[[y_vars]]
      index <- df[[x_vars]]
      label <- if (is.null(labelled::var_label(index_colour))) {x
      } else {
        labelled::var_label(index_colour) 
      }
      
      p <- ggplot(data=df, aes(x=index, y=index_y, color=index_colour, group=index_colour,
                               label = round(after_stat(y),text_label_digits))
                  )
      
      p1 <- if (stat_fun_sum == TRUE) {
        if (legend == TRUE) {
          if (add_point == TRUE) {
            if (add_text == TRUE) {
              p + 
                geom_line(stat = "summary", fun = "sum", linewidth = line_width, show.legend = TRUE) +
                geom_point(stat = "summary", fun = "sum", size = point_size, shape = point_shape, show.legend = TRUE) +
                geom_text(stat = "summary", fun = "sum", size = text_label_size, angle = text_label_angle,
                          fontface = text_label_fontface, position = text_label_position, check_overlap = text_label_overlap,
                          size.unit = text_label_size_unit, show.legend = FALSE ) +  
                guides(colour=guide_legend(nrow = nrow_legend, ncol = ncol_legend))
            } else {
              p + 
                geom_line(stat = "summary", fun = "sum", linewidth = line_width, show.legend = TRUE) +
                geom_point(stat = "summary", fun = "sum", size = point_size, shape = point_shape, show.legend = TRUE) +
                guides(colour=guide_legend(nrow = nrow_legend, ncol = ncol_legend))
            }
          } else {
            if (add_text == TRUE) {
              p + 
                geom_line(stat = "summary", fun = "sum", linewidth = line_width, show.legend = TRUE) +
                geom_text(stat = "summary", fun = "sum", size = text_label_size, angle = text_label_angle,
                          fontface = text_label_fontface, position = text_label_position, check_overlap = text_label_overlap,
                          size.unit = text_label_size_unit, show.legend = FALSE ) +  
                guides(colour=guide_legend(nrow = nrow_legend, ncol = ncol_legend))
            } else {
              p + 
                geom_line(stat = "summary", fun = "sum", linewidth = line_width, show.legend = TRUE) +
                guides(colour=guide_legend(nrow = nrow_legend, ncol = ncol_legend))
            }
          }
        } else {
          if (add_point == TRUE) {
            if (add_text == TRUE) {
              p + 
                geom_line(stat = "summary", fun = "sum", linewidth = line_width, show.legend = FALSE) + 
                geom_point(stat = "summary", fun = "sum", size = point_size, shape = point_shape, show.legend = FALSE) +
                geom_text(stat = "summary", fun = "sum", size = text_label_size, angle = text_label_angle,
                          fontface = text_label_fontface, position = text_label_position, check_overlap = text_label_overlap,
                          size.unit = text_label_size_unit, show.legend = FALSE ) 
            } else {
              p + 
                geom_line(stat = "summary", fun = "sum", linewidth = line_width, show.legend = FALSE) +
                geom_point(stat = "summary", fun = "sum", size = point_size, shape = point_shape, show.legend = FALSE) 
              }
            
          } else {
            if (add_text == TRUE) {
              p + 
                geom_line(stat = "summary", fun = "sum", linewidth = line_width, show.legend = FALSE) +
                geom_text(stat = "summary", fun = "sum", size = text_label_size, angle = text_label_angle,
                          fontface = text_label_fontface, position = text_label_position, check_overlap = text_label_overlap,
                          size.unit = text_label_size_unit, show.legend = FALSE )
            } else {
              p + 
                geom_line(stat = "summary", fun = "sum", linewidth = line_width, show.legend = FALSE) 
              }
            }
          }
      } else {
        if (legend == TRUE) {
          if (add_point == TRUE) {
            if (add_text == TRUE) {
              p + 
                geom_line(stat = "summary", fun = "mean", linewidth = line_width, show.legend = TRUE) +
                geom_point(stat = "summary", fun = "mean", size = point_size, shape = point_shape, show.legend = TRUE) +
                geom_text(stat = "summary", fun = "mean", size = text_label_size, angle = text_label_angle,
                          fontface = text_label_fontface, position = text_label_position, check_overlap = text_label_overlap,
                          size.unit = text_label_size_unit, show.legend = FALSE ) +
                guides(colour=guide_legend(nrow = nrow_legend, ncol = ncol_legend))
            } else {
              p + 
                geom_line(stat = "summary", fun = "mean", linewidth = line_width, show.legend = TRUE) +
                geom_point(stat = "summary", fun = "mean", size = point_size, shape = point_shape, show.legend = TRUE) +
                guides(colour=guide_legend(nrow = nrow_legend, ncol = ncol_legend)) 
              }
          } else {
            if (add_text == TRUE) {
              p + 
                geom_line(stat = "summary", fun = "mean", linewidth = line_width, show.legend = TRUE) +
                geom_text(stat = "summary", fun = "mean", size = text_label_size, angle = text_label_angle,
                          fontface = text_label_fontface, position = text_label_position, check_overlap = text_label_overlap,
                          size.unit = text_label_size_unit, show.legend = FALSE ) +
                guides(colour=guide_legend(nrow = nrow_legend, ncol = ncol_legend))
            } else {
              p + 
                geom_line(stat = "summary", fun = "mean", linewidth = line_width, show.legend = TRUE) +
                guides(colour=guide_legend(nrow = nrow_legend, ncol = ncol_legend))
              }
            }
        } else {
          if (add_point == TRUE) {
            if (add_text == TRUE) {
              p + 
                geom_line(stat = "summary", fun = "mean", linewidth = line_width, show.legend = FALSE) +
                geom_point(stat = "summary", fun = "mean", size = point_size, shape = point_shape, show.legend = FALSE) +
                geom_text(stat = "summary", fun = "mean", size = text_label_size, angle = text_label_angle,
                          fontface = text_label_fontface, position = text_label_position, check_overlap = text_label_overlap,
                          size.unit = text_label_size_unit, show.legend = FALSE )
            } else {
              p + 
                geom_line(stat = "summary", fun = "mean", linewidth = line_width, show.legend = FALSE) +
                geom_point(stat = "summary", fun = "mean", size = point_size, shape = point_shape, show.legend = FALSE)
              }
          } else {
            if (add_text == TRUE) {
              p + 
                geom_line(stat = "summary", fun = "mean", linewidth = line_width, show.legend = FALSE) +
                geom_text(stat = "summary", fun = "mean", size = text_label_size, angle = text_label_angle,
                          fontface = text_label_fontface, position = text_label_position, check_overlap = text_label_overlap,
                          size.unit = text_label_size_unit, show.legend = FALSE )
            } else {
              p + 
                geom_line(stat = "summary", fun = "mean", linewidth = line_width, show.legend = FALSE) 
              }
          }
        }
      }
      
      p2 <- if (class(index) == "Date") {
        p1 +
          scale_x_date(date_breaks = waiver(), date_labels = x_axis_date_label, limits = x_axis_limits,
                       breaks = c(min(as.Date(index)),
                                  seq(from = min(as.Date(index))%m+%months(x_axis_min_max_break_months),
                                      to = max(as.Date(index))%m-%months(x_axis_min_max_break_months),
                                      by = x_axis_date_breaks),
                                  max(as.Date(index))
                                  ),
                       expand = expansion(mult = expand_xaxis)) 
      } else if (class(index)[[1]] == "POSIXct") {
        p1 +
          scale_x_datetime(date_breaks = waiver(), date_labels = x_axis_date_label, limits = x_axis_limits,
                           breaks = c(min(as.Date(index)),
                                      seq(from = min(as.Date(index))%m+%months(x_axis_min_max_break_months),
                                          to = max(as.Date(index))%m-%months(x_axis_min_max_break_months),
                                          by = x_axis_date_breaks),
                                      max(as.Date(index))
                                      ),
                           expand = expansion(mult = expand_xaxis))
      } else if (class(index) == "factor") { 
        p1 +
          scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = x_axis_label_wrap_width))
      } else {
        p1
      }
      
      p3 <- p2 +
        facet_wrap(as.formula(paste("~", facet_vars)), ncol = facet_ncol, scales = facet_scales)
    } else {
      df <- df[!is.na(df[[x]]) & !is.na(df[[x_vars]]) & !is.na(df[[y_vars]]),]
      index_colour <- df[[x]]
      index_y <- df[[y_vars]]
      index <- df[[x_vars]]
      label <- if (is.null(labelled::var_label(index_colour))) {x
      } else {
        labelled::var_label(index_colour)
      }
      
      p <- ggplot(data=df, aes(x=index, y=index_y, color=index_colour, group=index_colour,
                               label = round(after_stat(y),text_label_digits))
                  )
      
      p1 <- if (stat_fun_sum == TRUE) {
        if (legend == TRUE) {
          if (add_point == TRUE) {
            if (add_text == TRUE) {
              p + 
                geom_line(stat = "summary", fun = "sum", linewidth = line_width, show.legend = TRUE) +
                geom_point(stat = "summary", fun = "sum", size = point_size, shape = point_shape, show.legend = TRUE) +
                geom_text(stat = "summary", fun = "sum", size = text_label_size, angle = text_label_angle,
                          fontface = text_label_fontface, position = text_label_position, check_overlap = text_label_overlap,
                          size.unit = text_label_size_unit, show.legend = FALSE ) +  
                guides(colour=guide_legend(nrow = nrow_legend, ncol = ncol_legend))
            } else {
              p + 
                geom_line(stat = "summary", fun = "sum", linewidth = line_width, show.legend = TRUE) +
                geom_point(stat = "summary", fun = "sum", size = point_size, shape = point_shape, show.legend = TRUE) +
                guides(colour=guide_legend(nrow = nrow_legend, ncol = ncol_legend))
            }
          } else {
            if (add_text == TRUE) {
              p + 
                geom_line(stat = "summary", fun = "sum", linewidth = line_width, show.legend = TRUE) +
                geom_text(stat = "summary", fun = "sum", size = text_label_size, angle = text_label_angle,
                          fontface = text_label_fontface, position = text_label_position, check_overlap = text_label_overlap,
                          size.unit = text_label_size_unit, show.legend = FALSE ) +  
                guides(colour=guide_legend(nrow = nrow_legend, ncol = ncol_legend))
            } else {
              p + 
                geom_line(stat = "summary", fun = "sum", linewidth = line_width, show.legend = TRUE) +
                guides(colour=guide_legend(nrow = nrow_legend, ncol = ncol_legend))
            }
          }
        } else {
          if (add_point == TRUE) {
            if (add_text == TRUE) {
              p + 
                geom_line(stat = "summary", fun = "sum", linewidth = line_width, show.legend = FALSE) + 
                geom_point(stat = "summary", fun = "sum", size = point_size, shape = point_shape, show.legend = FALSE) +
                geom_text(stat = "summary", fun = "sum", size = text_label_size, angle = text_label_angle,
                          fontface = text_label_fontface, position = text_label_position, check_overlap = text_label_overlap,
                          size.unit = text_label_size_unit, show.legend = FALSE ) 
            } else {
              p + 
                geom_line(stat = "summary", fun = "sum", linewidth = line_width, show.legend = FALSE) +
                geom_point(stat = "summary", fun = "sum", size = point_size, shape = point_shape, show.legend = FALSE) 
              }
            
          } else {
            if (add_text == TRUE) {
              p + 
                geom_line(stat = "summary", fun = "sum", linewidth = line_width, show.legend = FALSE) +
                geom_text(stat = "summary", fun = "sum", size = text_label_size, angle = text_label_angle,
                          fontface = text_label_fontface, position = text_label_position, check_overlap = text_label_overlap,
                          size.unit = text_label_size_unit, show.legend = FALSE )
            } else {
              p + 
                geom_line(stat = "summary", fun = "sum", linewidth = line_width, show.legend = FALSE) 
              }
            }
          }
      } else {
        if (legend == TRUE) {
          if (add_point == TRUE) {
            if (add_text == TRUE) {
              p + 
                geom_line(stat = "summary", fun = "mean", linewidth = line_width, show.legend = TRUE) +
                geom_point(stat = "summary", fun = "mean", size = point_size, shape = point_shape, show.legend = TRUE) +
                geom_text(stat = "summary", fun = "mean", size = text_label_size, angle = text_label_angle,
                          fontface = text_label_fontface, position = text_label_position, check_overlap = text_label_overlap,
                          size.unit = text_label_size_unit, show.legend = FALSE ) +
                guides(colour=guide_legend(nrow = nrow_legend, ncol = ncol_legend))
            } else {
              p + 
                geom_line(stat = "summary", fun = "mean", linewidth = line_width, show.legend = TRUE) +
                geom_point(stat = "summary", fun = "mean", size = point_size, shape = point_shape, show.legend = TRUE) +
                guides(colour=guide_legend(nrow = nrow_legend, ncol = ncol_legend)) 
              }
          } else {
            if (add_text == TRUE) {
              p + 
                geom_line(stat = "summary", fun = "mean", linewidth = line_width, show.legend = TRUE) +
                geom_text(stat = "summary", fun = "mean", size = text_label_size, angle = text_label_angle,
                          fontface = text_label_fontface, position = text_label_position, check_overlap = text_label_overlap,
                          size.unit = text_label_size_unit, show.legend = FALSE ) +
                guides(colour=guide_legend(nrow = nrow_legend, ncol = ncol_legend))
            } else {
              p + 
                geom_line(stat = "summary", fun = "mean", linewidth = line_width, show.legend = TRUE) +
                guides(colour=guide_legend(nrow = nrow_legend, ncol = ncol_legend))
              }
            }
        } else {
          if (add_point == TRUE) {
            if (add_text == TRUE) {
              p + 
                geom_line(stat = "summary", fun = "mean", linewidth = line_width, show.legend = FALSE) +
                geom_point(stat = "summary", fun = "mean", size = point_size, shape = point_shape, show.legend = FALSE) +
                geom_text(stat = "summary", fun = "mean", size = text_label_size, angle = text_label_angle,
                          fontface = text_label_fontface, position = text_label_position, check_overlap = text_label_overlap,
                          size.unit = text_label_size_unit, show.legend = FALSE )
            } else {
              p + 
                geom_line(stat = "summary", fun = "mean", linewidth = line_width, show.legend = FALSE) +
                geom_point(stat = "summary", fun = "mean", size = point_size, shape = point_shape, show.legend = FALSE)
              }
          } else {
            if (add_text == TRUE) {
              p + 
                geom_line(stat = "summary", fun = "mean", linewidth = line_width, show.legend = FALSE) +
                geom_text(stat = "summary", fun = "mean", size = text_label_size, angle = text_label_angle,
                          fontface = text_label_fontface, position = text_label_position, check_overlap = text_label_overlap,
                          size.unit = text_label_size_unit, show.legend = FALSE )
            } else {
              p + 
                geom_line(stat = "summary", fun = "mean", linewidth = line_width, show.legend = FALSE) 
              }
          }
        }
      }
      
      p2 <- if (class(index) == "Date") {
        p1 +
          scale_x_date(date_breaks = waiver(), date_labels = x_axis_date_label, limits = x_axis_limits,
                       breaks = c(min(as.Date(index)),
                                  seq(from = min(as.Date(index))%m+%months(x_axis_min_max_break_months),
                                      to = max(as.Date(index))%m-%months(x_axis_min_max_break_months),
                                      by = x_axis_date_breaks),
                                  max(as.Date(index))
                                  ),
                       expand = expansion(mult = expand_xaxis)) 
      } else if (class(index)[[1]] == "POSIXct") {
        p1 +
          scale_x_datetime(date_breaks = waiver(), date_labels = x_axis_date_label, limits = x_axis_limits,
                           breaks = c(min(as.Date(index)),
                                      seq(from = min(as.Date(index))%m+%months(x_axis_min_max_break_months),
                                          to = max(as.Date(index))%m-%months(x_axis_min_max_break_months),
                                          by = x_axis_date_breaks),
                                      max(as.Date(index))
                                      ),
                           expand = expansion(mult = expand_xaxis))
      } else if (class(index) == "factor") { 
        p1 +
          scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = x_axis_label_wrap_width))
      } else {
        p1
      }
      
      p3 <- p2 
      
    }
    
    plot1 <- if (y_axis_label_percent == TRUE) {
      plot +
        scale_y_continuous( labels = scales::percent, limits = y_axis_limits,
                            n.breaks = y_axis_breaks, expand = expansion(mult = expand_yaxis))
    } else {
      plot +
        scale_y_continuous( labels = function(x) base::format(x, scientific= FALSE), limits = y_axis_limits,
                            n.breaks = y_axis_breaks, expand = expansion(mult = expand_yaxis))
    }
    
    plot2 <- if (title_label == TRUE) {
      plot1 +
        labs(x=x_label, y=y_label, colour = "",  title = stringr::str_wrap(label, width = title_label_wrap_width))
    } else {
      plot1 +
        labs(x=x_label, y=y_label, colour = "",  title = "")
    }
    
    plot2
  })
  out
}


