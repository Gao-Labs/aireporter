# compare.R

# Script to compare performance metrics among reports for experiment.

# Set working directory
setwd(paste0(rstudioapi::getActiveProject(), "/report_v5"))

library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)


# Initially messed up the formatting; had to fix
# read_csv("experiment/answers.csv") %>%
#   select(accuracy = 1,
#          formality = 2,
#          faithfulness = 3,
#          clarity = 4,
#          succinctness = 5,
#          details = 6,
#          report_id = 7,
#          id = 8) %>%
#   mutate(success = str_extract(id, pattern = "[,]TRUE|FALSE") %>% str_remove("[,]") %>% readr::parse_logical()) %>%
#   mutate(id = str_remove(id, pattern = "[,]TRUE|FALSE")) %>%
#   select(report_id, id, success, accuracy, formality, faithfulness, clarity, succinctness, details) %>%
#   write_csv("experiment/answers.csv")


get_viz = function(answers, strata = "geoid"){

  # strata = c("metric")
  vars = c("accurate", "accuracy", "formality", "faithfulness", "clarity", "succinctness", "relevance")
  strata_label = switch(EXPR = strata, "geoid" = "By County", "county" = "By County", "pollutant" = "By Pollutant", "state" = "By State",
                        "metric" = "By Metric", "graph" = "By Graph Type", "aggregation" = "By Aggregation Level",
                        ... = "By Strata")

  # Recode the strata values
  strata_values = switch(
    EXPR = strata,
    "metric" = {
      c("emissions" = "Emissions",
        "vehicles" = "Vehicles",
        "vmt" = "VMT",
        "ratevehicles" = "Emissions per vehicle",
        "ratevmt" = "Emissions per mile",
        "ratepop" = "Emissions per capita",
        "starts" = "Vehicle Starts",
        "sourcehours" = "Time Driven",
        "idlehours" = "Idling",
        "hoteld" = "Hotelling (D)",
        "hotelb" = "Hotelling (B)",
        "hotelo" = "Hotelling (O)",
        "pop" = "Population"
      )
    },
    "pollutant" = {
      c(
        "98" = "CO2e",
        "2" = "CO",
        "3" = "NOx",
        "31" = "SO2",
        "87" = "VOC",
        "110" = "PM2.5",
        "100" = "PM10"
      )
    },
    "graph" = {
      c("donut" = "Donut Plot",
        "line" = "Line Plot",
        "multiline" = "Multi-Line Plot",
        "map" = "Heatmap",
        "rank" = "Rank Plot")
    },
    ... = NULL
  )


  colors = viridis::plasma(n = length(vars), begin = 0, end = 0.8)


  data = answers %>%
    select(any_of(strata), any_of(vars))

  if(!is.null(strata_values)){
    data = data %>%
      mutate(!!sym(strata) := !!sym(strata) %>%
               dplyr::recode_factor(!!!strata_values ))
  }
  data = data %>%
    pivot_longer(cols = any_of(vars), names_to = "var", values_to = "value") %>%
    mutate(varlabel = var %>% dplyr::recode_factor(
      "accurate" = "% Accurate",
      "accuracy" = "Accuracy",
      "clarity" = "Clarity",
      "faithfulness" = "Faithfulness",
      "formality" = "Formality",
      "succinctness" = "Succinctness",
      "relevance" = "Relevance"
    )) %>%
    group_by(across(any_of(strata)), varlabel, var) %>%
    summarize(mean = mean(value, na.rm = TRUE),
              sd = sd(value, na.rm = TRUE),
              n = n(),
              se = sd / sqrt(n),
              .groups = "drop") %>%
    mutate(
      lower =  mean - se * qnorm(0.975),
      upper =  mean + se * qnorm(0.975)
    )

  # Calculate a pooled standard error..
  overall = data %>%
    group_by(varlabel, var) %>%
    summarize(
      mean = mean(mean, na.rm = TRUE),
      se_pooled = sqrt( sum( sd^2 / n, na.rm = TRUE )),
      mean = round(mean, 2),
      se_pooled = round(se_pooled, 2),
      .groups = "drop"
    )

  n_reports = answers$report_id %>% unique() %>% length()
  n_sections = answers$id %>% unique() %>% length()
  n_groups = answers[, strata][[1]] %>% unique() %>% length()

  gg1 = ggplot() +
    geom_col(data = data %>% filter(var == "accurate"),
             mapping = aes(x = reorder(!!sym(strata), -as.numeric(!!sym(strata))),
                           y = mean),
             fill = colors[1]) +
    geom_linerange(
      data = data %>% filter(var == "accurate"),
      mapping = aes(x = reorder(!!sym(strata), -as.numeric(!!sym(strata))), y = mean, ymin = lower, ymax = upper)
    ) +
    scale_y_continuous(
      breaks = c(0, 0.25, 0.5, 0.75, 1),
      labels = scales::label_percent(),
      expand = expansion(c(0, 0.05))) +
    facet_wrap(~varlabel) +
    theme_bw(base_size = 14) +
    theme(panel.grid.major.x = element_line(color = "lightgrey"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = "black"),
          strip.text = element_text(color = "white")
    ) +
    labs(y = "Percent Accurate (95% CI)", x = NULL) +
    coord_flip()


  if("geoid" %in% strata){

    gg1 = gg1 + theme(axis.text.y = element_blank(),
                      axis.ticks.y = element_blank()) +
      labs(x = "Counties")

  }else if("state" %in% strata){

    gg1 = gg1 + theme(axis.text.y = element_blank(),
                      axis.ticks.y = element_blank()) +
      labs(x = "States")

  }


  gg2 = ggplot() +
    geom_col(data = data %>% filter(var != "accurate"),
             mapping = aes(x = reorder(!!sym(strata), as.numeric(!!sym(strata))) , y = mean, fill = varlabel)) +
    geom_linerange(
      data = data %>% filter(var != "accurate"),
      mapping = aes(x = reorder(!!sym(strata), as.numeric(!!sym(strata))), y = mean, ymin = lower, ymax = upper)
    ) +
    scale_y_continuous(breaks = c(1,2,3,4,5),
                       expand = expansion(c(0, 0.05))) +
    scale_fill_manual(values = colors[-1], guide = "none") +
    theme_bw(base_size = 14) +
    theme(panel.grid.major.y = element_line(color = "lightgrey"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
          strip.background = element_rect(fill = "black"),
          strip.text = element_text(color = "white")
    ) +
    labs(x = NULL, y = "Mean (95% CI)",
         title = "Quality by Likert Scale Measures (1-5)") +
    facet_wrap(~varlabel, nrow = 1)

  if("geoid" %in% strata){

    gg2 = gg2 + theme(axis.text.x = element_blank(),
                axis.ticks.x = element_blank()) +
      labs(x = "Counties")

  }else if("state" %in% strata){


    gg2 = gg2 + theme(axis.text.x = element_blank(),
                      axis.ticks.x = element_blank()) +
      labs(x = "States")
  }

  # Create a Top Summary Title
  ggtop1 = tibble(
    `Grouping` = strata_label,
    `N Reports` = n_reports,
    `N Sections` = n_sections,
    `N Groups` = n_groups
  ) %>%
    # pivot_longer(cols = -c(), names_to = "Metadata", values_to = "Details", values_transform = as.character) %>%
    ggpubr::ggtexttable(
      x = .,
      rows = NULL,
      theme = ggpubr::ttheme(
        colnames.style = ggpubr::colnames_style(color = "white", fill = "black"),
        tbody.style = ggpubr::tbody_style(color = "black", fill = c("grey", "white"))
      )) %>%
    ggpubr::tab_add_title(text = "Experiment Metadata", face = "bold", size = 14, hjust = 0)

  ggtop2 = overall %>%
    select(
      "Measure" = "varlabel",
      "Mean" = "mean",
      "SE (pooled)" = "se_pooled"
    ) %>%
    ggpubr::ggtexttable(
      x = .,
      rows = NULL,
      theme = ggpubr::ttheme(
        tbody.style = ggpubr::tbody_style(
          hjust = 0.5,
          color = "white",
          fill = colors
        )
      )
    ) %>%
    ggpubr::tab_add_title(text = "Experiment Overall Statistics", face = "plain", size = 10, hjust = 0)

  # ggtop2 = ggpubr::ggarrange(plotlist = list(ggtop1, ggtop2), ncol = 2)
  # tab_add_title(text = subtitle, face = "plain", size = 10)

  gg = ggpubr::ggarrange(
    ncol = 1, heights = c(1,3,3), labels = c("A", NA, "D"),
    plotlist = list(
      ggtop1,
      ggpubr::ggarrange(plotlist = list(ggtop2, gg1), nrow = 1, labels = c("B", "C")),
      gg2
    )
  )


  # # Align the plots horizontally
  # ggbottom = ggpubr::ggarrange(plotlist = list(gg1, gg2), nrow = 2)
  #
  # # Align the plots vertically
  # gg = ggpubr::ggarrange(plotlist = list(ggtop, ggbottom), nrow = 2, heights = c(1,6))


  path = paste0("experiment/viz/bars_", strata, ".png")
  ggsave(plot = gg, filename = path, dpi = 500, width = 8, height = 7)

  return(path)
}



# Read in answers
answers = read_csv("experiment/answers.csv") %>%
  # Join in metadata for each row
  left_join(by = c("report_id", "id"), y = read_rds("experiment/grid.rds")) %>%
  mutate(state = state %>% str_remove("[ ](Counties|Parishes|Municipios)")) %>%
  mutate(pollutant = as.factor(pollutant),
         year = as.factor(year))


# Generate plots!
answers %>% get_viz(strata = "geoid")
answers %>% get_viz(strata = "state")
answers %>% get_viz(strata = "pollutant")
answers %>% get_viz(strata = "year")
answers %>% get_viz(strata = "graph")
answers %>% get_viz(strata = "metric")
# answers %>% get_viz(strata = "aggregation") %>% browseURL()


# browseURL(path)

