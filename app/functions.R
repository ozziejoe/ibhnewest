#source("app.R")

get_focus_data <- function(file_data, individual, dep_var, ind_vars) {
  focus_data <- file_data %>%
    dplyr::filter(ID %in% individual) %>%
    dplyr::select(ID, Time, dep_var, ind_vars) %>%
    mutate(across(c(dep_var, ind_vars), ~ replace(., grepl("NA", .), NA))) %>%
    mutate(across(c(dep_var, ind_vars), ~ as.numeric(.)))

  if (!is.integer(focus_data$Time)) {
    focus_data <- focus_data %>%
      mutate(Time = as.Date(Time))
  }
  return(focus_data)
}

# VIEW DATA FUNCTIONS ####
## getting stats for all selected variables
get_variable_stat <- function(id_data, full_data){

  all_stats <- data.frame()
  id <- id_data[1, c("ID")]
  id_data <- id_data[,4:length(colnames(id_data))]

  for (var in colnames(id_data)) {
    var_full_data <- full_data %>% dplyr::select(ID, var)
    var_id_data <- id_data %>% dplyr::select(var)

    # Calculating global stats by ID
    g_mean = var_full_data %>% group_by(ID) %>% summarise_all(mean,na.rm=T)
    g_sd = var_full_data %>% group_by(ID) %>% summarise_all(sd,na.rm=T)
    g_skew = var_full_data %>% group_by(ID) %>% summarise_all(skew,na.rm=T)
    g_kurt = var_full_data %>% group_by(ID) %>% summarise_all(kurtosi,na.rm=T)

    # Calculating parameters of distributions of global stats
    g_mean_mean = mean(g_mean[[2]], na.rm=T)
    g_mean_sd = sd(g_mean[[2]], na.rm=T)
    g_sd_mean = mean(g_sd[[2]], na.rm=T)
    g_sd_sd = sd(g_sd[[2]], na.rm=T)
    g_skew_mean = mean(g_skew[[2]], na.rm=T)
    g_skew_sd = sd(g_skew[[2]], na.rm=T)
    g_kurt_mean = mean(g_kurt[[2]], na.rm=T)
    g_kurt_sd = sd(g_kurt[[2]], na.rm=T)

    # Values for output table
    id_mean = mean(var_id_data[[1]], na.rm=T) %>% round(2)
    z_mean = ((id_mean - g_mean_mean) / g_mean_sd) %>% round(2)
    perc_mean = (pnorm(z_mean)*100) %>% round(2)
    id_sd = sd(var_id_data[[1]], na.rm=T) %>% round(2)
    z_sd = ((id_sd - g_sd_mean) / g_sd_sd) %>% round(2)
    perc_sd = (pnorm(z_sd)*100) %>% round(2)
    id_skew = skew(var_id_data[[1]]) %>% round(2)
    z_skew = ((id_skew - g_skew_mean) / g_skew_sd) %>% round(2)
    perc_skew = (pnorm(z_skew)*100) %>% round(2)
    id_kurt = kurtosi(var_id_data[[1]]) %>% round(2)
    z_kurt = ((id_kurt - g_kurt_mean) / g_kurt_sd) %>% round(2)
    perc_kurt = (pnorm(z_kurt)*100) %>% round(2)

    stats = data.frame(ID=id, Variable=var,
                       Mean=id_mean, `Z-mean`=z_mean, `% mean`=perc_mean,
                       SD=id_sd, `Z-SD`=z_sd, `% SD`=perc_sd,
                       Skew=id_skew, `Z-Skew`=z_skew, `% Skew`=perc_skew,
                       Kurt=id_kurt, `Z-Kurt`=z_kurt, `% Kurt`=perc_kurt,
                       N=sum(!is.na(var_id_data))
                      )
    all_stats <- rbind(all_stats, stats)
  }

  return(all_stats)
}
## standardising the data
standardise_df <- function(id_data) {
  names <- names(id_data)[-1]
  df_naomit <- na.omit(id_data)

  for (i in names) {
    z_new <- paste0('Z_',i)
    percentile_new <- paste0('percentile_',i, ' (%)')

    df_naomit <- df_naomit %>%
      mutate(z = round(scale(df_naomit[[i]]),2)) %>%
      mutate(percentile = paste0(round(pnorm(df_naomit[[i]], mean(df_naomit[[i]], na.rm = TRUE),
                                             sd(df_naomit[[i]])),2)*100)) %>%
      rename(
        !!z_new := z,
        !!percentile_new := percentile
      )
  }
  return(df_naomit)
}

# FOREST PLOT FUNCTIONS ####
# GET LAGGED DATA SET
get_lagged_dataset <- function(focus_data, lagged=0) {
  # assuming focus data with only 1 ID
  dep_var <- colnames(focus_data)[3]
  ind_vars <- colnames(focus_data)[4:length(colnames(focus_data))]

  if (is.numeric(lagged)) {
    if (lagged != 0) {
      lagged_dataset <- focus_data %>%
        dplyr::select(dep_var, ind_vars) %>%
        dplyr::mutate_at(.vars = vars(dep_var, ind_vars),.funs = ~lag(., lagged)) %>% # lagging all features
        stats::setNames(paste0(names(.), "_lag"))  # renaming lagged features
      full_lagged_df <- dplyr::bind_cols(focus_data[, c("ID", "Time")], focus_data[, c(dep_var, ind_vars)], lagged_dataset)
    } else {
      full_lagged_df <- focus_data
    }
    return(full_lagged_df)
  } else {
    return("lagged argument needs to be zero or a positive numeric input")
  }
}

# GET INDIVIDUAL ARIMA STAT
get_indiv_arima <- function(focus_data, model="automate") {
  # assuming focus data is already lagged within run all participants
  dep_var <- colnames(focus_data)[3]
  ind_vars <- colnames(focus_data)[4:length(colnames(focus_data))]
  # removing lagged dependent variable column if its there in ind_vars
  if (paste0(dep_var, "_lag") %in% colnames(focus_data)) {
    lagged = TRUE
    ind_vars <- subset(ind_vars, !ind_vars %in% c(dep_var, paste0(dep_var, "_lag")))
  } else {
    lagged = FALSE
  }
  # object to store variable coefficients
  coef_matrix <- data.frame()
  se_matrix <- data.frame()
  phi_mat <- data.frame()
  delta_mat <- data.frame()
  theta_mat <- data.frame()

  for (ind_var in ind_vars) {
    # selecting indvar to train model
    if (lagged) {
      subset_data <- focus_data %>%
        dplyr:: select(ID, Time, dep_var, ind_var, paste0(dep_var, "_lag"))
    } else {
      subset_data <- focus_data %>%
        dplyr:: select(ID, Time, dep_var, ind_var)
    }
    # train model
    if (is.character(model)) {
      if (tolower(model) == "automate") {
        xreg_matrix <- as.matrix(subset_data[,-c(1,2,3)])
        trained_model <- auto.arima(y = subset_data[, 3], # dep_var needs to be Naffect even with a lag
                          xreg = xreg_matrix, # if lagged, ind_vars needs c(lagged_var + dep_var_lagged)
                          stepwise = FALSE,
                          approximation = FALSE)
      } else if (tolower(model) == "linear") {
        model_order = c(0, 0, 0)
        trained_model <- Arima(y = subset_data[, 3],
                     order = model_order,
                     xreg = xreg_matrix)
      }

      # TO DEBUG THE LAGGED ARIMA
      #print(names(subset_data[, -c(1,2,3)]))
      #print(trained_model$coef)

    } else {
      return("model input needs to a character string. Choose from: automate, linear, logistical")
    }
    # get standard errors
    se <- sqrt(diag(trained_model$var.coef))
    # get phi, delta and theta values
    phi <- as.vector(trained_model$trained_model$phi)
    theta <- as.vector(trained_model$trained_model$theta)
    delta <- as.vector(trained_model$trained_model$Delta)
    # get coefficient of each input variable and then their standard errors
    # if lagged is false, extract xreg
    if (!lagged){
      coef_matrix[1, ind_var] <- as.vector(coef(trained_model)['xreg'])
      se_matrix[1, ind_var] <- as.vector(se['xreg'])
      phi_mat[1, paste0(ind_var,'_phi')] <- sum(ifelse(phi != 0, 1, 0))
      theta_mat[1, paste0(ind_var, '_theta')] <- sum(ifelse(theta != 0, 1, 0))
      delta_mat[1, paste0(ind_var, '_delta')] <- sum(ifelse(delta != 0, 1, 0))
    }
    else {
      coef_matrix[1, ind_var] <- as.vector(coef(trained_model)[ind_var])
      #coef_matrix[1, paste0(ind_var,'_lag')] <- as.vector(coef(trained_model)[paste0(ind_var,'_lag')])
      se_matrix[1, ind_var] <- as.vector(se[ind_var])
      #se_matrix[1, paste0(ind_var,'_lag')] <- as.vector(se[paste0(ind_var,'_lag')])
      phi_mat[1, paste0(ind_var,'_lag_phi')] <- sum(ifelse(phi != 0, 1, 0))
      theta_mat[1, paste0(ind_var,'_lag_theta')] <- sum(ifelse(theta != 0, 1, 0))
      delta_mat[1, paste0(ind_var,'_lag_delta')] <- sum(ifelse(delta != 0, 1, 0))
    }
  }
  return(list(coef=coef_matrix, se=se_matrix, phi=phi_mat, theta=theta_mat, delta=delta_mat))
}

# GET ARIMA STATS ALL PARTICIPANTS
run_arima_full_dataset <- function(dataset, model="automate", lagged) {
  # assuming structure of full dataset is ID, Time, dep_var, ind_vars
  # scaling by ID
  dataset <- dataset %>%
    dplyr::group_by(ID) %>%
    dplyr::mutate(across(.cols=everything(), scale)) %>%
    as.vector() %>%
    as.data.frame() %>%
    ungroup()
  # setting vars/matrices
  IDs <- unique(dataset$ID)
  participants_coefficient_matrix <- data.frame()
  participants_se_matrix <- data.frame()
  participants_phi_matrix <- data.frame()
  participants_delta_matrix <- data.frame()
  participants_theta_matrix <- data.frame()

  for (id in IDs) {
    # filter for individual
    focus_data <- dataset %>%
      dplyr::filter(ID == id)
    # lag data
    lagged_df <- get_lagged_dataset(focus_data, lagged)
    # get ind arima
    ind_result <- get_indiv_arima(lagged_df, model)
    # make rownames as columns
    coef_matrix <- ind_result$coef
    se_matrix <- ind_result$se
    phi_mat <- ind_result$phi
    theta_mat <- ind_result$theta
    delta_mat <- ind_result$delta
    # append to higher matrix
    participants_coefficient_matrix <- append(participants_coefficient_matrix, list(coef_matrix))
    participants_se_matrix <- append(participants_se_matrix, list(se_matrix))
    participants_phi_matrix <- append(participants_phi_matrix, list(phi_mat))
    participants_theta_matrix <- append(participants_theta_matrix, list(theta_mat))
    participants_delta_matrix <- append(participants_delta_matrix, list(delta_mat))
  }
  # RENAME LIST NAMES
  names(participants_coefficient_matrix) <- IDs
  names(participants_se_matrix) <- IDs
  names(participants_phi_matrix) <- IDs
  names(participants_delta_matrix) <- IDs
  names(participants_theta_matrix) <- IDs
  # BIND ROWS INTO ONE DATA FRAME
  participants_coefficient_matrix <- bind_rows(participants_coefficient_matrix, .id = 'ID')
  participants_se_matrix <- bind_rows(participants_se_matrix, .id = 'ID')
  participants_phi_matrix <- bind_rows(participants_phi_matrix, .id = 'ID')
  participants_theta_matrix <- bind_rows(participants_theta_matrix, .id = 'ID')
  participants_delta_matrix <- bind_rows(participants_delta_matrix, .id = 'ID')
  return(list(se_mat = participants_se_matrix, coef_mat=participants_coefficient_matrix,
              phi=participants_phi_matrix, theta=participants_theta_matrix,
              delta=participants_delta_matrix))
}

# SUMMARISE ARIMA
format_arima <- function(results, focus_data, file_data) {
  stats <- get_variable_stat(focus_data, file_data)
  arima_sum <- tidyr::pivot_longer(results$se_mat, cols = -ID,
                                       names_to = 'variable',
                                       values_to = 'arima.SE') %>%
    dplyr::inner_join(tidyr::pivot_longer(results$coef_mat, cols = -ID,
                                          names_to = 'variable',
                                          values_to = 'arima.coef'),
                      by=c('ID', 'variable')) %>%
    mutate(Tvalue = arima.coef/arima.SE) %>%
    mutate(
      L = round(arima.coef - 2 * arima.SE, 3),
      U = round(arima.coef + 2 * arima.SE, 3),
      CI = paste0(round(arima.coef, 2), " [", L, ", ", U, "]"),
      CI_split = paste0(round(arima.coef, 2)),
      Index = 1:nrow(.),
      errcol = ifelse((L < 0 & U < 0) |
                        (L > 0 & U > 0), "Yes", "No")) %>%
    dplyr::select(Index, variable, Coef = arima.coef, SE = arima.SE,
                  L, U, CI, CI_split, errcol) %>%
    dplyr::left_join((stats %>%
            dplyr::select(variable=Variable, perc_mean = X..mean)),
          by="variable")
  return(arima_sum)
}

# BORUTA CALCULATION FUNCTION ####
run_boruta <- function(data){

  dep_var <- colnames(data)[3]
  ind_vars <- colnames(data)[4:length(colnames(data))]
  formula_str <- paste0(dep_var, '~ ', paste0(ind_vars, collapse='+'))
  model_formula <- as.formula(formula_str)
  # removing every missing values
  df <- data[complete.cases(data), ]
  # get unique IDs
  ids <- unique(df$ID)
  stat_results <- data.frame() # to get the stats
  variable_counts <- data.frame() # gets the variable decision and importance of all IDs

  for (id in unique(data$ID)){
    lborutaData <- df[df$ID == id, ]
    ##pbat
    boruta.train <- Boruta(model_formula, data=lborutaData, doTrace=0, maxruns=1000)
    # Get decision stat for each ID
    Istats <- attStats(boruta.train)
    Istats$ID <- id
    # statistics result data frame
    stat_results <- dplyr::bind_rows(stat_results, Istats)
    # variable decisions
    variable_decision <- data.frame(variable = rownames(Istats),
                                    meanImp = Istats$meanImp,
                                    decision = Istats$decision)
    # store variable names and decision
    variable_counts <- dplyr::bind_rows(variable_counts, variable_decision)
  }

  # variable decision counts
  var_decision_counts <- as.data.frame.array(
    table(variable_counts$variable, variable_counts$decision)
  )
  # Variable Mean importance
  var_mean_importance <- as.data.frame.array(
    tapply(X = variable_counts$meanImp,
           INDEX = variable_counts[c('variable','decision')],
           FUN = mean))
  # remove numbers and .... from rownames
  stat_results <- rownames_to_column(var = 'variable', stat_results) %>%
    mutate(variable = str_remove(variable, '...\\d+'))

  return (list(stat_results=stat_results,
               var_mean_importance=var_mean_importance,
               var_decision_counts=var_decision_counts))
}

# PLOTTING FOREST PLOT ####

plot_forest <- function(arima_summary) {
  dat <- arima_summary %>% # for second axis to work at the end of the ggplot code
    mutate(perc_mean_char = case_when(
      is.na(perc_mean) ~ "",
      .default = as.character(perc_mean)
    ))
  forest_plot <- arima_summary %>%
    ggplot(aes(y = Index, x = Coef)) +
    geom_point(aes(size = abs(Coef), col = errcol),
               shape = 15) +
    geom_errorbarh(aes(xmin = L, xmax = U, color = errcol),
                   height = 0.25) +
    geom_vline(
      xintercept = 0, color = "red",
      linetype = "dashed", cex = 1, alpha = 0.5 ) +
    xlab("Observed Outcome") +
    ylab("") +
    scale_color_manual(values=c("Yes" = "#009E73",
                                "No" = "red")) +
    theme(
      panel.border = element_blank(),
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black"),
      axis.text.y = element_text(size = 12,
                                 colour = "black"),
      axis.text.x.bottom = element_text(size = 12,
                                        colour = "black"),
      axis.title.x = element_text(size = 12,
                                  colour = "black"),
      legend.position = "none",
      axis.ticks.y = element_blank(),
      axis.line.y.right = element_blank(),
      axis.text.y.right = element_text(hjust = 0.5)
    ) +
    scale_y_continuous(
      breaks = 1:nrow(dat),
      labels = dat$variable,
      trans = "reverse",
      sec.axis = sec_axis( ~ .,
                           #name = "Percentile",
                           breaks = 1:nrow(dat),
                           labels = dat$perc_mean_char)
    )
  return(forest_plot)
}

plot_lagged_forest <- function(lag_arima_sum, lag_num) {

  unlag_arima_sum <- lag_arima_sum %>%
    dplyr::filter(!grepl("_lag$", variable)) %>%
    dplyr::mutate(group=paste0("Lag = 0"))

  lag_arima_sum <- lag_arima_sum %>%
    dplyr::filter(grepl("_lag$", variable)) %>%
    dplyr::mutate(group=paste0("Lag = ", as.character(lag_num)),
          variable = gsub("_lag$", "", variable))

  df = rbind(unlag_arima_sum, lag_arima_sum)

  forest_plot <- ggplot(df, aes(y = variable,
                 x = Coef,
                 xmin = Coef - 2*SE,
                 xmax = Coef + 2*SE,
                 col = errcol)) +
    geom_point() +
    geom_errorbarh(height = 0.3) +
    geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
    scale_color_manual(values=c("Yes" = "#009E73",
                                "No" = "red")) +
    theme_classic() +
    facet_grid(. ~ group, scales = "free", switch = "x") +
    labs(x = "Effect Size",
         y = "",
         title = "") +
    theme(panel.spacing = unit(0.8, "cm"),
          panel.border = element_blank(),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.text.y = element_text(size = 12,
                                     colour = "black"),
          axis.text.x.bottom = element_text(size = 12,
                                            colour = "black"),
          axis.title.x = element_text(size = 12,
                                      colour = "black"),
          legend.position = "none",
          axis.ticks.y = element_blank(),
          axis.line.y.right = element_blank(),
          axis.text.y.right = element_text(hjust = 0.5),
          strip.text.x = element_text(size = 12))
  return(forest_plot)
}

get_linear_analysis_summary <- function(arima_summary) {
  important_vars <- arima_summary %>%
    dplyr::filter(errcol == "Yes") %>%
    dplyr::select(variable) %>%
    as.list()
  if (is.na(important_vars)) {
    return("Linear analysis results produced no important processes for this individual.")
  } else {
    var_string <- paste0(important_vars$variable, collapse = ", ")
    return(paste0("Linear analysis results produced ", var_string, " as important processes for this individual."))
  }
}

get_change_analysis_summary <- function(focus_data) {

  dep_var <- colnames(focus_data)[3]

  min_time <- focus_data[focus_data[,3] == min(focus_data[,3], na.rm = T), 2]
  min_time <- min_time[!is.na(min_time)]
  max_time <- focus_data[focus_data[,3] == max(focus_data[,3], na.rm = T), 2]
  max_time <- max_time[!is.na(max_time)]

  min_var_string <- paste0(min_time, collapse = ", ")
  max_var_string <- paste0(max_time, collapse = ", ")

  #if (max_var_string != min_var_string) {
    out <- paste0("This person experienced the ",
            "highest",
            " value of ",
           dep_var,
           " on ",
           max_var_string,
           " time(s).\n",
           "This person experienced the lowest value of ",
           dep_var,
           " on ",
           min_var_string,
           " time(s).")
  #} else {
   #out <- "Nothing"
  #}
  return(out)
}

# PLOTTING RADAR CHART ####

plot_radar <- function(summary_data) {

  summary_data2 <- summary_data %>%
    mutate(grp_perc = 100,
           ind_perc = as.numeric(X..mean))

  add_closed_trace <- function(p, r, theta, ...) {
    plotly::add_trace(p, r = c(r, r[1]), theta = c(theta, theta[1]), ...)
  }

  radar_chart <-
    plot_ly(
      data = summary_data2,
      type = 'scatterpolar',
      fill = 'toself',
      mode = 'lines'
    ) %>%
    add_closed_trace(
      name = " ",
      mode = 'lines',
      r = summary_data2$grp_perc,
      theta = summary_data2$Variable,
      line=list(color='rgb(0, 0, 0)',dash='dash'),
      fillcolor='rgba(255, 255, 255, 0.01)',
      hoverinfo='skip'
    ) %>%
    add_trace(
      name = " ",
      mode = 'lines+markers+text',
      r = summary_data2$ind_perc,
      theta = summary_data2$Variable,
      name = "",
      marker = list(
        size = 10,
        line = list(
          color = 'rgb(0, 0, 0)',
          width = 1
        )
      )
    )
}


## linear importance to boruta like graph

plot_linear_imp_report <- function(arima_summary, dep_var) {

  max_x = 0.5
  min_x = -0.5

  if (max(arima_summary$Coef) > max_x) {
    max_x = max(arima_summary$Coef)
  }
  if (min(arima_summary$Coef) < min_x) {
    min_x = min(arima_summary$Coef)
  }

  p <- arima_summary %>%
    mutate(decision = case_when(
      errcol == "Yes" ~ "Important",
      errcol == "No" ~ "Not significantly important"),
      decision = factor(decision,
                        levels = c("Important", "Not significantly important")),
      xmin = Coef - 2*SE,
      xmax = Coef + 2*SE,
    ) %>%
    ggplot(aes(x = Coef,
               y = reorder(variable, Coef), # ordering the bars from high to low by the value of meanImp
               fill = decision,
               text = paste("Decision:", decision,
                            "\nCoef:",round(Coef, 3)))) + # the text argument is specific for the tooltip in plotly
    geom_col() +
    # manually changing color for each decision
    scale_fill_manual(values = c("Important" = "#87bc45",
                                 "Not significantly important" = "grey")) +
    # giving labels to x axis, y axis and legend (fill)
    labs(x = paste0("  \U2B05   more this way, less ", dep_var, "  |   more this way, more ", dep_var, "  \U2B95  "),
         y = " ", fill = " ") +
    xlim(min_x, max_x) +
    # using minimal theme
    theme_minimal() +
    theme(plot.background = element_rect(colour = "#909090", fill=NA, size=4),
                                        text = element_text(size = 15),
                                        axis.title.x = element_text(size = 12),
                                        legend.position = "none")
  ggplotly(p, tooltip = c("text"))

}

linear_imp_report <- function(arima_summary, focus_data) {
  imp_vars <- arima_summary %>%
    dplyr::filter(errcol == "Yes") %>%
    dplyr::pull(variable)

  dep_var <- colnames(focus_data)[3]

  if (length(imp_vars) == 0) {
    out <- "We do not yet have enough data to tell which processes are most important."
  } else {
    max_imp <- arima_summary %>%
      dplyr::filter(errcol == "Yes" & Coef == max(Coef, na.rm = TRUE)) %>%
      dplyr::pull(variable)
    min_imp <- arima_summary %>%
      dplyr::filter(errcol == "Yes" & Coef == min(Coef, na.rm = TRUE)) %>%
      dplyr::pull(variable)
    if (length(max_imp) > 0 & length(min_imp) > 0) {
      out <- paste0("If there is a significant process on the right, this means a positive relationship. For example, as you do more of ",
                    max_imp[1], ", ", dep_var, " goes up. \n",
                    "If there is a significant process on the left, this means a negative relationship. For example, as you do more of ",
                    min_imp[1], ", ", dep_var, " goes down. \n")
    } else if (length(max_imp) > 0 & length(min_imp) == 0) {
      out <- out <- paste0("If there is a significant process on the right, this means a positive relationship. For example, as you do more of ",
                           max_imp[1], ", ", dep_var, " goes up. \n")
    } else if (length(max_imp) == 0 & length(min_imp) > 0) {
      out <- out <- paste0("If there is a significant process on the left, this means a negative relationship. For example, as you do more of ",
                           min_imp[1], ", ", dep_var, " goes down. \n")
    }
  }
  return(out)
}

target_of_change <- function(arima_summary) {

  decrease <- arima_summary %>%
    dplyr::filter(errcol == "Yes" & Coef > 0) %>%
    dplyr::mutate(perc_diff = perc_mean - 50) %>%
    dplyr::filter(perc_diff > 0) %>%
    dplyr::filter(perc_diff == max(perc_diff, na.rm = TRUE)) %>%
    dplyr::pull(variable)

  increase <- arima_summary %>%
    dplyr::filter(errcol == "Yes" & Coef < 0) %>%
    dplyr::mutate(perc_diff = perc_mean - 50) %>%
    dplyr::filter(perc_diff < 0) %>%
    dplyr::filter(perc_diff == min(perc_diff, na.rm = TRUE)) %>%
    dplyr::pull(variable)

  if (length(decrease) > 0 & length(increase) > 0) {
    out <- paste0("For ",
                  increase[1],
                  ", it might benefit this person to increase this behaviour.\n",
                  "For ",
                  decrease[1],
                  ", it might benefit this person to decrease this behaviour.")
  } else if (length(decrease) > 0 & length(increase) == 0) {
    out <- paste0("For ",
                  decrease[1],
                  ", it might benefit this person to decrease this behaviour.")
  } else if (length(decrease) == 0 & length(increase) > 0) {
    out <- paste0("For ",
                  increase[1],
                  ", it might benefit this person to increase this behaviour.\n")
  } else {
    out <- "We do not yet have enough data to tell which processes are most important."
  }


  return(out)
}


information_theoretic_modelling <- function(focus_data) {
  dep_var <- colnames(focus_data)[3]
  #ind_vars <- colnames(focus_data)[4:length(colnames(focus_data))]
  focus_data <- focus_data[ , !(names(focus_data) %in% c("Time", "ID"))]
  for (col in colnames(focus_data)) {
    focus_data[,col] <- na_interpolation(focus_data[,col])
  }
  options(na.action = "na.fail")
  all <- lm(formula = as.formula(paste0(dep_var,  "~ .")), data=focus_data)
  dr1<-dredge(all)
  options(na.action = "na.omit")
  return(dr1)
}

get_itm_df <- function(dr1) {
  inf_mod <- subset(dr1, delta < 7)
  avg_mod <- model.avg(inf_mod)
  coef <- as.data.frame(avg_mod$coefficients[2,]) %>%
    dplyr::rename("coef" = "avg_mod$coefficients[2, ]") %>%
    dplyr::mutate(vars = rownames(.))
  cis <- as.data.frame(confint(avg_mod, full=TRUE)) %>%
    dplyr::mutate(vars = rownames(.))

  swights <- as.data.frame(sw(avg_mod)) %>%
    dplyr::rename("sw" = "sw(avg_mod)") %>%
    dplyr::mutate(vars = rownames(.))

  final <- merge(coef, cis) %>%
    merge(swights) %>%
    mutate(decision = case_when(
      `2.5 %` < 0 & `97.5 %` > 0 ~ "Not significantly important",
      `2.5 %` > 0 & `97.5 %` < 0 ~ "Not significantly important",
      `2.5 %` > 0 & `97.5 %` > 0 ~ "Important",
      `2.5 %` < 0 & `97.5 %` < 0 ~ "Important"
    ),
    decision = factor(decision,
                      levels = c("Important", "Not significantly important")))
  return(final)
}

get_itm_boruta_graph <- function(final, dep_var) {
  p <- final %>%
    ggplot(aes(x = coef,
              y = reorder(vars, sw),
              fill = decision,
              text = paste("Decision:", decision,
                            "\nCoef:", round(coef, 3)))) +
    geom_col() +
    scale_fill_manual(values = c("Important" = "#87bc45",
                                "Not significantly important" = "grey")) +
    labs(x = "Coefficient",
        y = " ",
        title = paste0("Information Theoretic Modelling (Relative Importance Rank) for the outcome ", dep_var)) +
    theme_minimal() +
    theme(text = element_text(size = 15),
          legend.position = "none")
  return(p)
}

get_itm_ci_graph <- function(final, dep_var) {
  p <- final %>%
          ggplot(aes(y = reorder(vars, sw), x = coef)) +
            geom_point(aes(size = abs(coef), col = decision),
                      shape = 15) +
            geom_errorbarh(aes(xmin = `2.5 %`, xmax = `97.5 %`, color = decision),
                          height = 0.25) +
            geom_vline(
              xintercept = 0, color = "red",
              linetype = "dashed", cex = 1, alpha = 0.5 ) +
            labs(x = "Coefficient",
                  y = " ",
              title = paste0("Information Theoretic Modelling (Relative Importance Rank) for the outcome ", dep_var)) +
            scale_color_manual(values=c("Important" = "#87bc45",
                                        "Not significantly important" = "grey")) +
            theme(
              panel.border = element_blank(),
              panel.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line = element_line(colour = "black"),
              axis.text.y = element_text(size = 15,
                                        colour = "black"),
              axis.text.x.bottom = element_text(size = 15,
                                                colour = "black"),
              axis.title.x = element_text(size = 15,
                                          colour = "black"),
              legend.position = "none",
              axis.ticks.y = element_blank(),
              axis.line.y.right = element_blank(),
              axis.text.y.right = element_text(hjust = 0.5)
            )
  return(p)
}

get_ordered_imp_vars <- function(final) {
  important <- final %>%
    filter(decision == "Important") %>%
    dplyr::select(vars) %>%
    pull()
  ordered_vars <- final %>%
    arrange(desc(sw)) %>%
    dplyr::select(vars) %>%
    pull()

  ordered_imp <- subset(ordered_vars, ordered_vars %in% important)
  return(ordered_imp)
}

get_ordered_notimp_vars <- function(final) {
  not_imp <- final %>%
    filter(decision != "Important") %>%
    dplyr::select(vars) %>%
    pull()
  ordered_vars <- final %>%
    arrange(desc(sw)) %>%
    dplyr::select(vars) %>%
    pull()

  ordered_notimp <- subset(ordered_vars, ordered_vars %in% not_imp)
  return(ordered_notimp)
}

plot_wellbeingchart <- function(arima_summary, dep_var) {
  low_well_being <- arima_summary %>%
    dplyr::filter(Coef < 0) %>%
    dplyr::mutate(font_size = case_when(
      errcol == "Yes" ~ 6,
      errcol == "No" ~ 3.5
    ),
    font_colour = case_when(
      errcol == "Yes" ~ "red",
      errcol == "No" ~ "orange"
    ))
  high_well_being <- arima_summary %>%
    dplyr::filter(Coef > 0) %>%
    dplyr::mutate(font_size = case_when(
      errcol == "Yes" ~ 6,
      errcol == "No" ~ 3.5
    ),
    font_colour = case_when(
      errcol == "Yes" ~ "blue",
      errcol == "No" ~ "cyan"
    ))
  low <- ggplot(low_well_being,
                aes(x = reorder(variable, -Coef), y = Coef, group=1)) +
    geom_line() +
    coord_flip() +
    geom_text(aes(label = variable, size = font_size, colour = font_colour), hjust=-.15) +
    scale_size(range = c(3, 6), guide = F) +
    scale_colour_manual(values=c("orange", "red"))+
    xlab("") + ylab(paste0("Is negatively related to ", dep_var)) +
    theme_minimal() +
    scale_y_continuous(breaks = c(-.5, -.4, -.3,-.2,-.1),
                       labels = c(-.5, -.4, -.3,-.2,-.1))+
    theme(axis.text.y = element_blank(),
          axis.title.x = element_text(hjust=1, size=14),
          axis.ticks = element_blank(),
          panel.grid.major.y = element_blank(),
          axis.line.x.bottom = element_line(size=1),
          legend.position = "none") +
    expand_limits(y = 0.5)


  high <- ggplot(high_well_being,
                 aes(x = reorder(variable, Coef), y = Coef, group=1)) +
    geom_line() +
    coord_flip() +
    geom_text(aes(label = variable, size = font_size, colour = font_colour), hjust=1.15) +
    scale_size(range = c(3, 6), guide = F) +
    scale_colour_manual(values=c("blue", "cyan"))+
    xlab("") + ylab(paste0("Is positively related to ", dep_var)) +
    theme_minimal() +
    scale_y_continuous(breaks = c(.5, .4, .3,.2,.1),
                       labels = c(.5, .4, .3,.2,.1))+
    theme(axis.text.y = element_blank(),
          axis.title.x = element_text(hjust=-.01, size=14),
          axis.ticks = element_blank(),
          panel.grid.major.y = element_blank(),
          axis.line.x.bottom = element_line(size=1),
          legend.position = "none") +
    expand_limits(y = -0.5)

  grid.arrange(high, low, ncol=2)
}
