library(comperank)
library(tidyverse)

fights <- readRDS(file = "~/GitHub/MMAscraper/records-3/fights_records.rds")

fights2 <- fights %>%
  filter(Result %in% c("win", "draw")) %>%
  mutate(score1 = ifelse(Result == "win", 1, 0.5),
         score2 = ifelse(Result == "win", 0, 0.5)) %>%
  select(game = match_id, player1 = Link1, score1, player2 = Link2, score2)

fights2 <- as_widecr(fights2)






######## Optimal K ########
elo_win_prob <- function(rating1, rating2, ksi = 400, ...) {
  norm_rating_diff <- (rating2 - rating1) / ksi
  
  1 / (1 + 10^norm_rating_diff)
}

elo_rate_fun2 <- function (rating1, score1, rating2, score2, K = 150, ksi = 400) {
  prob_win1 <- 1/(1 + 10^((rating2 - rating1)/ksi))
  game_res1 <- score1/(score1+score2)
  rating_delta <- K * (game_res1 - prob_win1)
  cbind(rating1 + rating_delta, rating2 - rating_delta)
}

elo_fun_gen <- function(K, ksi = 400) {
    function(rating1, score1, rating2, score2) {
      elo_rate_fun2(rating1, score1, rating2, score2, K = K, ksi = ksi)[1, ]
    }
  }


# elo_fun_gen <- function(K, ksi = 400) {
#   function(rating1, score1, rating2, score2) {
#     comperank::elo(rating1, score1, rating2, score2, K = K, ksi = ksi)[1, ]
#   }
# }

get_match_result <- function(score1, score2) {
  # There are no ties in snooker but this handles general case
  near_score <- dplyr::near(score1, score2)
  
  dplyr::if_else(near_score, 0.5, as.numeric(score1 > score2))
}

# Function to split cases between "train", "validation", and "test" types
split_cases <- function(n, props = c(0.5, 0.25, 0.25)) {
  breaks <- n * cumsum(head(props, -1)) / sum(props)
  id_vec <- findInterval(seq_len(n), breaks, left.open = TRUE) + 1
  
  c("train", "validation", "test")[id_vec]
}

fights_k <- fights2 %>%
  mutate(matchType = split_cases(n()))

fights_k_validation <- fights_k %>% filter(matchType != "test")
fights_k_test <- fights_k

# Grid for K factor
k_grid <- seq(50,200,10)



compute_goodness <- function(matches, test_type, k_vec, rate_fun_gen,
                             get_win_prob, initial_ratings = 0) {
  cat("\n")
  map_dfr(k_vec, function(cur_k) {
    # Track execution
    cat(cur_k, " ")
    matches %>%
      arrange(game) %>%
      add_iterative_ratings(
        rate_fun = rate_fun_gen(cur_k), initial_ratings = initial_ratings
      ) %>%
      left_join(y = matches %>% select(game, matchType), by = "game") %>%
      filter(matchType %in% test_type) %>%
      mutate(
        # Number of frames needed to win in a match
        framesToWin = pmax(score1, score2),
        # Probability of player 1 winning a match with `frame_to_win` frames
        # needed to win.
        winProb = get_win_prob(
          rating1 = rating1Before, rating2 = rating2Before,
          frames_to_win = framesToWin
        ),
        result = get_match_result(score1, score2),
        squareError = (result - winProb)^2
      ) %>%
      summarise(goodness = sqrt(mean(squareError)))
  }) %>%
    mutate(k = k_vec) %>%
    select(k, goodness)
}

#' A wrapper for `compute_goodness()` to be used with design matrix data.
compute_goodness_wrap <- function(matches_name, test_type, k_vec,
                                  rate_fun_gen_name, win_prob_fun_name,
                                  initial_ratings = 0) {
  matches_tbl <- get(matches_name)
  rate_fun_gen <- get(rate_fun_gen_name)
  get_win_prob <- get(win_prob_fun_name)
  
  compute_goodness(
    matches_tbl, test_type, k_vec, rate_fun_gen, get_win_prob, initial_ratings
  )
}




do_experiment <- function(test_type = c("validation", "test"),
                          rating_type = "elo",
                          k_vec = k_grid,
                          initial_ratings = 0) {
  crossing(
    testType = test_type, ratingType = rating_type
  ) %>%
    mutate(
      dataName = paste0("fights_k_", testType),
      kVec = rep(list(k_vec), n()),
      rateFunGenName = paste0(ratingType, "_fun_gen"),
      winProbFunName = paste0(ratingType, "_win_prob"),
      initialRatings = rep(list(initial_ratings), n()),
      experimentData = pmap(
        list(dataName, testType, kVec,
             rateFunGenName, winProbFunName, initialRatings),
        compute_goodness_wrap
      )
    ) %>%
    unnest(experimentData) %>%
    select(testType, ratingType, k, goodness)
}


experiment_tbl <- do_experiment()







cap_first <- function(x) {
  paste0(toupper(substring(x, 1, 1)), substring(x, 2))
}

plot_data <- experiment_tbl %>%
  unite(group, ratingType) %>%
  mutate(
    testType = cap_first(testType),
    groupName = recode(
      group, elo = "Elo"
    ),
    # Ensure preferred order. This is needed because sorting of strings will
    # give "Elo, all matches", "EloBeta, all matches", "EloBeta, official
    # matches", and "Elo, official matches" as, apperently, non-letters are
    # ignored while sorting.
    groupName = factor(groupName, levels = unique(groupName))
  )

compute_optimal_k <- . %>% group_by(testType, groupName) %>%
  slice(which.min(goodness)) %>%
  ungroup()
compute_k_labels <- . %>% compute_optimal_k() %>%
  mutate(label = paste0("K = ", k)) %>%
  group_by(groupName) %>%
  # If optimal K within future facet is on the right, it needs a little
  # adjustment to the right. If on the left - full and a little adjustment to
  # the left.
  mutate(hjust = - (k == max(k)) * 1.1 + 1.05) %>%
  ungroup()

plot_experiment_results <- function(results_tbl) {
  ggplot(results_tbl) +
    geom_hline(
      yintercept = 0.5, colour = "#AA5555", size = 0.5, linetype = "dotted"
    ) +
    geom_line(aes(k, goodness, colour = testType)) +
    geom_vline(
      data = compute_optimal_k,
      mapping = aes(xintercept = k, colour = testType),
      linetype = "dashed", show.legend = FALSE
    ) +
    geom_text(
      data = compute_k_labels,
      mapping = aes(k, Inf, label = label, hjust = hjust),
      vjust = 1.2
    ) +
    facet_wrap(~ groupName) +
    scale_colour_manual(
      values = c(Validation = "#377EB8", Test = "#FF7F00"),
      guide = guide_legend(
        title = "Experiment", reverse = TRUE,
        override.aes = list(size = 4)
      )
    ) +
    labs(
      x = "K factor", y = "Goodness of fit (RMSE)",
      title = "Best goodness of fit of Elo",
      subtitle = paste0(
        'All optimal K values from test experiment (with longer "warm up") are',
        ' lower than from validation experiment.'
      )
    ) +
    theme(title = element_text(size = 14), strip.text = element_text(size = 12))
}

plot_experiment_results(plot_data)
