# predict() with the k nearest temporal patterns of each pixel: the label with the most
# votes, the share of the votes of each label, and the nearest patterns without a vote.
# The votes are tested in the package twdtw. These tests are about the maps.

# Model that keeps the time series of all samples, i.e. several patterns for each label
sample_model <- function(dc, samples) {
  twdtw_knn(x = dc,
            y = samples,
            cycle_length = 'year',
            time_scale = 'day',
            time_weight = c(steepness = 0.1, midpoint = 50))
}

# Model with one pattern for each label: the mean of the samples on each date
pattern_model <- function(dc, samples) {
  twdtw_knn(x = dc,
            y = samples,
            resampling_fun = function(data) lm(y ~ factor(x), data = data),
            cycle_length = 'year',
            time_scale = 'day',
            time_weight = c(steepness = 0.1, midpoint = 50))
}

# Distance of each pixel of a data cube to each pattern of a model, calculated with
# twdtw() one pair at a time. The pixels are in the order of the map.
distance_to_patterns <- function(dc, model) {
  image_dates <- st_get_dimension_values(dc, "time")
  pixels <- expand.grid(i = seq_len(dim(dc)[1]), j = seq_len(dim(dc)[2]))
  t(vapply(seq_len(nrow(pixels)), function(p) {
    ts <- data.frame(time = image_dates, lapply(dc, function(band) band[pixels$i[p], pixels$j[p], ]))
    vapply(model$data$observations, function(pattern) {
      as.numeric(twdtw(x = ts,
                       y = as.data.frame(pattern),
                       cycle_length = 'year',
                       time_scale = 'day',
                       time_weight = c(steepness = 0.1, midpoint = 50)))
    }, numeric(1))
  }, numeric(nrow(model$data))))
}

# Vote of the k nearest patterns of one time series, written without matrices:
# the label with the most votes and the share of the votes of each label
expected_vote <- function(distance, labels, k, weighted = FALSE) {
  nearest <- order(distance)[seq_len(k)]
  weight <- if (weighted) 1 / distance[nearest] else rep(1, k)
  votes <- tapply(weight, labels[nearest], sum)
  most_voted <- names(votes)[votes == max(votes)]
  first <- nearest[labels[nearest] %in% most_voted][1]
  share <- setNames(rep(0, length(unique(labels))), sort(unique(labels)))
  share[names(votes)] <- votes / sum(votes)
  list(label = labels[first], distance = distance[first], share = share)
}

test_that("k nearest samples classify a data cube as a vote calculated one pair at a time", {
  dc <- example_cube()
  samples <- example_samples()
  m <- sample_model(dc, samples)
  labels <- m$data$label

  # A part of the data cube keeps the test fast
  dc_part <- dc[, 1:4, 1:3, ]
  distance <- distance_to_patterns(dc_part, m)
  k <- 7

  for (vote in c("majority", "weighted")) {
    expected <- lapply(seq_len(nrow(distance)), function(p) {
      expected_vote(distance[p, ], labels, k, weighted = vote == "weighted")
    })

    # The label with the most votes and the distance to its nearest sample
    lu <- predict(m, newdata = dc_part, k = k, vote = vote)
    expect_s3_class(lu, "stars")
    expect_named(lu, c("prediction", "distance"))
    expect_equal(dim(lu), dim(dc_part)[c("x", "y")])
    expect_equal(as.character(lu$prediction), vapply(expected, function(e) e$label, character(1)))
    expect_equal(as.vector(lu$distance), vapply(expected, function(e) e$distance, numeric(1)))

    # The share of the votes of each label: one attribute for each label of the model
    share <- predict(m, newdata = dc_part, k = k, vote = vote, type = "prob")
    expect_s3_class(share, "stars")
    expect_named(share, sort(unique(labels)))
    expect_equal(dim(share), dim(dc_part)[c("x", "y")])
    for (label in names(share)) {
      expect_equal(as.vector(share[[label]]), vapply(expected, function(e) e$share[[label]], numeric(1)))
    }
    expect_equal(as.vector(Reduce(`+`, lapply(share, unclass))), rep(1, nrow(distance)))
  }

  # No vote: the labels and the distances of the k nearest samples
  lu <- predict(m, newdata = dc_part, k = k, type = "neighbors")
  expect_named(lu, c(paste0("prediction_", seq_len(k)), paste0("distance_", seq_len(k))))
  for (j in seq_len(k)) {
    nearest <- apply(distance, 1, function(d) order(d)[j])
    expect_equal(as.character(lu[[paste0("prediction_", j)]]), labels[nearest])
    expect_equal(as.vector(lu[[paste0("distance_", j)]]), distance[cbind(seq_len(nrow(distance)), nearest)])
    expect_equal(levels(lu[[paste0("prediction_", j)]]), sort(unique(labels)))
  }

  # stars::predict() passes k, vote, and type to the model and gives the same maps
  for (vote in c("majority", "weighted")) {
    for (type in c("class", "prob", "neighbors")) {
      expect_equal(suppressMessages(predict(dc_part, model = m, k = k, vote = vote, type = type)),
                   predict(m, newdata = dc_part, k = k, vote = vote, type = type))
    }
  }

  # The default is the nearest sample
  expect_equal(predict(m, newdata = dc_part, k = 1), predict(m, newdata = dc_part))
  expect_equal(as.character(predict(m, newdata = dc_part)$prediction), labels[apply(distance, 1, which.min)])
})

test_that("with one pattern for each label a vote gives the label of the nearest pattern", {
  dc <- example_cube()
  m <- pattern_model(dc, example_samples())
  dc_part <- dc[, 1:6, 1:6, ]

  lu <- predict(m, newdata = dc_part)
  expect_equal(predict(m, newdata = dc_part, k = 3), lu)
  expect_equal(predict(m, newdata = dc_part, k = 3, vote = "weighted"), lu)
  expect_equal(predict(m, newdata = dc_part, k = nrow(m$data)), lu)

  # The nearest pattern is the prediction with k = 1
  neighbors <- predict(m, newdata = dc_part, k = 2, type = "neighbors")
  expect_equal(as.character(neighbors$prediction_1), as.character(lu$prediction))
  expect_equal(as.vector(neighbors$distance_1), as.vector(lu$distance))
  expect_true(all(neighbors$distance_2 >= neighbors$distance_1))
  expect_false(any(as.character(neighbors$prediction_2) == as.character(neighbors$prediction_1)))

  # Weighted votes of all patterns give shares that are proportional to 1/distance
  distance <- distance_to_patterns(dc_part, m)
  share <- predict(m, newdata = dc_part, k = nrow(m$data), vote = "weighted", type = "prob")
  expected <- (1 / distance) / rowSums(1 / distance)
  for (i in seq_len(nrow(m$data))) {
    expect_equal(as.vector(share[[m$data$label[i]]]), expected[, i])
  }
})

test_that("a pixel without complete observations has no prediction for any k and type", {
  dc <- example_cube()
  m <- sample_model(dc, example_samples())

  # The second pixel has no observation of one band
  dc_part <- dc[, 1:3, 1:2, ]
  dc_part$BLUE[2, 1, ] <- NA

  for (k in c(1, 5)) {
    for (vote in c("majority", "weighted")) {
      lu <- predict(m, newdata = dc_part, k = k, vote = vote)
      expect_equal(which(is.na(lu$prediction)), 2)
      expect_equal(which(is.na(lu$distance)), 2)
      share <- predict(m, newdata = dc_part, k = k, vote = vote, type = "prob")
      for (label in names(share)) {
        expect_equal(which(is.na(share[[label]])), 2)
      }
    }
    neighbors <- predict(m, newdata = dc_part, k = k, type = "neighbors")
    for (attribute in names(neighbors)) {
      expect_equal(which(is.na(neighbors[[attribute]])), 2)
    }
  }
})

test_that("k, vote, and type that cannot be used are an error", {
  dc <- example_cube()
  m <- pattern_model(dc, example_samples())
  dc_part <- dc[, 1:2, 1:2, ]
  msg <- "'k' must be an integer between one and the number of temporal patterns of the model (5)"

  expect_error(predict(m, newdata = dc_part, k = 0), msg, fixed = TRUE)
  expect_error(predict(m, newdata = dc_part, k = 6), msg, fixed = TRUE)
  expect_error(predict(m, newdata = dc_part, k = 1.5), msg, fixed = TRUE)
  expect_error(predict(m, newdata = dc_part, k = NA), msg, fixed = TRUE)
  expect_error(predict(m, newdata = dc_part, k = "2"), msg, fixed = TRUE)
  expect_error(predict(m, newdata = dc_part, k = 2, vote = "mean"), "'arg' should be one of")
  expect_error(predict(m, newdata = dc_part, k = 2, vote = "none"), "'arg' should be one of")
  expect_error(predict(m, newdata = dc_part, k = 2, type = "label"), "'arg' should be one of")

  # Arguments that are not known are not ignored
  expect_error(predict(m, newdata = dc_part, K = 2), "Unknown arguments in predict: 'K'.", fixed = TRUE)

  # The dates of a data cube are in its time dimension
  expect_error(predict(m, newdata = dc_part, time = st_get_dimension_values(dc_part, "time")),
               "The argument 'time' cannot be used with a data cube", fixed = TRUE)
})
