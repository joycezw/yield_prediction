library(plyr) # DO NOT change the order of loading plyr and dplyr library
library(dplyr) # DO NOT change the order of loading plyr and dplyr library
library(stats)
library(data.table)
library(ggplot2)
library(reshape2)
library(randomForest)

# Utility functions -------------------------------------------------------

#' @description Function to set resolution to be either \field{CRD} or \field{State}.
#' @param resolution a character string containing the resolution setting
#' @return a character string containing the resolution setting
#' @example resolution <- set.resolution("CRD")
set.resolution <- function(resolution) {
    # check if resolution is properly set
    if (!(resolution %in% c("CRD", "State"))) {
        paste("'set.resolution' function: Unknown 'resolution' setting.",
              "Valid values are 'CRD' or 'State'") %>%
            simpleError()
    }

    return(resolution)
}

#' @description Function to add RID column to \templateVar{data}, an identifier column
#' containing region and scale information.
#' @param data A data frame containing yield and monthly weather data that can be
#' loaded using \code{read.data} function. \templateVar{data} must contain column named
#' in \templateVar{resolution}.
#' @param resolution a character string containing the resolution setting
#' @return a data frame containing detrended yield in an additional column \field{RID}
#' @example resolution <- set.RID(data, "CRD")
set.RID <- function(data,
                    resolution) {
    # check input
    resolution <- set.resolution(resolution = resolution)
    if (!(resolution %in% names(data))) {
        paste0("'set.RID' function: Input data is missing '", resolution, "' column") %>%
            simpleError()
    }
    if ((resolution == "CRD") && (!("State" %in% names(data)))) {
        simpleError("'set.RID' function: Input data is missing 'State' column")
    }

    # create RID column
    if (resolution == "CRD") {
        data <- data %>%
            dplyr::mutate(RID = paste(State, ", CRD", CRD, sep = "") %>% as.factor())
    } else if (resolution == "State") {
        data <- data %>%
            dplyr::mutate(RID = paste(State, " (",
                                      resolution, " level)", sep = "") %>% as.factor())
    }
    return(data)
}

#' @description Function to read yield and weather data at CRD or state resolution.
#' @param resolution A string indicating the resolution of the data to be read in.
#' Valid values are \code{"CRD"} and \code{"State"}. The filename of the yield and
#' weather data files are in the base directory in csv format. Their filenames
#' begin with "IA Data - Corn Yield - " and "IA Data - Weather - " followed by the
#' specified resolution.
#' @param study.period A two-number-numeric-array containing the begining and ending
#' year of the study period. If supplied, data outside the study period will be
#' filtered out and the time index will be included as an additional column.
#' @return a data frame containing yield and weather data.
#' @example data.State <- read.data("State", c(1960, 2006))
read.data <- function(resolution,
                      study.period) {
    # check input
    resolution <- set.resolution(resolution)

    # read yield data
    yield.filename <- paste0("IA Data - Corn Yield - ", resolution, ".csv")
    yield <- read.csv(file = yield.filename, header = TRUE, skip = 1) %>%
        dplyr::rename(harvested.yield.bu.per.acre = Harvested.Yield..bu.acre.,
                      planted.yield.bu.per.acre = Planted.Yield..bu.acre.,
                      harvested.acres = Harvested.Acres,
                      planted.acres = Planted.Acres)

    # read weather data
    weather.filename <- paste0("IA Data - Weather - ", resolution, ".csv")
    weather <- read.csv(file = weather.filename, header = TRUE, skip = 7) %>%
        reshape2::melt(measure.vars =
                           c("X1", "X2", "X3", "X4", "X5", "X6",
                             "X7", "X8", "X9", "X10", "X11", "X12"), na.rm = FALSE) %>%
        dplyr::mutate(variable = paste0(Met.Var,
                                        gsub(pattern = "X", replacement = "", variable)))
    if (resolution == "CRD") {
        weather <- weather %>%
            data.table::dcast(State + CRD + YEAR ~ variable, drop = FALSE)
    } else if (resolution == "State") {
        weather <- weather %>%
            data.table::dcast(State + YEAR ~ variable, drop = FALSE)
    }

    # merge yield and weather data
    mapping.vars <- c("State" = "State", "YEAR" = "YEAR")
    if (resolution == "CRD") mapping.vars <- c(mapping.vars, "CRD" = "CRD")
    data <- dplyr::left_join(yield, weather, by = mapping.vars)

    # create RID column
    data <- set.RID(data, resolution)

    # if study.period is supplied, apply year filter
    if (!missing(study.period)) {
        data <- data %>% dplyr::filter(YEAR %between% study.period)
    }

    # create time index column
    data <- data %>% dplyr::mutate(time.index = YEAR - min(YEAR) + 1)

    return(data)
}

#' @description Function to compute yield trend.
#' @param data.yield An array of numeric data containing yield data
#' @param data.time.index An array of numeric data containing time index, which
#' equals (end year - current year + 1).
#' @return an object of class \family{stats::lm}.
#' @seealso help document for \family{stats::lm} using \code{?stats::lm}.
#' @example fit <- fit.linear.trend(data$planted.yield.bu.per.acre, data$time.index)
fit.linear.trend <- function(data.yield,
                             data.time.index) {
    fit <- stats::lm(data.yield ~ data.time.index)
    return(fit)
}

#' @description Function to calculate de-trended yield.
#' @param data A data frame containing yield and monthly weather data that can be
#' loaded using \code{read.data} function. \templateVar{data} must contain columns
#' \field{RID}, \field{planted.yield.bu.per.acre}, \field{harvested.yield.bu.per.acre},
#' and \field{time.index}.
#' @param keep.params Logical. If TRUE, the computed linear trend coefficient values
#' and fitted values will be attached to \templateVar{data} when returned. Column
#' names of the added coefficients are \code{c("b.harvested",
#' "linear.harvested.yield.trend", "b.planted", "linear.planted.yield.trend")}.
#' Default to FALSE.
#' @return a data frame containing detrended yield in additional columns named
#' \field{detrended.harvested.yield.bu.per.acre} and
#' \field{detrended.planted.yield.bu.per.acre}
#' @references Tannura, Michael A., Scott H. Irwin, and Darrel L. Good. "Weather,
#' technology, and corn and soybean yields in the US corn belt." Technology, and Corn
#' and Soybean Yields in the US Corn Belt (February 1, 2008) (2008).
#' \url{http://farmdoc.illinois.edu/marketing/morr/morr_08-01/morr_08-01.pdf}
#' @example data.CRD <- detrend.yield("CRD")
detrend.yield <- function(data,
                          keep.params = FALSE) {
    # check input
    required.fields <- c("harvested.yield.bu.per.acre",
                         "planted.yield.bu.per.acre",
                         "time.index",
                         "RID")
    if (!all(required.fields %in% names(data))) {
        simpleError("'detrend.yield' function: Input data is missing required column(s).")
    }

    # estimate linear trend coefficient
    data[, c("b.harvested", "linear.harvested.yield.trend",
             "b.planted", "linear.planted.yield.trend")] <- NA
    for (r in unique(data$RID)) {
        rid <- which(data$RID == r)

        # fit harvested yield
        fit.harvested.yield <- fit.linear.trend(data$harvested.yield.bu.per.acre[rid],
                                                data$time.index[rid])
        data$b.harvested[rid] <- fit.harvested.yield$coefficient[2] %>% as.numeric()
        data$linear.harvested.yield.trend[rid] <- fit.harvested.yield$fitted.values

        # fit planted yield
        fit.planted.yield <- fit.linear.trend(data$planted.yield.bu.per.acre[rid],
                                              data$time.index[rid])
        data$b.planted[rid] <- fit.planted.yield$coefficient[2] %>% as.numeric()
        data$linear.planted.yield.trend[rid] <- fit.planted.yield$fitted.values
    }

    # compute de-trended yields using equation in Pg.4
    # If study period is 1960-2006, Yt = yt + b * (47 - t); where:
    #   Yt = the de-trended yield in year t,
    #   yt = the actual yield in year t,
    #   b = the estimated linear trend coefficient,
    #   and t = a time index running from 1 to 47.
    data$detrended.harvested.yield.bu.per.acre <-
        data$harvested.yield.bu.per.acre +
        data$b.harvested * (max(data$YEAR) - data$YEAR)
    data$detrended.planted.yield.bu.per.acre <-
        data$planted.yield.bu.per.acre +
        data$b.planted * (max(data$YEAR) - data$YEAR)

    if (!keep.params) {
        data <- dplyr::select(data,
                              -b.harvested, -linear.harvested.yield.trend,
                              -b.planted, -linear.planted.yield.trend)
    }

    return(data)
}

#' @description Function to map variable column name to display name.
#' @param variable.data data containing variable column names.
#' @return data containing variable display names.
#' @example data$variable <- map.displayname(map.displayname)
map.displayname <- function(variable.data) {
    variable.data <-
        plyr::mapvalues(x = variable.data,
                        from = c("planted.yield.bu.per.acre", "linear.planted.yield.trend",
                                 "detrended.planted.yield.bu.per.acre", "planted.acres",
                                 "harvested.yield.bu.per.acre", "linear.harvested.yield.trend",
                                 "detrended.harvested.yield.bu.per.acre", "harvested.acres",
                                 "original.thompson", "modified.thompson",
                                 "rf.model", "AB.segmented.model",
                                 "in.season.ppt", "out.season.ppt", "annual.ppt",
                                 "in.season.tmean", "cdf.match.model"),
                        to = c("Planted yield", "Fitted planted yield",
                               "De-trended planted yield", "Planted acre",
                               "Yield", "Fitted yield",
                               "De-trended yield", "Harvested acre",
                               "Original Thompson", "Modified Thompson",
                               "Random forest", "Segmented model",
                               "Growing season P", "Out-of-season P",
                               "Annual P",
                               "Growing season Tmean",
                               "CDF-matching"),
                        warn_missing = FALSE)
    return(variable.data)
}

# Prediction models -------------------------------------------------------

#' @description Function to fit the original Thompson Model.
#' @param data A data frame containing yield and monthly weather data that can be
#' loaded using \code{read.data} function. \templateVar{data} must contain columns:
#' \field{RID}, \field{YEAR}, \field{ppt1}, \field{ppt2}, \field{ppt3}, \field{ppt4},
#' \field{ppt5}, \field{ppt6}, \field{ppt7}, \field{ppt8}, \field{ppt9}, \field{ppt10},
#' \field{ppt11}, \field{ppt12}, \field{tmean5}, \field{tmean6}, \field{tmean7},
#' \field{tmean8}, and \templateVar{objective.field}.
#' @param objective.field A character string indicating the column name of the
#' y variable to be fitted to. Default to \field{harvested.yield.bu.per.acre}
#' @param VERBOSE Logical. If TRUE, more information about the fit will be printed.
#' @return an object of class \family{stats::lm}.
#' @seealso help document for \family{stats::lm} using \code{?stats::lm}.
#' @reference Thompson, L.M. “Weather and Technology in the Production of Corn and
#' Soybeans.” Report No. 17, Center for Agricultural and Rural Development, Iowa
#' State University, 1963.
#' @example old.thompson <- fit.original.thompson.model(data)
fit.original.thompson.model <- function(data,
                                        objective.field = "harvested.yield.bu.per.acre",
                                        VERBOSE = FALSE) {
    # check input
    required.field = c("ppt1", "ppt2", "ppt3", "ppt4",
                       "ppt5", "ppt6", "ppt7", "ppt8",
                       "ppt9", "ppt10", "ppt11", "ppt12",
                       "tmean5", "tmean6", "tmean7", "tmean8",
                       objective.field, "RID", "YEAR")
    if (!all(required.field %in% names(data))) {
        simpleError("'original.thompson.model' function: Input data is missing required column(s).")
    }

    # calculate model parameters
    data <- data %>%
        dplyr::mutate(
            ppt9.sum.to.ppt5 = ppt9 + ppt10 + ppt11 + ppt12 + ppt1 + ppt2 + ppt3 + ppt4 + ppt5,
            ppt9.sum.to.ppt5.square = ppt9.sum.to.ppt5 * ppt9.sum.to.ppt5,
            ppt6.square = ppt6 * ppt6,
            ppt7.square = ppt7 * ppt7,
            ppt8.square = ppt8 * ppt8,
            tmean5.square = tmean5 * tmean5,
            tmean6.square = tmean6 * tmean6,
            tmean7.square = tmean7 * tmean7,
            tmean8.square = tmean8 * tmean8,
            ppt6.times.tmean6 = ppt6 * tmean6,
            ppt7.times.tmean7 = ppt7 * tmean7,
            ppt8.times.tmean8 = ppt8 * tmean8)

    # fit the original Thompson (1963) model
    old.fit <- stats::lm(data[[objective.field]] ~
                             YEAR +
                             ppt9.sum.to.ppt5 +
                             ppt9.sum.to.ppt5.square +
                             ppt6 +
                             ppt6.square +
                             ppt7 +
                             ppt7.square +
                             ppt8 +
                             ppt8.square +
                             tmean5 +
                             tmean5.square +
                             tmean6 +
                             tmean6.square +
                             tmean7 +
                             tmean7.square +
                             tmean8 +
                             tmean8.square +
                             ppt6.times.tmean6 +
                             ppt7.times.tmean7 +
                             ppt8.times.tmean8,
                         data = data)

    if (VERBOSE) summary(old.fit) %>% print() # show results

    return(old.fit)
}
original.thompson.model <- function(data,
                                    objective.field = "harvested.yield.bu.per.acre",
                                    VERBOSE = FALSE) {
    data$original.thompson <- NA
    for (r in unique(data$RID)) {
        rid <- which(data$RID == r)

        # fit model
        original.fit <- fit.original.thompson.model(data[rid, ])

        # get model prediction
        data$original.thompson[rid] <- original.fit$fitted.values
    }

    return(data)
}

#' @description Function to fit the modified Thompson Model.
#' @param data A data frame containing yield and monthly weather data that can be
#' loaded using \code{read.data} function. \templateVar{data} must contain columns
#' \field{RID}, \field{YEAR}, \field{ppt1}, \field{ppt2}, \field{ppt3}, \field{ppt4},
#' \field{ppt5}, \field{ppt6}, \field{ppt7}, \field{ppt8}, \field{ppt9}, \field{ppt10},
#' \field{ppt11}, \field{ppt12}, \field{tmean5}, \field{tmean6}, \field{tmean7},
#' \field{tmean8}, and \templateVar{objective.field}.
#' @param objective.field A character string indicating the column name of the
#' y variable to be fitted to. Default to \field{harvested.yield.bu.per.acre}
#' @param VERBOSE Logical. If TRUE, more information about the fit will be printed.
#' @return an object of class \family{stats::lm}.
#' @seealso help document for \family{stats::lm} using \code{?stats::lm}.
#' @reference Tannura, Michael A., Scott H. Irwin, and Darrel L. Good. "Weather,
#' technology, and corn and soybean yields in the US corn belt." Technology, and
#' Corn and Soybean Yields in the US Corn Belt (February 1, 2008) (2008).
#' \url{http://farmdoc.illinois.edu/marketing/morr/morr_08-01/morr_08-01.pdf}
#' @example new.thompson <- fit.modified.thompson.model(data)
fit.modified.thompson.model <- function(data,
                                        objective.field = "harvested.yield.bu.per.acre",
                                        VERBOSE = FALSE) {
    # check input
    required.fields = c("ppt1", "ppt2", "ppt3", "ppt4",
                        "ppt5", "ppt6", "ppt7", "ppt8",
                        "ppt9", "ppt10", "ppt11", "ppt12",
                        "tmean5", "tmean6", "tmean7", "tmean8",
                        objective.field, "RID", "YEAR")
    if (!all(required.fields %in% names(data))) {
        simpleError("'modified.thompson.model' function: Input data is missing required column(s).")
    }

    # calculate model parameters
    data <- data %>%
        dplyr::mutate(
            ppt9.sum.to.ppt4 = ppt9 + ppt10 + ppt11 + ppt12 + ppt1 + ppt2 + ppt3 + ppt4,
            ppt6.square = ppt6 * ppt6,
            ppt7.square = ppt7 * ppt7,
            ppt8.square = ppt8 * ppt8)

    # fit the modified Thompson model
    new.fit <- stats::lm(data[[objective.field]] ~
                             YEAR +
                             ppt9.sum.to.ppt4 +
                             ppt5 +
                             ppt6 +
                             ppt6.square +
                             ppt7 +
                             ppt7.square +
                             ppt8 +
                             ppt8.square +
                             tmean5 +
                             tmean6 +
                             tmean7 +
                             tmean8,
                         data = data)

    if (VERBOSE) summary(new.fit) %>% print() # show results

    return(new.fit)
}

modified.thompson.model <- function(data,
                                    objective.field = "harvested.yield.bu.per.acre",
                                    VERBOSE = FALSE) {
    data$modified.thompson <- NA
    for (r in unique(data$RID)) {
        rid <- which(data$RID == r)

        # fit model
        modified.fit <- fit.modified.thompson.model(data[rid, ])

        # get model prediction
        data$modified.thompson[rid] <- modified.fit$fitted.values
    }

    return(data)
}

#' @description Function to get input data for random forest model.
#' @param data A data frame containing yield and monthly weather data. \templateVar{data}
#' must contain columns: \templateVar{objective.field}, \field{RID}, \field{YEAR},
#' \field{ppt1}, \field{ppt2}, \field{ppt3}, \field{ppt4}, \field{ppt5}, \field{ppt6},
#' \field{ppt7}, \field{ppt8}, \field{ppt9}, \field{ppt10}, \field{ppt11}, \field{ppt12},
#' \field{tmean5}, \field{tmean6}, \field{tmean7}, \field{tmean8},
#' \field{tmax5}, \field{tmax6}, \field{tmax7}, \field{tmax8},
#' \field{tmin5}, \field{tmin6}, \field{tmin7}, \field{tmin8}.
#' @param objective.field A character string indicating the column name of the measured
#' y variable. Default to \field{harvested.yield.bu.per.acre}
#' @return a data frame containing input weather data for random forest model.
#' @example rf.data <- get.rf.features(data)
get.rf.features <- function(data,
                            objective.field = "harvested.yield.bu.per.acre") {
    # check input
    required.fields <- c("RID", "YEAR", objective.field,
                         mapply(seq(5, 8, by = 1),
                                FUN = function(x) paste("tmax", x,
                                                        sep = "",
                                                        collapse = "")),
                         mapply(seq(5, 8, by = 1),
                                FUN = function(x) paste("tmin", x,
                                                        sep = "",
                                                        collapse = "")),
                         mapply(seq(5, 8, by = 1),
                                FUN = function(x) paste("tmean", x,
                                                        sep = "",
                                                        collapse = "")),
                         mapply(seq(1, 12, by = 1),
                                FUN = function(x) paste("ppt", x,
                                                        sep = "",
                                                        collapse = "")))
    if (!all(required.fields %in% names(data))) {
        simpleError("'get.rf.features' function: Input data is missing required column(s)")
    }

    # prepare feature data
    data <- data %>%
        dplyr::mutate(
            RID = RID,
            YEAR = as.numeric(YEAR),
            ppt9.sum.to.ppt4 = ppt9 + ppt10 + ppt11 + ppt12 + ppt1 + ppt2 + ppt3 + ppt4,
            ppt9.sum.to.ppt4.square = ppt9.sum.to.ppt4 * ppt9.sum.to.ppt4)

    weather.fields <- c(mapply(seq(5, 8, by = 1),
                               FUN = function(x) paste("ppt", x,
                                                       sep = "",
                                                       collapse = "")),
                        mapply(seq(5, 8, by = 1),
                               FUN = function(x) paste("tmax", x,
                                                       sep = "",
                                                       collapse = "")),
                        mapply(seq(5, 8, by = 1),
                               FUN = function(x) paste("tmin", x,
                                                       sep = "",
                                                       collapse = "")),
                        mapply(seq(5, 8, by = 1),
                               FUN = function(x) paste("tmean", x,
                                                       sep = "",
                                                       collapse = "")))

    data <- data[c(objective.field,
                   "YEAR",
                   "RID",
                   "ppt9.sum.to.ppt4",
                   "ppt9.sum.to.ppt4.square",
                   weather.fields)]

    # compute T and P interaction variables
    for (m in seq(5, 8, by = 1)) {
        field1 <- paste0("ppt", m, sep = "", collapse = "")

        field2 <- paste0("tmax", m, sep = "", collapse = "")
        new.field <- paste0(field1, ".times.", field2, sep = "", collapse = "")
        data[new.field] <- data[[field1]] * data[[field2]]

        field2 <- paste0("tmin", m, sep = "", collapse = "")
        new.field <- paste0(field1, ".times.", field2, sep = "", collapse = "")
        data[new.field] <- data[[field1]] * data[[field2]]

        field2 <- paste0("tmean", m, sep = "", collapse = "")
        new.field <- paste0(field1, ".times.", field2, sep = "", collapse = "")
        data[new.field] <- data[[field1]] * data[[field2]]
    }

    return(data)
}

#' @description Function to predict yield using random forest model.
#' @param data A data frame containing the training weather features and year number.
#' use \code{get.rf.features} function to obtain. must contain the column specified
#' in \templateVar{objective.field}.
#' @param training.ratio A numeric value giving the ratio of total data records
#' for training. The remaining data records will be used as test data. Default to 0.7.
#' @param objective.field A character string containing the column name of yield
#' data to be fitted. Default to \field{harvested.yield.bu.per.acre}
#' @return a randomForest object.
#' @seealso help documentation for \code{randomForest::randomForest} using
#' \code{?randomForest::randomForest}
#' @example rf <- random.forest.model(data)
random.forest.model <- function(data,
                                training.ratio = 0.7,
                                objective.field = "harvested.yield.bu.per.acre") {
    # check input
    if (!(objective.field %in% names(data))) {
        paste0("'random.forest.model' function: Input data is missing '",
               objective.field, "' column") %>%
            simpleError()
    }

    # seperate training and test data
    train_sample <- sample(nrow(data), size = nrow(data) * training.ratio)
    train_data <- data[train_sample, ]
    test_data <- data[-train_sample, ]

    # build random forest model
    rf <-
        randomForest::randomForest(y = train_data[, objective.field],
                                   x = train_data[, setdiff(names(train_data),
                                                            objective.field)],
                                   ytest = test_data[, objective.field],
                                   xtest = test_data[, setdiff(names(test_data),
                                                               objective.field)],
                                   ntree = 50,
                                   importance = TRUE,
                                   keep.forest = TRUE)

    return(rf)
}

#' @description Function to prepare training y data for logistic models. For each data
#' record, this is a categorical value giving the name of the best model with the
#' smallest absolute error.
#' @param data A data frame containing the training weather features and year number.
#' This can be computed using \code{get.rf.features} function.
#' @param model.names An array of character strings containing column names of the
#' models used to predict yield. Default to \code{c("original.thompson",
#' "modified.thompson")} and columns in \templateVar{data} with names ending in \code{".model"}
#' @param objective.field A character string containing the column name of yield
#' data to be fitted. Default to \field{harvested.yield.bu.per.acre}
#' @return a data frame containing detrended yield in additional columns named
#' \field{best.model}.
#' @example data <- get.best.model(data)
get.best.model <- function(data,
                           model.names = c("original.thompson", "modified.thompson",
                                           names(data)[grep("*.model", names(data))]),
                           objective.field = "harvested.yield.bu.per.acre") {
    # check input
    required.fields <- c(model.names, objective.field)
    if (!all(required.fields %in% names(data))) {
        simpleError("'get.best.model' function: Input data is missing required column(s)")
    }

    # calculate absolute error for each model
    data.copy <- data
    for (var in model.names) {
        data.copy[var] <- abs(data[var] - data[objective.field])
    }

    # find the model with smallest mean absolute error (MAE) among model.names
    data$best.model <- model.names[apply(data.copy[model.names], 1, which.min)]

    return(data)
}

# Plot functions ----------------------------------------------------------

#' @description Function to plot predicted yield against training data.
#' @param data A data frame containing yield and monthly weather data that can be
#' loaded using \code{read.data} function. \templateVar{data} must contain columns
#' \field{RID}, \field{YEAR}, \templateVar{model.names} and \templateVar{objective.field}.
#' @param model.names An array of character strings containing column names of the
#' models used to predict yield. Default to \code{c("original.thompson",
#' "modified.thompson")}
#' @param objective.field A character string indicating the column name of the measured
#' y variable. Default to \field{harvested.yield.bu.per.acre}
#' @param xlabel A character string containing X axis label. Default to
#' "Yield (bu/acre)"
#' @param ylabel A character string containing Y axis label. Default to
#' "Predicted yield (bu/acre)"
#' @return a ggplot object that can be constructed incrementally.
#' @seealso help document for \family{ggplot2::ggplot} using \code{?gglot2::ggplot}.
#' @example h <- plot.scatterpoints(data)
plot.scatterpoints <- function(data,
                               model.names = c("original.thompson",
                                               "modified.thompson"),
                               objective.field = "harvested.yield.bu.per.acre",
                               xlabel = "Yield (bu/acre)",
                               ylabel = "Predicted yield (bu/acre)") {
    # check input data have all required fields
    required.fields <- c("RID", "YEAR", model.names, objective.field)
    if (!all(required.fields %in% names(data))) {
        simpleError("'plot.scatterpoints' function: Input data is missing required column(s)")
    }

    # reformat data
    data.reformatted <- reshape2::melt(data, measure.vars = c(model.names))
    data.reformatted$variable <- map.displayname(data.reformatted$variable)

    # create ggplot object h
    h <- ggplot2::ggplot(data = data.reformatted,
                         aes(x = data.reformatted[[objective.field]],
                             y = data.reformatted$value)) +
        ggplot2::geom_point(aes(color = factor(variable))) +
        ggplot2::xlab(xlabel) +
        ggplot2::ylab(ylabel) +
        ggplot2::geom_abline(intercept = 0, slope = 1, color = "gray") +
        ggplot2::theme_bw() +
        ggplot2::theme(strip.background = ggplot2::element_rect(colour = "white", fill = "white"),
                       strip.text = ggplot2::element_text(colour = "black"),
                       legend.direction = "horizontal",
                       legend.justification = "center",
                       legend.key = ggplot2::element_rect(fill = "white", color = "white"),
                       legend.background = ggplot2::element_rect(fill = "white", colour = "white"),
                       legend.position = "bottom",
                       legend.title = ggplot2::element_blank(),
                       panel.grid.major = ggplot2::element_line(linetype = "dashed", colour = "grey40"),
                       panel.grid.minor = ggplot2::element_blank(),
                       axis.text = ggplot2::element_text(colour = "black"))

    # if multiple districts, plot in multiple facets
    if (nlevels(data.reformatted$RID) > 1) {
        facet.ncol <- nlevels(data.reformatted$RID) %>% sqrt() %>% as.integer()
        h <- h + ggplot2::facet_wrap(~RID, ncol = facet.ncol)
    }

    # display ggplot
    h

    return(h)
}

#' @description Function to plot the error distribution of predicted yield.
#' @param data A data frame containing yield and monthly weather data that can be
#' loaded using \code{read.data} function. \templateVar{data} must contain columns
#' \field{RID}, \field{YEAR}, \templateVar{model.names} and \templateVar{objective.field}.
#' @param model.names An array of character strings containing column names of the
#' models used to predict yield. Default is \code{c("original.thompson",
#' "modified.thompson")}
#' @param objective.field A character string indicating the column name of the measured
#' y variable. Default to \field{harvested.yield.bu.per.acre}
#' @param xlabel A character string containing X axis label. Default to
#' \code{"Error in predicted yield (model-obs, bu/acre)"}.
#' @param show.curve Logical. If TRUE, figure will show density curve. Default to FALSE.
#' @return a ggplot object that can be constructed incrementally.
#' @seealso help document for \family{ggplot2::ggplot} using \code{?gglot2::ggplot}.
#' @example h <- plot.error.histogram(data)
plot.error.histogram <- function(data,
                                 model.names = c("original.thompson", "modified.thompson"),
                                 objective.field = "harvested.yield.bu.per.acre",
                                 xlabel = "Error in predicted yield (prediction - actual, bu/acre)",
                                 show.curve = FALSE) {
    # check input data have all required fields
    required.fields <- c("RID", "YEAR", model.names, objective.field)
    if (!all(required.fields %in% names(data))) {
        simpleError("'plot.error.histogram' function: Input data is missing required column(s)")
    }

    # reformat data
    data.reformatted <- reshape2::melt(data, measure.vars = c(model.names))
    data.reformatted$variable <- map.displayname(data.reformatted$variable)

    # calculate error = model prediction - objective.field
    data.reformatted <- data.reformatted %>%
        dplyr::mutate(yield.error = as.numeric(data.reformatted$value -
                                                   data.reformatted[[objective.field]]),
                      variable = as.factor(variable))

    # create ggplot object h
    h <- ggplot2::ggplot(data.reformatted, ggplot2::aes(yield.error, fill = variable))

    if (show.curve) {
        h <- h +
            ggplot2::geom_histogram(ggplot2::aes(y = ..density..,
                                                 fill = factor(variable)),
                                    position = "identity",
                                    color = "white",
                                    alpha = 0.4,
                                    bins = 30) +
            ggplot2::geom_density(ggplot2::aes(y = ..density.., color = variable),
                                  fill = NA,
                                  size = 1,
                                  alpha = 1)
    } else {
        h <- h +
            ggplot2::geom_histogram(ggplot2::aes(y = ..count..,
                                                 fill = factor(variable)),
                                    position = "identity",
                                    color = "white",
                                    alpha = .4,
                                    bins = 20)
    }
    h <- h +
        ggplot2::xlab(xlabel) +
        ggplot2::ylab("Density") +
        ggplot2::theme_bw() +
        ggplot2::theme(strip.background = ggplot2::element_rect(colour = "white", fill = "white"),
                       strip.text = ggplot2::element_text(colour = "black"),
                       legend.direction = "horizontal",
                       legend.justification = "center",
                       legend.key = ggplot2::element_rect(fill = "white", color = "white"),
                       legend.background = ggplot2::element_rect(fill = "white", colour = "white"),
                       legend.position = "bottom",
                       legend.title = ggplot2::element_blank(),
                       panel.grid.major = ggplot2::element_line(linetype = "dashed", colour = "grey40"),
                       panel.grid.minor = ggplot2::element_blank(),
                       axis.text = ggplot2::element_text(colour = "black"))

    # if multiple districts, plot in multiple facets
    if (nlevels(data.reformatted$RID) > 1) {
        facet.ncol <- nlevels(data.reformatted$RID) %>% sqrt() %>% as.integer()
        h <- h + ggplot2::facet_wrap(~RID, ncol = facet.ncol)
    }

    # display ggplot
    h

    return(h)
}

#' @description Function to plot time series of a variable for each region. The figures will
#' be plotted in multiple facets grouped by \templateVar{data}$\field{RID}.
#' @param data A data frame containing yield and monthly weather data that can be
#' loaded using \code{read.data} function. \templateVar{data} must contain columns
#' \field{RID}, \field{YEAR} and \templateVar{var.names}.
#' @param var.names An array of character strings containing column names of variables
#' to be plotted. Default to \code{c("original.thompson", "modified.thompson")}
#' @param ylabel A character string containing Y axis label. Default to
#' "Predicted yield (bu/acre)"
#' @param study.period A two-number-numeric-array containing the begining and ending
#' year of the study period. If supplied, will be used to set x axis limit.
#' @return a ggplot object that can be constructed incrementally.
#' @seealso help document for \family{ggplot2::ggplot} using \code{?gglot2::ggplot}.
#' @example h <- plot.time.series(data)
plot.time.series <- function(data,
                             var.names = c("original.thompson", "modified.thompson"),
                             var.displaynames,
                             ylabel = "Predicted yield (bu/acre)",
                             study.period) {
    # check input
    required.fields <- c("RID", "YEAR", var.names)
    if (!all(required.fields %in% names(data))) {
        simpleError("'plot.time.series' function: Input data is missing required column(s)")
    }
    if (!missing(var.displaynames))
        if (length(var.names) != length(var.displaynames))
            simpleError("'plot.time.series' function: 'var.names' and 'var.displaynames' must have the same length.")

    data.reformatted <- reshape2::melt(data, measure.vars = var.names)

    data.reformatted$variable <- map.displayname(data.reformatted$variable)
    if (!missing(var.displaynames)) {
        data.reformatted$variable <-
            plyr::mapvalues(x = data.reformatted$variable,
                            from = var.names,
                            to = var.displaynames,
                            warn_missing = FALSE)
    }

    h <- ggplot2::ggplot(data = data.reformatted,
                         ggplot2::aes(x = YEAR, y = value)) +
        ggplot2::geom_line(ggplot2::aes(color = factor(variable), width = 2)) +
        ggplot2::xlab("Year") +
        ggplot2::ylab(ylabel) +
        ggplot2::theme_bw() +
        ggplot2::theme(strip.background = ggplot2::element_rect(colour = "white", fill = "white"),
                       strip.text = ggplot2::element_text(colour = "black"),
                       legend.direction = "horizontal",
                       legend.justification = "center",
                       legend.key = ggplot2::element_rect(fill = "white", color = "white"),
                       legend.background = ggplot2::element_rect(fill = "white", colour = "white"),
                       legend.position = "bottom",
                       legend.title = ggplot2::element_blank(),
                       panel.grid.major = ggplot2::element_line(linetype = "dashed", colour = "grey40"),
                       panel.grid.minor = ggplot2::element_blank(),
                       axis.text = ggplot2::element_text(colour = "black"))

    # set xlimit and xbreaks
    if (missing(study.period)) {
        xlimits <- c(min(data.reformatted$YEAR), max(data.reformatted$YEAR))
    } else {
        xlimits <- c(min(study.period), max(study.period))
    }
    all.years <- seq(1000, 9999, by = 10)
    xbreaks <- all.years[all.years %>%
                             between(lower = min(xlimits),
                                     upper = max(xlimits),
                                     incbounds = TRUE)]
    h <- h + ggplot2::scale_x_discrete(drop = TRUE,
                                       limits = xbreaks)

    # if multiple districts, plot in multiple facets
    if (nlevels(data.reformatted$RID) > 1) {
        facet.ncol <- nlevels(data.reformatted$RID) %>% sqrt() %>% as.integer()
        h <- h + ggplot2::facet_wrap(~RID, ncol = facet.ncol)
    }

    # display ggplot
    h

    return(h)
}
