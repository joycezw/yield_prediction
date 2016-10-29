# This file contains the functions and implemented models not included in presentation

# Unused functions ---------------------------------------------
#' @description Function to plot changes in absolute error comparing to the
#' original Thompson model (model - original).
#' @param data A data frame containing yield and monthly weather data that can be
#' loaded using \code{read.data} function. \templateVar{data} must contain columns
#' \field{RID}, \field{YEAR}, \templateVar{model.names} and \templateVar{objective.field}.
#' @param model.names An array of character strings containing column names of the
#' models used to predict yield. Default is \code{c("original.thompson",
#' "modified.thompson")}
#' @param objective.field A character string indicating the column name of the measured
#' y variable. Default to \field{harvested.yield.bu.per.acre}
#' @param breaks A numeric vector giving the number of intervals into which
#' \templateVar{objective.field} is to be cut.
#' @param xlabel A character string containing X axis label. Default to
#' "Yield (bu/acre)"
#' @param ylabel A character string containing Y axis label. Default to
#' "Absolute error in predicted yield (bu/acre)"
#' @return a ggplot object that can be constructed incrementally.
#' @seealso help document for \family{ggplot2::ggplot} using \code{?gglot2::ggplot}.
#' @example h <- plot.absolute.error.boxplot(data)
plot.absolute.error.boxplot <- function(data,
                                        model.names = c("original.thompson", "modified.thompson"),
                                        objective.field = "harvested.yield.bu.per.acre",
                                        breaks = c(-Inf, seq(40, 180, by = 10), Inf),
                                        xlabel = "Yield (bu/acre)",
                                        ylabel = "Absolute error in predicted yield (bu/acre)") {
    # check input data have all required fields
    required.fields <- c("RID", "YEAR", model.names, objective.field)
    if (!all(required.fields %in% names(data))) {
        simpleError("'plot.absolute.error.boxplot' function: Input data is missing required column(s)")
    }

    # reformat data
    data.reformatted <- reshape2::melt(data, measure.vars = c(model.names))
    data.reformatted$variable <- map.displayname(data.reformatted$variable)

    # calculate absolute error
    data.reformatted$absolute.error <-
        abs(data.reformatted$value - data.reformatted[[objective.field]])

    # create ggplot object h
    h <- ggplot2::ggplot(data.reformatted,
                         ggplot2::aes(x = cut(data.reformatted[[objective.field]],
                                              breaks = breaks),
                                      y = data.reformatted$absolute.error)) +
        ggplot2::geom_boxplot(ggplot2::aes(color = factor(variable))) +
        ggplot2::xlab(xlabel) +
        ggplot2::ylab(ylabel) +
        ggplot2::theme_bw() +
        ggplot2::theme(strip.background = ggplot2::element_rect(colour = "white",
                                                                fill = "white"),
                       strip.text = ggplot2::element_text(colour = "black"),
                       legend.direction = "horizontal",
                       legend.justification = "center",
                       legend.key = ggplot2::element_rect(fill = "white",
                                                          colour = "white"),
                       legend.background = ggplot2::element_rect(fill = "white",
                                                                 colour = "white"),
                       legend.position = "bottom",
                       legend.title = ggplot2::element_blank(),
                       panel.grid.major = ggplot2::element_line(linetype = "dashed",
                                                                colour = "grey40"),
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

# Two-fold random forest ----------------------------------------
set.seed(4)
model.names <- c("original.thompson", "modified.thompson", "AB.segmented.model")
if (resolution == "CRD") model.names <- c(model.names, "rf.model")

data <- get.best.model(data, model.names = model.names) %>%
    dplyr::mutate(best.model = as.factor(best.model))

rf <- random.forest.model(get.rf.features(data, objective.field = "best.model"),
                          objective.field = "best.model")
randomForest::varImpPlot(rf, sort = TRUE, n.var = 10)
data$best.model.predicted <-
    predict(rf, get.rf.features(data, objective.field = "best.model"))
data$best.model.predicted <-
    levels(data$best.model.predicted)[data$best.model.predicted]
data$best.prediction <- NA
for (r in seq_len(nrow(data))) {
    data$best.prediction[r] <- data[r, data$best.model.predicted[r]]
}

# LASSO model ---------------------------------------------------------
x <- dplyr::select(modified.fit$model,
                   -harvested.yield.bu.per.acre) %>% as.matrix()
y <- modified.fit$model$harvested.yield.bu.per.acre
lasso.fit <- glmnet::glmnet(x = x,
                            y = y,
                            family = "gaussian",
                            nlambda = 50,
                            alpha = 1,
                            standardize = TRUE)
coef(lasso.fit, s = c(fit$lambda[23], 0.1))
plot(lasso.fit, xvar = "lambda", label = TRUE)
out.lasso <- predict(lasso.fit, newx = x,
                     s = c(fit$lambda[23], 0.1),
                     type = "link")
data$lasso <- (out.lasso[, 1] + out.lasso[, 2]) / 2

ggplot2::ggplot(reshape2::melt(data,
                               measure.vars = c("original.thompson",
                                                "modified.thompson",
                                                "lasso")),
                ggplot2::aes(x = detrended.harvested.yield.bu.per.acre,
                             y = value)) +
    ggplot2::geom_point(ggplot2::aes(color = factor(variable))) +
    ggplot2::xlab("De-trended Harvested yield (bu/acre)") +
    ggplot2::ylab("Modeled de-trended Harvested yield (bu/acre)") +
    ggplot2::geom_abline(intercept = 0,
                         slope = 1,
                         color = "gray")

# scatterplot colored by RID
if (resolution == "CRD") {
    ggplot2::ggplot(reshape2::melt(data,
                                   measure.vars = c("original.thompson",
                                                    "modified.thompson")),
                    ggplot2::aes(x = planted.yield.bu.per.acre,
                                 y = value)) +
        ggplot2::geom_point(ggplot2::aes(color = as.factor(RID),
                                         shape = as.factor(variable)), size = 2.5) +
        ggplot2::xlab("Yield (bu/acre)") +
        ggplot2::ylab("Predicted yield (modified Thompson, bu/acre)") +
        ggplot2::geom_abline(intercept = 0, slope = 1, color = "gray") +
        ggplot2::theme_linedraw()
}
