rm(list = ls()) # Clear workspace
setwd("/Users/wzhan/Dropbox/Workspace/")
source("utils.R") # Source functions and libraries
library(plyr) # DO NOT change the order of loading plyr and dplyr library
library(dplyr) # DO NOT change the order of loading plyr and dplyr library
library(ggplot2)
library(reshape2)
library(rpart)
library(rpart.plot)
library(randomForest)
library(hydroGOF)

resolution <- set.resolution("CRD")

# EDA ---------------------------------------------------------------------
# read data and detrend
data <- read.data(resolution, study.period = c(1960, 2006)) %>%
    detrend.yield(keep.params = TRUE)

# display detrended yield statstics
for (r in unique(data$RID)) {
    rid <- which(data$RID == r)
    data.rid <- data[rid, ] %>%
        dplyr::mutate(in.season.tmean = 1 / 4 *
                          (tmean5 + tmean6 + tmean7 + tmean8),
                      in.season.ppt = 1 / 12 *
                          (ppt5 + ppt6 + ppt7 + ppt8))

    fit.P <- fit.linear.trend(data.rid$in.season.ppt,
                              data.rid$time.index)
    fit.T <- fit.linear.trend(data.rid$in.season.tmean,
                              data.rid$time.index)
    fit <- fit.linear.trend(data.yield = data.rid$harvested.yield.bu.per.acre,
                            data.time.index = data.rid$time.index)
    print(paste(r,
                formatC(fit$coefficients[2], 3),
                "(", formatC(fit.P$coefficients[2], 3),
                ",", formatC(fit.T$coefficients[2], 3), ")"),
          quote = FALSE)
}

# plot yield trend w/wo detrend
plot.time.series(data = data,
                 var.names = c("detrended.harvested.yield.bu.per.acre",
                               "harvested.yield.bu.per.acre",
                               "linear.harvested.yield.trend"),
                 ylabel = "Yield (bu/acre)")

# plot yield trend w/wo detrend
plot.time.series(data = data %>% dplyr::mutate(RID = State),
                 var.names = c("detrended.harvested.yield.bu.per.acre"),
                 ylabel = "Yield (bu/acre)")

# plot P trend results
plot.time.series(data = data %>%
                     dplyr::mutate(in.season.ppt = 1 / 4 *
                                       (ppt5 + ppt6 + ppt7 + ppt8)),
                 var.names = c("in.season.ppt"),
                 ylabel = "P (mm/month)")

# plot T trend results
plot.time.series(data = data %>%
                     dplyr::mutate(in.season.tmean = 1 / 4 *
                                       (tmean5 + tmean6 + tmean7 + tmean8)),
                 var.names = c("in.season.tmean"),
                 ylabel = "Average T (degC)")

# Thompson models -----------
data <- read.data(resolution, study.period = c(1960, 2006)) %>%
    original.thompson.model() %>% modified.thompson.model()

# plot yield prediction scatterplots
if (resolution == "CRD") plot.scatterpoints(data %>% dplyr::mutate(RID = State))
if (resolution == "State") plot.scatterpoints(data)

# plot error distribution
if (resolution == "CRD")
    plot.error.histogram(data %>% dplyr::mutate(RID = State), show.curve = TRUE) +
    ggplot2::xlim(-50, 50) + ggplot2::ylim(0, 0.08)
if (resolution == "State")
    plot.error.histogram(data, show.curve = TRUE) +
    ggplot2::xlim(-50, 50) + ggplot2::ylim(0, 0.08)

# print error statistics
print("Model RMSE MAE")
print(paste("Original",
            formatC(hydroGOF::rmse(data$original.thompson,
                                   data$harvested.yield.bu.per.acre), 3),
            formatC(hydroGOF::mae(data$original.thompson,
                                  data$harvested.yield.bu.per.acre), 3)))
print(paste("Modified",
            formatC(hydroGOF::rmse(data$modified.thompson,
                                   data$harvested.yield.bu.per.acre), 3),
            formatC(hydroGOF::mae(data$modified.thompson,
                                  data$harvested.yield.bu.per.acre), 3)))

# Residual analysis --------------------------------------------------
features.data <-
    data %>%
    dplyr::mutate(harvested.yield.bu.per.acre = harvested.yield.bu.per.acre,
                  residual.original = original.thompson - harvested.yield.bu.per.acre,
                  residual.modified = modified.thompson - harvested.yield.bu.per.acre) %>%
    dplyr::select(harvested.yield.bu.per.acre, residual.original, residual.modified,
                  YEAR,
                  tmin1, tmin2, tmin3, tmin4, tmin5, tmin6,
                  tmin7, tmin8, tmin9, tmin10, tmin11, tmin12,
                  tmax1, tmax2, tmax3, tmax4, tmax5, tmax6,
                  tmax7, tmax8, tmax9, tmax10, tmax11, tmax12,
                  tmean1, tmean2, tmean3, tmean4, tmean5, tmean6,
                  tmean7, tmean8, tmean9, tmean10, tmean11, tmean12,
                  ppt1, ppt2, ppt3, ppt4, ppt5, ppt6, ppt7, ppt8, ppt9, ppt10, ppt11, ppt12)
tree <- rpart::rpart(residual.modified ~ .,
                     features.data %>% dplyr::select(-residual.original,
                                                     -harvested.yield.bu.per.acre),
                     control = rpart::rpart.control(maxdepth = 1),
                     parms = list(prior = c(0.6, 0.4)))
rpart.plot::prp(tree)

# Scaling effect ----------------------------------------------------------
# plot CRD aggregated State level prediction
data <-
    read.data("CRD", study.period = c(1960, 2006)) %>%
    original.thompson.model() %>% modified.thompson.model() %>%
    dplyr::mutate(RID, State, YEAR, harvested.acres,
                  harvested.yield.bu.per.acre = harvested.yield.bu.per.acre * harvested.acres,
                  original.thompson = original.thompson * harvested.acres,
                  modified.thompson = modified.thompson * harvested.acres) %>%
    dplyr::group_by(State, YEAR) %>%
    dplyr::summarise(harvested.yield.bu.per.acre =
                         sum(harvested.yield.bu.per.acre) / sum(harvested.acres),
                     original.thompson = sum(original.thompson) / sum(harvested.acres),
                     modified.thompson = sum(modified.thompson) / sum(harvested.acres))
plot.error.histogram(data %>% as.data.frame(),
                     model.names = c("original.thompson", "modified.thompson"),
                     objective.field = "harvested.yield.bu.per.acre", show.curve = TRUE) +
    ggplot2::xlim(-50, 50) + ggplot2::ylim(0, 0.08)

# plot State level prediction
plot.error.histogram(read.data("State", study.period = c(1960, 2006)) %>%
                         original.thompson.model() %>% modified.thompson.model(),
                     model.names = c("original.thompson", "modified.thompson"),
                     objective.field = "harvested.yield.bu.per.acre", show.curve = TRUE) +
    ggplot2::xlim(-50, 50) + ggplot2::ylim(0, 0.08)

# aggregated error
data <- data %>% as.data.frame() %>% dplyr::group_by(CRD) %>%
    dplyr::summarise(original.rmse =
                         ((original.thompson - harvested.yield.bu.per.acre) ^ 2.) %>%
                         mean() %>% sqrt(),
                     modified.rmse =
                         ((modified.thompson - harvested.yield.bu.per.acre) ^ 2.) %>%
                         mean() %>% sqrt(),
                     original.mae =
                         abs(original.thompson - harvested.yield.bu.per.acre) %>% mean(),
                     modified.mae =
                         abs(modified.thompson - harvested.yield.bu.per.acre) %>% mean())

# display error statistics
print("Model RMSE MAE")
print(paste("Original",
            formatC(hydroGOF::rmse(data$original.thompson,
                                   data$harvested.yield.bu.per.acre), 3),
            formatC(hydroGOF::mae(data$original.thompson,
                                  data$harvested.yield.bu.per.acre), 3)))
print(paste("Modified",
            formatC(hydroGOF::rmse(data$modified.thompson,
                                   data$harvested.yield.bu.per.acre), 3),
            formatC(hydroGOF::mae(data$modified.thompson,
                                  data$harvested.yield.bu.per.acre), 3)))

# Time series model: segmented model -----------------------------------------------
# find segmentation
data <- read.data("CRD", study.period = c(1960, 2006)) %>%
    original.thompson.model() %>% modified.thompson.model()
features.data <- data %>%
    dplyr::transmute(YEAR,
                     tmin1, tmin2, tmin3, tmin4, tmin5, tmin6,
                     tmin7, tmin8, tmin9, tmin10, tmin11, tmin12,
                     tmax1, tmax2, tmax3, tmax4, tmax5, tmax6,
                     tmax7, tmax8, tmax9, tmax10, tmax11, tmax12,
                     tmean1, tmean2, tmean3, tmean4, tmean5, tmean6,
                     tmean7, tmean8, tmean9, tmean10, tmean11, tmean12,
                     ppt1, ppt2, ppt3, ppt4, ppt5, ppt6,
                     ppt7, ppt8, ppt9, ppt10, ppt11, ppt12) %>% as.data.frame()
tree <- rpart::rpart((abs(data$harvested.yield.bu.per.acre -
                              data$original.thompson) -
                          abs(data$harvested.yield.bu.per.acre -
                                  data$modified.thompson)) ~ .,
                     features.data,
                     control = rpart::rpart.control(maxdepth = 1),
                     parms = list(prior = c(0.6, 0.4)))
tree
rpart.plot::rpart.plot(tree)
data$AB.segmented.model <- data$modified.thompson
if (resolution == "CRD") {
    data$AB.segmented.model[data$tmax6 >= 26.34] <-
        data$original.thompson[data$tmax6 >= 26.34]
} else if (resolution == "State") {
    data$AB.segmented.model[data$ppt5 < 73.055] <-
        data$original.thompson[data$ppt5 < 73.055]
}

# plot results
model.names <- c("original.thompson", "modified.thompson",
                 "AB.segmented.model")
data.aggregate <- data %>% dplyr::mutate(RID = State)
plot.scatterpoints(data.aggregate, model.names = model.names)
plot.error.histogram(data.aggregate, model.names = model.names,
                     show.curve = TRUE)

# Time series model: post-processing: cdf matching ---------------------------------
fit.quant <- qmap::fitQmap(obs = data$harvested.yield.bu.per.acre,
                           mod = data$modified.thompson,
                           method = "QUANT",
                           qstep = 0.01)
data$cdf.match.model <- qmap::doQmap(data$modified.thompson,
                                     fit.quant)
# plot results
model.names <- c("original.thompson", "modified.thompson",
                 "AB.segmented.model", "cdf.match.model")
plot.scatterpoints(data %>% dplyr::mutate(RID = State),
                   model.names = model.names)
plot.error.histogram(data %>% dplyr::mutate(RID = State),
                     model.names = model.names,
                     show.curve = TRUE)

# Random forest -------------------------------------------------
set.seed(1002)
rf <- data %>% get.rf.features() %>% random.forest.model()
randomForest::varImpPlot(rf, sort = TRUE, n.var = 10)

# gather prediction
data.predict <- get.rf.features(data)
data$rf.model <- predict(rf, data.predict %>%
                             dplyr::select(-harvested.yield.bu.per.acre))

# plot results
model.names <- c("original.thompson", "modified.thompson",
                 "AB.segmented.model", "cdf.match.model", "rf.model")
plot.scatterpoints(data %>% dplyr::mutate(RID = State),
                   model.names = model.names)
plot.error.histogram(data %>% dplyr::mutate(RID = State),
                     model.names = model.names,
                     show.curve = TRUE)


