library(tercen)
library(dplyr)

library(tim)
options("tercen.workflowId" = "6015a4dd34cef273755e1a1b1500427b")
options("tercen.stepId"     = "c51e5178-b2ae-4bda-80c5-ab8ed2c1edff")

do.wilcoxtest = function(df, ...) {
  p.value = NaN
  statistic = NaN
  parameter = NaN
  df <- df[order(df$.group.labels), ]
  
  grp <- unique(df$.group.colors)
  a <- df$.y[df$.group.colors == grp[1]]
  b <- df$.y[df$.group.colors == grp[2]]
  
  result = try(wilcox.test(a, b, ...), silent = TRUE)
  if(!inherits(result, 'try-error')) {
    p.value = result$p.value
    statistic = result$statistic
    parameter = result$parameter
    if(is.null(parameter)) parameter = NaN
  } 
  return (data.frame(.ri = df$.ri[1], .ci = df$.ci[1],
                     statistic = c(statistic), parameter = c(parameter), p.value = c(p.value)))
}

ctx = tercenCtx()

if (length(ctx$colors) < 1) stop("A color factor is required.")

if(as.logical(ctx$op.value('paired'))) {
  if (length(ctx$labels) < 1) stop("Labels are required for a paired test.")
}

alternative <- "two.sided"
if(!is.null(ctx$op.value('alternative'))) alternative <- ctx$op.value('alternative')
mu <- 0.0
if(!is.null(ctx$op.value('mu'))) mu <- as.double(ctx$op.value('mu'))
paired <- FALSE
if(!is.null(ctx$op.value('paired'))) paired <- as.logical(ctx$op.value('paired'))
conf.int <- TRUE
if(!is.null(ctx$op.value('conf.int'))) conf.int <-as.logical(ctx$op.value('conf.int'))
conf.level <- 0.95
if(!is.null(ctx$op.value('conf.level'))) conf.level <- as.double(ctx$op.value('conf.level'))

df.out <- ctx %>% 
  select(.ci, .ri, .y) %>%
  mutate(.group.colors = do.call(function(...) paste(..., sep='.'), ctx$select(ctx$colors)),
         .group.labels = do.call(function(...) paste(..., sep='.'), ctx$select(ctx$labels))) %>%
  group_by(.ci, .ri) %>%
  do(do.wilcoxtest(., 
                   alternative = alternative,
                   mu = mu,
                   paired = paired,
                   conf.int = conf.int,
                   conf.level = conf.level)) %>%
  ctx$addNamespace()

df.out%>%
  ctx$save()

tim::build_test_data(res_table = df.out, ctx = ctx, test_name = "test1")
