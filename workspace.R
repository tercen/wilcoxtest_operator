library(tercen)
library(dplyr)

options("tercen.workflowId" = "d330322c43363eb4f9b27738ef0042b9")
options("tercen.stepId"     = "0267186b-309e-4c40-babd-6f5139f52a6c")


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
    if(is.null(parameter)) parameter = NA
  } 
  return (data.frame(.ri = df$.ri[1], .ci = df$.ci[1],
                     statistic = c(statistic), parameter = c(parameter), p.value = c(p.value)))
}

ctx = tercenCtx()

if (length(ctx$colors) < 1) stop("A color factor is required.")

if(as.logical(ctx$op.value('paired'))) {
  if (length(ctx$labels) < 1) stop("Labels are required for a paired test.")
}

df <- ctx %>% 
  select(.ci, .ri, .y) %>%
  mutate(.group.colors = do.call(function(...) paste(..., sep='.'), ctx$select(ctx$colors)),
         .group.labels = do.call(function(...) paste(..., sep='.'), ctx$select(ctx$labels))) %>%
  group_by(.ci, .ri) %>%
  do(do.wilcoxtest(., 
                   alternative = ctx$op.value('alternative'),
                   mu = as.double(ctx$op.value('mu')),
                   paired = as.logical(ctx$op.value('paired')),
                   conf.int = as.logical(ctx$op.value('conf.int')),
                   conf.level = as.double(ctx$op.value('conf.level')))) %>%
  ctx$addNamespace() %>%
  ctx$save()