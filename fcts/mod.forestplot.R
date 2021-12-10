#Modified version of ggforestplot: https://nightingalehealth.github.io/ggforestplot/index.html
#MIT license: ggforestplot code can be modified: https://github.com/NightingaleHealth/ggforestplot/blob/master/LICENSE
##Modfications##
#Use lower and upper CIs instead of standard error values
#This breaks ci, it's value can no longer be adjusted 
#Commented out geom_stripes too, made vline blank

mod.forestplot <- function (df, name = name, estimate = estimate, 
                            ci.lower = ci.lower, ci.upper = ci.upper,
                            #se = se,
                            pvalue = NULL, 
                            colour = NULL, shape = NULL, logodds = FALSE, psignif = 0.05, 
                            ci = 0.95, ...) 
{
  stopifnot(is.data.frame(df))
  stopifnot(is.logical(logodds))
  name <- enquo(name)
  estimate <- enquo(estimate)
  #se <- enquo(se)
  ci.lower <- enquo(ci.lower)
  ci.upper <- enquo(ci.upper)
  pvalue <- enquo(pvalue)
  colour <- enquo(colour)
  shape <- enquo(shape)
  const <- stats::qnorm(1 - (1 - ci)/2)
  df <- df %>% dplyr::mutate(`:=`(!!name, factor(!!name, levels = !!name %>% 
                                                   unique() %>% rev(), ordered = TRUE)), 
                             .xmin = 1*!!ci.lower, 
                             .xmax = 1*!!ci.upper, 
                             #.xmin = !!estimate - const * !!se, 
                             #.xmax = !!estimate + const * !!se, 
                             .filled = TRUE, 
                             .label = sprintf("%.2f", !!estimate))
  if (logodds) {
    df <- df %>% mutate(.xmin = exp(.data$.xmin), .xmax = exp(.data$.xmax), 
                        `:=`(!!estimate, exp(!!estimate)))
  }
  if (!quo_is_null(pvalue)) {
    df <- df %>% dplyr::mutate(.filled = !!pvalue < !!psignif)
  }
  g <- ggplot2::ggplot(df, aes(x = !!estimate, y = !!name))
  if (logodds) {
    g <- g + scale_x_continuous(trans = "log10", breaks = scales::log_breaks(n = 7))
  }
  g <- g + theme_forest() + scale_colour_ng_d() + scale_fill_ng_d() + 
    #geom_stripes() + 
    geom_vline(xintercept = ifelse(test = logodds,                               
                                   yes = 1, no = 0), linetype = "blank", size = 0.4, colour = "black")
  g <- g + geom_effect(ggplot2::aes(xmin = .data$.xmin, xmax = .data$.xmax, 
                                    colour = !!colour, shape = !!shape, filled = .data$.filled), 
                       position = ggstance::position_dodgev(height = 0.5)) + 
    ggplot2::scale_shape_manual(values = c(21L, 22L, 23L, 
                                           24L, 25L)) + guides(colour = guide_legend(reverse = TRUE), 
                                                               shape = guide_legend(reverse = TRUE))
  args <- list(...)
  if ("title" %in% names(args)) {
    g <- g + labs(title = args$title)
  }
  if ("subtitle" %in% names(args)) {
    g <- g + labs(subtitle = args$subtitle)
  }
  if ("caption" %in% names(args)) {
    g <- g + labs(caption = args$caption)
  }
  if ("xlab" %in% names(args)) {
    g <- g + labs(x = args$xlab)
  }
  if (!"ylab" %in% names(args)) {
    args$ylab <- ""
  }
  g <- g + labs(y = args$ylab)
  if ("xlim" %in% names(args)) {
    g <- g + coord_cartesian(xlim = args$xlim)
  }
  if ("ylim" %in% names(args)) {
    g <- g + ylim(args$ylim)
  }
  g
}