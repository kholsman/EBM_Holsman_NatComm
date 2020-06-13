#' moving average:
ma2    <-  function(x, n = 10){stats::filter(x, rep(1 / n, n), sides = 2)}