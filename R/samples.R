#    Copyright 2022 Australian Institute of Marine Science
#
#    Licensed under the Apache License, Version 2.0 (the "License");
#    you may not use this file except in compliance with the License.
#    You may obtain a copy of the License at
#
#       https://www.apache.org/licenses/LICENSE-2.0
#
#    Unless required by applicable law or agreed to in writing, software
#    distributed under the License is distributed on an "AS IS" BASIS,
#    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#    See the License for the specific language governing permissions and
#    limitations under the License.

xhcsamples <- function(x, args, what) {
  if(grepl("^ssd_p", what)) {
    args$q <- x
  } else {
    args$p <- x
  }
  samples <- do.call(what, args)
  samples
}

hcsamples <- function(estimates, what, x, .names = NULL) {
  args <- transpose(estimates, .names = .names)
  args <- purrr::map_depth(args, 2, function(x) {if(is.null(x)) NA_real_ else x})
  args <- lapply(args, as.double)
  x <- lapply(x, xhcsamples, args, what)
}

weighted_samples <- function(x, w, ...){
  min_nboot <- min(unlist(lapply(x, FUN = function(y){
    sapply(y, length)
  })))
  lapply(1:length(x[[1]]), FUN = function(i){
    lapply(1:length(x), FUN = function(y){
      sample(x=x[[y]][[i]], size=round(min_nboot)*w[y]) 
    }) %>% unlist()        
  })
}
