radioactive_colors <- c(`red`               = "#BC1A10", 
                        `orange`            = "#FFC901", 
                        `gorse`             = "#F7FF56", 
                        `charteuse`         = "#94FC13", 
                        `turqoise`          = "#4BE3AC", 
                        `caribbean green`   = "#00BDAA", 
                        `electric violet`   = "#951DED")
#' Function to extract drsimonj colors as hex codes
#'
#' @param ... Character names of radioactive_colors 
#'
radioactive_cols <- function(...) {
        cols <- c(...)
        
        if(is.null(cols)) 
                return(radioactive_colors)
        
        radioactive_colors[cols]
}

radioactive_palettes <- list(
        `main`  = radioactive_cols("gorse", "turqoise", "electric violet"),
        `mixed` = radioactive_cols("gorse", "charteuse", "turqoise", 
                                   "caribbean green", "electric violet"),
        `hot`   = radioactive_cols("red", "orange")
)

#' Return function to interpolate a radioactive color palette
#'
#' @param palette Character name of palette in radioactive_colors
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#'
radioactive_pal <- function(palette = "main", reverse = FALSE, ...) {
        pal <- radioactive_palettes[[palette]]
        
        if(reverse) pal <- rev(pal)
        
        colorRampPalette(pal, ...)
}

#' Color scale constructor for radioactive colors
#'
#' @param palette Character name of palette in radioactive_colors
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to \code{discrete_scale()} or
#'            \code{scale_color_gradientn()}, used respectively when discrete is
#'            TRUE or FALSE
#'
scale_color_radioactive <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
        pal <- radioactive_pal(palette = palette, reverse = reverse)
        
        if(discrete) {
                ggplot2::discrete_scale("color", paste0("radioactive_", palette), 
                                        palette = pal, ...)
        } else {
                ggplot2::scale_color_gradientn(colours = pal(256), ...)
        }
}
        
#' Fill scale constructor for radioactive colors
#'
#' @param palette Character name of palette in radioactive_colors
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'   scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#'   
scale_fill_radioactive <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
        pal <- radioactive_pal(palette = palette, reverse = reverse)
        
        if(discrete) {
                ggplot2::discrete_scale("fill", paste0("radioactive_", palette), 
                                        palette = pal, ...)
        } else {
                ggplot2::scale_fill_gradientn(colours = pal(256), ...)
        }
}
