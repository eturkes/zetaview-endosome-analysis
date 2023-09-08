#    This file is part of zetaview-endosome-analysis.
#    Copyright (C) 2021, 2023  Emir Turkes, Lizzie Glennon, UK DRI at UCL
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#    Emir Turkes can be contacted at emir.turkes@eturkes.com

# This file holds common functions and methods.

#' Adds download buttons and horizontal scrolling to \code{"DT::datatable"}.
#'
#' @param dt A data.table object.
#' @examples
#' datatable_download(dt = data_table)
#'
datatable_download <- function(dt) {

  datatable(
    dt,
    list(
      scrollX = TRUE, dom = "Blfrtip",
      buttons = list(
        "copy", "print",
        list(extend = "collection", buttons = c("csv", "excel", "pdf"), text = "Download")
      )
    ),
    extensions = "Buttons"
  )
}

#' @param d a data frame or tibble with columns including left, right and freq
#' @param keepdata passed through to fitdistcens
#' @param ... extra variables to pass to fitdistcens eg starting values for the estimation process
nb_size <- function(d, keepdata = TRUE){
  d_bin <- as.data.frame(d[rep(1:nrow(d), d$frequency), c("left", "right")])
  fit <- fitdistrplus::fitdistcens(d_bin, "nbinom", keepdata = keepdata)
  return(fit$estimate[["size"]])
}

#' @param d a data frame or tibble with columns including left, right and freq
#' @param keepdata passed through to fitdistcens
#' @param ... extra variables to pass to fitdistcens eg starting values for the estimation process
nb_mu <- function(d, keepdata = TRUE){
  d_bin <- as.data.frame(d[rep(1:nrow(d), d$frequency), c("left", "right")])
  fit <- fitdistrplus::fitdistcens(d_bin, "nbinom", keepdata = keepdata)
  return(fit$estimate[["mu"]])
}
