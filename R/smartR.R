#' Spatial Management and Assessment of demersal Resources for Trawl
#' fisheries in R
#'
#' smartR (Spatial Management and Assessment of demersal Resources for
#' Trawl fisheries in R), a tool for assessing bio-economic feedback in
#' different management scenarios. smartR combines information from different
#' tasks gathered within the European Data Collection Framework for the fishery
#' sector. The smartR package implements the SMART model in R, through the
#' object-oriented programming paradigm, and within this package it is possible
#' to achieve the complete set of analyses required by the SMART approach:
#' from the editing and formatting of the raw data; the construction and
#' maintenance of coherent datasets; the numerical and visual inspection of
#' the generated metadata; to the final simulation of management scenarios and
#' the forecast of their effects. The interaction between the user and the
#' application could take place through invocation of methods via the command
#' line or could be entirely committed to the graphical user interfaces (GUI).
#'
#' @section Start Here:
#' Here are listed the most important elements of a Smart analysis. The first, 
#' \link{smartRgui}, is the GUI developed to guide and assist the user,
#'  while the other five are the classes that make up the smartR package.
#'  See \link{SmartProject} for an example workflow.
#'  
#' \describe{
#'   \item{\link{smartRgui}}{GUI to assist the analysis}
#'   \item{\link{SmartProject}}{main project class}
#'   \item{\link{SurveyBySpecie}}{survey data class}
#'   \item{\link{FisheryBySpecie}}{fishery data class}
#'   \item{\link{SampleMap}}{Environment data class}
#'   \item{\link{FishFleet}}{Fleet data class}
#' }
#'
#' @docType package
#' @name smartR
NULL
