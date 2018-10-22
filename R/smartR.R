#' Spatial Management and Assessment of demersal Resources for Trawl 
#' fisheries in R
#'
#' smartR (Spatial Management and Assessment of demersal Resources for 
#' Trawl fisheries in R), a tool for assessing bio-economic feedback in 
#' different management scenarios. SMART combines information from different 
#' tasks gathered within the European Data Collection Framework on fisheries 
#' and is composed of: 1) spatial models of fishing effort, environmental 
#' characteristics and distribution of demersal resources; 2) an Artificial 
#' Neural Network which captures the relationships among these aspects in a 
#' spatially explicit way and uses them to predict resources abundances; 3) a 
#' deterministic module which analyzes the size structure of catches and the 
#' associated revenues, according to different spatially-based management 
#' 
#' @section Start Here:
#' Here are listed the most important elements of a Smart analysis. The first
#' is the GUI developed to guide and assist the user, while the other five are
#' the classes that make up the smartR package.
#' \describe{
#'   \item{\link{smart_gui}}{GUI to assist the analysis}
#'   
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