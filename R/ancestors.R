#' Scrape data for an academic record
#'
#' @param id integer number, the identifier of a mathematician in the Mathematics Genealogy Project
#' @examples
#' \dontrun{
#' mathGen:::getNode(id = 15860) # John Wilder Tukey
#' dh <- mathGen:::getNode(id = 7298) # David Hilbert
#' }
getNode <- function(id) {
  url <- sprintf("http://genealogy.math.ndsu.nodak.edu/id.php?id=%s", id)
  tabs <- httr::GET(url)
  kids <- XML::readHTMLTable(rawToChar(tabs$content), stringsAsFactors = F)

#  kids <- XML::readHTMLTable(url, stringsAsFactors=FALSE)
  if (length(kids) > 0) kids <- kids[[1]]
  else kids <- NULL

  doc <- XML::htmlParse(tabs)
  who <- XML::getNodeSet(doc, "//h2")
  name <- trimws(XML::xmlValue(who[[1]]))
  self <- data.frame(Name = name, mgID = id, stringsAsFactors = FALSE)

  # more demographics
  spans <- XML::getNodeSet(doc, "//span")
  spanlist <- trimws(sapply(spans, XML::xmlValue))
# browser()
  # highly dependent on the format of Math Gen - very ugly
  degree_University_Year <- spanlist[1]
  self$School <- spanlist[2]
  self$Thesistitle <- spanlist[4]
  splits <- strsplit(degree_University_Year, split=" ")[[1]]
  self$Degree <- splits[[1]]
  self$Year <- as.numeric(splits[[length(splits)]])

  # get all links and parse them for Math Gen IDs
  hrefs <- XML::getNodeSet(doc, "//a")
  refName <- sapply(hrefs, XML::xmlValue)
  refIDs <- sapply(hrefs, XML::xmlGetAttr, name="href")
  hits <- grep("id.php?id=", refIDs, fixed = TRUE)
  refName <- refName[hits]
  refIDs <- refIDs[hits]
  refIDs <- gsub("id.php?id=", "", refIDs, fixed = TRUE)
  selfID <- grep(id, refIDs, fixed = TRUE) # zero or one child leads to NULL
  if (length(selfID) == 1) {
    advID <- selfID-1
    kidID <- selfID + 1
  }
  if (length(selfID) == 0) {
    n <- length(refIDs)
    if (!is.null(kids)) {
      kidID <- n - nrow(kids) + 1
      n <- kidID-1
    }
    advID <- n
  }

  if (advID <= 0) advisors <- NULL
  else advisors <- data.frame(Name=as.character(refName[1:advID]),
                              mgID=as.numeric(refIDs[1:advID]),
                              stringsAsFactors = FALSE)
  if (!is.null(kids)) {
    kids$mgID <- as.numeric(refIDs[kidID:length(refIDs)])
    kids$advisorName <- name
    kids$advisorMGID <- id
  }
  list(self = self, advisor = advisors, advisees = kids)
}
#' Get a set number of ancestors based on name and Math Gen ID
#'
#' Starting from the given node, doctoral advisors are followed recursively for the specified number of steps. In case no advisor is known, the algorithm stops even if the number of steps is not yet reached.
#' Loops in the academic tree (thicket??) are problematic - e.g. if a person was advised by two advisors, and one of them was an advisor to the other (advised by both academic brother and father). This results in duplicate entries.
#' Each call to ancestry results in multiple requests to the Mathematics Genealogy Project, so use responsibly.
#' @param id identifier given to a mathematician in the Mathematics Genealogy Project.
#' @param steps integer number of number of steps to follow from the root
#' @param verbose Boolean - should results be reported during the scraping? defaults to FALSE.
#' @return data frame with the following variables
#' \itemize{
#'   \item Name name of the mathematician
#'   \item mgID Math Gen ID
#'   \item School university granting the degree
#'   \item Thesistitle
#'   \item Degree
#'   \item Year
#'   \item advisorName
#'   \item advisorMGID Math Gen ID of the advisor(s)
#' }
#' @export
#' @examples
#' \dontrun{
#' hw <- ancestry(id = 145799, steps = 3) # Hadley Wickham
#' ancestry(id = 145799,  steps = 2, siblings = TRUE) # Hadley Wickham
#' dh <- ancestry(id=7298,  steps = 5) # David Hilbert
#'
#' library(ggplot2)
#' library(geomnet)
#' hw <- plyr::rbind.fill(data.frame(advisorMGID=145799, advisorName="Hadley Alexander Wickham"), hw)
#' ggplot() +
#'   geom_net(aes(from_id=factor(advisorMGID), to_id=factor(mgID),
#'                label=advisorName), directed=TRUE, data=hw) +
#'   theme_net() + xlim(c(-0.1, 1.1))
#' qplot(y = rep(1:10, length=nrow(dh)), x=as.numeric(as.character(Year)), label = Name,
#'       data=dh, geom="label", alpha = I(0.5))
#'}
ancestry <- function(id, steps, verbose = FALSE, includeself = TRUE) {
  res <- NULL
  if (steps < 0) return(res)

  self <- getNode(id)
#  browser()
  if (is.null(self$advisor)) return(res)

  for (i in 1:nrow(self$advisor)) {
    resi <- ancestry(self$advisor$mgID[i], steps = steps - 1,
                         verbose = verbose, includeself = TRUE)
    res <- plyr::rbind.fill(res, resi)
  }

  if (includeself) {
    self$self <- data.frame(self$self,
               advisorName = self$advisor$Name,
               advisorMGID = self$advisor$mgID)
    res <- plyr::rbind.fill(self$self, res)
  }
  return(res)
}


#' Descendants of a Mathematician
#'
#' @param id identifier given to a mathematician in the Mathematics Genealogy Project.
#' @param steps integer if specified, the number of steps to follow from the root. If NULL, all descendants will be reported.
#' @param includeself Boolean, should the person him/herself be included in the results?
#' @return data frame
#' @export
#' @examples
#' \dontrun{
#' au <- descendants(45024) # Antony Unwin
#' }
descendants <- function(id, steps = 3, includeself = TRUE) {
  if (steps < 0) return()

  self <- getNode(id)
  res <- NULL
  if (!is.null(self$advisees)) {
    res <- self$advisees
    for (i in 1:nrow(self$advisees)) {
      resi <- descendants(self$advisees$mgID[i], steps = steps-1, includeself = FALSE)
      res <- plyr::rbind.fill(res, resi)
    }
  }
  if (includeself) {
#    browser()
    self$self <- data.frame(self$self,
                            advisorName = self$advisor$Name,
                            advisorMGID = self$advisor$mgID)
    self$self <- self$self[, setdiff(names(self$self), c("Thesistitle", "Degree"))]
    res <- plyr::rbind.fill(self$self, res)
  }
  res
}
