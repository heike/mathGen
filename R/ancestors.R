#' Scrape data for an academic record
#'
#' @examples
#' mathGen:::getNode(id = 15860, "John Wilder Tukey")
#' dh <- mathGen:::getNode(id = 7298, "David Hilbert")
getNode <- function(id, name) {
  url <- sprintf("http://genealogy.math.ndsu.nodak.edu/id.php?id=%s", id)
  kids <- XML::readHTMLTable(url, stringsAsFactors=FALSE)
  if (length(kids) > 0) kids <- kids[[1]]
  else kids <- NULL

  doc <- XML::htmlParse(url)
  hrefs <- XML::getNodeSet(doc, "//a")
  refName <- sapply(hrefs, XML::xmlValue)
  refIDs <- sapply(hrefs, XML::xmlGetAttr, name="href")
  hits <- grep("id.php?id=", refIDs, fixed = TRUE)
  refName <- refName[hits]
  refIDs <- refIDs[hits]
  refIDs <- gsub("id.php?id=", "", refIDs, fixed = TRUE)
  self <- grep(id, refIDs, fixed = TRUE) # zero or one child leads to NULL
  if (length(self) == 1) {
    advID <- self-1
    kidID <- self + 1
  }
  if (length(self) == 0) {
    n <- length(refIDs)
    if (!is.null(kids)) {
      kidID <- n - nrow(kids) + 1
      n <- kidID-1
    }
    advID <- n
  }

  if (advID <= 0) advisors <- NULL
  else advisors <- data.frame(name=as.character(refName[1:advID]),
                              mgID=as.numeric(refIDs[1:advID]))
  if (!is.null(kids)) {
    kids$mgID <- as.numeric(refIDs[kidID:length(refIDs)])
    kids$advisorName <- name
    kids$advisorMGID <- id
  }
  list(self = data.frame(name = name, mgID = id), advisor = advisors, advisees = kids)
}

#' Get a set number of ancestors based on name and Math Gen ID
#'
#' Starting from the given node, doctoral advisors are followed recursively for the specified number of steps. In case no advisor is known, the algorithm stops even if the number of steps is not yet reached.
#' Loops in the academic tree (brush??) are problematic - e.g. if a person was advised by two advisors, and one of them was an advisor to the other (advised by both academic brother and father). This results in duplicate entries.
#' @param id identifier given to a mathematician in the Mathematics Genealogy Project.
#' @param name character, mathematician's name.
#' @param steps integer number of number of steps to follow from the root
#' @param verbose Boolean - should results be reported during the scraping? defaults to FALSE.
#' @param siblings Boolean specifying the extent of the scrape. Are academic siblings to be reported as well?
#' @param from Math Gen ID
#' @return data frame with the following variables
#' @export
#' @examples
#' hw <- ancestry(id = 145799, "Hadley Wickham", steps = 2)
#' ancestry(id = 145799, "Hadley Wickham", steps = 2, siblings = TRUE)
#' dh <- ancestry(id=7298, "David Hilbert", steps = 5)
#' library(ggplot2)
#' library(geomnet)
#' ggplot() + geom_net(data=dh,
#'            aes(from_id=factor(advisorMGID), to_id=factor(mgID),
#'                label=advisorName), directed=TRUE) +
#'   theme_net() + xlim(c(-0.1, 1.1))
#' qplot(x = 1, y=as.numeric(as.character(Year)), data=dh, geom="point") +
#'   geom_label(aes(label=Name), alpha=0.5)
ancestry <- function(id, name, steps, siblings = FALSE, from = NULL, verbose = FALSE) {
  self <- getNode(id, name)
  if (!siblings) {
    if (!is.null(from)) {
      res <- subset(self$advisees, mgID == from)
    } else {
      res <- self$advisees
    }
    from <- id
  } else {
    res <- self$advisees
  }
  if(!is.null(res)) {
    res$step <- steps
    if (verbose) print(res)
  }
  if (steps == 0) return(res)

  if (length(self$advisor$mgID) > 0) {
    for (i in 1:length(self$advisor$mgID)) {
      resi <- ancestry(self$advisor$mgID[i], self$advisor$name[i],
                       steps = steps-1, siblings=siblings, from = from)
      res <- rbind(resi, res)
    }
  }

  return(res)
}
