#' toRMarkdown
#'
#' @param txt character: vector with lines of Moodle Markdown
#'
#' @return lines with RMarkdown
#' @export
#'
#' @examples
#' txt <- c("[image]\n",
#'          "Ein Paar hat 8 gute Bekannte, von denen die beiden 5 zum Essen einladen möchten.",
#'          "Wie viele verschiedene Reihenfolgen des Eintreffens der eingeladenen 5 Gäste gibt es?\n",
#'          ": 56",
#'          "; 120",
#'          "; 336",
#'          "; 2002",
#'          "; 6720",
#'          "; 32768",
#'          "; 40320",
#'          "; Keine Antwort ist richtig")
#' toRMarkdown(txt)          
toRMarkdown <- function(txt) {
  txt <- gsub("\\[(.*?)\\]", "\\$\\1\\$", txt)
  txt <- gsub("\\b\\_(.*?)\\_\\b", "\\*\\1\\*", txt)
  txt <- gsub("\\n: ", "\n\n* ", txt)
  txt <- gsub("\\n; ", "\n* ", txt)
  # find tables
  txt <- gsub("\\n\\t\\t", "\\|@\\|", txt)
  txt    <- strsplit(txt, "\n")[[1]]
  alphas <- strsplit(txt, "\\|@\\|")
  nalpha <- lengths(alphas)
  header <- 0
  for (i in 1:length(nalpha)) {
    if (nalpha[i]>1) {
      if (header!=nalpha[i]) {
        alphas[[i]][1] <- paste0("\n", 
                                 paste0(rep("| ", nalpha[i]-1), collapse=""), "|\n",
                                 paste0(rep("|-:", nalpha[i]-1), collapse=""), "|\n")
        alphas[[i]] <- paste0(paste0(alphas[[i]], collapse="|"), "|")
        header <- nalpha[i]
      } else {
        alphas[[i]] <- paste0(paste0(alphas[[i]], collapse="|"), "|")
      }
    } else {
      if (nalpha[i]==0) alphas[[i]] <- ''
      header <- 0
    }
  }
#  txt <- gsub("\\n\\t\\* ", "\n* ", txt)
  paste0(unlist(alphas), collapse="\n")
}