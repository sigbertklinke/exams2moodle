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
  txt <- gsub("\\n\\t\\* ", "\n* ", txt)
  txt
}