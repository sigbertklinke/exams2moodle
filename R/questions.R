#' questions
#' 
#' Based on the results of  [exams2moodle::moodle_text] and a template file a RMarkdown file is created for each exercise.
#' If a template is not given then \code{system.file('template', 'question_german', package="exams2moodle")} is used.
#' If no yaml is given then no YAML header will be inserted into the generated Rmd files.
#' 
#' @param html list: result from [exams2moodle::moodle_text]
#' @param template character: file name (default: \code{NULL})
#' @param yaml list: list with yaml parameters (default: \code{NULL})
#' @param nowlen integer: length of time stamps to search for
#'
#' @return a list with RMarkdown documents
#' @export
#' @importFrom yaml as.yaml
#' @importFrom stringr str_match_all regex
#' @md
#'
#' @examples
#' x <- 1
questions <- function(html, template=NULL, yaml=NULL, nowlen=4) {
  timestamp <- function(time, question) {
    res <- str_match_all(question, sprintf(" alt=\"(%s.*?)\"", time))[[1]]
    res[,2]
  }
  # 
  replace_amp_in_math <- function(html) {
    math_mode <- str_match_all(html, regex('<span class="math display".*?\\[.*?\\]<\\/span>', dotall=TRUE))[[1]]
    if (length(math_mode)) {
      browser()
      math_repl <- gsub("&amp;", "&", math_mode, fixed = TRUE)
      for (i in 1:length(math_mode)) html <- gsub(math_mode[i], math_repl[i], html, fixed=TRUE)
    }
    browser()
    html
  }
  #
  replace_images <- function (htmli, question) {
    res <- str_match_all(question, '<img src=\"@@PLUGINFILE@@/(.*?)\"')[[1]]
    if (length(res)==0) return(question)
    fnames <- paste0("\"", res[,2], "\"")
    # browser()
    for (i in 1:length(fnames)) {
      index <- which(fnames[i]==htmli$file_attr[,2])
      img   <- sprintf('<img src="data:image/%s;%s, %s"', mime_image(res[i,2]), unquote(htmli$file_attr[index,3]),
                       htmli$file[index])
      question <- gsub(res[index,1], img, question, fixed=TRUE)
    }
    question
  }
  #
  ret <- list()
  k   <- 1
  if (is.null(template)) template <- system.file('template', 'question_german.Rmd', package="exams2moodle")
  header <- ifelse(is.null(yaml), '', as.yaml(yaml))
  for (i in 1:length(html)) {
    tcont <- header
    if (html[[i]]$type!="\"category\"") {
      tcont <- readLines(template)
      tcont <- gsub('{{name}}', html[[i]]$name, tcont, fixed=TRUE)
      tcont <- gsub('{{questiontext}}', html[[i]]$questiontext, tcont, fixed=TRUE)
      tcont <- gsub('{{generalfeedback}}', html[[i]]$generalfeedback, tcont, fixed=TRUE)
      answer <- ''
      if(!is.null(html[[i]]$answer)) {
        lend   <- '</ul>'
        answer <- '<ul>'
        if (!is.null(html[[i]]$answernumbering)) {
          lend   <- '</ol>'
          answer <- sprintf("<ol type=\"%s\">", substr(html[[i]]$answernumbering, 1, 1))
        }
        browser()
        fraction <- if(is.null(dim(html[[i]]$answer_attr))) html[[i]]$answer_attr['fraction'] else html[[i]]$answer_attr[,'fraction']
        answer <- c(answer, paste0('<li>', html[[i]]$answer, ' <b>(', unquote(fraction), ')</b> ', "</li>"))
        answer <- c(answer, lend)
      }
      tcont <- gsub('{{answer}}', paste0(answer, collapse="\n"), tcont, fixed=TRUE)   
      tcont <- paste0(tcont, collapse="\n")
      tcont <- replace_amp_in_math(tcont)
      ret[[k]] <- list(rmd=replace_images(html[[i]], tcont), 
                       now=timestamp(now(nowlen), tcont))
      k <- k+1
    }
  }
  ret
}