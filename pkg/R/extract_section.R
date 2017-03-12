#' Extract numbered sections from a .Rmd document.
#'
#' This is designed for use with the homework .rmd files for the grad stats sequence in PSY at UO. 
#' All of the homework problems (question text, data, answer code, and solution text) can be stored in one .rmd file.
#' This function automatically extracts the desired questions (sections) and prints them in a new .rmd file, optionally including the solutions, preserving the header information from the original file.
#' This makes it easy for instructors to keep all of the homework material in just one file.
#' If you wish to update the approach for a particular kind of analysis, you can easily update it in all of the relevant questions at once,
#' and then quickly re-generate individual .rmd files to send to students.
#' 
#' Note that the formatting of the master .rmd file is very important. 
#' This function will not work as expected if the .rmd file is not formatted according to these specifications:
#' Each question must be its own level-1 section, beginning with \code{# } and then the question number, and a period (e.g. \code{# 1.} for the first question).
#' Note that there must be exactly one space between the \code{#} and the question number, and the question number must be followed by a period. 
#' Whitespace after the period is optional. 
#' Sub-parts to the questions must use level-2 or greater headers, but the numbering/lettering system is unimportant (e.g. \code{## a} or \code{## 1.} are both fine for a sub-section header).
#' All question text must be in bold (e.g., \code{**This is a question.**}). 
#' You can use bold in answer text, but it cannot be a whole line; for example, \code{This answer requires **emphasis**.} will be considered answer text, 
#' since the line does not begin and end with \code{**}. 
#' It is okay if there is whitespace (e.g. tab indenting) before or after the \code{**} that begin and end question lines, but no other characters.
#' 
#' When \code{solutions = TRUE}, the document header wil be included (everything up until the beginning of section 1) 
#' as will all text from the selected section(s). Any white space, etc. in the file will be preserved.
#' When \code{solutions = FALSE}, the header is included, but only the section text that is marked as being part of the question (section headers, bolded lines, and certain code chunks).
#' White space is discarded, and a newline is entered between each markdown line to preserve formatting. 
#' 
#' @param file The .rmd file to extract sections from.
#' @param sections The numbers of the sections to extract, as a vector.
#' @param dir The directory where \code{file} is saved, and where the extracted file will be written. Defaults to \code{getwd()}.
#' @param solutions Logical indicating whether or not to include solutions text. Default is TRUE.
#' 
#' @return Does not return anything, but writes a new .rmd file in \code{dir} with the extracted section(s), 
#' with either "solutions" or "questions" included in the file name and the relevant question numbers. 
#' Note that if a file of the same name already exists in \code{dir}, it will overwrite it without warning.
#'
#' @examples
#' # extract the question text for questions 1, 2, 5, and 8 from homework_all_solutions.Rmd
#' extract_sections(file = homework_all_solutions.Rmd, section=c(1,2,5,8), solutions = FALSE)
#' 
#' # extract the corresponding answer key for the above questions
#' extract_sections(file = homework_all_solutions.Rmd, section=c(1,2,5,8), solutions = TRUE)
#'
#' @export
extract_sections <- function(file, sections, dir=getwd(), solutions = TRUE){
  all <- readLines(file.path(dir, file))
  header_ends <- grep(x = all, pattern = "# 1[.]") - 1
  
  header <- all[1:header_ends]
  
  # if solutions = TRUE, make print_data = FALSE, otherwise print_data=TRUE
  header[grep(x=header, pattern = "print_data")] <- ifelse(solutions, "print_data = FALSE", "print_data = TRUE")
  
  # pull out each section
  for(s in sections){
    # find where this section starts
    section_starts <- grep(x = all, pattern = paste0("# ", s, "[.]"))
    if(!section_starts > 0) stop("Section not found. Check formatting of file.")
    
    # find where this section ends
    if(any(grepl(x = all, pattern = paste0("# ", s + 1, "[.]")))){
      # if there is a section after this one, end there
      section_ends <- grep(x = all, pattern = paste0("# ", s + 1, "[.]")) - 1
    } else {
      # otherwise, end at the end of the document
      section_ends <- length(all)
    }
    
    this_text <- all[section_starts:section_ends]
    
    if(s == section[1]){
      section_text <- this_text
    } else {
      section_text <- c(section_text, this_text)
    }
  }
  
  if(!solutions){
  # to omit the solutions and just show the questions, 
  # only keep sections, bolded text, and chunks including print_data or df[[:digit:]]+ 
    
    # drop code, except chunks with print_data or df[[:digit:]]+ in the chunk options
    all_chunk_markers <- grep(x=section_text, pattern = "```")
    keep_chunks_start <- grep(x=section_text, pattern = "```[{]r.*print_data|```[{]r.*df[[:digit:]]+")
    
    n.chunks <- length(all_chunk_markers)/2
    chunk_markers <- data.frame(line.num=all_chunk_markers, type=c("start", "end"), chunk.num = as.numeric(gl(n=n.chunks, k=2))) %>% 
      spread(type, line.num) %>% 
      mutate(keep = start %in% keep_chunks_start)
    
    for(c in chunk_markers$chunk.num){
      if(chunk_markers$keep[c]){
        # if this chunk is marked TRUE for keep, record its line numbers
        keep <- chunk_markers$start[c]:chunk_markers$end[c]
        if(c == 1) {
          keep_chunk_markers <- keep
        } else {
          keep_chunk_markers <- c(keep_chunk_markers, keep)
        }
      }
    }
    
    # drop any text that isn't part of the question (section or bolded)
    question_text_markers <- grep(x=section_text, pattern = "^[:blank:]*[*][*].*[*][*][:blank:]*$|^#+ ") # any line beginning with a section marker #, or where the whole line is bolded
    
    # remove ones that are in code chunks (these will be commments mistaken for section headers)
    for(c in chunk_markers$chunk.num){
      # remove code lines from the question_text_markers
      drop <- chunk_markers$start[c]:chunk_markers$end[c]
      question_text_markers <- question_text_markers[!question_text_markers %in% drop]
    }
    # put a blank line after each line of question text, to preserve markdown formatting
    section_text[question_text_markers] <- paste(section_text[question_text_markers], "\n")
    
    # all of the line numbers to keep
    keep_index <- sort(c(question_text_markers, keep_chunk_markers))
    
    # update the section_text to only include those line numbers
    section_text <- section_text[keep_index]
    
  } # end of if(!solutions)
  
  # append the section text to the header
  to_print <- c(header, section_text)
  
  # write to a new file
  type <- ifelse(solutions, "solution_", "question_")
  save.file <- paste0("homework_", type, paste(section, collapse = "_"), ".Rmd")
  writeLines(to_print, file.path(dir, save.file))
}