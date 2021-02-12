#' SNSF numeric grade transformation
#'
#' This function transforms the ordinal grade to a numeric grade.
#' @param grade the grade ("character")
#' @details  A = 6, AB = 5, B = 4, BC = 3, C = 2, D = 1. All other grades (COI,
#' etc) are transformed to NA.
#' @export
get_num_grade_snsf <- function(grade) {
  return(
    case_when(grade == "A" ~ 6,
              grade == "AB" ~ 5,
              grade == "B" ~ 4,
              grade == "BC" ~ 3,
              grade == "C" ~ 2,
              grade == "D" ~ 1)
  )
}

#' Individual votes to correct format
#'
#' This function transforms the default csv file with the individual votes into
#' the correct long format
#' @param individual_votes The matrix containing the individual votes.
#' @param prefix_voter The column name prefix used to indicate the voters.
#' @param delete_NA should the missing (numeric) grades be deleted?
#' (default = TRUE)
#' @param fun_grade_to_num function that translates grades to numeric values
#' @import stringr
#' @export
get_right_data_format <- function(individual_votes, prefix_voter = "voter",
                                  fun_grade_to_num = get_num_grade_snsf,
                                  delete_NA = TRUE) {
  long_data <- individual_votes %>%
    # Get the data into a long format
    pivot_longer(cols = starts_with(prefix_voter), names_to = "voter",
                 values_to = "grade") %>%
    # Make sure all the grades are upper case and convert them to a numeric
    # variable using the function specified in fun_grade_to_num:
    mutate(grade = str_to_upper(grade),
           num_grade = fun_grade_to_num(grade))
  if (delete_NA) {
    long_data <- long_data %>%
      filter(!is.na(num_grade))
  }
  return(long_data)
}


#' Load mock data
#'
#' This function loads and returns mock data
#' @param panels default to 'all', for three panels. other possibilities:
#' 'one', 'two', and 'three'.
#' @export
get_mock_data <- function(panels = "all"){
  panel1 <- tibble(application = rep(paste0("#", 1:15), each = 5),
                   voter = rep(paste0("v_", 1:5), 15),
                   grade = c(1,	1, 1,	2, 5, 1, 1,	2, 2,	6, 1,	2, 1,	3, 5,
                             1,	3, 2,	1, 5, 1, 2,	2, 3,	5, 1, 3, 1,	4, 5,
                             1,	2, 3,	4, 5, 1, 5,	2, 2,	5, 1,	3, 4,	3, 5,
                             1,	4, 2,	5, 5, 2, 3,	4, 4,	5, 2,	4, 3,	5, 5,
                             2,	5, 4,	4, 6, 2, 3,	4, 6,	6, 2,	5, 5,	4, 6)) %>%
    mutate(num_grade = 7 - grade,
           panel = "p1")

  panel2 <- tibble(application = rep(paste0("#", 16:30), each = 5),
                   voter = rep(paste0("v_", 6:10), 15),
                   grade = c(1,	1, 1,	2, 1, 1, 2,	2, 1,	1, 1,	3, 1,	1, 2,
                             2,	2, 2,	2, 2, 2, 2,	2, 3,	3, 3,	2, 3,	3, 3,
                             3,	3, 3,	3, 3, 3, 3,	4, 4,	3, 4,	3, 3,	4, 4,
                             4,	4, 4,	5, 4, 4, 4, 5, 5,	5, 4,	6, 4, 5, 5,
                             6,	5, 5,	5, 5, 5, 5,	5, 6,	6, 5,	6, 6,	6, 6)) %>%
    mutate(num_grade = 7 - grade,
           panel = "p2")

  panel3 <- tibble(application = rep(paste0("#", 31:45), each = 5),
                   voter = rep(paste0("v_", 11:15), 15),
                   grade = c(1,	1, 1,	1, 5, 2, 1, 3, 1, 5, 2,	3, 2,	1, 5,
                             2,	4, 1,	3, 5, 4, 2,	3, 3,	5, 3,	4, 4,	2, 5,
                             4, 3, 3,	3, 5, 2, 4,	5, 3,	5, 3, 3, 3,	5, 6,
                             5,	2, 2,	6, 6, 3, 5, 4, 5, 6, 5,	4, 5,	4, 6,
                             4, 6, 5,	5, 6, 6, 4,	6, 5,	6, 5,	6, 6,	5, 6)) %>%
    mutate(num_grade = 7 - grade,
           panel = "p3")

  all_panels <- panel1 %>%
    bind_rows(panel2) %>%
    bind_rows(panel3)

  if (panels == "all")
    ret <- all_panels

  if (panels == "one")
    ret <- p1

  if (panels == "two")
    ret <- p2

  if (panels == "three")
    ret <- p3

  return(ret)
}
