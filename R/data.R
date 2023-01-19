#' SNSF numeric grade transformation
#'
#' This function transforms the ordinal grade to a numeric grade.
#' @param grade the grade ("character")
#' @details  A = 6, AB = 5, B = 4, BC = 3, C = 2, D = 1. All other grades (COI,
#' etc) are transformed to NA.
#' @export
#' @import stringr
get_num_grade_snsf <- function(grade) {
  grade <- str_trim(grade)
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
#' @param prefix_assessor The column name prefix used to indicate the assessors.
#' @param delete_NA should the missing (numeric) grades be deleted?
#' (default = TRUE)
#' @param fun_grade_to_num function that translates grades to numeric values
#' @param first_assessor The column name of the first assessor in the matrix (not
#' needed if prefix_assessor is not null).
#' @param last_assessor The column name of the last assessor in the matrix (not
#' needed if prefix_assessor is not null).
#' @import stringr
#' @export
get_right_data_format <- function(individual_votes, prefix_assessor = NULL,
                                  fun_grade_to_num = get_num_grade_snsf,
                                  delete_NA = TRUE, first_assessor = NULL,
                                  last_assessor = NULL) {

  if (is.null(prefix_assessor) & is.null(first_assessor) &
      is.null(last_assessor)){
    stop(paste0(
      "Please either give a prefix of the assessor variables, e.g. assessor if",
      " assessor1:assessor9, or the name of the first_assessor and ",
      "last_assessor in the individivual votes matrix."))
  }
  if (!is.null(prefix_assessor) & !(is.null(first_assessor) &
                                    is.null(last_assessor))){
    print(paste0("Note that only the parameter prefix_assessor is used to ",
                 "define the assessor variables."))
  }
  if (is.null(prefix_assessor) & !is.null(first_assessor) &
      is.null(last_assessor)){
    stop(paste0(
      "You need to specify the column name of the last_assessor too."))
  }
  if (is.null(prefix_assessor) & is.null(first_assessor) &
      !is.null(last_assessor)){
    stop(paste0(
      "You need to specify the column name of the first_assessor too."))
  }

  if (is.null(prefix_assessor)){
    long_data <- individual_votes %>%
      # Get the data into a long format
      pivot_longer(cols = !!(first_assessor):!!(last_assessor),
                   names_to = "assessor",
                   values_to = "grade") %>%
      # Make sure all the grades are upper case and convert them to a numeric
      # variable using the function specified in fun_grade_to_num:
      mutate(grade = str_to_upper(.data$grade),
             num_grade = fun_grade_to_num(.data$grade))
  } else {
    long_data <- individual_votes %>%
      # Get the data into a long format
      pivot_longer(cols = starts_with(prefix_assessor), names_to = "assessor",
                   values_to = "grade") %>%
      # Make sure all the grades are upper case and convert them to a numeric
      # variable using the function specified in fun_grade_to_num:
      mutate(grade = str_to_upper(.data$grade),
             num_grade = fun_grade_to_num(.data$grade))
  }

  if (delete_NA) {
    long_data <- long_data %>%
      filter(!is.na(.data$num_grade))
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
  panel1 <- tibble(proposal = rep(paste0("#", 1:15), each = 5),
                   assessor = rep(paste0("v_", 1:5), 15),
                   grade = c(1,	1, 1,	2, 5, 1, 1,	2, 2,	6, 1,	2, 1,	3, 5,
                             1,	3, 2,	1, 5, 1, 2,	2, 3,	5, 1, 3, 1,	4, 5,
                             1,	2, 3,	4, 5, 1, 5,	2, 2,	5, 1,	3, 4,	3, 5,
                             1,	4, 2,	5, 5, 2, 3,	4, 4,	5, 2,	4, 3,	5, 5,
                             2,	5, 4,	4, 6, 2, 3,	4, 6,	6, 2,	5, 5,	4, 6)) %>%
    mutate(num_grade = 7 - .data$grade,
           panel = "p1")

  panel2 <- tibble(proposal = rep(paste0("#", 16:30), each = 5),
                   assessor = rep(paste0("v_", 6:10), 15),
                   grade = c(1,	1, 1,	2, 1, 1, 2,	2, 1,	1, 1,	3, 1,	1, 2,
                             2,	2, 2,	2, 2, 2, 2,	2, 3,	3, 3,	2, 3,	3, 3,
                             3,	3, 3,	3, 3, 3, 3,	4, 4,	3, 4,	3, 3,	4, 4,
                             4,	4, 4,	5, 4, 4, 4, 5, 5,	5, 4,	6, 4, 5, 5,
                             6,	5, 5,	5, 5, 5, 5,	5, 6,	6, 5,	6, 6,	6, 6)) %>%
    mutate(num_grade = 7 - .data$grade,
           panel = "p2")

  panel3 <- tibble(proposal = rep(paste0("#", 31:45), each = 5),
                   assessor = rep(paste0("v_", 11:15), 15),
                   grade = c(1,	1, 1,	1, 5, 2, 1, 3, 1, 5, 2,	3, 2,	1, 5,
                             2,	4, 1,	3, 5, 4, 2,	3, 3,	5, 3,	4, 4,	2, 5,
                             4, 3, 3,	3, 5, 2, 4,	5, 3,	5, 3, 3, 3,	5, 6,
                             5,	2, 2,	6, 6, 3, 5, 4, 5, 6, 5,	4, 5,	4, 6,
                             4, 6, 5,	5, 6, 6, 4,	6, 5,	6, 5,	6, 6,	5, 6)) %>%
    mutate(num_grade = 7 - .data$grade,
           panel = "p3")

  all_panels <- panel1 %>%
    bind_rows(panel2) %>%
    bind_rows(panel3)

  if (panels == "all")
    ret <- all_panels

  if (panels == "one")
    ret <- panel1

  if (panels == "two")
    ret <- panel2

  if (panels == "three")
    ret <- panel3

  return(ret)
}
