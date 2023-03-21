#' Les SQL-fil
#'
#' Leser innholdet fra en SQL-fil. Filen må være en .sql fil.
#'
#' @section Parametrisering:
#'
#'
#' SQL-filen kan inneholde parametere. Disse må være kodet som
#' `{params$<params-navn>}`, f.eks `{params$kalkulasjon}`. Disse angis som input
#' til funksjonen som en navngitt liste, f.eks
#' `list(kalkulasjon = "'B MND SBM'")`. Parametriseringen bruker `glue` som
#' motor, som evaluering av et parameter verdien må gi det innskuddet i filen
#' som filen behøver. I eksempelet over innføres en tekstparameter i filen.
#' Dersom vi skulle sende inn en dato kunne vi gjort slik:
#' `list(kalkulasjonsdato = "TODATE('2023-01-01','YYYY-MM-DD')")`.
#'
#'
#'
#'
#' @param filsti `character/path``. Må angi en fil med endelse .sql eller .SQL.
#' @param params `list`. Parameteren er valgfri, men må inneholde en nangitt
#' liste med parametere som korresponderer med de i sql-filen.
#' (A list with the parameters in the corresponding sql file.
#' @importFrom glue glue
#' @return `character`. Returnerer SQL i filstien.
#' @export
#'
#'
#'
#'
#'
read_sql_file <- function(filsti,params=NULL){

  empty_space <- function(x){
    x[ ifelse(x=="", FALSE,TRUE)]
  }

  if (!is.character(filsti))
    stop("filsti må være tekststregnge")
  if (!file.exists(filsti))
    stop("Finner ikke filen")
  if (tolower(substr(filsti, (nchar(filsti) - 3), nchar(filsti))) != ".sql")
    stop("Filen må ende med .sql")
  sql_query <-
    filsti %>%
    file("r") %>%
    readLines() %>%
    gsub(pattern = '--.+$',replacement = '') %>%
    empty_space() %>%
    paste(collapse = " ") %>%
    glue::glue()

  sql_query
}
