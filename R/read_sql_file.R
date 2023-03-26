#' Les SQL-fil
#'
#' Leser innholdet fra en SQL-fil. Filen maa vaere en .sql fil.
#'
#' @section Parametrisering:
#'
#'
#' SQL-filen kan inneholde parametere. Disse ma vaere kodet som
#' `{params$<params-navn>}`, f.eks `{params$kalkulasjon}`. Disse angis som input
#' til funksjonen som en navngitt liste, f.eks
#' `list(kalkulasjon = "'B MND SBM'")`. Parametriseringen bruker `glue` som
#' motor, som evaluering av et parameter verdien maa gi det innskuddet i filen
#' som filen behoever. I eksempelet over innfoeres en tekstparameter i filen.
#' Dersom vi skulle sende inn en dato kunne vi gjort slik:
#' `list(kalkulasjonsdato = "TODATE('2023-01-01','YYYY-MM-DD')")`.
#'
#'
#'
#'
#' @param filsti `character/path``. Maa angi en fil med endelse .sql eller .SQL.
#' @param params `list`. Parameteren er valgfri, men maa inneholde en nangitt
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
    stop("filsti maa vaere tekststregnge")
  if (!file.exists(filsti))
    stop("Finner ikke filen")
  if (tolower(substr(filsti, (nchar(filsti) - 3), nchar(filsti))) != ".sql")
    stop("Filen maa ende med .sql")
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
