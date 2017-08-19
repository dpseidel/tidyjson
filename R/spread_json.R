## test readr
#debugonce(readr::read_csv)
#readr::read_csv('test.csv', col_types = readr::cols_only(key=readr::col_character(),notpresent=readr::col_character()))

readr:::col_spec_standardise
## the idea is that this should work like read_csv...
## default to providing lots of stuff, but enable "ensuring" and "altering"
## behavior if need be...

## need to define a "spec" which can be altered to fit... and then used to "ensure"
## that the data structure is consistent...

## arrays, etc. should all be gathered appropriately...
## should gathering objects have column names specified... 
## so that "complete" is not necessary...?

## should it be possible to convert to datetime / date / etc.?
#wb <- tidyjson::worldbank %>% as.tbl_json() %>% dplyr::filter(document.id < 5)

#j <- attr(wb,'JSON')

tr <- function(l) {
  uq <- unique(unlist(lapply(l,names)))
  
  d <- bind_rows(lapply(l,function(x){
    as_data_frame(t(x))
    }
    ))
  
  typ <- lapply(d, function(x){typeof(unlist(x,recursive=FALSE))})
  nam <- purrr::map_lgl(d, function(x) !is.null(attr(unlist(x,recursive=FALSE), "names")))
  
  purrr::map_chr(d$boardapprovaldate, as.character)
  ## does not handle NULL well
  ## purrr::map_chr(d$closingdate, as.character)
}


## start with a single object...

## step 1 - determine names... handle missing or duplicated... how to do this with recursion?  Recurse first?  
## Get all possibilities?

## step 2 - generate spec... json_skip, json_factor, json_chr, json_dbl, json_int, json_lgl
## json_date, json_datetime, json_only

## json_spec class

## step 3 - execute spec.. spread json as designated


## json_spec - a list of collectors

json_spec <- function(json_types, default=json_guess()) {
  stopifnot(is.list(json_types))
  stopifnot(is.jcollector(default))
  
  is_jcollector <- vapply(json_types, is.jcollector, logical(1))
  if (any(!is_jcollector)) {
    stop("Some `json_types` are not S3 jcollector objects: ",
         paste(which(!is_jcollector), collapse=', '), call. = FALSE)
  }
  structure(
    list(
      json = json_types,
      default = default
    ),
    class='json_spec'
  )
}

## a jcollector is a json key / value collector

jcollector <- function(type, ...) {
  structure(list(...), class=c(paste0('collector_',type), 'jcollector'))
}

is.jcollector <- function(x) inherits(x,'jcollector')

jcollector_find <- function(name) {
  if (is.na(name)) {
    return(json_character())
  }
  
  get(paste0('json_',name), envir = asNamespace('tidyjson'))()
}

## are we over-do-ing it, since parsing has already taken place
## by jsonlite?  Should we try to re-parse...?  

## i.e. if jsonlite gives a number, and we want a character,
## should we convert it?  Or ignore it?... I would think try to
## convert it...

## always return the desired type, and coerce if you can...


jparse <- function(x, jcollector) {
  if (is.character(jcollector)) {
    jcollector <- jcollector_find(jcollector)
  }
  
  jparse_(x,jcollector)
}

skip <- function(x) {
  return(NULL)
}

## returns a function used to parse...
jparse_get <- function(jcollector) {
  if (inherits(jcollector,'collector_skip')) {
    return(skip)
  } else if( inherits(jcollector,'collector_logical')) {
    return(as.logical)
  } else if (inherits(jcollector,'collector_integer')) {
    return(as.integer)
  } else if (inherits(jcollector, 'collector_double')) {
    return(as.double)
  } else if (inherits(jcollector, 'collector_number')) {
    return(as.numeric)
  } else if (inherits(jcollector,'collector_character')) {
    return(as.character)
  } else if (inherits(jcollector,'collector_date')) {
    return(as.Date)
  } else if (inherits(jcollector, 'collector_datetime')) {
    return(as.POSIXct)
  } else if (inherits(jcollector, 'collector_time')) {
    return(as.character) ## Need TIME
  } else if (inherits(jcollector, 'collector_factor')) {
    return(as.factor)
  } else {
    return(skip)
  }
}

jparse_ <- function(x, jcollector) {
  jparse_get(jcollector = jcollector)(x)
}

jparse_(c('T',F,'z'),jcollector('logical'))
