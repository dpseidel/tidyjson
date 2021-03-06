context("tbl_json: as.tbl_json.character")

test_that("correctly parses length(json) == 1", {
  expect_identical(
    as.tbl_json('{"name": "bob", "age": 32}'),
    tbl_json(
      data.frame(document.id = 1L),
      list(list(name = "bob", age = 32L))
    )
  )
})

test_that("correctly parses length(json) > 1", {
  expect_identical(
    as.tbl_json(
      c('{"name": "bob", "age": 32}',
        '{"name": "susan", "age": 25}')
    ),
    tbl_json(
      data.frame(document.id = 1L:2L),
      list(
        list(name = "bob", age = 32L),
        list(name = "susan", age = 25L)
      )
    )
  )
})

test_that("correctly parses character(0)", {
  expect_identical(
    as.tbl_json(character(0)),
    tbl_json(
      data.frame(document.id = integer(0)),
      list()
    )
  )
})

test_that("correctly parses empty objects", {

  nl <- list()
  names(nl) <- character(0)

  expect_identical(
    as.tbl_json(c('[]', '{}')),
    tbl_json(
      data.frame(document.id = 1L:2L),
      list(list(), nl)
    )
  )

})

test_that("correctly structures an array", {
  expect_identical(
    as.tbl_json('[{"name": "bob"}, {"name": "susan"}]'),
    tbl_json(
      data.frame(document.id = 1L),
      list(list(list(name = "bob"), list(name = "susan")))
    )
  )
})

test_that("throws error on invalid json", {

    expect_error(as.tbl_json(''))

  }
)

context("tbl_json: as.character.tbl_json")

inverts_json_test <- function(json) {
  expect_identical(json, json %>% as.tbl_json %>% as.character)
}

test_that("works for simple cases", {

  inverts_json_test('"a"')
  inverts_json_test('1')
  inverts_json_test('true')
  inverts_json_test('false')
  inverts_json_test('null')
  inverts_json_test('{}')
  inverts_json_test('[]')

})

test_that("works for more complex cases", {

  inverts_json_test('{"name":"a"}')
  inverts_json_test('{"name":1}')
  inverts_json_test('{"name":[1]}')
  inverts_json_test('{"name":[null]}')
  inverts_json_test('{"name":null}')
  inverts_json_test('[[1,2],1]')

})

test_that("works for worldbank data", {

  inverts_json_test(worldbank[1:5])

})

test_that("throws informative warning message when attr(.,'JSON') is missing", {
  j <- '{"a": 1, "b": "test"}' %>% as.tbl_json()
  attr(j,'JSON') <- NULL
  
  expect_warning(j %>% as.character(),'attr.*JSON.*remove.*tbl_json')
  expect_identical(suppressWarnings(j %>% as.character()),character())
})


context("as.tbl_json.tbl_json")

test_that('functions as the identity on a simple pipeline', {
   x <- commits %>% gather_array() %>% enter_object('commit') %>% spread_all()
   
   expect_identical(
     x, as.tbl_json(x)
   )
   
   y <- commits %>% gather_array() %>% gather_object()
   
   expect_identical(
     y, as.tbl_json(y)
   )
})

test_that('functions as the identity on a more advanced pipeline', {
  x <- commits %>% gather_array() %>% spread_values(
    sha=jstring('sha')
    , name=jstring('commit','author','name')
    , msg=jstring('commit','message')
    , comment_count=jnumber('commit','comment_count')
    , committer.name=jstring('commit','committer','name')
    , committer.date=jstring('commit','committer','date')
    , tree.sha=jstring('committ','tree','sha')
    , tree.url=jstring('committ','tree','url')
    , url=jstring('url')
  )
  
  expect_identical(
    x, as.tbl_json(x)
  )
})

context("print.tbl_json")

test_that("jsonlite::toJSON works as anticipated", {
  expect_identical(jsonlite::toJSON(attr(as.tbl_json('"a"'),'JSON')
                                    , null='null'
                                    , auto_unbox = TRUE) %>% as.character
                   , "[\"a\"]")
})

test_that("purrr::map_chr works as expected", {
  a <- attr(as.tbl_json('"a"','JSON'),'JSON') %>% purrr::map_chr(jsonlite::toJSON,
                          null = "null",
                          auto_unbox = TRUE)
  
  expect_identical(a,'\"a\"')
})

test_that('print.tbl_df works as expected', {

  skip('tests failing due to upstream print.tbl_df')
  z <- dplyr::data_frame(col='"a"')
  
  expect_identical(capture.output(print(z))
                   , c(
                     "# A tibble: 1 x 1"
                     , "    col"
                     , "  <chr>"
                     , "1   \"a\""
                   ))
})

test_that("print.tbl_json works for a simple case", {
  skip('tests failing due to upstream print.tbl_df')
  
  expect_identical(
    capture.output(print(as.tbl_json('"a"'))),
    c('# A tbl_json: 1 x 1 tibble with a \"JSON\" attribute',
      '  `attr(., "JSON")` document.id',
      '              <chr>       <int>',
      '1               "a"           1')
  )
})

test_that("print.tbl_json json.width works correctly", {
  skip('tests failing due to upstream print.tbl_df')
  
  expect_identical(
    capture.output(print(as.tbl_json('"12345"'), json.width = 4)),
    c('# A tbl_json: 1 x 1 tibble with a \"JSON\" attribute',
      '  `attr(., "JSON")` document.id',
      '              <chr>       <int>',
      '1           "123...           1')
  )

})

test_that("print.tbl_json json.n works correctly", {
  skip('tests failing due to upstream print.tbl_df')
  
  expect_identical(
    capture.output(print(as.tbl_json(c('"a"', '"b"')), json.n = 1)),
    c('# A tbl_json: 2 x 1 tibble with a \"JSON\" attribute',
      '  `attr(., "JSON")` document.id',
      '              <chr>       <int>',
      '1               "a"           1',
      '2               ...           2')
  )

})

test_that('does not throw an error', {
  printregex <- 'tbl_json.*JSON.*attribute.*document\\.id'
  json <- '{"a":1, "b": "test", "c": [1,2,3]}'
  
  expect_output(json %>% as.tbl_json() %>% print, printregex)
  
  j <- json %>% spread_all() %>% enter_object('c') %>% 
    gather_array('c_id') %>% append_values_number()
  
  expect_output(j %>% print, printregex)
  
  attr(j,'JSON') <- NULL
  
  expect_output(suppressWarnings(j %>% print), printregex)
})

context("tbl_json: as.tbl_json.data.frame")

test_that("works for a data.frame and data_frame created objects", {

    df <- data.frame(
      document.id = 1:2,
      json = c('{"name": "bob"}', '{"name": "susan"}'),
      stringsAsFactors = FALSE)
    # data.frame
    expect_identical(
      as.tbl_json(df, json.column = "json"),
      as.tbl_json(df$json)
    )
    # data_frame
    df <- dplyr::data_frame(
      document.id = 1:2,
      json = c('{"name": "bob"}', '{"name": "susan"}'))
    expect_identical(
      as.tbl_json(df, json.column = "json"),
      as.tbl_json(df$json)
    )

  }
)

test_that("works in a pipeline", {

    df <- dplyr::data_frame(
      age = c(32, 45),
      json = c('{"name": "bob"}', '{"name": "susan"}')
    )

    expect_identical(
      df %>% as.tbl_json(json.column = "json") %>%
        spread_values(name = jstring("name")) %>%
        dplyr::filter(age == 32) %>%
        `[[`("name"),
      "bob"
    )

  }
)

test_that("throws an error without json.column specified", {
    expect_error(as.tbl_json(iris))
  }
)

test_that("throws an error if json column doesn't exist", {
    expect_error(as.tbl_json(iris, json.column = "json"))
  }
)

context("tbl_json")

test_that("tbl_json constructor works with no data", {

    expect_identical(tbl_json(data.frame(), list()) %>% nrow, 0L)

  }
)

test_that("tbl_json fails if ..JSON is in the names of the data.frame", {

    expect_error(tbl_json(data.frame(..JSON = character(0)), list()))

  }
)

test_that("[ row filtering works with a simple example", {

    expect_identical(
      as.tbl_json(c('{"name": "bob"}', '{"name": "susan"}'))[1, ],
      tbl_json(
        data.frame(document.id = 1L),
        list(list(name = "bob"))
      )
    )

  }
)

test_that("[ column filtering doesn't change the JSON", {

    x <- c(
      '{"name": "bob", "children": [{"name": "george"}]}',
      '{"name": "susan", "children": [{"name": "sally"}, {"name": "bobby"}]}'
        ) %>% as.tbl_json %>%
      spread_values("parent" = jstring("name")) %>%
      enter_object("children") %>%
      gather_array %>%
      spread_values("child" = jstring("name"))

    expect_identical(
      attr(x, "JSON"),
      attr(x[, c("parent", "child")], "JSON")
    )

  }
)


test_that('handles "drop" like a tbl_df', {
  mydata <- as.tbl_json('[{"name": "Frodo", "occupation": "Ring Bearer"}
                        ,{"name": "Aragorn", "occupation": "Ranger"}]') %>%
    gather_array() %>%
    spread_values(name=jstring('name'), occupation=jstring('occupation'))
   
  expect_is(mydata[,],'tbl_json')
  expect_is(mydata[,'name'],'tbl_json')
  expect_is(suppressWarnings(mydata[,'occupation',drop=TRUE]),'tbl_json')
  expect_warning(is.tbl_json(mydata[,'name',drop=TRUE]),'drop ignored')
})

context('as_tibble') 

test_that('as_tibble drops the JSON attribute and tbl_json class', {
  
  jtidy <- issues %>% gather_array() %>% spread_all()
  
  expect_identical(attr(dplyr::as_tibble(jtidy),'JSON'),NULL)
  expect_false('tbl_json' %in% class(dplyr::as_tibble(jtidy)))
})

test_that('as_data_frame functions like as_tibble', {
  
  jtidy <- issues %>% gather_array() %>% spread_values(
    url=jstring('url')
    , body=jstring('body')
    , user.id=jnumber('user.id')
    , user.login=jstring('user.login')
  )
  
  expect_identical(attr(dplyr::as_data_frame(jtidy),'JSON'),NULL)
  expect_false('tbl_json' %in% class(dplyr::as_data_frame(jtidy)))
})

context("tbl_json: dplyr NSE verbs")

test_that("dplyr::filter works with a simple example", {

    x <- as.tbl_json(c('{"name": "bob"}', '{"name": "susan"}'))

    expect_identical(
      dplyr::filter(x, document.id == 1),
      tbl_json(
        data.frame(document.id = 1L),
        list(list(name = "bob"))
      )
    )

  }
)

test_that("dplyr::filter works in a more complex pipeline", {

    json <- c(
      '{"name": "bob", "children": [{"name": "george"}]}',
      '{"name": "susan", "children": [{"name": "sally"}, {"name": "bobby"}]}'
        )
    susan.children <- json %>% as.tbl_json %>%
      spread_values(name = jstring("name")) %>%
      dplyr::filter(name == "susan") %>%
      enter_object("children") %>%
      gather_array %>%
      spread_values(child = jstring("name"))

    expect_identical(susan.children$child, c("sally", "bobby"))

  }
)


test_that("dplyr::arrange works with a simple example", {

    x <- as.tbl_json(c('{"name": "bob"}', '{"name": "susan"}'))

    expect_identical(
      x %>% dplyr::arrange(desc(document.id)),
      tbl_json(
        data.frame(document.id = c(2L, 1L)),
        list(list(name = "susan"), list(name = "bob"))
      )
    )

  }
)

test_that("dplyr::mutate works with a simple example", {

    x <- as.tbl_json(c('{"name": "bob"}', '{"name": "susan"}'))

    expect_identical(
      x %>%
        spread_values(name = jstring("name")) %>%
        dplyr::mutate(fullname = paste(name, "green")),
      tbl_json(
        dplyr::data_frame(
          document.id = c(1L, 2L),
          name = c("bob", "susan"),
          fullname = c("bob green", "susan green")),
        list(list(name = "bob"), list(name = "susan"))
      )
    )

  }
)

test_that("dplyr::mutate works in a more complex pipeline", {

    json <- c(
      '{"name": "bob", "children": [{"name": "george"}]}',
      '{"name": "susan", "children": [{"name": "sally"}, {"name": "bobby"}]}')

    children <- json %>% as.tbl_json %>%
      spread_values(name = jstring("name")) %>%
      dplyr::mutate(parent.rank = rank(name)) %>%
      enter_object("children") %>%
      gather_array %>%
      spread_values(child = jstring("name"))

    expect_identical(children$parent.rank, c(1, 2, 2))
    expect_identical(children$child, c("george", "sally", "bobby"))

  }
)

test_that("dplyr::slice works", {

  new <- '[1, 2, 3]' %>% gather_array %>% dplyr::slice(1:2)

  expect_is(new, "tbl_json")
  expect_identical(nrow(new), 2L)
  expect_identical(length(attr(new, "JSON")), 2L)

})

test_that('dplyr::select works', {
  json <- '[{"id":1, "object":"first"}, {"id":2, "object":"second"}]'
  
  f <- json %>% as.tbl_json %>% gather_array %>% spread_all %>%
    dplyr::select(ID=id, object)
  
  expect_equal(names(f), c('ID','object'))
  expect_equal(nrow(f),2)
  expect_is(f,'tbl_json')
})

test_that("dplyr::rename works", {

  new <- '[1, 2, 3]' %>% gather_array %>% dplyr::rename(blah = document.id)

  expect_is(new, "tbl_json")
  expect_identical(names(new), c("blah", "array.index"))

})

test_that("dplyr::transmute works", {

  new <- '[1, 2, 3]' %>% gather_array %>% dplyr::transmute(blah = document.id)

  expect_is(new, "tbl_json")
  expect_identical(names(new), "blah")

})

test_that("dplyr::sample_n works", {

  new <- '[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]' %>% gather_array %>% dplyr::sample_n(2)

  expect_is(new, "tbl_json")
  expect_identical(new$array.index, attr(new, "JSON") %>% purrr::flatten_int())

})

test_that("bind_rows works with tbl_json", {

  # Define a simple JSON array
  people <- '
  [
      {
      "name": "bob",
      "age": 32
      }, 
      {
      "name": "susan", 
      "age": 54
      }
  ]'
  
  # Structure the data
  people_df <- people %>%
    gather_array %>%
    spread_values(
      name = jstring("name"),
      age = jnumber("age"))
  
  z <- people_df %>% bind_rows(people_df)
  

  expect_is(attr(z,'JSON'),'list')
  expect_is(z, 'tbl_json')
  expect_equal(nrow(z), nrow(people_df) * 2)
  expect_equal(length(attr(z,'JSON')), nrow(people_df) * 2)
})

test_that("bind_rows falls back to normal behavior if not tbl_json", {
  a <- dplyr::data_frame(a=c(1,2), b=c('one','two'))
  c <- dplyr::data_frame(a=c(3,4), b=c('three','four'))
  
  out <- bind_rows(a,c)
  expect_equal(nrow(out), nrow(a) + nrow(c))
  expect_equal(names(out), c('a','b'))
  expect_is(out,'tbl_df')
})

context('tbl_json: dplyr SE verbs')

test_that('dplyr::filter_ works', {
  json <- '[{"a": "fun", "b": 2},{"a": "blam", "b": 3}]'
  v <- c('a == "fun"')
  
  f <- json %>% gather_array %>% spread_all %>%
    dplyr::filter_(.dots=v)
  
  expect_identical(f$a,c('fun'))
  expect_identical(f$b,c(2))
  expect_identical(nrow(f),1L)
  expect_is(f,'tbl_json')
})

test_that('dplyr::mutate_ works', {
  json <- '{ "one": "zip", "two": "zap", "three": "zzz" }'
  v <- c(four='paste(one,two,sep="/")', five='three')
  
  f <- json %>% spread_all %>% dplyr::mutate_(.dots=v)
  
  expect_identical(f$four,'zip/zap')
  expect_identical(f$five, 'zzz')
  expect_is(f,'tbl_json')
})

test_that('dplyr::rename_ works', {
  json <- '{ "first": "bill", "last":"bo" }'
  v <- c(firstName='first', lastName='last')
  
  f <- json %>% spread_all %>% dplyr::rename_(.dots=v)
  
  expect_identical(names(f),c('document.id','firstName','lastName'))
  expect_is(f,'tbl_json')
})

test_that('dplyr::select_ works', {
  json <- '{ "hill": "top", "valley": "floor", "mountain": "top" }'
  v <- c(Hill='hill','valley')
  
  f <- json %>% spread_all %>% dplyr::select_(.dots=v)
  
  expect_identical(names(f),c('Hill','valley'))
  expect_is(f,'tbl_json')
})

test_that('dplyr::arrange_ works', {
  json <- '[{ "somewhere": "over" },{"somewhere": "fun"}, {"somewhere": "else"}]'
  v <- c('somewhere')
  
  f <- json %>% gather_array %>% spread_all %>% dplyr::arrange_(.dots=v)
  
  expect_identical(f$somewhere,c('else','fun','over'))
  expect_identical(f$array.index, c(3L,2L,1L))
  expect_is(f,'tbl_json')
})

test_that('dplyr::transmute_ works', {
  json <- '{ "first": "frodo", "last": "baggins"}'
  v <- c(firstName='first')
  
  f <- json %>% spread_all %>% dplyr::transmute_(.dots=v)
  
  expect_identical(names(f), 'firstName')
  expect_is(f,'tbl_json')
})

test_that('dplyr::slice_ works', {
  json <- '[{"id":7, "obj":"a"}
  ,{"id":8, "obj":"a"}
  ,{"id":9, "obj":"b"}
  ,{"id":10, "obj":"c"}]'
  v <- '1'
  
  f <- json %>% gather_array %>% spread_all %>% slice_(.dots=v)
  expect_identical(nrow(f),1L)
  expect_identical(f$id,7)
  expect_is(f,'tbl_json')
})
