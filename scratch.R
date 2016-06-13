library(dplyr)


# get labels of a file's cached chunks
cached_chunks <- function(file, output_format) {
  cache_dir <- rmarkdown:::knitr_cache_dir(file, output_format)
  cache_dir_files <- list.files(cache_dir)
  cache_files <- cache_dir_files[tools::file_ext(cache_dir_files) %in%
                                   c("RData", "rdb", "rdx")]
  cache_files %>%
    lapply(function(cache_file) {
      parts <- unlist(strsplit(cache_file, "_"))
      parts[1:(length(parts) - 1)]
    }) %>%
    unlist() %>%
    unique()
}

cached_chunks("multiknit_test.Rmd", "html")


# load a chunk's cache
load_chunk_cache <- function(label, file, output_format = "html") {
  cache_dir <- rmarkdown:::knitr_cache_dir(file, output_format)
  knitr::load_cache(label, path = cache_dir)
}

load_chunk_cache("cars", file = "multiknit_test.Rmd")


# read a file's chunks, which can then be referenced by label to output
include_file_chunks <- function(file) {
  knitr::purl(file, quiet = TRUE)
  knitr::read_chunk(gsub(".Rmd", ".R", file))
}

include_file_chunks("multiknit_test.Rmd")


external_chunks <- list()

# read a file's chunks, which can then be run using run_chunk
include_rmarkdown <- function(file) {

  file_script <- sub(".Rmd$", ".R", file)

  # # doesn't re-do if file already exists???
  # file.remove(file_script)
  knitr::purl(file, quiet = TRUE, documentation = 1)

  chunks <- readLines(file_script) %>%
    paste(collapse = "\n") %>%
    strsplit("\n\n") %>%
    unlist() %>%
    strsplit("\n") %>%
    lapply(function(chunk) {
      header <- chunk[1]
      list(label = sub("^## ----([A-Za-z_]+).*$", "\\1", header),
           code = chunk[2:length(chunk)] %>% paste(collapse = "\n"))
    })

  external_chunks[[file]] <<- chunks
}

include_rmarkdown("multiknit_test.Rmd")


# run a chunk that's been read into external_chunks
run_chunk <- function(file, label) {
  chunks <- external_chunks[[file]]
  chunk_code <- Filter(function(chunk) chunk$label == label, chunks)[[1]]$code
  eval(parse(text = chunk_code))
}

run_chunk("multiknit_test.Rmd", "cars")
run_chunk("multiknit_test.Rmd", "pressure")


# lines = readLines(file)
# read_chunk(lines = lines, labels = c("setup", "cars", "pressure"), from = "^```\\{r.*\\}$",
#            to = "^```$")

#knitr::all_labels()
#knitr:::knit_code$restore()
#knitr:::knit_code$get()
# text <- readLines("multiknit_test.Rmd")
# pat_md()
# .knitEnv <- new.env()
# rs <- split_file(lines = text)
# group <- rs[[2]]
# process_group(group)
