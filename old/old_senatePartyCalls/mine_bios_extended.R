load("inst/extdata/raw_bios.RData")
bios <- raw_bios
bios <- gsub("\u0092", "'", bios)
bios <- gsub("\u0093", '"', bios)
bios <- gsub("\u0094", '"', bios)
bios <- gsub("\\r\\n\\r\\nBibliography", "Bibliography: ", bios)
bios <- gsub("\\n", " ", bios)
bios <- gsub("\\r", " ", bios)
bios <- gsub("  +", " ", bios)
stop <- sapply(bios, regexpr, pattern = "(function(i,s,o,g,r,a,m)",
  fixed = TRUE) - 1
bios <- lapply(seq_along(bios), function(x) substr(bios[[x]], 1, stop[x]))
bios <- lapply(bios, function(bio) {
  stop <- regexpr("Bibliography:", bio)
  if (stop != -1) {
    bio <- substr(bio, 1, stop - 1)
  }
  bio
})
bios <- trimws(bios)

first_word <- do.call(c, lapply(bios, function(bio) {
  stop <- regexpr(",", bio, fixed = TRUE) - 1
  substr(bio, 1, stop)
}))
preambles <- lapply(seq_along(bios), function(x) {
  bio <- bios[[x]]
  start <- regexpr(",", bio, fixed = TRUE) + 1
  suffix <- substr(bio, start, 1e7)
  stop1 <- regexpr(first_word[x], suffix) + 1
  suffix <- substr(suffix, stop1, 1e7)
  stop2 <- regexpr(first_word[x], suffix)
  substr(bio, 1, start + stop1 + stop2 - 4)
})
main_texts <- lapply(seq_along(bios), function(x) {
  bio <- bios[[x]]
  start <- gregexpr(first_word[x], bio)[[1]][3]
  main_text <- substr(bio, start, 1e7)
  pattern <- paste0("\\(", "(((?!\\)).)*);", "(.*)")
  old_main_text <- main_text
  new_main_text <- gsub(pattern, "(\\1,\\3", main_text, perl = TRUE)
  while (old_main_text != new_main_text) {
    test <- new_main_text
    old_main_text <- new_main_text
    new_main_text <- gsub(pattern, "(\\1,\\3", test, perl = TRUE)
  }
  new_main_text
})
main_texts[[294]] <- gsub("Tennessee, born in", "Tennessee; born in",
  main_texts[[294]])
main_texts[[705]] <- gsub("\\(1726-1791\\);", "\\(1726-1791\\),",
  main_texts[[705]])
main_texts[[1122]] <- gsub("Massachusetts; probably", "Massachusetts;",
  main_texts[[1122]])
main_texts[[1245]] <- gsub("Ohio;", "Ohio; birth date unknown",
  main_texts[[1245]])
main_texts[[2636]] <- gsub("New York;", "New York; born in",
  main_texts[[2636]])
main_texts[[2812]] <- gsub("Virginia; birth", "Virginia, birth",
  main_texts[[2812]])
main_texts[[3602]] <- gsub("Maine, born in", "Maine; born in",
  main_texts[[3602]])
main_texts[[7595]] <- gsub("Illinois, born in", "Illinois; born in",
  main_texts[[7595]])
main_texts[[11147]] <- gsub("California, born in", "California; born in",
  main_texts[[11147]])
main_texts[[11751]] <- gsub("Ill.; March 18, 1947", "Ill., March 18, 1947",
  main_texts[[11751]], fixed = TRUE)
main_texts <- strsplit(do.call(c, main_texts), ";")
main_texts <- lapply(main_texts, trimws)
main_texts <- lapply(main_texts, function(main_text) {
  main_text[2] <- gsub("^was born", "born", main_text[2])
  main_text
})
main_texts <- lapply(main_texts, function(main_text) {
  main_text[3] <- gsub("^was ", "", main_text[3])
  main_text
})
check <- grep("^Jan|^Feb|^Mar|^Apr|^May|^Jun|^Jul|^Aug|^Sep|^Oct|^Nov|^Dec",
  sapply(main_texts, function(x) x[3]))
for (i in check) {
  main_texts[[i]][2] <- paste0(main_texts[[i]][2], ", ", main_texts[[i]][3])
  main_texts[[i]] <- c(head(main_texts[[i]], 2), tail(main_texts[[i]], -3))
}

main_texts_3 <- sapply(main_texts, function(x) x[3])
terms <- c("graduated", "A\\.A\\.", "B\\.A\\.", "A\\.S\\.", "B\\.S\\.",
  "B\\.G\\.S\\.", "A\\.G\\.S\\.", "A\\.B\\.", "Ph\\.B\\.", "R\\.N\\.",
  "B\\.L\\.S\\.", "J\\.D\\.", "B\\.F\\.A\\.", "B\\. S\\.", "B\\. A\\.",
  "B\\.J\\.", "Associate B\\.A\\.", "B\\.C\\.S\\.",
  "attended", "educated", "graduate", "enrolled", "student", "pursued",
  "obtained", "tutored", "received", "studied", "completed",
  "self-taught", "self-educated", "instructed", "privately",
  "moved", "immigrated", "when", "lived", "brought",
  "became an orphan")
check <- grep(paste0("^", terms, collapse = "|"), main_texts_3)
main_texts_3[-check]

check <- grep("^privately", main_texts_3)
main_texts_3[check]



check <- grep(paste0("^served"),
  sapply(main_texts, function(x) x[3]))
sapply(main_texts, function(x) x[3])[check]


main_texts <- lapply(main_texts, function(main_text) {
  c(head(main_text, 2), NA, tail(main_text, -2))
})
main_texts <- lapply(main_texts, function(main_text) {

})

cat(paste(sapply(main_texts, function(x) x[3]), collapse = "\n"), file = "~/desktop/born.txt")

pattern <- "B.S., Austin Peay State University, Clarksville, Tenn., 1967"
which(sapply(main_texts, function(x) x[2]) == pattern)