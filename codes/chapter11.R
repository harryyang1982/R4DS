library(tidyverse)
library(stringr)

# String Basics

string1 <- "This is a string"
string2 <- 'To put a "quote" inside a string, use single quotes'

double_quote <- "\""
single_quote <- '\''
double_quote
single_quote

x <- c("\"", "\\")
x
writeLines(x)

x <- "\u00b5"
x

c("one", "two", "three")

## String Length

str_length(c("a", "R for data science", NA))

## Combining Strings

str_c("x", "y")
str_c("x", "y", "z")

str_c("x", "y", sep = ", ")
x <- c("abc", NA)
str_c("|-", x, "-|")
str_c("|-", str_replace_na(x), "-|")

str_c("prefix-", c("a", "b", "c"), "-suffix")

name <- "Hadley"
time_of_day <- "morning"
birthday <- F

str_c(
  "Good ", time_of_day, " ", name,
  if (birthday) " and Happy Birthday",
  "."
)

str_c(c("x", "y", "z"), collapse = ", ")
str_c(c("x", "y", "z"))

## Subsetting Strings

x <- c("Apple", "Banana", "Pear")
x
str_sub(x, 1, 3)

str_sub(x, -3, -1)

str_sub("a", 1, 5)

str_sub(x, 1, 1) <- str_to_lower(str_sub(x, 1, 1))
str_to_lower(str_sub(x, 1, 1))
str_sub(x, 1, 1)
x

## Locales

str_to_upper(c("i", "l"))
str_to_upper(c("i", "i"), locale = "tr")

x <- c("apple", "eggplant", "banana")

str_sort(x, locale = "en")
str_sort(x, locale = "haw")

## Exercises
#1.
paste(string1, string2)
paste0(string1, string2)

#2.
y <- c("kim", "lee", "yang")
str_c(y, collapse = ", ")
str_c(y, sep = ", ")

#3
str_sub(x, 1, 3)
str_length(x)

#4
?str_wrap
thanks_path <- file.path(R.home("doc"), "THANKS")
thanks <- str_c(readLines(thanks_path), collapse = "\n")
thanks <- word(thanks, 1, 3, fixed("\n\n"))
cat(str_wrap(thanks), "\n")
cat(str_wrap(thanks, width = 40), "\n")
cat(str_wrap(thanks, width = 60, indent = 2), "\n")
cat(str_wrap(thanks, width = 60, exdent = 2), "\n")
cat(str_wrap(thanks, width = 0, exdent = 2), "\n")


#5
str_trim("  String with trailing and leading white space\t")

#6
string3 <- c("a", "b", "c")

combinef <- function(strings) {
  a <- str_c(str_c(strings[-length(strings)], collapse = " "))
  b <- strings[length(strings)]
  str_c(a, b, sep = " and ")
}

combinef(string3)

# Matching Patterns with Regular Expressions

## Basic Matches

x <- c("apple", "banana", "pear")
str_view(x, "an")

str_view(x, ".a.")
dot <- "\\."

writeLines(dot)

str_view(c("abc", "a.c", "bef"), "a\\.c")
writeLines("a\\.c")

x <- "a\\b"
writeLines(x)

str_view(x, "\\\\")

## Exercises
#1
#writeLines("\")
writeLines("\\")
#writeLines("\\\")

#2
writeLines("\'\\\\")
k <- "'\\"
str_view(k, "\'\\\\")

#3
writeLines("\\\\..\\\\..\\\\..")
k <- "\\..\\..\\.."
str_view(k, "\\\\..\\\\..\\\\..")

## Anchors

x <- c("apple", "banana", "pear")
str_view(x, "^a")
str_view(x, "a$")

x <- c("apple pie", "apple", "apple cake")
str_view(x, "apple")
str_view(x, "^apple$")

## Exercises

#1
writeLines("\\$\\^\\$")
k <- "$^$"
str_view(k, "\\$\\^\\$")

#2
stringr::words
data(words)

str_view(words, "^y", match = T)
str_view(words, "$x", match = T)
str_view(words, "^...$", match = T)
str_view(words, "^.......", match = T)