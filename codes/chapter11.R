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

## Character Classes and Alternatives

str_view(c("grey", "gray"), "gr(e|a)y")

## Exercises

#1.
#a. 
str_view(words, "^[aeiou]", match = T)
#b.
str_view(words, "^[^aeiou]*$", match = T)
#c.
str_view(words, "[^e]ed$", match =T)
#d.
str_view(words, "i(ng|ze)", match = T)
#2.
str_view(words, "([^c]|)ei", match = T)
#3.
str_view(words, "q[^u]", match = T)
#4
str_view(words, "ize", match = T)
str_view(words, "ise", match = T)
#5
phone_numbers = c("02-491-7454","010-3298-7454","01032987454")
str_view(phone_numbers, "010(-|)\\d(\\d|)(-|)\\d\\d\\d(-|)\\d\\d(-|)\\d\\d", match = T)

## Repetition

x <- "1888 is the longest year in Roman numerals: MDCCCLXXXVIII"
str_view(x, "CC?")
str_view(x, "CC+")
str_view(x, 'C[LX]+')

str_view(x, "C{2}")
str_view(x, "C{2,}")
str_view(x, "C{2,3}")

str_view(x, 'C{2,3}?')
str_view(x, 'C[LX]+?')

## Exercises
#1
#{1}, {1,}, {0,}
#2
#a
str_view(x, "^.*$")
str_view(x, "\\{.+\\}", match = T)
str_view(x, "\\d{4}-\\d{2}-\\d{2}", match = T)
str_view(x, "\\\\{4}", match = T)

#3
str_view(words, "^[^aeiou]{3}", match = T)
str_view(words, "[^aeiou]{3,}", match = T)
str_view(words, "([aeiou][^aeiou]){2,}", match = T)

#4.

## Grouping and Backreferences

str_view(fruit, "(..)\\1", match = T)

## Exercises
#1
str_view(words, "(.)\\1\\1", match = T)
str_view(words, "(.)(.)\\2\\1", match = T)
str_view(words, "(..)\\1", match = T)
str_view(words, "(.).\\1.\\1", match = T)
str_view(words, "(.)(.)(.).*\\3\\2\\1", match = T)

#2
str_view(words, "^(.).*\\1$", match = T)
str_view(words, "(..).*\\1", match = T)
str_view(words, "(.).*\\1.*\\1", match = T)

## Tools

## Detect Matches

x <- c("apple", "banana", "pear")
str_detect(x, "e")
sum(str_detect(words, "^t"))
# what proportion of common words end with a vowel?
mean(str_detect(words, "[aeiou]$"))

no_vowels_1 <- !str_detect(words, "[aeiou]")
no_vowels_2 <- str_detect(words, "^[^aeiou]+$")
identical(no_vowels_1, no_vowels_2)

words[str_detect(words, "x$")]
str_subset(words, "x$")
