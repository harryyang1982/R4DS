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

df <- tibble(
  word = words,
  i = seq_along(word)
)

df %>% 
  filter(str_detect(words, "x$"))

x <- c("apple", "banana", "pear")
str_count(x, "a")

mean(str_count(words, "[aeiou]"))

df %>% 
  mutate(
    vowels = str_count(word, "[aeiou]"),
    consonants = str_count(word, "[^aeiou]")
  )

str_count("abababa", "aba")
str_view_all("abababa", "aba")

## Exercises
#1a
words[str_detect(words, "^x|x$")]
#1b
words[str_detect(words, "^[aeiou]") & str_detect(words, "[^aeiou]$")]
#1c
words[str_detect(words, "a") &
        str_detect(words, "e") &
        str_detect(words, "i") &
        str_detect(words, "o") &
        str_detect(words, "u")]
#1d
DFF <- tibble(word = words,
  vowel_count = str_count(words, "[aeiou]"),
  consonant_count = str_count(words, "[^aeiou]"))
DFF %>% 
  arrange(desc(vowel_count)) %>% 
  head(10)

DFF %>% 
  arrange(desc(proportion)) %>% 
  head(10)

## Extract Matches

length(sentences)
head(sentences)

colors <- c(
  "red", "orange", "yellow", "green", "blue", "purple"
)

color_match <- str_c(colors, collapse = "|")
color_match

has_color <- str_subset(sentences, color_match)
matches <- str_extract(has_color, color_match)
head(matches)

more <- sentences[str_count(sentences, color_match) > 1]
str_view_all(more, color_match)
str_extract(more, color_match)

str_extract_all(more, color_match)
str_extract_all(more, color_match, simplify = T)

x <- c("a", "a b", "a b c")
str_extract_all(x, "[a-z]", simplify = T)
str_extract_all(x, "[a-z]")

## Exercises

#1
color_match2 <- str_c("\\b(", str_c(colors, collapse = "|"), ")\\b")
color_match2

more2 <- sentences[str_count(sentences, color_match2) > 1]
str_view_all(more2, color_match2)
str_view_all(has_color, color_match2)

#2a
str_extract(sentences, "[a-z]")
word(sentences, 1)
#2b
str_extract(sentences, ".+ing.$")
#2c
str_extract(sentences, ".+s.$")

is_noun <- str_subset(sentences, "(a|the) [^ \\.]+")
nouns <- str_extract(is_noun, "(a|the) [^ \\.]+")
is_plural <- str_subset(nouns, "(a|the) [^ \\.\\']+(s|\\')$")
str_extract(is_plural, "(a|the) [^ \\.\\']+(s|\\')$")

## Grouped Matches

noun <- "(a|the) ([^ ]+)"
has_noun <- sentences %>% 
  str_subset(noun) %>% 
  head(10)
has_noun %>% 
  str_extract(noun)

has_noun %>% 
  str_match(noun)

tibble(sentence = sentences) %>% 
  extract(
    sentence, c("article", "noun"), "(a|the) ([^ ]+)",
    remove = F
  )

## Exercises

#1
tibble(sentence = sentences) %>% 
  extract(
    sentence, c("number", "word"), "(one|two|three|four|five|six|seven|eight|nine|ten) ([^ ]+)",
    remove = F
  ) %>% 
  na.omit(number)

#2
contraction <- "([A-Za-z]+)'([A-Za-z]+)"
sentences %>% 
  str_subset(contraction)

## Replacing Matches

x <- c("apple", "pear", "banana")
str_replace(x, "[aeiou]", "-")
str_replace_all(x, "[aeiou]", "-")

x <- c("1 house", "2 cars", "3 people")
str_replace_all(x, c("1" = "one", "2" = "two", "3" = "three"))

sentences %>% 
  str_replace("([^ ]+) ([^ ]+) ([^ ]+)", "\\1 \\3 \\2") %>% 
  head(5)

## Exercises

#1
str = "heoagjt/jgoiwe/\\/geoa"
writeLines(str_replace_all(str, "/", "\\\\"))

str = "fuckyouall/babo///busain///killus/////kkkk"
str

#2
str_replace_all(sentences[1:10], c("A" = "a", "B" = "b", "C" = "c", "D" = "d", "E" = "e", "F" = "f",
                                   "G" = "g", "H" = "h", "I" = "i", "J" = "j", "K" = "k", "L" = "l",
                                   "M" = "m", "N" = "n", "O" = "o", "P" = "p", "Q" = "q", "R" = "r",
                                   "S" = "s", "T" = "t", "U" = "u", "V" = "v", "W" = "w", "X" = "x",
                                   "Y" = "y", "Z" = "z"))

#3
words %>% 
  str_replace("^(.)(.*)(.)$", "\\3 \\2 \\1") %>% 
  head(5)
