library(dplyr)
library(stringr)
library(tidytext)
library(tidyr)
library(widyr)
#####################
# Here, I am working with: chapter 15 of Jockers and Thalken; starting p. 178
# Note: they have xml files, I have csv files. Let's see what happens
#####################
# Not necessary here, but for future reference: rm(list=ls()) remove all variables and functions from the current environment in R
# Look at pp. 195ff in Wickham's "R for data science" for regex
input_dir <- "eebo-tcp-partial-p1"
files_v <- dir(path = input_dir, pattern = ".*xml")
files_v
# For each file name, we are going to load and process the corresponding text
# Step 1: start by loading the xml2 package
library(xml2)
# Step 2: create a vector of the file path of my selected texts
file.path(input_dir, files_v)
# Step 3: create an empty list that will eventually contain all the documents as I process them via a "for loop"
book_freqs_l <-list()
### Use the TEI encoding elements to organize the text in the XML files.
# For Early Modern texts, paragraphs are the most meaningful division.
# In TEI: <p> is a child of <body> which is a child of <text>

#Step 4: define two custom functions to get word frequencies from the XML files, using the TEI encoding conventions
# node is the XML node object, ns is the namespace. The function finds all the child nodes in the "node" object that match
# the xpath argument; it extracts the textual content; pastes everything together into a string
get_node_text <- function(node, xpath, ns){
  paragraph_nodes <- xml_find_all(node, xpath, ns)
  paragraph_v <- xml_text(paragraph_nodes)
  paste(paragraph_v, collapse = " ")
}
# make a custom tokenize function where I can easily change whether I want to keep upper case or not
tokenize <- function(files_v, pattern = "[^A-Za-z0-9']", lower = TRUE){
  if(lower){
    files_v <- tolower(files_v)
  }
  word_v <- unlist(strsplit(files_v, pattern))
  word_v[which(word_v != " ")]
}
# Step 5: put together the for loop> Note: use the TEI encoding elements to organize the text in the XML files
# for Early Modern texts, paragraphs are the most meaningful division.
# In TEI: <p> is a child of <body> which is a child of <text>

# If I want to check that things are working so far, use:
# i <- 1
# file.path(input_dir, files_v[i])

for(i in seq_along(files_v)){
#loading the xml files
  xml_doc <- read_xml(file.path(input_dir, files_v[i]))
  para_text <- get_node_text(xml_doc, 
                             xpath = "/tei:TEI/tei:text/tei:body//tei:p",
# specifying that all elements within the document are in the TEI namespace
                             ns = c(tei = "http://www.tei-c.org/ns/1.0")
)
# parse the string of text into a vector of individual word tokens using the custom tokenize function
word_v <-tokenize(para_text)
#use table to count the occurrences of each word and then divide by the length of the entire vector to get frequencies
freq_table <- table(word_v)/length(word_v)
book_freqs_l[[files_v[i]]] <- as.data.frame(
  freq_table, stringsASFactors = FALSE
)
}

      # In order to perform hierarchical clustering, we need to compute the Euclidean distance between each document.
      #Note: there are other measures that will yield different clusters, but let's start with Euclidean
#Step 6: To compute the distance, we need to turn the book_freqs_l into a data matrix; 
#First we turn the list into a data frame suing "do.call"

freqs_df <- do.call(cbind, book_freqs_l)

