
# explore_xml_for_textmining

<!-- badges: start -->
<!-- badges: end -->


This repo documents my explorations of using the {xml2} package to process multiple xml documents.

My insights are in `03_looking_for_paragraphs.qmd` and `04_iterate.qmd`

## summary

Exploring the techniques of Jockers and Thalken's [*Text Analysis with R*](https://link.springer.com/book/10.1007/978-3-030-39643-5) @jockers2020.  It's clear that a major issue is composing the proper xpath.  There are many useful youtube tutorials on xpath, or xpath and tei.  Getting the xpath correct may be a key to good analysis.  I have taken a stab, with an eye towards functionality.  But I have stopped short of being concerned about the precision of the xpath.  Researchers should pay special attention to the xpath so they are clear about their corpus.

Beyond Jockers and Thalken, I highly recomment Sigle and Robinson's [Text Mining with R](https://www.tidytextmining.com/) and the helpful {tidytext} package.  I think Silge and Robinson's data wrangling techniques, being tidyverse inspired, are more accessible to most people.  Employing Jockers and Thalken algoritms may still be necessary on a strategic basis, but at least the researcher will not have to be delayed but base-R techniques.  Of course this recommendation is only relevant if a person prefers the grammatical assumptions of tidyverse.  Base-R people are free to continue their approach without any asperstions from me.

Lastly, while I have not read it.  Hvitfeldt and Silge's [Supervised Machine Learning for Text Analysis in R](https://smltar.com/) is a text I would read if I were interested in deepening my understanding of text analysis.


