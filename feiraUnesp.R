library(tidyverse)
library(rvest)
library(pdftools)
library(writexl)

editoras <- "https://feiradolivrodaunesp.com.br/editoras"

links <- editoras %>%
  read_html() %>%
  html_nodes("h4") %>%
  html_nodes("a") %>%
  (function(x) {
    link <- html_attr(x, "href")
    editora <- html_text(x)
    names(link) <- editora
    return(link)
  }) %>%
  map(. %>%
        read_html() %>%
        html_nodes("a") %>%
        html_attr("href") %>%
        str_subset("\\.pdf"))

links <- links[names(links)!="Saber Publicações"]

links %>% 
  unlist() %>%
  map(function(link) {
    arquivo <- basename(link)
    if(!(arquivo %in% list.files(pattern="*.pdf"))) {
      download.file(link, arquivo, mode="wb")
    }
  })

textos <- links %>%
  enframe(name = "editora", value = "link") %>%
  unnest_longer(link) %>%
  mutate(arquivo = basename(link)) %>%
  filter(arquivo != "Anexo-II-Lista-de-Livros-Saber-e-Ler_Nanabooks-Carlos-Eduardo-Sanita-1.pdf") %>% 
  pmap_dfr(function(editora, link, arquivo) {
    print(editora)
    textos_editora <- tibble(editora=editora, link=link, arquivo=arquivo,
           texto=pdf_text(arquivo)) %>%
      mutate_at(vars(texto), paste, collapse="\n") %>%
      mutate_at(vars(texto), str_replace_all, "\\t", "  ") %>% 
      mutate_at(vars(texto), str_extract_all,
                ".* *(R\\$ *)?\\d+[,\\.]\\d+ *( *(R\\$ *)?\\d+[,\\.]\\d+ *)?") %>%
      unnest_longer(texto)
    print(nrow(textos_editora))
    return(textos_editora)
  })

textos %>%
  write_csv("Livros.csv")

str_extract_all(paste(pdf_text(basename(links[[1]][1])), collapse = "\n"),
                ".* *R\\$ *\\d+[,\\.]\\d+ *( *R\\$ *\\d+[,\\.]\\d+ *)?")


       