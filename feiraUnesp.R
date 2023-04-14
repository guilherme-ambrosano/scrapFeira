library(tidyverse)
library(rvest)
library(pdftools)
library(writexl)

editoras <- "https://feiradolivrodaunesp.com.br/editoras"

# Pegando os links dos arquivos
links <- editoras %>%
  read_html() %>%
  html_nodes("h4") %>%
  html_nodes("a") %>%  # primeiro o link das editoras
  (function(x) {
    link <- html_attr(x, "href")
    editora <- html_text(x)
    names(link) <- editora
    return(link)
  }) %>%  # depois o link de cada um dos PDFs (presencial e virtual)
  map(. %>%
        read_html() %>%
        html_nodes("a") %>%
        html_attr("href") %>%
        str_subset("\\.pdf"))

# PDF bugado
links <- links[names(links)!="Saber Publicações"]

# Baixando os PDFs
#!!!!!!!!!!!!!!-------- SETWD NA PASTA CERTA --------!!!!!!!!!!!!!!!
links %>% 
  unlist() %>%
  map(function(link) {
    arquivo <- basename(link)
    if(!(arquivo %in% list.files(pattern="*.pdf"))) { # se já não tiver baixado
      download.file(link, arquivo, mode="wb")
    }
  })

# Lendo os preços dos PDFs
livros <- links %>%
  enframe(name = "editora", value = "link") %>%
  unnest_longer(link) %>%
  mutate(arquivo = basename(link)) %>%
  pmap_dfr(function(editora, link, arquivo) {
    print(editora)
    texto <- str_extract_all(
      str_replace_all(
        str_replace_all(
          paste(pdf_text(arquivo), collapse="\n"),
          "\\t", "  "),
        "Gratuito", "R$0,0"),
      ".* *(R\\$)? *\\d+[,\\.]\\d+ *( *(R\\$)? *\\d+[,\\.]\\d+ *)*.*")[[1]]
    livros_editora <- tibble(editora=editora, link=link, arquivo=arquivo,
                             texto=texto) %>%
      unnest_longer(texto)
    print(length(texto))
    if (length(texto) > 2000) {
      return(tibble(editora=editora, link=link, arquivo=arquivo,
                    texto=NA))
    }
    return(livros_editora)
  })

# teste
str_extract_all(paste(pdf_text(basename(links[[150]][1])), collapse = "\n"),
                ".* *(R\\$)? *\\d+[,\\.]\\d+ *( *(R\\$)? *\\d+[,\\.]\\d+ *)*.*")

# Salvando
precos_livros <- livros %>%
  filter(!str_detect(texto, "\\(21\\)3882\\.8200")) %>% 
  select(editora, arquivo, texto) %>%
  mutate_at(vars(texto), str_replace_all, " +", " ") %>% 
  mutate_at(vars(texto), str_replace, "^ ", "") %>% 
  mutate_at(vars(texto), str_replace_all, "\\$ ", "$") %>% 
  mutate(menor_preco = str_extract(texto, "(R\\$)? *\\d+[,\\.]\\d{2} *( *(R\\$)? *\\d+[,\\.]\\d{2} *)*")) %>% 
  mutate(texto = str_remove(texto, "(R\\$)? *\\d+[,\\.]\\d{2} *( *(R\\$)? *\\d+[,\\.]\\d{2} *)*")) %>% 
  mutate(texto = str_remove(texto, "^9\\d{12} ")) %>% 
  mutate(menor_preco = str_extract_all(menor_preco, "(R\\$)? *\\d+[,\\.]\\d{2}")) %>% 
  mutate_at(vars(menor_preco), ~sapply(., function(x) str_replace_all(x, ",", "."))) %>%
  mutate_at(vars(menor_preco), ~sapply(., parse_number)) %>% 
  mutate(maior_preco = menor_preco) %>% 
  mutate_at(vars(menor_preco), ~sapply(., min)) %>%
  mutate_at(vars(maior_preco), ~sapply(., max)) %>%
  mutate(desconto = (maior_preco-menor_preco)/maior_preco*100)

precos_livros %>% 
  write_xlsx("Livros.xlsx")

precos_livros %>% 
  filter(editora %in% sample(c("Devir", "Martins Fontes", "WMF Martins Fontes",
                               "Companhia das Letras", "34", "Aleph",
                               "Antofágica", "Autêntica", "Brasiliense", "Cult",
                               "Dublinense", "É Realizações", "Edições Sesc",
                               "Edusp", "Elefante", "Excelsior", "Expressão Popular",
                               "Gente", "Grupo Editorial Record", "Intrínseca",
                               "L&PM", "LeYa Brasil", "Lote 42", "Madras", 
                               "Martin Claret", "Morro Branco", "n-1 edições",
                               "Nemo", "Parábola", "Senac", "Todavia", "Ubu",
                               "Unesp", "Vozes", "Unicamp"), size=7)) %>%
  filter(desconto >= 50, menor_preco <= 25, menor_preco > 20) %>% 
  mutate_at(vars(texto), factor) %>%
  mutate(texto = fct_reorder(texto, menor_preco, .desc=T)) %>% 
  ggplot(aes(y=menor_preco, x=texto, color=editora)) +
  geom_point(show.legend=F) +
  facet_grid(~editora, scales = "free_x", space="free_x") +
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))


precos_livros %>% 
  filter(editora %in% c("Devir", "Martins Fontes", "WMF Martins Fontes",
                        "Companhia das Letras", "34", "Aleph",
                        "Antofágica", "Autêntica", "Brasiliense", "Cult",
                        "Dublinense", "É Realizações", "Edições Sesc",
                        "Edusp", "Elefante", "Excelsior", "Expressão Popular",
                        "Gente", "Grupo Editorial Record", "Intrínseca",
                        "L&PM", "LeYa Brasil", "Lote 42", "Madras", 
                        "Martin Claret", "Morro Branco", "n-1 edições",
                        "Nemo", "Parábola", "Senac", "Todavia", "Ubu",
                        "Unesp", "Vozes", "Unicamp")) %>%
  filter(desconto >= 50, menor_preco <= 20, menor_preco > 10) %>%
  arrange(editora, menor_preco) %>%
  View()

  

       