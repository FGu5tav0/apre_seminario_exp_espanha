library(tidyverse)


dados <- readxl::read_excel("conta_espanha.xlsx",
                            sheet = "dados") |>
  janitor::clean_names()


df <- dados |>
  mutate(data_correta = dmy(data_correta, quiet = T),
         mes = month(data_correta)) |>
filter(is.na(viagem)) |>
  filter(!mes %in% c(4,5)) |>
  mutate(mes = factor(mes, levels = c(11,12,1,2,3),
                      labels = c("Novembro 2025", "Dezembro 2024", "Janeiro 2025",
                                 "Fevereiro 2025", "Março 2025")))

library(patchwork)

p1 <- df |>
  group_by(mes) |>
  filter(grepl("^mer", despesa, ignore.case = TRUE)) |>
  filter(quem_paga != "ecle") |>
  summarise(soma = sum(valor),
            soma_rela = ((soma*100)/1300)/2) |>
  ungroup() |>
  ggplot() +
  aes(x = mes, soma_rela, group = 1) +
  geom_line() +
  geom_label(aes(label = soma_rela |> round(digits = 1)), size = 8) +
  labs(title = "Gasto com mercado",
       x = NULL,
       y = "% da bolsa") +
  theme_minimal(24)


p2 <- df |>
  group_by(mes) |>
  filter(grepl("^alu", despesa, ignore.case = TRUE)) |>
  mutate(valor = case_when(
    quem_paga == "eu e ecle" ~ valor/2,
    .default = valor
  )) |>
  summarise(soma = sum(valor),
            soma_rela = ((soma*100)/1300)) |>
  ungroup() |>
  ggplot() +
  aes(x = mes, soma_rela, group = 1) +
  geom_line() +
  geom_label(aes(label = soma_rela |> round(digits = 1)), size = 8) +
  labs(title = "Gasto com aluguel e luz",
       x = NULL,
       y = "% da bolsa") +
  theme_minimal(24)

p3 <- df |>
  group_by(mes) |>
  filter(grepl("^bar", despesa, ignore.case = TRUE)) |>
  mutate(valor = case_when(
    quem_paga == "eu e ecle" ~ valor/2,
    .default = valor
  )) |>
  summarise(soma = sum(valor),
            soma_rela = ((soma*100)/1300)) |>
  ungroup() |>
  ggplot() +
  aes(x = mes, soma_rela, group = 1) +
  geom_line() +
  geom_label(aes(label = soma_rela |> round(digits = 1)), size = 8) +
  labs(title = "Gasto com bares e vida boa!",
       x = NULL,
       y = "% da bolsa") +
  theme_minimal(24)

p4 <- df |>
  group_by(mes) |>
  filter(grepl("^Trem rab", despesa, ignore.case = TRUE)) |>
  mutate(valor = case_when(
    quem_paga == "eu e ecle" ~ valor/2,
    .default = valor
  )) |>
  summarise(soma = sum(valor),
            soma_rela = ((soma*100)/1300)) |>
  ungroup() |>
  ggplot() +
  aes(x = mes, soma_rela, group = 1) +
  geom_line() +
  geom_label(aes(label = soma_rela |> round(digits = 1)), size = 8) +
  labs(title = "Gasto com trem para universidade",
       x = NULL,
       y = "% da bolsa") +
  theme_minimal(24)


(p1 | p2) / (p3 | p4)

ggsave(filename = "gasto.jpeg", width = 25, height = 15, dpi = 600)
