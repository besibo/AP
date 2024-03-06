# Functions to prettify tables

## Adds styling and eventually, column totals
beautify_table <- function(tbl, total = TRUE) {
  if (total){
    tbl |> 
      adorn_totals("row") |> 
      kbl() |> 
      kable_styling(c("striped", "hover")) |> 
      row_spec(nrow(tbl) + 1, bold = TRUE) |> 
      row_spec(c(0, nrow(tbl)), extra_css = "border-bottom: 1px solid;")
  } else {
    tbl |> 
      kbl() |> 
      kable_styling(c("striped", "hover"))
  }
}

## Adds styling, currency symbols and eventually, column totals
beautify_cost_table <- function(tbl, total = TRUE) {
  if (total) {
    tbl |> 
      adorn_totals("row") |> 
      mutate_if(is.numeric, 
                ~dollar(., prefix = "", suffix = "€")) |> 
      kbl(align = "r") |> 
      kable_styling(c("striped", "hover")) |> 
      row_spec(nrow(tbl) + 1, bold = TRUE) |> 
      row_spec(c(0, nrow(tbl)), extra_css = "border-bottom: 1px solid;")
  } else {
    tbl |> 
      mutate_if(is.numeric, 
                ~dollar(., prefix = "", suffix = "€")) |> 
      kbl(align = "r") |> 
      kable_styling(c("striped", "hover"))
  }
}

# Divers list
nb_membres <- length(membres)
nb_invites <- length(invites)
plongeurs <- c(membres.full, invites.full)
plongeurs_tbl <- tibble(Plongeur = plongeurs, 
                        statut = rep(c("membre", "invité"), c(nb_membres, nb_invites)))

# Creation of the table listing diving sites
## First, get the dates and sites
divelist <- tibble(Date = seq(dmy(first.day), dmy(last.day), by = "day") |> rep(each = 2),
                   Quand = rep(c("am", "pm"), length(Date)/2),
                   Site = sites) |> 
  mutate(Date = stamp("29 10 1999", quiet = TRUE)(Date))

## Then, add dives for each diver, unite the tables and compute totals
liste_plongees <- plongees |> 
  t() |> 
  as_tibble() |> 
  set_names(plongeurs) |> 
  bind_cols(divelist) |> 
  relocate(Date, Quand, Site) |> 
  na.omit() |> 
  adorn_totals(where = "col")

## Finally, compute the total number of dives for each diver
Total <- liste_plongees |> 
  summarise_if(is.numeric, sum)

# Creation of the table for dive-related spendings
depenses <- depenses |> 
  mutate(Qui = membres.full) |> 
  adorn_totals(where = "col")

# Computation of diving price for each diver
cout_membres <- round(sum(depenses$Total)/sum(liste_plongees$Total),2) + overhead
cout_guest <- dive.guest + overhead
cout_guest <- if_else(cout_membres < cout_guest, cout_guest, cout_membres)

# Computation of dive-related costs for each diver
cout_plongee <- plongeurs_tbl |> 
  mutate(Nb_plongée = Total |> select(-Total) |> as.numeric()) |> 
  mutate(Cout = if_else(statut == "membre", cout_membres * Nb_plongée, cout_guest * Nb_plongée))

# Other spendings
autres <- autres |> 
  mutate(Qui = plongeurs) |> 
  adorn_totals(where = "col")

# Number of nights and meals for each diver
nr <- rbind(nuitees,repas)
colnames(nr) <- plongeurs
rownames(nr) <- c("Nuitées", "Repas")

# Other costs for each person
n_pers  <- sum(famille)
n_nuits <- sum(nuitees)
n_repas <- sum(repas)

autres_unit <- autres |> 
  pivot_longer(-c(Qui, Total), names_to = "Poste", values_to = "Montant") |> 
  summarise(Total = sum(Montant), .by = Poste) |> 
  mutate(Total_indiv = Total / c(n_nuits, n_repas, rep(n_pers, ncol(autres) - 4))) |> 
  filter(Total != 0) |> 
  select(-Total) |> 
  pivot_wider(names_from = Poste, values_from = Total_indiv) |> 
  rename(Nuitée = Logement, Repas = Nourriture)

# Other costs for each diver
autres_couts <- autres |> 
  pivot_longer(-c(Qui, Total), names_to = "Poste", values_to = "Montant") |> 
  summarise(Total = sum(Montant), .by = Poste) |> 
  mutate(Total_indiv = Total / c(n_nuits, n_repas, rep(n_pers, ncol(autres) - 4)))

quant_divers <- tibble(Qui = plongeurs,
       Logement = nuitees,
       Nourriture = repas,
       Carburant = famille,
       Peage = famille,
       Navette = famille,
       Divers = famille)|> 
  column_to_rownames("Qui") |> 
  as.matrix()

couts_unitaires <- autres_couts |> 
  select(-Total) |> 
  pivot_wider(names_from = Poste, values_from = Total_indiv) |> 
  as.matrix()

cout_par_plongeur <- quant_divers |> 
  sweep(2, couts_unitaires, "*") |> 
  t() |> 
  as.data.frame() |> 
  rownames_to_column("Poste") |> 
  adorn_totals(where = "col") |> 
  filter(Total != 0)


# Cout total pour chaque plongeur, plongée et autres
cout_all <- cout_plongee |> 
  select(Plongeur, Cout) |> 
  pivot_wider(names_from = Plongeur, values_from = Cout) |> 
  mutate(Poste = "Plongée") |> 
  relocate(Poste) |> 
  bind_rows(cout_par_plongeur |> select(-Total))

# Reimbursment computations
## How much does each diver owe
cout_total <- cout_all |> 
  summarise_if(is.numeric, sum)

## How much did each AP member spend (dive-related purchases)
dep <- depenses |> 
  select(Qui, Depenses = Total)

## How much did each diver spend (non-dive-related purchases)
aut <- autres |> 
  select(Qui, Depenses = Total)

## Computation of reimbursments
bilan <- aut |> 
  bind_rows(dep) |> 
  summarise(Depenses = sum(Depenses), .by = Qui) |> 
  mutate(Dettes = t(cout_total)[,1],
         Ecarts = Depenses - Dettes,
         `Fait quoi` = if_else(Ecarts > 0, 
                               "se fait rembourser par l'AP",
                               "rembourse à l'AP"))

rembourse <- bilan |> 
  mutate(Ecarts = abs(Ecarts)) |> 
  select(Qui, `Fait quoi`, Montant = Ecarts) |> 
  arrange(`Fait quoi`, Qui)

benef <- bilan |> 
  summarise(sum(Ecarts)) |> 
  pull() |> 
  abs()


# A few plots

## How many divers on board for each dive?
tmp <- plongees |> 
  mutate(Qui = plongeurs,
         Statut = rep(c("Membre", "Invité"), c(length(membres), length(invites)))) |> 
  pivot_longer(cols = -c(Qui, Statut),
               names_to = "Plongée",
               values_to = "Présence") |> 
  summarise(n = sum(Présence), .by = c(Plongée, Statut)) |> 
  pivot_wider(names_from = Statut, values_from = n)

if (ncol(tmp) == 2) {
  tmp <- tmp |> 
    mutate(Invité = 0)
}
  
pl1 <-  tmp |>  
  mutate(Total = Membre + Invité) |> 
  filter(Total != 0) |> 
  mutate(Plongée = seq_along(Plongée)) |> 
  pivot_longer(-Plongée, names_to = "Statut", values_to = "n") |> 
  ggplot(aes(x = Plongée, y = n, color = Statut)) +
  geom_point() +
  geom_line(aes(group = Statut)) +
  expand_limits(y = 0) +
  labs(x = "Plongée", 
       y = "Effectif", 
       title = "Nombre de plongeurs sur le Valiant",
       color = "") +
  scale_color_brewer(palette = "Accent") +
  theme_bw()

## Type of spendings as a proportion of all spendings
tab1 <- depenses |> 
  adorn_totals(where = "row") |> 
  filter(Qui == "Total") |> 
  select(-Qui, -Total) |> 
  pivot_longer(everything(), names_to = "Poste", values_to = "Montant") |> 
  mutate(Type = "Plongée")

tab2 <- autres |> 
  adorn_totals(where = "row") |> 
  filter(Qui == "Total") |> 
  select(-Qui, -Total) |> 
  pivot_longer(everything(), names_to = "Poste", values_to = "Montant") |> 
  mutate(Type = "Autre")

tab3 <- tab1 |> 
  bind_rows(tab2) |> 
  filter(Montant != 0) |> 
  mutate(Type = factor(Type, levels = c("Plongée", "Autre"))) |> 
  arrange(Type, Montant) |> 
  mutate(Poste = factor(Poste, levels = Poste))

pl2 <- tab3 |> 
  ggplot(aes(x = Type,
             y = Montant, fill = Poste)) +
  geom_col(position = "fill", color = "grey20") +
  labs(x = "Type de dépense",
       y = "Proportion") +
  theme_bw() +
  scale_fill_viridis_d(option = "D") +
  scale_y_continuous(labels = percent_format())

pl3 <- tab3 |> 
  ggplot(aes(x = 1,
             y = Montant, fill = Poste)) +
  geom_col(position = "fill", color = "grey20") +
  labs(x = "",
       y = "Proportion") +
  theme_bw() +
  scale_fill_viridis_d(option = "D") +
  scale_y_continuous(labels = percent_format()) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  coord_flip()
  

