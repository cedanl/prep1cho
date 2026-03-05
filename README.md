<div align="center">
  <h1>prep1cho</h1>

  <p>📊 R package voor 1CijferHO data voorbereiding</p>

  <p>
    <img src="https://img.shields.io/badge/R-276DC3?logo=r&logoColor=white" alt="R">
    <img src="https://img.shields.io/badge/Package-R-blue" alt="R Package">
    <img src="https://badgen.net/github/last-commit/cedanl/1cho_ins_preparation_r" alt="Last Commit">
    <img src="https://badgen.net/github/contributors/cedanl/1cho_ins_preparation_r" alt="Contributors">
  </p>
</div>

## 📋 Overzicht

> [!NOTE]
> Dit package bevat synthetische testdata, zodat je direct aan de slag kunt zonder eigen data.

**Een R package om het 1CijferHO instellingsbestand te transformeren naar een dataproduct dat geschikt is voor visualisatie in dashboards.**

`prep1cho` is een CEDA package voor het voorbereiden van 1CijferHO inschrijvingsdata. Voor een voorbeeld van een dashboard gebouwd op deze data, zie de [1cho_ins_visualisation_tableau repository](https://github.com/ed2c/1cho_ins_visualisation_tableau).

### Waarom 1CijferHO?

- **Vergelijkbaarheid** - Landelijke uniformiteit maakt vergelijking met andere instellingen eenvoudig
- **Hoge kwaliteit** - Gevalideerd door DUO én instellingen zelf
- **Uitgebreide documentatie** - Meer dan 100 variabelen, inclusief CBS en DUO data
- **Niet intern beschikbaar** - Bevat gegevens die niet in interne datasets voorkomen

<br>

## 🚀 Snel Starten

### Installatie

```r
# Installeer vanaf GitHub
# install.packages("devtools")
devtools::install_github("cedanl/prep1cho")
```

### Basis Gebruik

```r
library(prep1cho)

# Laad 1CHO inschrijvingsgegevens
enrollments <- read.csv2("EV299XX24_DEMO_decoded.csv")

# Voer de complete pipeline uit
data <- run_pipeline(
  enrollments,
  year = 2024,
  institution_brin = "21XX"
)

# Bekijk resultaat
head(data)

# Sla op (optioneel)
write.csv2(data, "output.csv", row.names = FALSE)
```

### Stap-voor-Stap Gebruik

Voor meer controle kun je de pipeline ook stap voor stap uitvoeren:

```r
library(prep1cho)

# Laad ruwe data
enrollments <- read.csv2("EV299XX24_DEMO_decoded.csv")

# Stap 1: Haal RIO referentiedata op
rio_data <- get_rio(force_download = FALSE)

# Stap 2: Controleer datakwaliteit
enrollments_clean <- audit_enrollments(enrollments)

# Stap 3: Bereid RIO data voor
rio_prepared <- prepare_rio(
  rio_data,
  year = 2024,
  institution_brin = "21XX",
  create_synthetic = TRUE
)

# Stap 4: Bereid inschrijvingen voor met mappingtabellen
enrollments_prep <- enrollments_clean |>
  prepare_enrollments_mapping() |>
  prepare_enrollments_supplemental(
    year = 2024,
    institution_brin = "21XX"
  )

# Stap 5: Combineer met RIO en voeg berekende velden toe
data <- enrollments_prep |>
  combine_enrollments_rio(rio_prepared$rio_per_jaar) |>
  combine_enrollments_calculations() |>
  combine_enrollments_final()

# Klaar! Data is nu gereed voor export/visualisatie
head(data)
```

**Voordelen van stap-voor-stap:**
- 🔍 Inspecteer tussenresultaten na elke stap
- 🐛 Vind problemen sneller tijdens development
- 🔧 Pas individuele stappen aan zonder hele pipeline te herschrijven
- 📊 Gebruik tussenresultaten voor analyses

### Development Setup

Voor package development:

```bash
# 1. Clone repository
git clone https://github.com/cedanl/prep1cho.git
cd prep1cho

# 2. Open in RStudio
# Open het .Rproj bestand
```

```r
# 3. Laad package voor development
devtools::load_all()

# 4. Run tests
devtools::test()

# 5. Build package
devtools::build()
```

### Data Voorbereiden (indien nodig)

Heb je ruwe 1CijferHO data in ASCII-formaat? Gebruik dan eerst de [1cijferho tool](https://github.com/cedanl/1cijferho/) om:
- Het instellingsbestand om te zetten van ASCII naar CSV
- Automatisch decoderingbestanden te koppelen

<br>

## 📁 Package Structuur

```
├── R/               # Package functies
├── man/             # Documentatie
├── inst/            # Metadata (mapping tabellen, assertions)
├── data/            # Example datasets
└── tests/           # Package tests
```

<br>

## 🎯 Package Functies

Het package biedt functies om 1CijferHO data voor te bereiden voor visualisatie: kolomnamen en waarden worden begrijpelijk gemaakt, met categorische variabelen die beperkt genoeg zijn om als kleurgroep in grafieken te gebruiken.

### Belangrijkste Functies

#### Pipeline Functies
- `run_pipeline()` - Voer volledige pipeline uit (AANBEVOLEN voor productie)
- `get_rio()` - Haal RIO referentiedata op

#### Stap-voor-Stap Functies
- `audit_enrollments()` - Controleer datakwaliteit van ruwe data
- `prepare_rio()` - Bereid RIO referentiedata voor
- `prepare_enrollments_mapping()` - Pas mappingtabellen toe op inschrijvingen
- `prepare_enrollments_supplemental()` - Voeg aanvullende berekende velden toe
- `combine_enrollments_rio()` - Verrijk inschrijvingen met RIO gegevens
- `combine_enrollments_calculations()` - Bereken studiesucces indicatoren
- `combine_enrollments_final()` - Finale transformaties en opschoning

### Afgeleide Variabelen

1. **Verrijking met karakterwaarden** - Ruwe data bevat codes; ondersteunende bestanden leveren de bijbehorende tekstwaarden

2. **Nieuwe variabelen**:
   - Studiejaar (Student in opleiding)
   - Uitval eerste jaar vóór 1 februari
   - Aantal inschrijvingen bij instelling in gegeven jaar (dubbele studie)
   - VO-profielen
   - Tussenjaren
   - Aansluiting (instroom)

3. **CROHO-verrijking** - Opleidingsdata met EC's en nominale studieduur

4. **Prestatie-indicatoren** - Nominaal studiesucces en uitval

<br>

## ⚙️ Package Parameters

De pipeline kan worden aangepast via functie parameters:

```r
result <- run_pipeline(
  enrollments,
  year = 2024,                    # Academisch jaar
  institution_brin = "21XX",      # BRIN code instelling
  create_synthetic = TRUE,        # Synthetische testdata toevoegen
  download_rio = FALSE            # RIO data ophalen
)
```

<br>

## 🔗 Afhankelijkheden

Dit package gebruikt packages van de [vusaverse](https://github.com/vusaverse/):

- **vusa** - Data transformatie utilities
- **vvauditor** - Data auditing
- **vvconverter** - Waarden mappen en basis aanpassingen
- **vvmover** - Dynamisch opslaan en laden

<br>

## ✅ CEDA Checklist

<details>
<summary>Bekijk status</summary>

| Item | Status |
|------|--------|
| Code draait succesvol | ✅ |
| Config bestand voor instellingsspecifieke settings | ✅ |
| Build bestand | ✅ |
| Instructiebestand met doel en context | ✅ |
| Duidelijke structuur volgens best practices | ✅ |
| Data dictionaries bij start en eind | ✅ |
| Gestijlde code ([tidyverse guide](https://style.tidyverse.org/)) | ✅ |
| Machine-leesbare bestanden (.R, .csv, .yaml, .md, .qmd) | ✅ |
| Automatische validatie van databestanden | ⏳ |
| Synthetische of dummy startdata | ✅ |
| Engelse taal voor code en documentatie | ✅ |
| Glossary met kolomnamen en uitleg | ⏳ |

</details>

<br>

## 🤝 Bijdragen

Bijdragen zijn welkom! Als je bugs vindt, feature requests hebt, of code-verbeteringen wilt bijdragen, open dan een issue of pull request op de GitHub repository.

<br>

## 📚 Meer Informatie

- [Package documentatie](man/) - Functie referenties
- [DUO 1CijferHO pagina](https://duo.nl/zakelijk/hoger-onderwijs/studentenadministratie/bron-controleren/deelnames-en-resultaten-duo-registers.jsp)
- [CEDA GitHub](https://github.com/cedanl)
