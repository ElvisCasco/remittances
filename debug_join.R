library(tidyverse)
library(readxl)
library(here)
library(stringi)

# Read both files
df_municipios_completo <- read_excel('Municipios.xlsx', sheet = 'municipios_completo')
df_remesas_municipio <- read_excel('Remesas.xlsx', sheet = 'Municipio', skip = 0)

# Show columns
cat('=== Municipios completo columns ===\n')
print(names(df_municipios_completo))
cat('\n=== Remesas municipio columns ===\n')
print(names(df_remesas_municipio))

# Show sample data
cat('\n=== First 3 rows of Municipios ===\n')
print(head(df_municipios_completo, 3))
cat('\n=== First 3 rows of Remesas (first 8 cols) ===\n')
print(head(df_remesas_municipio[, 1:min(8, ncol(df_remesas_municipio))], 3))

# Try to identify the matching columns
cat('\n=== Trying to join ===\n')

# Trim and normalize
rem <- df_remesas_municipio %>% mutate(across(where(is.character), str_trim))
mun <- df_municipios_completo %>% mutate(across(where(is.character), str_trim))

# Check for common column patterns
cat('\nSearching for department column in municipios...\n')
dept_cols <- names(mun)[str_detect(tolower(names(mun)), 'depto|departamento|dept|nomdepto')]
print(dept_cols)

cat('\nSearching for municipality column in municipios...\n')
mun_cols <- names(mun)[str_detect(tolower(names(mun)), 'mun|nommun')]
print(mun_cols)

cat('\nSearching for department column in remesas...\n')
dept_cols_rem <- names(rem)[str_detect(tolower(names(rem)), 'depto|departamento|dept')]
print(dept_cols_rem)

cat('\nSearching for municipality column in remesas...\n')
mun_cols_rem <- names(rem)[str_detect(tolower(names(rem)), 'mun')]
print(mun_cols_rem)

cat('\nNumeric columns in municipios (likely lat/lon):\n')
print(names(mun)[sapply(mun, is.numeric)])
