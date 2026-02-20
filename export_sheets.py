import polars as pl
from pathlib import Path

FILE = Path(__file__).parent / "Ingresos de Divisas por Remesas Familiares_0303_Departamento_2017-2026_Municipios_2020_2026.xlsx"

# 1. Read every sheet raw (no header) to detect where headers are
raw_sheets: dict[str, pl.DataFrame] = pl.read_excel(
    FILE,
    sheet_id=0,          # 0 = all sheets
    has_header=False,
    drop_empty_rows=True,
    drop_empty_cols=True,
)

dfs: dict[str, pl.DataFrame] = {}
for sheet, raw in raw_sheets.items():
    # 2. First row where ≥2 cells are non-empty strings → real header
    header_row = 0
    for i in range(min(20, raw.height)):
        row = raw.row(i)
        n_str = sum(1 for v in row if isinstance(v, str) and v.strip() != "")
        if n_str >= 2:
            header_row = i
            break

    # 3. Re-read skipping pre-header rows; calamine passes skip_rows to fastexcel
    df = pl.read_excel(
        FILE,
        sheet_name=sheet,
        read_options={"skip_rows": header_row},
        drop_empty_rows=True,
        drop_empty_cols=True,
    )
    # Strip whitespace from column names
    df = df.rename({c: c.strip() for c in df.columns})
    dfs[sheet] = df
    print(f"[{sheet}]  header_row={header_row}  shape={df.shape}")
    print(df.head(3), "\n")

# Access each sheet as:
#   dfs["Departamentos"]
#   dfs["Municipios"]  … etc.
