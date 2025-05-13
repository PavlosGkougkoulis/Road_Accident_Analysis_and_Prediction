import pandas as pd

input_file = "E:/University/Thesis/Thesis_Other/Thesis_Test_Code/Excel_Preprocessing/Accidents-1996-2022-2.xlsx"
output_file = "E:/University/Thesis/Thesis_Other/Thesis_Test_Code/Excel_Preprocessing/fatalities_pivot_tables_1996_2014.xlsx"

try:
    data = pd.read_excel(input_file, header=0)
    print(data.head())  
except FileNotFoundError:
    print(f"The file {input_file} was not found.")
    exit()

data.columns = ["accident_month", "accident_week"] + [str(year) for year in range(1996, 2015)]

data = data.fillna(0)

month_map = {
    "ΙΑΝΟΥΑΡΙΟΣ": 1,
    "ΦΕΒΡΟΥΑΡΙΟΣ": 2,
    "ΜΑΡΤΙΟΣ": 3,
    "ΑΠΡΙΛΙΟΣ": 4,
    "ΜΑΪΟΣ": 5,
    "ΙΟΥΝΙΟΣ": 6,
    "ΙΟΥΛΙΟΣ": 7,
    "ΑΥΓΟΥΣΤΟΣ": 8,
    "ΣΕΠΤΕΜΒΡΙΟΣ": 9,
    "ΟΚΤΩΒΡΙΟΣ": 10,
    "ΝΟΕΜΒΡΙΟΣ": 11,
    "ΔΕΚΕΜΒΡΙΟΣ": 12
}
data["accident_month"] = data["accident_month"].map(month_map)

print(data[data["accident_month"].isnull()])

data = data.dropna(subset=["accident_month"])

with pd.ExcelWriter(output_file, engine='openpyxl') as writer:

    for year in range(1996, 2015):

        year_data = data[["accident_month", "accident_week", str(year)]]
        all_months = list(range(1, 13))
        pivot_table = pd.pivot_table(
            year_data,
            values=str(year),
            index="accident_week",
            columns="accident_month",
            aggfunc="sum",
            dropna=False
        )

        pivot_table = pivot_table.reindex(columns=all_months)

        pivot_table.to_excel(writer, sheet_name=str(year))

        print(f"Pivot table for {year} successfully saved to sheet {year}.")

print(f"All pivot tables have been saved to {output_file}.")
