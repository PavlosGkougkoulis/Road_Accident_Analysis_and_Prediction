UPDATE yearly_accidents
SET fatalities = (
    SELECT SUM(COALESCE(md.fatalities, 0))
    FROM monthly_accidents md
    WHERE md.year = yearly_accidents.year
)
WHERE year = 2022;