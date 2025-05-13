UPDATE monthly_accidents
SET fatalities = (
    SELECT SUM(COALESCE(wd.fatalities, 0))
    FROM weekly_accidents wd
    WHERE wd.year = monthly_accidents.year
    AND wd.month = monthly_accidents.month
)
WHERE year = 2022;