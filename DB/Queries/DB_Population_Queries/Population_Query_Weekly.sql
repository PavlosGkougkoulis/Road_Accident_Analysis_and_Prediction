UPDATE weekly_accidents
SET fatalities = 
    CASE
        WHEN week = 1 THEN 14
        WHEN week = 2 THEN 13
        WHEN week = 3 THEN 11
        WHEN week = 4 THEN 10
        WHEN week = 5 THEN 6
        WHEN week = 6 THEN NULL  
        ELSE fatalities  
    END
WHERE year = 2022
  AND month = 1
  AND week BETWEEN 1 AND 6;

UPDATE weekly_accidents
SET fatalities = 
    CASE
        WHEN week = 1 THEN 6
        WHEN week = 2 THEN 5
        WHEN week = 3 THEN 10
        WHEN week = 4 THEN 8
        WHEN week = 5 THEN 2
        WHEN week = 6 THEN NULL  
        ELSE fatalities  
    END
WHERE year = 2022
  AND month = 2
  AND week BETWEEN 1 AND 6;

  UPDATE weekly_accidents
SET fatalities = 
    CASE
        WHEN week = 1 THEN 2
        WHEN week = 2 THEN 10
        WHEN week = 3 THEN 5
        WHEN week = 4 THEN 7
        WHEN week = 5 THEN 8
        WHEN week = 6 THEN NULL 
        ELSE fatalities  
    END
WHERE year = 2022
  AND month = 3
  AND week BETWEEN 1 AND 6;

    UPDATE weekly_accidents
SET fatalities = 
    CASE
        WHEN week = 1 THEN 1
        WHEN week = 2 THEN 10
        WHEN week = 3 THEN 18
        WHEN week = 4 THEN 19
        WHEN week = 5 THEN 11
        WHEN week = 6 THEN NULL  
        ELSE fatalities  
    END
WHERE year = 2022
  AND month = 4
  AND week BETWEEN 1 AND 6;

      UPDATE weekly_accidents
SET fatalities = 
    CASE
        WHEN week = 1 THEN 14
        WHEN week = 2 THEN 9
        WHEN week = 3 THEN 9
        WHEN week = 4 THEN 18
        WHEN week = 5 THEN 4
        WHEN week = 6 THEN NULL  
        ELSE fatalities  
    END
WHERE year = 2022
  AND month = 5
  AND week BETWEEN 1 AND 6;

  UPDATE weekly_accidents
SET fatalities = 
    CASE
        WHEN week = 1 THEN 12
        WHEN week = 2 THEN 11
        WHEN week = 3 THEN 12
        WHEN week = 4 THEN 11
        WHEN week = 5 THEN 12
        WHEN week = 6 THEN NULL  
        ELSE fatalities  
    END
WHERE year = 2022
  AND month = 6
  AND week BETWEEN 1 AND 6;

UPDATE weekly_accidents
SET fatalities = 
    CASE
        WHEN week = 1 THEN NULL
        WHEN week = 2 THEN 13
        WHEN week = 3 THEN 10
        WHEN week = 4 THEN 25
        WHEN week = 5 THEN 15
        WHEN week = 6 THEN 5
        ELSE fatalities  
    END
WHERE year = 2022
  AND month = 7
  AND week BETWEEN 1 AND 6;

  UPDATE weekly_accidents
SET fatalities = 
    CASE
        WHEN week = 1 THEN 7
        WHEN week = 2 THEN 18
        WHEN week = 3 THEN 8
        WHEN week = 4 THEN 8
        WHEN week = 5 THEN 13
        WHEN week = 6 THEN NULL  
        ELSE fatalities  
    END
WHERE year = 2022
  AND month = 8
  AND week BETWEEN 1 AND 6;

    UPDATE weekly_accidents
SET fatalities = 
    CASE
        WHEN week = 1 THEN 5
        WHEN week = 2 THEN 13
        WHEN week = 3 THEN 14
        WHEN week = 4 THEN 15
        WHEN week = 5 THEN 16
        WHEN week = 6 THEN NULL  
        ELSE fatalities  
    END
WHERE year = 2022
  AND month = 9
  AND week BETWEEN 1 AND 6;

      UPDATE weekly_accidents
SET fatalities = 
    CASE
        WHEN week = 1 THEN 18
        WHEN week = 2 THEN 11
        WHEN week = 3 THEN 16
        WHEN week = 4 THEN 22
        WHEN week = 5 THEN 6
        WHEN week = 6 THEN NULL  
        ELSE fatalities  
    END
WHERE year = 2022
  AND month = 10
  AND week BETWEEN 1 AND 6;

    UPDATE weekly_accidents
SET fatalities = 
    CASE
        WHEN week = 1 THEN 7
        WHEN week = 2 THEN 16
        WHEN week = 3 THEN 12
        WHEN week = 4 THEN 12
        WHEN week = 5 THEN 7
        WHEN week = 6 THEN NULL  
        ELSE fatalities  
    END
WHERE year = 2022
  AND month = 11
  AND week BETWEEN 1 AND 6;

      UPDATE weekly_accidents
SET fatalities = 
    CASE
        WHEN week = 1 THEN 1
        WHEN week = 2 THEN 9
        WHEN week = 3 THEN 13
        WHEN week = 4 THEN 12
        WHEN week = 5 THEN 17
        WHEN week = 6 THEN 2  
        ELSE fatalities  
    END
WHERE year = 2022
  AND month = 12
  AND week BETWEEN 1 AND 6;

