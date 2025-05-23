SELECT ma.*, 
       yi.total_gdp, yi.gdp_per_capita, yi.unemployment, yi.population, 
       yi.environmental_taxes, yi.vehicles, yi.petrol_cons, yi.ng_cons, 
       yi.el_cons, yi.fatal_per_million_veh, yi.speed_infr, yi.drink_infr, 
       yi.belt_infr, yi.helmet_infr, yi.inflation_rate, yi.gini_index
FROM monthly_accidents ma
JOIN yearly_indicators yi ON ma.year = yi.year
ORDER BY ma.year, ma.month;