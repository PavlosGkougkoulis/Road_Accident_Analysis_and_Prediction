SELECT wi.*, 
       yi.total_gdp, yi.gdp_per_capita, yi.unemployment, yi.population, 
       yi.environmental_taxes, yi.vehicles, yi.petrol_cons, yi.ng_cons, 
       yi.el_cons, yi.fatal_per_million_veh, yi.speed_infr, yi.drink_infr, 
       yi.belt_infr, yi.helmet_infr, yi.inflation_rate, yi.gini_index
FROM weekly_accidents wi
JOIN yearly_indicators yi ON wi.year = yi.year
ORDER BY wi.year, wi.month, wi.week;
