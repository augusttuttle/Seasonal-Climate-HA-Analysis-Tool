Home HVAC analysis
Home Assistant logs hourly temperature and humidity from the two ecobee thermostats in my house, plus furnace and AC runtime and heating and cooling degree days. This project uses R to look at that data:

how indoor temperature on each floor changed over the season
how the upstairs and main floor differ, month by month
furnace hours per heating degree day, as a measure of how efficiently the house heats
The thermostat schedule that produces this data is in ha-ecobee-seasonal-climate.
