
#### Background
**solR** is a R library to perform comparative analysis of solar system power performance.

Conclusions are based on environmental monitoring data from two types of solar panels placed in five different spatial orientations in the University of Latvia Botanical Garden area.

##### Experimental setup

Combinations of directions and degrees in the setup:  
D: 13, 40, 90  
R: 13  
A: 13  

Working example of reading this information from column names for *solD90LG_PV_W*

| e.g | component   | parameters                                      |
|---- | ----------- |:------------------                              |
| sol | sol         | system: solar charger                           |
| D   | direction   | D for south, A for east, R for west             |
| 90  | degrees     | degree between panel and horizon: 13, 40 or 90  |
| LG  | type        | solar panel type: JA or LG                      |
| PV  | device      | Battery or Photovoltaics (PV)                   |
| W   | measurement | voltage, current or power defined by the units  |