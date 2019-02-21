# trial for main import

colNames = function(id){
# returns colNames needed, based on id
if (id == "T000000"){
    # return meteo colNames
    colNames = c("timestamp", "tz", "wdir", "velocity", "pressure", "humidity",
                 "temperature", "solarIrradiance")
}    
else if (id == "kwh"){
    # return solar kWh colNames
    colNames  = c("timestamp", "gridToBattery", "gridToConsumers", "PVToBattery", "PVToGrid",
                     "PVToConsumers", "batteryToConsumers", "batteryToGrid", "gensetToConsumers",
                     "gensetToBattery", "gas")
}
else if (id == "main"){
    # return solar main colNames
    colNames = c("timestamp", "gatewayLogTime", "gatewayScheduledCarging", "gatewayRelay1state",
                 "busSysInputVPhase", "busSysInputAPhase", "busSysInputFreq1", "busSysInputPower1",
                 "busSysOutputVPhase", "busSysOutputAPhase", "busSysOutputFreq1", "busSysOutputPower1",
                 "busSysBatteryV", "busSysBatteryA", "busSysPhaseCount", "busSysActiveInput",
                 "busSysActiveInputALimit", "busSysBusChargeState", "busSysBusState", "busSysBusError",
                 "busSysSwitchPosition", "busSysTemp", "busSysLowBattery",
                 "busSysOverload", "busSysTempSensorAlarm", "busSysVSensorAlarm",
                 "busSysTempL1", "busSysLowBatteryL1", "busSysOverloadL1", "busSysRippleL1",
                 "busSysTempL2", "busSysLowBatteryL2", "busSysOverloadL2", "busSysRippleL2",
                 "busSysTempL3", "busSysLowBatteryL3", "busSysOverloadL3", "busSysRippleL3",
                 "batCheckBattV288", "batCheckStartBattV288", "batCheckA288", "batCheckConsumAmph288",
                 "batCheckChargeState288", "batCheckTimeToGo288", "batCheckAlarmLowV288", "batCheckAlarmHighV288",
                 "batCheckAlarmLowStartV288", "batCheckAlarmHighStartV288", "batCheckAlarmLowCharge288", 
                 "batCheckAlarmLowBatTemp288", "batCheckAlarmHighBatTemp288", "batCheckAlarmMidV288",
                 "batCheckRelayStatus288", "batCheckDeepDischargeAh288", "batCheckLastDischargeAh288",
                 "batCheckMeanDischargeAh288", "batCheckChargeCycles288", "batCheckFullDischarges288", 
                 "batCheckTotalAh288", "batCheckMinV288", "batCheckMaxV288", "batCheckTimeSinceCharge288",
                 "batCheckAutoSync288", "batCheckAlarmLowV288","batCheckAlarmHighV288", "batCheckMinStartV288",
                 "batCheckMaxStartV288", "batCheckEnergyDischarged288", "batCheckEnergyCharged288",
                 "batCheckBattV303", "batCheckStartBattV303", "batCheckA303", "batCheckConsumAmph303",
                 "batCheckChargeState303", "batCheckTimeToGo303", "batCheckAlarmLowV303", "batCheckAlarmHighV303",
                 "batCheckAlarmLowStartV303", "batCheckAlarmHighStartV303", "batCheckAlarmLowCharge303",
                 "batCheckAlarmLowBatTemp303", "batCheckAlarmHighBatTemp303", "batCheckAlarmMidV303", 
                 "batCheckRelayStatus303", "batCheckDeepDischargeAh303", "batCheckLastDischargeAh303",
                 "batCheckMeanDischargeAh303", "batCheckChargeCycles303", "batCheckFullDischarges303",
                 "batCheckTotalAh303", "batCheckMinV303", "batCheckMaxV303", "batCheckTimeSinceCharge303",
                 "batCheckAutoSync303", "batCheckAlarmLowV303", "batCheckAlarmHighV303", "batCheckMinStartV303", 
                 "batCheckMaxStartV303", "batCheckEnergyDischarged303", "batCheckEnergyCharged303",
                 "solD40JA_BatV", "solD40JA_BatA", "solD40JA_BatW", "solD40JA_LoadState", "solD40JA_LoadCurrent",
                 "solD40JA_ChargerOn", "solD40JA_ChargeState", "solD40JA_PV_V", "solD40JA_PV_I", "solD40JA_PV_W", 
                 "solD40JA_Yield_kWh", "solD40JA_MaxWToday", "solD40JA_Error", "solD40JA_UsrYield_kWh",
                 "solD13JA_BatV", "solD13JA_BatA", "solD13JA_BatW", "solD13JA_LoadState", "solD13JA_LoadCurrent",
                 "solD13JA_ChargerOn", "solD13JA_ChargeState", "solD13JA_PV_V", "solD13JA_PV_I", "solD13JA_PV_W", 
                 "solD13JA_Yield_kWh", "solD13JA_MaxWToday", "solD13JA_Error", "solD13JA_UsrYield_kWh",
                 "solA13JA_BatV", "solA13JA_BatA", "solA13JA_BatW", "solA13JA_LoadState", "solA13JA_LoadCurrent",
                 "solA13JA_ChargerOn", "solA13JA_ChargeState", "solA13JA_PV_V", "solA13JA_PV_I", "solA13JA_PV_W", 
                 "solA13JA_Yield_kWh", "solA13JA_MaxWToday", "solA13JA_Error", "solA13JA_UsrYield_kWh",
                 "solA13LG_BatV", "solA13LG_BatA", "solA13LG_BatW", "solA13LG_LoadState", "solA13LG_LoadCurrent",
                 "solA13LG_ChargerOn", "solA13LG_ChargeState", "solA13LG_PV_V", "solA13LG_PV_I", "solA13LG_PV_W", 
                 "solA13LG_Yield_kWh", "solA13LG_MaxWToday", "solA13LG_Error", "solA13LG_UsrYield_kWh",
                 "solR13LG_BatV", "solR13LG_BatA", "solR13LG_BatW", "solR13LG_LoadState", "solR13LG_LoadCurrent",
                 "solR13LG_ChargerOn", "solR13LG_ChargeState", "solR13LG_PV_V", "solR13LG_PV_I", "solR13LG_PV_W", 
                 "solR13LG_Yield_kWh", "solR13LG_MaxWToday", "solR13LG_Error", "solR13LG_UsrYield_kWh",
                 "solR13JA_BatV", "solR13JA_BatA", "solR13JA_BatW", "solR13JA_LoadState", "solR13JA_LoadCurrent",
                 "solR13JA_ChargerOn", "solR13JA_ChargeState", "solR13JA_PV_V", "solR13JA_PV_I", "solR13JA_PV_W", 
                 "solR13JA_Yield_kWh", "solR13JA_MaxWToday", "solR13JA_Error", "solR13JA_UsrYield_kWh",
                 "solD90LG_BatV", "solD90LG_BatA", "solD90LG_BatW", "solD90LG_LoadState", "solD90LG_LoadCurrent",
                 "solD90LG_ChargerOn", "solD90LG_ChargeState", "solD90LG_PV_V", "solD90LG_PV_I", "solD90LG_PV_W", 
                 "solD90LG_Yield_kWh", "solD90LG_MaxWToday", "solD90LG_Error", "solD90LG_UsrYield_kWh",
                 "solD90JA_BatV", "solD90JA_BatA", "solD90JA_BatW", "solD90JA_LoadState", "solD90JA_LoadCurrent",
                 "solD90JA_ChargerOn", "solD90JA_ChargeState", "solD90JA_PV_V", "solD90JA_PV_I", "solD90JA_PV_W", 
                 "solD90JA_Yield_kWh", "solD90JA_MaxWToday", "solD90JA_Error", "solD90JA_UsrYield_kWh",
                 "solD13LG_BatV", "solD13LG_BatA", "solD13LG_BatW", "solD13LG_LoadState", "solD13LG_LoadCurrent",
                 "solD13LG_ChargerOn", "solD13LG_ChargeState", "solD13LG_PV_V", "solD13LG_PV_I", "solD13LG_PV_W", 
                 "solD13LG_Yield_kWh", "solD13LG_MaxWToday", "solD13LG_Error", "solD13LG_UsrYield_kWh",
                 "solD40LG_BatV", "solD40LG_BatA", "solD40LG_BatW", "solD40LG_LoadState", "solD40LG_LoadCurrent",
                 "solD40LG_ChargerOn", "solD40LG_ChargeState", "solD40LG_PV_V", "solD40LG_PV_I", "solD40LG_PV_W", 
                 "solD40LG_Yield_kWh", "solD40LG_MaxWToday", "solD40LG_Error", "solD40LG_UsrYield_kWh",
                 "tank20", "tank21", "tank22",
                 "sysOFFDischarge", "sysBatteryLife", "sysOFFDischargeBMS", "sysOFFChargeBMS", "sysSlowCharge",
                 "sysOFFChargeUsr", "sysOFFDischargeUsr", "sysACinput", "sysPV_DCcoupled", "sysACuseL1", "sysGridL1", 
                 "sysBatV", "sysBatA", "sysBusChargeA", "sysBatW", "sysBusChargeW", 
                 "sysBatChargeState", "sysBatUseAh", "sysBatTime", "sysCCGXState", 
                 "tempStatus23", "tempStatus24")
}
else{
    warning('Incorrect id: for solar choose one of "main" or "kwh", for meteo "T000000".')
}
return(colNames)
}