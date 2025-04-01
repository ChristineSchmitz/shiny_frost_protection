library(decisionSupport)
library(tidyverse)

frost_protection_apple<-function(){
  
  #time span with protection
  n_years_protection<-n_years-(first_protection_year-1)
  
  #setup empty vectors and lists for general calculations
  harmful_frost_event<-rep(NA, n_years)
  frost_duration_hours<-rep(NA, n_years) 
  share_harmful_of_protection<-rep(NA, n_years_protection)
  too_late_start_list <- vector(mode = "list", length = n_years_protection)
  missed_start_list <- vector(mode = "list", length = n_years_protection)
  not_prepared <- rep(NA, n_years_protection)
  not_prepared_list <- vector(mode = "list", length = n_years_protection)
  unnecessary_prepared<-rep(NA, n_years_protection)
  preparation_done<-rep(NA, n_years_protection)
  n_unnecessary_start_prep<-rep(NA, n_years_protection)
  share_harmful_of_protection_prep<-rep(NA, n_years_protection)
  n_prep_and_protection_done<-rep(NA, n_years_protection)
  n_protection_no_prep_need<-rep(NA, n_years_protection)
  price<-rep(NA, n_years)
  
  #general risks####
  
  #number of frost nights per year (during critical time)
  frost_event<-chance_event(frost_risk,
                            value_if = round(vv(mean_n_nights_frost,
                                                var_CV = var_cv_n_nights_frost,
                                                n = n_years,
                                                lower_limit = 1)),
                            value_if_not = rep(0, n_years), n = n_years )
  
  #risk of unnecessary starting the protection
  #number of unnecessary starts per year
  n_unnecessary_start<-chance_event(unnecessary_start_risk,
                                    value_if =(round(vv(mean_n_starts_unnecessary,
                                                        var_CV = var_cv_unnecessary_starts,
                                                        n = n_years_protection,
                                                        lower_limit = 1))),
                                    value_if_not = rep(0, n_years_protection),
                                    n = n_years_protection )
  #Loop the risk chance_events over n_years
  for (i in 1: n_years){
    #number of harmful frost nights per year
    if (frost_event[i] != 0){
      harmful_frost_event[i]<-sum(chance_event(share_harmful_frost,
                                               n = frost_event[i]))}else{
                                                 harmful_frost_event[i]<-0}
    
    #frost duration per frost night
    if (harmful_frost_event[i] != 0){
      frost_duration_hours[i]<-vv(mean_frost_duration,
                                  var_CV = var_cv_frost_duration,
                                  n = n_years,
                                  lower_limit = 0.1)[i]}else{
                                    frost_duration_hours[i]<-0}
    
  }
  for(i in 1:n_years_protection){
    #share of the protection in harmful frost nights on all nights with protection
    if(harmful_frost_event[i+(first_protection_year-1)] != 0){
      share_harmful_of_protection[i]<-harmful_frost_event[i+(first_protection_year-1)]*
        (1/(harmful_frost_event[i+(first_protection_year-1)]+n_unnecessary_start[i]))} else{
          share_harmful_of_protection[i]<-0}
    
    #risk of starting the protection to late
    if  (harmful_frost_event[i+(first_protection_year-1)]!=0){
      too_late_start_list[[i]]<-chance_event(risk_late_start,
                                             n= harmful_frost_event[i+(first_protection_year-1)])}else{
                                               too_late_start_list[[i]]<-0}
    
    #risk of totally miss the protection start
    if (harmful_frost_event[i+(first_protection_year-1)]!=0){
      missed_start_list[[i]]<-chance_event(risk_missed_protection,
                                           n= harmful_frost_event[i+(first_protection_year-1)])}else{
                                             missed_start_list[[i]]<-0}
    
    #risk of no preparation but frost (only for candles, heaters and mobile wind machines)
    #you can't protect your trees if the preparation is missing -> yield losses
    if (harmful_frost_event[i+(first_protection_year-1)]!=0){
      not_prepared[i]<-chance_event(risk_missed_preparation,
                                    n= n_years_protection)[i]}else{
                                      not_prepared[i]<-0}
    if (not_prepared[i]!=0){
      not_prepared_list[[i]]<-rep(not_prepared[i], harmful_frost_event[i+(first_protection_year-1)])}else{
        not_prepared_list[[i]]<-rep(0, harmful_frost_event[i+(first_protection_year-1)])}
    
    #risk of preparation but no frost (only for candles, heaters and mobile wind machines)
    #you have work (wage and material costs) without benefit
    if (harmful_frost_event[i+(first_protection_year-1)] == 0){
      unnecessary_prepared[i]<-chance_event(risk_unnecessary_preparation,
                                            n=n_years_protection)[i]}else{
                                              unnecessary_prepared[i]<-0}
  }
  #convert the lists into matrices 
  too_late_start_matrix<-t(sapply(too_late_start_list,
                                  `length<-`, max(2,max(harmful_frost_event)))) #max(2,...) the 2 ensures to get an matrix also in years without frost
  too_late_start_matrix_na_0<-too_late_start_matrix %>% replace(is.na(.),0)
  
  missed_start_matrix<-t(sapply(missed_start_list,
                                `length<-`, max(2,max(harmful_frost_event))))
  missed_start_matrix_na_0<-missed_start_matrix %>% replace(is.na(.), 0)
  
  not_prepared_matrix<-t(sapply(not_prepared_list,
                                `length<-`, max(2,max(harmful_frost_event))))
  not_prepared_matrix_na_0<-not_prepared_matrix %>% replace(is.na(.),0)
  
  for (i in 1: n_years_protection){
    #years with preparation (relevant for the costs)
    if ((harmful_frost_event[i+(first_protection_year-1)] != 0 & not_prepared[i]== 0)|
        unnecessary_prepared[i] == 1){
      preparation_done[i]<-1}else{
        preparation_done[i]<-0}
    
    #number of unnecessary starts per year for measures with preparation
    if (n_unnecessary_start[i] != 0 & preparation_done[i] == 1){
      n_unnecessary_start_prep[i]<-round(vv(mean_n_starts_unnecessary,
                                            var_CV = var_cv_unnecessary_starts,
                                            n = n_years_protection,
                                            lower_limit = 1))[i]}else{
                                              n_unnecessary_start_prep[i]<-0}
    
    #share of the protection in harmful frost nights on all nights with protection
    if(harmful_frost_event[i+(first_protection_year-1)] != 0){
      share_harmful_of_protection_prep[i]<-harmful_frost_event[i+(first_protection_year-1)]*
        (1/(harmful_frost_event[i+(first_protection_year-1)]+n_unnecessary_start_prep[i]))} else{
          share_harmful_of_protection_prep[i]<-0}
    
    #number of protection nights (for measures with preparation need)
    if (harmful_frost_event[i+(first_protection_year-1)] != 0 & not_prepared[i]== 0){
      n_prep_and_protection_done[i]<-harmful_frost_event[i+(first_protection_year-1)]-
        rowSums(missed_start_matrix_na_0)[i]}else{
          n_prep_and_protection_done[i]<-0}
    
    #number of protection nights (for measures without huge preparation effort)
    if (harmful_frost_event[i+(first_protection_year-1)] != 0){
      n_protection_no_prep_need[i]<-harmful_frost_event[i+(first_protection_year-1)]-
        rowSums(missed_start_matrix_na_0)[i]}else{
          n_protection_no_prep_need[i]<-0}
  }
  
  
  #general calculations ####
  
  #gas sold out in spring (Frostguard, Frostbuster)
  gas_sold_out_spring<-chance_event(sold_out_risk_gas, n=n_years_protection)
  
  #Apple yield
  fruit_yield <-
    gompertz_yield(
      max_harvest = mean_yield,
      time_to_first_yield_estimate = time_to_first_yield_est,
      time_to_second_yield_estimate = time_to_second_yield_est,
      first_yield_estimate_percent = first_yield_percent,
      second_yield_estimate_percent = second_yield_percent,
      n_years = n_years,
      var_CV = var_cv_yield,
      no_yield_before_first_estimate = TRUE
    )
  
  #yield loss due to frost 
  yield_loss_per_frost_night<-vv(mean_yield_loss_frost,
                                 var_CV = var_cv_loss_frost,
                                 n = n_years,
                                 lower_limit = 0,
                                 upper_limit = 1)
  
  #apple price (amount of money which the grower gets for the apples)
  year_price<-vv(mean_price,
                 var_CV = var_cv_price,
                 n=n_years,
                 lower_limit = 0)
  #apple price increase in frost years
  for(i in 1:n_years){
    if(harmful_frost_event[i]!=0){price[i]<-year_price[i]+price_increase_frost}else{
      price[i]<-year_price[i]
    }
  }
  
  ##costs
  
  #production cost change in frost years
  cost_change_frost<-vv(mean_cost_change_frost,
                        var_CV= var_cv_cost_change_frost,
                        n=n_years,
                        upper_limit = 1)
  #diesel price 
  diesel_price<-vv(mean_diesel_price,
                   var_CV= var_cv_diesel_price,
                   n=n_years,
                   lower_limit = 0)
  #costs for preparing and planting a new orchard
  establishment_costs<-rep(0, n_years)
  establishment_costs[1]<-establishment_costs_per_ha*plot_area
  
  #yearly costs per ha without harvest and post-harvest costs
  production_costs<-vv(mean_production_costs,
                       var_CV = var_cv_production_costs,
                       n = (n_years-1),
                       lower_limit = 0)
  production_costs<-c(0,production_costs)
  
  #harvest and post-harvest costs per kg apple yield
  harvest_costs<-vv(mean_harvest_costs,
                    var_CV = var_cv_harvest_costs,
                    n = n_years,
                    lower_limit = 0)
  post_harvest_costs<-vv(mean_post_harvest_costs,
                         var_CV = var_cv_post_harvest_costs,
                         n = n_years,
                         lower_limit = 0)
  harvest_and_following_costs<-harvest_costs+
    post_harvest_costs
  
  #water and well drilling costs
  if(groundwater_well==1){
    water_costs<-water_withdrawl_fee
    well_drilling_costs<-well_drilling_costs
  }else{
    water_costs<-water_price
    well_drilling_costs<-0
  }
  #No frost protection####
  
  #setup empty vectors for "no protection"
  yield_no_protection<-rep(NA, n_years)
  income_no_protection<-rep(NA, n_years)
  costs_no_protection<- rep(NA, n_years)
  
  for (i in 1 : n_years){
    #Yield without protection
    if (harmful_frost_event[i] != 0){
      yield_no_protection[i] <- fruit_yield[i]*
        ((1-yield_loss_per_frost_night[i])^harmful_frost_event[i])} else {
          yield_no_protection[i] <- fruit_yield[i]}
    
    #Income:
    income_no_protection[i]<-price[i]*
      yield_no_protection[i]*
      plot_area
    
    #Costs
    if (harmful_frost_event[i] != 0){
      costs_no_protection[i] <- production_costs[i]*
        plot_area*(1+cost_change_frost[i])+
        establishment_costs[i]+
        harvest_and_following_costs[i]*yield_no_protection[i]*plot_area} else {
          costs_no_protection[i] <- production_costs[i]*plot_area+establishment_costs[i]+
            harvest_and_following_costs[i]*yield_no_protection[i]*plot_area}
  }
  
  #Benefits
  benefits_no_protection<-income_no_protection/plot_area-costs_no_protection/plot_area
  
  #NPV
  NPV_no_protection<-
    discount(benefits_no_protection, discount_rate, calculate_NPV = TRUE)
  
  #Candles####
  #setup empty vectors and lists for "candles"
  fire_costs<-rep(NA, n_years_protection)
  ineffective_protection_candles_list <- vector(mode = "list", length = n_years_protection)
  n_protection_stop_missing_candles<-rep(NA, n_years_protection)
  prot_stop_missing_harmful_candles<-rep(NA, n_years_protection)
  protection_stop_missing_candles_list<- vector(mode = "list", length = n_years_protection)
  yield_candles<-rep(NA, n_years)
  income_candles<- rep(NA, n_years)
  running_costs_candles<-rep(NA, n_years_protection)
  preparation_deconstruction_costs_candles<-rep(NA, n_years_protection)
  preparation_deconstruction_time_candles<-rep(NA, n_years_protection)
  candles_needed_per_candle_place<-rep(NA,n_years_protection)
  remaining_candles<-rep(NA,n_years_protection)
  candle_places_per_ha<-ceiling(candle_places_per_ha)
  residual_value_candles<-rep(0,n_years)
  initial_cost_candles <- rep(0, n_years)
  production_costs_candles<-rep(NA, n_years)
  
  #Risks
  #Fire damage on trees or skin burn of workers
  fire_damage_candles<-chance_event(fire_damage_risk_candles, n = n_years_protection)
  
  
  for (i in 1 : n_years_protection){
    if (fire_damage_candles[i] == 1){
      fire_costs[i] <- costs_fire_damage} else {
        fire_costs[i] <- 0}
    
    #ineffective_protection e.g. due to very cold temperature or wind
    if (harmful_frost_event[i+(first_protection_year-1)]!=0){
      ineffective_protection_candles_list[[i]]<-chance_event(risk_ineffective_protection_candles,
                                                             n= harmful_frost_event[i+(first_protection_year-1)])}else{
                                                               ineffective_protection_candles_list[[i]]<-0}
  }
  ineffective_protection_candles_matrix<-t(sapply(ineffective_protection_candles_list,
                                                  `length<-`, max(2,max(harmful_frost_event))))
  ineffective_protection_candles_matrix_na_0<-ineffective_protection_candles_matrix %>% replace(is.na(.), 0)
  
  #candles sold out in spring
  candles_sold_out_spring<-chance_event(sold_out_risk_candles, n=n_years_protection)
  
  #Candles needed per year
  burning_time_candles<-vv(mean_burning_time_candles,
                           var_CV = var_cv_burning_time_candles,
                           n=n_years_protection,
                           lower_limit = 1)
  
  for(i in 1: n_years_protection){
    candles_needed_per_candle_place[i]<-ceiling((frost_duration_hours[i+(first_protection_year-1)]*
                                                   n_prep_and_protection_done[i]/
                                                   burning_time_candles[i])+
                                                  (mean_frost_duration*
                                                     n_unnecessary_start_prep[i]/
                                                     burning_time_candles[i]))
    
    #initial candle purchase = amount in storage
    initial_candle_purchase<-candle_places_per_ha*
      plot_area*
      storage_recommendation
    #number of nights which could not be protected with storage candles
    remaining_candles[i]<-round(storage_recommendation)-candles_needed_per_candle_place[i]}
  
  for (i in 1 : n_years_protection){
    #number and point in time of a protection stop due to missing candles
    if (remaining_candles[i] < 0 & candles_sold_out_spring[i] == 1){
      n_protection_stop_missing_candles[i] <- ceiling(remaining_candles[i]*
                                                        (-1)*
                                                        burning_time_candles[i]/
                                                        frost_duration_hours[i+(first_protection_year-1)])} else {
                                                          n_protection_stop_missing_candles[i] <- 0}
    
    if(harmful_frost_event[i+(first_protection_year-1)] != 0 & n_protection_stop_missing_candles[i] > 0){
      prot_stop_missing_harmful_candles[i]<-pmin(sum(chance_event(share_harmful_of_protection_prep[i],
                                                                  n= n_protection_stop_missing_candles[i])),
                                                 harmful_frost_event[i+(first_protection_year-1)])} else{
                                                   prot_stop_missing_harmful_candles[i]<-0}
    
    if (n_protection_stop_missing_candles[i] != 0){
      protection_stop_missing_candles_list[[i]]<-c(rep(0, (harmful_frost_event[i+(first_protection_year-1)]-
                                                             prot_stop_missing_harmful_candles[i])),
                                                   rep(1, prot_stop_missing_harmful_candles[i]))}else{
                                                     protection_stop_missing_candles_list[[i]]<-0}
  }
  protection_stop_missing_candles_matrix<-t(sapply(protection_stop_missing_candles_list,
                                                   `length<-`, max(2,max(harmful_frost_event))))
  protection_stop_missing_candles_matrix_na_0<-protection_stop_missing_candles_matrix %>% replace(is.na(.), 0)
  
  #combine risks
  unsuccessful_protection_candles = (missed_start_matrix_na_0 + 
                                       not_prepared_matrix_na_0 + 
                                       ineffective_protection_candles_matrix_na_0 + 
                                       protection_stop_missing_candles_matrix_na_0 > 0) * 1
  
  #nights reduced protection effect
  reduced_protection_candles = (too_late_start_matrix_na_0 > 0) * 1  
  
  reduced_protection_candles<-reduced_protection_candles- unsuccessful_protection_candles
  reduced_protection_candles[reduced_protection_candles==-1]<-0
  
  #sum it up to calculate unsuccessful protection nights
  n_unsuccessful_protection_candles<-rowSums(unsuccessful_protection_candles)
  n_reduced_protection_candles<-rowSums(reduced_protection_candles)
  #time needed for candle preparation and deconstruction
  preparation_time_candles<-(candle_places_per_ha*plot_area)/
    vv(mean_candles_placed_per_h,
       var_CV = var_cv_candle_placing,
       n= n_years_protection,
       lower_limit = 1)#lower limit to avoid negative values and the division by 0
  
  deconstruction_time_candles<-(candle_places_per_ha*plot_area)/
    vv(mean_candles_removed_per_h,
       var_CV = var_cv_candle_removing,
       n= n_years_protection,
       lower_limit= 1)
  
  #Protection efficiency 
  efficiency_candles<-vv(mean_efficiency_candles,
                         var_CV = var_cv_efficiency_candles,
                         n=n_years_protection,
                         lower_limit = 0)
  #efficeincy reduce due to late start of the protection
  efficiency_reduce_smaller_problem_candles<-vv(mean_efficiency_reduce_smaller_problem_candles,
                                                var_CV = var_cv_efficiency_reduce_smaller_problem_candles,
                                                n=n_years_protection,
                                                lower_limit = 0)
  #Yield 
  for (i in first_protection_year : n_years){
    if (harmful_frost_event[i] != 0){
      yield_candles[i] <- fruit_yield[i]*
        ((1-(1-efficiency_candles[i-(first_protection_year-1)])*
            yield_loss_per_frost_night[i])^(harmful_frost_event[i]-
                                              n_unsuccessful_protection_candles[i-(first_protection_year-1)]-
                                              n_reduced_protection_candles[i-(first_protection_year-1)])*
           (1-yield_loss_per_frost_night[i])^n_unsuccessful_protection_candles[i-(first_protection_year-1)]*
           (1-(1-efficiency_reduce_smaller_problem_candles[i-(first_protection_year-1)])*
              yield_loss_per_frost_night[i])^n_reduced_protection_candles[i-(first_protection_year-1)])} else {
                yield_candles[i] <- fruit_yield[i]}}
  
  for (i in 1 : (first_protection_year-1)){
    if (harmful_frost_event[i] != 0){
      yield_candles[i] <- fruit_yield[i]*
        yield_loss_per_frost_night[i]} else {
          yield_candles[i] <- fruit_yield[i]}}
  
  #Residual value (assumption: selling the candles in the last year)
  residual_value_candles[n_years]<-candle_places_per_ha*remaining_candles[n_years-(first_protection_year-1)]*resale_value_candles
  #Income:
  
  for(i in 1:n_years){
    income_candles[i]<-price[i]*yield_candles[i]*plot_area+residual_value_candles[i]*plot_area
    
    #Costs###
    #Initial cost (number of candles to buy plus the general planning costs)
    initial_cost_candles[first_protection_year]<-initial_candle_purchase*candle_price+
      planning_cost_frost_protection
    
    #running costs (storage costs and frost monitoring)
    running_costs_candles[i]<-initial_candle_purchase*
      storage_cost_per_candle+
      frost_monitoring_costs
  }
  
  for(i in 1:n_years_protection){
    #preparation and deconstruction costs
    #transport costs *2: setting up an second transport for deconstruction
    if (preparation_done[i] == 1){
      preparation_deconstruction_costs_candles[i] <-(preparation_time_candles[i]
                                                     +deconstruction_time_candles[i])*
        hourly_wage_worker+
        (transport_costs_field_candles*candle_places_per_ha*plot_area)*2
      preparation_deconstruction_time_candles[i] <-(preparation_time_candles[i]
                                                    +deconstruction_time_candles[i])} else {
                                                      preparation_deconstruction_costs_candles[i] <- 0
                                                      preparation_deconstruction_time_candles[i]<-0}
  }
  
  preparation_deconstruction_costs_candles<-c(rep(0,(first_protection_year-1)),preparation_deconstruction_costs_candles)
  
  #costs in case of candle use
  
  #candle change costs
  change_time_candles<-(candles_needed_per_candle_place*
                          candle_places_per_ha*
                          plot_area)/
    vv(mean_candles_changed_per_h,
       var_CV = var_cv_candle_change,
       n= n_years_protection,
       lower_limit = 1)
  
  candle_change_costs<-rep(NA, n_years_protection)
  for (i in 1 : n_years_protection){
    if(candles_needed_per_candle_place[i] > 1){
      candle_change_costs[i]<-change_time_candles[i]*hourly_wage_worker+
        transport_costs_field_candles*candle_places_per_ha} else{
          candle_change_costs[i] <- 0}}
  
  candle_change_costs<-c(rep(0,(first_protection_year-1)),candle_change_costs)
  
  #ignite and extinguish the candles
  #assumption we clear the candles always and didn't wait until they are empty
  #until now it is missing that you have to ignite again if you change the candle during the night!
  ignition_time_candles<-(candle_places_per_ha*
                            plot_area)/
    vv(mean_candles_ignited_per_h,
       var_CV = var_cv_candle_ignition,
       n= n_years_protection,
       lower_limit = 1)
  extinguish_time_candles<-(candle_places_per_ha*
                              plot_area)/
    vv(mean_candles_extinguished_per_h,
       var_CV = var_cv_candle_extingtion,
       n= n_years_protection,
       lower_limit = 1)
  
  ignition_extinguish_costs<-(ignition_time_candles+extinguish_time_candles)*
    hourly_wage_worker*
    n_prep_and_protection_done
  
  ignition_extinguish_costs<-c(rep(0,(first_protection_year-1)),ignition_extinguish_costs)
  
  #Disposal costs (Entsorgungskosten)
  disposal_costs_candles<-candles_needed_per_candle_place*
    candle_places_per_ha*
    plot_area*
    disposal_cost_per_candle
  
  disposal_costs_candles<-c(rep(0,(first_protection_year-1)), disposal_costs_candles)
  
  #Candle Repurchase
  repurchase_costs_candles<-candles_needed_per_candle_place*
    candle_places_per_ha*
    plot_area*
    candle_price
  
  repurchase_costs_candles<-c(rep(0,(first_protection_year-1)), repurchase_costs_candles)
  #Production costs
  n_unsuccessful_no_protection_candles<-c(rep(1,(first_protection_year-1)),n_unsuccessful_protection_candles)
  for (i in 1 : n_years){
    if(n_unsuccessful_no_protection_candles[i] == 1){
      production_costs_candles[i]<- production_costs[i]*(1+cost_change_frost[i])*
        plot_area+
        yield_candles[i]*plot_area*harvest_and_following_costs[i]} else{
          production_costs_candles[i] <- production_costs[i]*plot_area+
            yield_candles[i]*plot_area*harvest_and_following_costs[i]}}
  
  fire_costs<-c(rep(0,(first_protection_year-1)),fire_costs)
  #Candle costs
  costs_candles<-initial_cost_candles+
    preparation_deconstruction_costs_candles+
    candle_change_costs+
    disposal_costs_candles+
    running_costs_candles+
    fire_costs+
    ignition_extinguish_costs+
    repurchase_costs_candles+
    production_costs_candles+
    establishment_costs
  
  #Benefits
  benefits_candles<-income_candles/plot_area-costs_candles/plot_area
  
  #NPV
  NPV_candles<-
    discount(benefits_candles, discount_rate, calculate_NPV = TRUE)
  
  #Frostbuster (tailed or 3 point mounted)####
  
  #setup empty vectors and lists for "frostbuster"
  technical_failure_frostbuster <- vector(mode = "list", length = n_years_protection)
  ineffective_protection_frostbuster_list <- vector(mode = "list", length = n_years_protection)
  n_protection_stop_missing_gas_frostbuster<-rep(NA, n_years_protection)
  prot_stop_missing_gas_harmful_frostbuster<-rep(NA, n_years_protection)
  protection_stop_missing_gas_frostbuster<- vector(mode = "list", length = n_years_protection)
  yield_frostbuster_frost<-rep(NA, n_years)
  yield_frostbuster<-rep(NA, n_years)
  income_frostbuster<- rep(NA, n_years)
  deposit_gas_frostbuster<-rep(0, n_years)
  production_costs_frostbuster<-rep(NA, n_years)
  workload_frostbuster<-rep(NA,n_years_protection)
  initial_cost_frostbuster<-rep(0,n_years)
  
  #Risks frostbuster
  for (i in 1:n_years_protection){
    #Risk of technical failure
    if (harmful_frost_event[i+(first_protection_year-1)]!=0){
      technical_failure_frostbuster[[i]]<-chance_event(tech_failure_risk_frostbuster,
                                                       n= harmful_frost_event[i+(first_protection_year-1)])}else{
                                                         technical_failure_frostbuster[[i]]<-0}
    
    #risk of ineffective protection e.g. due to very cold temperatures or wind 
    if (harmful_frost_event[i+(first_protection_year-1)]!=0){
      ineffective_protection_frostbuster_list[[i]]<-chance_event(risk_ineffective_protection_frostbuster,
                                                                 n= harmful_frost_event[i+(first_protection_year-1)])}else{
                                                                   ineffective_protection_frostbuster_list[[i]]<-0}
  }
  
  technical_failure_frostbuster_matrix<-t(sapply(technical_failure_frostbuster,
                                                 `length<-`, max(2,max(harmful_frost_event))))
  technical_failure_frostbuster_matrix_na_0<-technical_failure_frostbuster_matrix %>% replace(is.na(.), 0)
  
  ineffective_protection_frostbuster_matrix<-t(sapply(ineffective_protection_frostbuster_list,
                                                      `length<-`, max(2,max(harmful_frost_event))))
  ineffective_protection_frostbuster_matrix_na_0<-ineffective_protection_frostbuster_matrix %>% replace(is.na(.), 0)
  
  #Risk of burns on part of plants and following yield losses
  plant_burn_damage_frostbuster<-chance_event(risk_plant_burns_frostbuster,
                                              value_if = vv(mean_yield_loss_burns_frostbuster,
                                                            var_CV = var_cv_loss_burns_frostbuster,
                                                            n=n_years_protection, 
                                                            lower_limit = 0,
                                                            upper_limit = 1),
                                              value_if_not = rep(0, n_years_protection),
                                              n= n_years_protection)
  
  #frostbusters needed for the plot
  frostbusters_needed<-ceiling(plot_area/max(area_performance_frostbuster,0.1)) #0.1 is a lower limit to avoid crazy outliers
  #gas bottles needed per year
  gas_bottles_needed_frostbuster<-ceiling((frost_duration_hours[i+(first_protection_year-1)]*
                                             harmful_frost_event[i+(first_protection_year-1)]*
                                             gas_consumption_frostbuster/
                                             gas_bottle_volume)+
                                            (mean_frost_duration*
                                               n_unnecessary_start[i]*
                                               gas_consumption_frostbuster/
                                               gas_bottle_volume))*
    frostbusters_needed
  
  #initial gas purchase = amount in storage
  initial_gas_purchase_frostbuster<-ceiling(gas_storage_n_nights*
                                              gas_consumption_frostbuster*
                                              mean_frost_duration/
                                              gas_bottle_volume*
                                              frostbusters_needed)
  
  #Gas availability 
  for (i in 1 : n_years_protection){
    
    #gas bottles needed per year
    gas_bottles_needed_frostbuster[i]<-ceiling((frost_duration_hours[i+(first_protection_year-1)]*
                                                  harmful_frost_event[i+(first_protection_year-1)]*
                                                  gas_consumption_frostbuster/
                                                  gas_bottle_volume)+
                                                 (mean_frost_duration*
                                                    n_unnecessary_start[i]*
                                                    gas_consumption_frostbuster/
                                                    gas_bottle_volume))*
      frostbusters_needed
    
    #initial gas purchase = amount in storage
    initial_gas_purchase_frostbuster<-ceiling(gas_storage_n_nights*
                                                gas_consumption_frostbuster*
                                                mean_frost_duration/
                                                gas_bottle_volume*
                                                frostbusters_needed)
    
    if (gas_storage_n_nights-(harmful_frost_event[i+(first_protection_year-1)]+
                              n_unnecessary_start[i]) < 0 &
        gas_sold_out_spring[i] == 1){
      n_protection_stop_missing_gas_frostbuster[i] <- ceiling((gas_storage_n_nights-
                                                                 (harmful_frost_event[i+(first_protection_year-1)]+
                                                                    n_unnecessary_start[i]))*
                                                                (-1))} else{
                                                                  n_protection_stop_missing_gas_frostbuster[i] <- 0}
    
    if(n_protection_stop_missing_gas_frostbuster[i] != 0){
      prot_stop_missing_gas_harmful_frostbuster[i]<-pmin(sum(chance_event(share_harmful_of_protection[i],
                                                                          n= n_protection_stop_missing_gas_frostbuster[i])),
                                                         harmful_frost_event[i+(first_protection_year-1)])} else{
                                                           prot_stop_missing_gas_harmful_frostbuster[i]<-0}
    
    if (prot_stop_missing_gas_harmful_frostbuster[i] != 0){
      protection_stop_missing_gas_frostbuster[[i]]<-c(rep(0,
                                                          (harmful_frost_event[i+(first_protection_year-1)]-
                                                             prot_stop_missing_gas_harmful_frostbuster[i])),
                                                      rep(1, prot_stop_missing_gas_harmful_frostbuster[i]))}else{
                                                        protection_stop_missing_gas_frostbuster[[i]]<-0}
  }
  protection_stop_missing_gas_frostbuster_matrix<-t(sapply(protection_stop_missing_gas_frostbuster,
                                                           `length<-`, max(2,max(harmful_frost_event))))
  protection_stop_missing_gas_frostbuster_matrix_na_0<-protection_stop_missing_gas_frostbuster_matrix %>% replace(is.na(.), 0)
  
  
  #combine risks
  unsuccessful_protection_frostbuster <- (missed_start_matrix_na_0 + 
                                            ineffective_protection_frostbuster_matrix_na_0 + 
                                            protection_stop_missing_gas_frostbuster_matrix_na_0 + 
                                            technical_failure_frostbuster_matrix_na_0 > 0) * 1
  
  reduced_protection_frostbuster = (too_late_start_matrix_na_0 > 0) * 1  
  
  reduced_protection_frostbuster<-reduced_protection_frostbuster- unsuccessful_protection_frostbuster
  reduced_protection_frostbuster[reduced_protection_frostbuster==-1]<-0
  
  #sum it up to calculate unsuccessful protection nights
  n_unsuccessful_protection_frostbuster<-rowSums(unsuccessful_protection_frostbuster)
  n_reduced_protection_frostbuster<-rowSums(reduced_protection_frostbuster)
  #Frost protection efficiency of frostbuster
  efficiency_frostbuster<-vv(mean_efficiency_frostbuster,
                             var_CV = var_cv_efficiency_frostbuster,
                             n=n_years_protection,
                             lower_limit = 0)
  #Reduced efficiency in case the farmer starts driving thur the orchard too late
  efficiency_reduce_smaller_problem_frostbuster<-vv(mean_efficiency_reduce_smaller_problem_frostbuster,
                                                    var_CV = var_cv_efficiency_reduce_smaller_problem_frostbuster,
                                                    n=n_years_protection,
                                                    lower_limit = 0)
  #Yield
  for (i in first_protection_year : n_years){
    if (harmful_frost_event[i] != 0){
      yield_frostbuster_frost[i] <- fruit_yield[i]*
        ((1-(1-efficiency_frostbuster[i-(first_protection_year-1)])*
            yield_loss_per_frost_night[i])^(harmful_frost_event[i]-
                                              n_unsuccessful_protection_frostbuster[i-(first_protection_year-1)]-
                                              n_reduced_protection_frostbuster[i-(first_protection_year-1)])*
           (1-yield_loss_per_frost_night[i])^n_unsuccessful_protection_frostbuster[i-(first_protection_year-1)]*
           (1-(1-efficiency_reduce_smaller_problem_frostbuster[i-(first_protection_year-1)])*
              yield_loss_per_frost_night[i])^n_reduced_protection_frostbuster[i-(first_protection_year-1)])} else {
                yield_frostbuster_frost[i] <- fruit_yield[i]}}
  
  for (i in 1 : (first_protection_year-1)){
    if (harmful_frost_event[i] != 0){
      yield_frostbuster_frost[i] <- fruit_yield[i]*
        yield_loss_per_frost_night[i]} else {
          yield_frostbuster_frost[i] <- fruit_yield[i]}}
  
  plant_burn_damage_frostbuster<-c(rep(0,(first_protection_year-1)),plant_burn_damage_frostbuster)
  for (i in 1 : n_years){ 
    #yield reduction due to burning issues
    
    if (plant_burn_damage_frostbuster[i] != 0){
      yield_frostbuster[i] <- yield_frostbuster_frost[i]*
        (1-plant_burn_damage_frostbuster[i])} else {
          yield_frostbuster[i] <- yield_frostbuster_frost[i]}
    
    #Residual value (assumption: selling the frostbuster in the last year)
    residual_value_frostbuster<-rep(0,n_years)
    residual_value_frostbuster[n_years]<-frostbusters_needed*
      frostbuster_price*
      percentage_residual_value_frostbuster
    #income
    income_frostbuster[i]<-price[i]*yield_frostbuster[i]*plot_area+residual_value_frostbuster[i]
  }
  
  #Costs
  #Initial costs frostbuster
  initial_cost_frostbuster[first_protection_year]<-frostbuster_price*
    frostbusters_needed+
    planning_cost_frost_protection+
    initial_gas_purchase_frostbuster*
    gas_bottle_volume*
    gas_bottle_fill_price
  
  #Deposit cost in year one, revenue in last year
  deposit_gas_frostbuster[first_protection_year]<-gas_bottle_deposit*initial_gas_purchase_frostbuster
  deposit_gas_frostbuster[n_years]<-(-gas_bottle_deposit*
                                       initial_gas_purchase_frostbuster)
  
  #running costs frostbuster
  running_cost_frostbuster<-vv(mean_maintenance_costs_frostbuster,
                               var_CV = var_cv_maintenance_frostbuster,
                               n=n_years_protection,
                               lower_limit = 0)*
    frostbusters_needed+
    frost_monitoring_costs
  running_cost_frostbuster<-c(rep(0,(first_protection_year-1)),running_cost_frostbuster)
  
  #costs in case of frostbuster use
  #wage costs preparation and start
  setup_time_frostbuster<-vv(mean_setup_time_frostbuster,
                             var_CV = var_cv_setup_time_frostbuster,
                             n=n_years_protection,
                             lower_limit = 0)
  for(i in 1:n_years_protection){
    workload_frostbuster[i]<-(((n_protection_no_prep_need[i]+n_unnecessary_start[i])*
                                 setup_time_frostbuster[i]+
                                 (n_protection_no_prep_need[i]+n_unnecessary_start[i])*travel_time_field*2+ #*2 way to field and back home
                                 n_protection_no_prep_need[i]*frost_duration_hours[i+(first_protection_year-1)]+
                                 n_unnecessary_start[i]*mean_frost_duration)*frostbusters_needed)/plot_area}
  #workload frostbuster
  wage_costs_frostbuster<-workload_frostbuster*plot_area
  wage_costs_frostbuster<-c(rep(0,(first_protection_year-1)),wage_costs_frostbuster)
  #gas costs
  gas_costs_frostbuster<-gas_bottles_needed_frostbuster*gas_bottle_volume*gas_bottle_fill_price
  gas_costs_frostbuster<-c(rep(0,(first_protection_year-1)),gas_costs_frostbuster)
  
  #tractor costs: fuel 
  fuel_consumption_tractor_frostbuster<-vv(mean_fuel_consumption_tractor_frostbuster,
                                           var_CV = var_cv_fuel_frostbuster,
                                           n= n_years_protection,
                                           lower_limit = 0)
  for(i in 1:n_years_protection){
    tractor_costs_frostbuster<-(frostbusters_needed*
                                  (travel_time_field*2*
                                     fuel_consumption_tractor_frostbuster)+
                                  frost_duration_hours[i+(first_protection_year-1)]*harmful_frost_event[i+(first_protection_year-1)]*fuel_consumption_tractor_frostbuster[i]+
                                  mean_frost_duration*n_unnecessary_start[i]*fuel_consumption_tractor_frostbuster[i])*
      diesel_price[i]}
  tractor_costs_frostbuster<-c(rep(0,(first_protection_year-1)),tractor_costs_frostbuster)
  
  #Production costs
  n_unsuccessful_no_protection_frostbuster<-c(rep(1,(first_protection_year-1)),n_unsuccessful_protection_frostbuster)
  for (i in 1 : n_years){
    if(n_unsuccessful_no_protection_frostbuster[i] == 1){
      production_costs_frostbuster[i]<- production_costs[i]*(1+cost_change_frost[i])*plot_area+
        yield_frostbuster[i]*plot_area*harvest_and_following_costs[i]} else{
          production_costs_frostbuster[i] <- production_costs[i]*plot_area+
            yield_frostbuster[i]*plot_area*harvest_and_following_costs[i]}}  
  
  #total costs frostbuster
  costs_frostbuster<-initial_cost_frostbuster+
    deposit_gas_frostbuster+
    running_cost_frostbuster+
    wage_costs_frostbuster+
    tractor_costs_frostbuster+
    production_costs_frostbuster+
    gas_costs_frostbuster+
    establishment_costs
  
  #Benefits
  benefits_frostbuster<-income_frostbuster/plot_area-costs_frostbuster/plot_area
  
  #NPV
  NPV_frostbuster<-
    discount(benefits_frostbuster, discount_rate, calculate_NPV = TRUE)  
  
  #Frostguard (stationary) (the bigger version with 4 gas bottles)####
  
  #setup empty vectors and lists for "frostguard"
  ineffective_protection_frostguard_list <- vector(mode = "list", length = n_years_protection)
  technical_failure_frostguard <- vector(mode = "list", length = n_years_protection)
  n_protection_stop_missing_gas_frostguard<-rep(NA, n_years_protection)
  prot_stop_missing_gas_harmful_frostguard<-rep(NA, n_years_protection)
  protection_stop_missing_gas_frostguard<- vector(mode = "list", length = n_years_protection)
  deposit_gas_frostguard<-rep(0, n_years)
  yield_frostguard_frost<-rep(NA, n_years_protection)
  yield_frostguard<-rep(NA, n_years_protection)
  income_frostguard<- rep(NA, n_years)
  initial_cost_frostguard <- rep(0,n_years)
  production_costs_frostguard<-rep(NA, n_years)
  
  #Risk of technical failure
  for (i in 1:n_years_protection){
    if (harmful_frost_event[i]!=0){
      technical_failure_frostguard[[i]]<-chance_event(tech_failure_risk_frostguard,
                                                      n= harmful_frost_event[i+(first_protection_year-1)])}else{
                                                        technical_failure_frostguard[[i]]<-0}}
  
  technical_failure_frostguard_matrix<-t(sapply(technical_failure_frostguard,
                                                `length<-`, max(2,max(harmful_frost_event))))
  
  technical_failure_frostguard_matrix_na_0<-technical_failure_frostguard_matrix %>% replace(is.na(.), 0)
  
  #risk of ineffective protection e.g. due to very cold temperatures or wind
  
  for (i in 1:n_years_protection){
    if (harmful_frost_event[i+(first_protection_year-1)]!=0){
      ineffective_protection_frostguard_list[[i]]<-chance_event(risk_ineffective_protection_frostguard, n= harmful_frost_event[i+(first_protection_year-1)])}else{
        ineffective_protection_frostguard_list[[i]]<-0}}
  ineffective_protection_frostguard_matrix<-t(sapply(ineffective_protection_frostguard_list, `length<-`, max(2,max(harmful_frost_event))))
  ineffective_protection_frostguard_matrix_na_0<-ineffective_protection_frostguard_matrix %>% replace(is.na(.), 0)
  #Risk of burns on part of plants and following yield losses
  plant_burn_damage_frostguard<-chance_event(risk_plant_burns_frostguard, value_if = vv(mean_yield_loss_burns_frostguard,
                                                                                        var_CV = var_cv_loss_burns_frostguard,
                                                                                        n=n_years_protection,
                                                                                        lower_limit = 0,
                                                                                        upper_limit = 1), value_if_not = rep(0, n_years_protection), n= n_years_protection)
  
  #frostguards needed for the plot
  #0.1 as lower limit (100 frostguards per ha)
  frostguards_needed<-ceiling(plot_area/max(area_performance_frostguard,0.1))
  #gas bottles needed per year
  gas_bottles_needed_frostguard<-ceiling((frost_duration_hours[i+(first_protection_year-1)]*
                                            n_prep_and_protection_done*gas_consumption_frostguard/
                                            gas_bottle_volume)+
                                           (mean_frost_duration*
                                              n_unnecessary_start*
                                              gas_consumption_frostguard/
                                              gas_bottle_volume))*
    frostguards_needed
  
  #initial gas purchase = amount in storage
  initial_gas_purchase_frostguard<-ceiling(gas_storage_n_nights*
                                             gas_consumption_frostguard*
                                             mean_frost_duration/
                                             gas_bottle_volume*
                                             frostguards_needed)
  
  #Gas purchase
  for (i in 1 : n_years_protection){
    if (gas_storage_n_nights-(harmful_frost_event[i+(first_protection_year-1)]+n_unnecessary_start_prep[i]) < 0 & gas_sold_out_spring[i] == 1){
      n_protection_stop_missing_gas_frostguard[i] <- ceiling((gas_storage_n_nights-
                                                                (harmful_frost_event[i+(first_protection_year-1)]+
                                                                   n_unnecessary_start_prep[i]))*(-1))} else{n_protection_stop_missing_gas_frostguard[i] <- 0}
    if(n_protection_stop_missing_gas_frostguard[i] != 0){
      prot_stop_missing_gas_harmful_frostguard[i]<-pmin(sum(chance_event(share_harmful_of_protection_prep[i],n= n_protection_stop_missing_gas_frostguard[i])),
                                                        harmful_frost_event[i+(first_protection_year-1)])} else{
                                                          prot_stop_missing_gas_harmful_frostguard[i]<-0}
    
    if (prot_stop_missing_gas_harmful_frostguard[i] != 0){
      protection_stop_missing_gas_frostguard[[i]]<-c(rep(0, (harmful_frost_event[i+(first_protection_year-1)]-
                                                               prot_stop_missing_gas_harmful_frostguard[i])),
                                                     rep(1, prot_stop_missing_gas_harmful_frostguard[i]))}else{
                                                       protection_stop_missing_gas_frostguard[[i]]<-0}
  }
  protection_stop_missing_gas_frostguard_matrix<-t(sapply(protection_stop_missing_gas_frostguard,
                                                          `length<-`,
                                                          max(2,max(harmful_frost_event))))
  protection_stop_missing_gas_frostguard_matrix_na_0<-protection_stop_missing_gas_frostguard_matrix %>% replace(is.na(.), 0)
  
  #combine risks
  unsuccessful_protection_frostguard = (missed_start_matrix_na_0 +
                                          ineffective_protection_frostguard_matrix_na_0 +
                                          protection_stop_missing_gas_frostguard_matrix_na_0 +
                                          technical_failure_frostguard_matrix_na_0 +
                                          not_prepared_matrix_na_0 > 0) * 1
  
  #nights reduced protection effect
  reduced_protection_frostguard = (too_late_start_matrix_na_0 > 0) * 1  
  
  reduced_protection_frostguard<-reduced_protection_frostguard- unsuccessful_protection_frostguard
  reduced_protection_frostguard[reduced_protection_frostguard==-1]<-0
  
  #sum it up to calculate unsuccessful protection nights
  n_unsuccessful_protection_frostguard<-rowSums(unsuccessful_protection_frostguard)
  n_reduced_protection_frostguard<-rowSums(reduced_protection_frostguard)
  
  #Protection efficiency with frostguard
  efficiency_frostguard<-vv(mean_efficiency_frostguard,
                            var_CV = var_cv_efficiency_frostguard,
                            n= n_years_protection,
                            lower_limit = 0)
  #Reduced efficiendy in case of late start
  efficiency_reduce_smaller_problem_frostguard<-vv(mean_efficiency_frostguard,
                                                   var_CV = var_cv_efficiency_frostguard,
                                                   n= n_years_protection,
                                                   lower_limit = 0)
  #Yield
  for (i in first_protection_year : n_years){
    if (harmful_frost_event[i] != 0){
      yield_frostguard_frost[i] <- fruit_yield[i]*
        ((1-(1-efficiency_frostguard[i-(first_protection_year-1)])*
            yield_loss_per_frost_night[i])^(harmful_frost_event[i]-
                                              n_unsuccessful_protection_frostguard[i-(first_protection_year-1)]-
                                              n_reduced_protection_frostguard[i-(first_protection_year-1)])*
           (1-yield_loss_per_frost_night[i])^n_unsuccessful_protection_frostguard[i-(first_protection_year-1)]*
           (1-(1-efficiency_reduce_smaller_problem_frostguard[i-(first_protection_year-1)])*
              yield_loss_per_frost_night[i])^n_reduced_protection_frostguard[i-(first_protection_year-1)])} else {
                yield_frostguard_frost[i] <- fruit_yield[i]}}
  
  for (i in 1 : (first_protection_year-1)){
    if (harmful_frost_event[i] != 0){
      yield_frostguard_frost[i] <- fruit_yield[i]*
        yield_loss_per_frost_night[i]} else {
          yield_frostguard_frost[i] <- fruit_yield[i]}}
  
  # yield reduction due to burning issues
  plant_burn_damage_frostguard<-c(rep(0,(first_protection_year-1)),plant_burn_damage_frostguard)
  for (i in 1 : n_years){ 
    if (plant_burn_damage_frostguard[i] != 0){
      yield_frostguard[i] <- yield_frostguard_frost[i]*
        (1-plant_burn_damage_frostguard[i])} else {
          yield_frostguard[i] <- yield_frostguard_frost[i]}}
  
  #Residual value (assumption: selling the frostguards in the last year)
  residual_value_frostguard<-rep(0,n_years)
  residual_value_frostguard[n_years]<-frostguards_needed*
    frostguard_price*
    percentage_residual_value_frostguard
  #Income:
  for(i in 1 : n_years){
    income_frostguard[i]<-price[i]*yield_frostguard[i]*plot_area+residual_value_frostguard[i]
  }
  
  #Costs#
  #Initial costs frostguard
  initial_cost_frostguard[first_protection_year]<-frostguard_price*
    frostguards_needed+
    planning_cost_frost_protection+
    initial_gas_purchase_frostguard*
    gas_bottle_fill_price*
    gas_bottle_volume
  
  
  #Deposit cost in year one, revenue in last year
  deposit_gas_frostguard[first_protection_year]<-gas_bottle_deposit*initial_gas_purchase_frostguard
  deposit_gas_frostguard[n_years]<-(-gas_bottle_deposit*
                                      initial_gas_purchase_frostguard)
  
  #running costs frostguard
  running_cost_frostguard<-vv(mean_maintenance_costs_frostguard,
                              var_CV = var_cv_maintenance_frostguard,
                              n=n_years_protection,
                              lower_limit = 0)*
    frostguards_needed+
    frost_monitoring_costs
  running_cost_frostguard<-c(rep(0,(first_protection_year-1)),running_cost_frostguard)
  #costs in case of frostguard use
  
  #wage costs preparation and start
  setup_time_frostguard<-vv(mean_setup_time_frostguard,
                            var_CV = var_cv_setup_time_frostguard,
                            n=n_years_protection,
                            lower_limit = 0)
  workload_frostguard<-((setup_time_frostguard*
                           frostguards_needed*
                           (n_prep_and_protection_done+
                              n_unnecessary_start_prep)+
                           vv(mean_bottle_change_frostguard,
                              var_CV = var_cv_bottle_change_frostguard,
                              n=n_years_protection,
                              lower_limit = 0)*
                           ceiling(gas_bottles_needed_frostguard/
                                     gas_bottles_frostguard)+
                           start_stop_time_frostguard*
                           frostguards_needed+
                           supervision_time_frostguard))/plot_area
  
  wage_costs_frostguard<-workload_frostguard*hourly_wage_worker*plot_area
  wage_costs_frostguard<-c(rep(0,(first_protection_year-1)),wage_costs_frostguard)
  
  #gas costs
  gas_costs_frostguard<-gas_bottles_needed_frostguard*gas_bottle_volume*
    gas_bottle_fill_price
  gas_costs_frostguard<-c(rep(0,(first_protection_year-1)),gas_costs_frostguard)
  #transport costs
  transport_costs_frostguard<-transport_costs_field_frostguard*
    frostguards_needed*preparation_done
  transport_costs_frostguard<-c(rep(0,(first_protection_year-1)),transport_costs_frostguard)
  
  #Production costs
  n_unsuccessful_no_protection_frostguard<-c(rep(1,(first_protection_year-1)),n_unsuccessful_protection_frostguard)
  for (i in 1 : n_years){
    if(n_unsuccessful_no_protection_frostguard[i] == 1){
      production_costs_frostguard[i]<- production_costs[i]*
        (1+cost_change_frost[i])*plot_area+
        yield_frostguard[i]*plot_area*harvest_and_following_costs[i]} else{
          production_costs_frostguard[i] <- production_costs[i]*
            plot_area+
            yield_frostguard[i]*plot_area*harvest_and_following_costs[i]}}
  
  #total costs frostguard
  costs_frostguard<-initial_cost_frostguard+
    deposit_gas_frostguard+
    running_cost_frostguard+
    wage_costs_frostguard+
    transport_costs_frostguard+
    production_costs_frostguard+
    establishment_costs
  
  #Benefits
  benefits_frostguard<-income_frostguard/plot_area-costs_frostguard/plot_area
  
  #NPV
  NPV_frostguard<-
    discount(benefits_frostguard, discount_rate, calculate_NPV = TRUE)
  
  #Wind machine####
  #(in general)
  
  #setup empty vectors and lists for "wind machine"
  technical_failure_windmachine <- vector(mode = "list", length = n_years_protection)
  ineffective_protection_mobile_windmachine_list <- vector(mode = "list",
                                                           length = n_years_protection)
  ineffective_protection_stationary_windmachine_list <- vector(mode = "list",
                                                               length = n_years_protection)
  damage_wrong_conditions_windmachine<-rep(NA, n_years_protection)
  yield_mobile_windmachine_frost<-rep(NA, n_years)
  yield_mobile_windmachine<-rep(NA, n_years)
  income_mobile_windmachine<- rep(NA, n_years)
  yield_stationary_windmachine_frost<-rep(NA, n_years)
  yield_stationary_windmachine<-rep(NA, n_years)
  income_stationary_windmachine<- rep(NA, n_years)
  residual_value_mobile_windmachine<-rep(0,n_years)
  initial_cost_mobile_windmachine <-rep(0,n_years)
  fuel_costs_mobile_windmachine<-rep(NA,n_years_protection)
  production_costs_mobile_windmachine<-rep(NA, n_years)
  residual_value_stationary_windmachine<-rep(0,n_years)
  initial_cost_stationary_windmachine<-rep(0,n_years)
  workload_stationary_windmachine<-rep(NA,n_years_protection)
  wage_costs_stationary_windmachine<-rep(NA,n_years_protection)
  fuel_costs_stationary_windmachine<-rep(NA,n_years_protection)
  production_costs_stationary_windmachine<-rep(NA, n_years)
  
  for (i in 1:n_years_protection){
    #Risk of technical failure
    if (harmful_frost_event[i+(first_protection_year-1)]!=0){
      technical_failure_windmachine[[i]]<-chance_event(tech_failure_risk_windmachine,
                                                       n= harmful_frost_event[i+(first_protection_year-1)])}else{
                                                         technical_failure_windmachine[[i]]<-0}
    
    #Risk of ineffective protection
    #mobile wind machine
    if (harmful_frost_event[i+(first_protection_year-1)]!=0){
      ineffective_protection_mobile_windmachine_list[[i]]<-chance_event(risk_ineffective_protection_mobile_windmachine,
                                                                        n= harmful_frost_event[i+(first_protection_year-1)])}else{
                                                                          ineffective_protection_mobile_windmachine_list[[i]]<-0}
    #stationary wind machine
    if (harmful_frost_event[i+(first_protection_year-1)]!=0){
      ineffective_protection_stationary_windmachine_list[[i]]<-chance_event(risk_ineffective_protection_stationary_windmachine,
                                                                            n= harmful_frost_event[i+(first_protection_year-1)])}else{
                                                                              ineffective_protection_stationary_windmachine_list[[i]]<-0}
    
    
    #Risk of additional damage (in %), in case of non-inversion weather conditions
    if (frost_event[i+(first_protection_year-1)]!=0){
      damage_wrong_conditions_windmachine[i]<-chance_event(risk_damage_wrong_conditions_windmachine,
                                                           value_if = yield_damage_wrong_conditions_windmachine,
                                                           value_if_not = 0,
                                                           n= n_years_protection)[i]}else{
                                                             damage_wrong_conditions_windmachine[i]<-0}
  }
  damage_wrong_conditions_windmachine<-c(rep(0,(first_protection_year-1)),damage_wrong_conditions_windmachine)
  
  
  technical_failure_windmachine_matrix<-t(sapply(technical_failure_windmachine,
                                                 `length<-`,
                                                 max(2,max(harmful_frost_event))))
  technical_failure_windmachine_matrix_na_0<-technical_failure_windmachine_matrix %>% replace(is.na(.), 0)
  
  ineffective_protection_mobile_windmachine_matrix<-t(sapply(ineffective_protection_mobile_windmachine_list,
                                                             `length<-`,
                                                             max(2,max(harmful_frost_event))))
  ineffective_protection_mobile_windmachine_matrix_na_0<-ineffective_protection_mobile_windmachine_matrix %>% replace(is.na(.), 0)
  ineffective_protection_stationary_windmachine_matrix<-t(sapply(ineffective_protection_stationary_windmachine_list,
                                                                 `length<-`,
                                                                 max(2,max(harmful_frost_event))))
  ineffective_protection_stationary_windmachine_matrix_na_0<-ineffective_protection_stationary_windmachine_matrix %>% replace(is.na(.), 0)
  
  #wind machine (mobile)
  #combine risks
  unsuccessful_protection_mobile_windmachine = (missed_start_matrix_na_0 +
                                                  ineffective_protection_mobile_windmachine_matrix_na_0 +
                                                  technical_failure_windmachine_matrix_na_0 +
                                                  not_prepared_matrix_na_0 > 0) * 1
  
  reduced_protection_mobile_windmachine = (too_late_start_matrix_na_0 > 0) * 1 
  
  reduced_protection_mobile_windmachine<-reduced_protection_mobile_windmachine- unsuccessful_protection_mobile_windmachine
  reduced_protection_mobile_windmachine[reduced_protection_mobile_windmachine==-1]<-0
  #sum it up to calculate unsuccessful protection nights
  n_unsuccessful_protection_mobile_windmachine<-rowSums(unsuccessful_protection_mobile_windmachine)
  n_reduced_protection_mobile_windmachine<-rowSums(reduced_protection_mobile_windmachine)
  
  #Protection efficiency of mobile wind machines
  efficiency_mobile_windmachine<-vv(mean_efficiency_windmachine,
                                    var_CV = var_cv_efficiency_windmachine,
                                    n=n_years_protection,
                                    lower_limit = 0)
  #Reduced efficiency in case of late start
  efficiency_reduce_smaller_problem_mobile_windmachine<-vv(mean_efficiency_reduce_smaller_problem_windmachine,
                                                           var_CV = var_cv_efficiency_reduce_smaller_problem_windmachine,
                                                           n=n_years_protection,
                                                           lower_limit = 0)
  
  #Yield
  for (i in first_protection_year : n_years){
    if (harmful_frost_event[i] != 0){
      yield_mobile_windmachine_frost[i] <- fruit_yield[i]*
        ((1-(1-efficiency_mobile_windmachine[i-(first_protection_year-1)])*
            yield_loss_per_frost_night[i])^(harmful_frost_event[i]-
                                              n_unsuccessful_protection_mobile_windmachine[i-(first_protection_year-1)]-
                                              n_reduced_protection_mobile_windmachine[i-(first_protection_year-1)])*
           (1-yield_loss_per_frost_night[i])^n_unsuccessful_protection_mobile_windmachine[i-(first_protection_year-1)]*
           (1-(1-efficiency_reduce_smaller_problem_mobile_windmachine[i-(first_protection_year-1)])*
              yield_loss_per_frost_night[i])^n_reduced_protection_mobile_windmachine[i-(first_protection_year-1)])} else {
                yield_mobile_windmachine_frost[i] <- fruit_yield[i]}}
  
  for (i in 1 : (first_protection_year-1)){
    if (harmful_frost_event[i] != 0){
      yield_mobile_windmachine_frost[i] <- fruit_yield[i]*
        yield_loss_per_frost_night[i]} else {
          yield_mobile_windmachine_frost[i] <- fruit_yield[i]}}
  
  #further yield reduction due to use under wrong weather conditions (no inversion)
  for (i in 1 : n_years){ 
    if (damage_wrong_conditions_windmachine[i] != 0){
      yield_mobile_windmachine[i] <- yield_mobile_windmachine_frost[i]*
        (1-damage_wrong_conditions_windmachine[i])} else {
          yield_mobile_windmachine[i] <- yield_mobile_windmachine_frost[i]}}
  
  #mobile wind machines needed for the orchard
  mobile_windmachines_needed<-ceiling(plot_area/
                                        max(area_performance_mobile_windmachine,0.1))
  
  #Residual value (assumption: selling the mobile wind machine in the last year)
  residual_value_mobile_windmachine[n_years]<-mobile_windmachines_needed*
    mobile_windmachine_price*
    percentage_residual_value_mobile_windmachine
  
  #Income:
  income_mobile_windmachine<-price*
    yield_mobile_windmachine*plot_area+residual_value_mobile_windmachine
  
  
  #Costs# assuming a mobile wind machine with integrated diesel motor
  
  #Initial costs mobile wind machine
  initial_cost_mobile_windmachine[first_protection_year]<-mobile_windmachine_price*
    mobile_windmachines_needed+
    planning_cost_frost_protection
  
  #running costs mobile windmachine
  running_cost_mobile_windmachine<-vv(mean_maintenance_costs_mobile_windmachine,
                                      var_CV = var_cv_maintenance_mobile_windmachine,
                                      n=n_years_protection,
                                      lower_limit = 0)*
    mobile_windmachines_needed+
    frost_monitoring_costs
  running_cost_mobile_windmachine<-c(rep(0,(first_protection_year-1)),running_cost_mobile_windmachine)
  
  #costs in case of mobile wind machine use
  
  #wage costs preparation and start
  setup_time_mobile_windmachine<-vv(mean_setup_time_mobile_windmachine,
                                    var_CV = var_cv_setup_time_mobile_windmachine,
                                    n=n_years_protection,
                                    lower_limit = 0)
  start_stop_time_mobile_windmachine<-vv(mean_start_stop_time_mobile_windmachine,
                                         var_CV = var_cv_start_stop_time_mobile_windmachine,
                                         n=n_years_protection,
                                         lower_limit = 0)
  workload_mobile_windmachine<-((setup_time_mobile_windmachine*
                                   mobile_windmachines_needed*
                                   (n_prep_and_protection_done+unnecessary_prepared)+
                                   start_stop_time_mobile_windmachine*
                                   mobile_windmachines_needed*
                                   n_prep_and_protection_done))/plot_area
  
  wage_costs_mobile_windmachine<-workload_mobile_windmachine*
    hourly_wage_worker*
    plot_area
  wage_costs_mobile_windmachine<-c(rep(0,(first_protection_year-1)),wage_costs_mobile_windmachine)
  #diesel costs
  fuel_consumption_mobile_windmachine<-vv(mean_fuel_consumption_mobile_windmachine,
                                          var_CV = var_cv_fuel_windmachine,
                                          n=n_years_protection,
                                          lower_limit = 0)
  for(i in 1:n_years_protection){
    fuel_costs_mobile_windmachine[i]<-n_prep_and_protection_done[i]*
      frost_duration_hours[i+(first_protection_year-1)]*
      fuel_consumption_mobile_windmachine[i]*
      mobile_windmachines_needed*
      diesel_price[i+(first_protection_year-1)]}
  
  fuel_costs_mobile_windmachine<-c(rep(0,(first_protection_year-1)),fuel_costs_mobile_windmachine)
  
  #transport costs
  transport_costs_mobile_windmachine<-transport_costs_field_mobile_windmachine*
    mobile_windmachines_needed*preparation_done
  
  transport_costs_mobile_windmachine<-c(rep(0,(first_protection_year-1)),transport_costs_mobile_windmachine)
  
  #Production costs
  n_unsuccessful_no_protection_mobile_windmachine<-c(rep(1,(first_protection_year-1)),n_unsuccessful_protection_mobile_windmachine)
  for (i in 1 : n_years){
    if(n_unsuccessful_no_protection_mobile_windmachine[i] == 1){
      production_costs_mobile_windmachine[i]<- production_costs[i]*
        (1+cost_change_frost[i])*plot_area+
        yield_mobile_windmachine[i]*plot_area*harvest_and_following_costs[i]} else{
          production_costs_mobile_windmachine[i] <- production_costs[i]*
            plot_area+
            yield_mobile_windmachine[i]*plot_area*harvest_and_following_costs[i]}}
  
  #total costs mobile windmachine
  costs_mobile_windmachine<-initial_cost_mobile_windmachine+
    running_cost_mobile_windmachine+
    wage_costs_mobile_windmachine+
    transport_costs_mobile_windmachine+
    fuel_costs_mobile_windmachine+
    production_costs_mobile_windmachine+
    establishment_costs
  
  
  #Benefits
  benefits_mobile_windmachine<-income_mobile_windmachine/plot_area-costs_mobile_windmachine/plot_area
  
  #NPV
  NPV_mobile_windmachine<-
    discount(benefits_mobile_windmachine, discount_rate, calculate_NPV = TRUE)
  
  
  #wind machine (stationary)
  #combine risks
  unsuccessful_protection_stationary_windmachine = (missed_start_matrix_na_0 +
                                                      ineffective_protection_stationary_windmachine_matrix_na_0 +
                                                      technical_failure_windmachine_matrix_na_0 > 0) * 1
  
  
  reduced_protection_stationary_windmachine = (too_late_start_matrix_na_0 > 0) * 1  
  
  reduced_protection_stationary_windmachine<-reduced_protection_stationary_windmachine- unsuccessful_protection_stationary_windmachine
  reduced_protection_stationary_windmachine[reduced_protection_stationary_windmachine==-1]<-0
  #sum it up to calculate unsuccessful protection nights
  n_unsuccessful_protection_stationary_windmachine<-rowSums(unsuccessful_protection_stationary_windmachine)
  n_reduced_protection_stationary_windmachine<-rowSums(reduced_protection_stationary_windmachine)
  efficiency_stationary_windmachine<-vv(mean_efficiency_windmachine,
                                        var_CV = var_cv_efficiency_windmachine,
                                        n=n_years_protection,
                                        lower_limit = 0)
  efficiency_reduce_smaller_problem_stationary_windmachine<-vv(mean_efficiency_reduce_smaller_problem_windmachine,
                                                               var_CV = var_cv_efficiency_reduce_smaller_problem_windmachine,
                                                               n=n_years_protection,
                                                               lower_limit = 0)
  
  
  for (i in first_protection_year : n_years){
    if (harmful_frost_event[i] != 0){
      yield_stationary_windmachine_frost[i] <- fruit_yield[i]*
        ((1-(1-efficiency_stationary_windmachine[i-(first_protection_year-1)])*
            yield_loss_per_frost_night[i])^(harmful_frost_event[i]-
                                              n_unsuccessful_protection_stationary_windmachine[i-(first_protection_year-1)]-
                                              n_reduced_protection_stationary_windmachine[i-(first_protection_year-1)])*
           (1-yield_loss_per_frost_night[i])^n_unsuccessful_protection_stationary_windmachine[i-(first_protection_year-1)]*
           (1-(1-efficiency_reduce_smaller_problem_stationary_windmachine[i-(first_protection_year-1)])*
              yield_loss_per_frost_night[i])^n_reduced_protection_stationary_windmachine[i-(first_protection_year-1)])} else {
                yield_stationary_windmachine_frost[i] <- fruit_yield[i]}}
  
  for (i in 1 : (first_protection_year-1)){
    if (harmful_frost_event[i] != 0){
      yield_stationary_windmachine_frost[i] <- fruit_yield[i]*
        yield_loss_per_frost_night[i]} else {
          yield_stationary_windmachine_frost[i] <- fruit_yield[i]}}
  
  #further yield reduction due to use under wrong weather conditions (no inversion)
  for (i in 1 : n_years){ 
    if (damage_wrong_conditions_windmachine[i] != 0){
      yield_stationary_windmachine[i] <- yield_stationary_windmachine_frost[i]*
        (1-damage_wrong_conditions_windmachine[i])} else {
          yield_stationary_windmachine[i] <- yield_stationary_windmachine_frost[i]}
    
    #stationary wind machines needed for the orchard
    stationary_windmachines_needed<-ceiling(plot_area/
                                              max(area_performance_stationary_windmachine,0.1))   
    #Residual value (assumption: selling the wind machine in the last year)
    residual_value_stationary_windmachine[n_years]<-stationary_windmachines_needed*
      stationary_windmachine_price+
      percentage_residual_value_stationary_windmachine
    
    #Income:
    income_stationary_windmachine[i]<-price[i]*
      yield_stationary_windmachine[i]*plot_area+residual_value_stationary_windmachine[i]}
  
  
  #Costs
  #assuming a stationary wind machine with integrated diesel motor
  
  #terrain correction costs
  terrain_correction_cost<-if(terrain_correction_needed == 1){terrain_correction_cost<-terrain_correction_price}else{terrain_correction_needed<-0}
  
  #Initial costs stationary wind machine
  initial_cost_stationary_windmachine[first_protection_year]<-stationary_windmachine_price*
    stationary_windmachines_needed+
    planning_cost_frost_protection+
    terrain_correction_cost*stationary_windmachines_needed+
    approval_costs_stationary_windmachine+
    installation_costs_stationary_windmachine*stationary_windmachines_needed
  
  #running costs stationary windmachine
  running_cost_stationary_windmachine<-vv(mean_maintenance_costs_stationary_windmachine,
                                          var_CV = var_cv_maintenance_stationay_windmachine,
                                          n=n_years_protection,
                                          lower_limit = 0)*
    stationary_windmachines_needed+
    frost_monitoring_costs
  running_cost_stationary_windmachine<-c(rep(0,(first_protection_year-1)),running_cost_stationary_windmachine)
  #costs in case of mobile windmachine use
  
  #wage costs preparation and start
  start_stop_time_stationary_windmachine<-vv(mean_start_stop_time_stationary_windmachine,
                                             var_CV = var_cv_start_stop_time_stationary_windmachine,
                                             n=n_years_protection,
                                             lower_limit = 0)
  
  for(i in 1:n_years_protection){
    
    workload_stationary_windmachine[i]<-(start_stop_time_stationary_windmachine[i]*
                                           stationary_windmachines_needed*
                                           harmful_frost_event[i+(first_protection_year-1)])/plot_area
    
    wage_costs_stationary_windmachine[i]<-workload_stationary_windmachine[i]*
      hourly_wage_worker*
      plot_area
    
    
    #diesel costs
    
    fuel_costs_stationary_windmachine[i]<-n_protection_no_prep_need[i]*
      frost_duration_hours[i+(first_protection_year-1)]*
      fuel_consumption_stationary_windmachine*
      stationary_windmachines_needed*
      diesel_price[i+(first_protection_year-1)]}
  
  fuel_costs_stationary_windmachine<-c(rep(0,(first_protection_year-1)),fuel_costs_stationary_windmachine)
  wage_costs_stationary_windmachine<-c(rep(0,(first_protection_year-1)),wage_costs_stationary_windmachine)
  #Production costs
  n_unsuccessful_no_protection_stationary_windmachine<-c(rep(1,(first_protection_year-1)),n_unsuccessful_protection_stationary_windmachine)
  for (i in 1 : n_years){
    if(n_unsuccessful_no_protection_stationary_windmachine[i] == 1){
      production_costs_stationary_windmachine[i]<- production_costs[i]*
        (1+cost_change_frost[i])*plot_area+
        yield_stationary_windmachine[i]*plot_area*harvest_and_following_costs[i]} else{
          production_costs_stationary_windmachine[i] <- production_costs[i]*
            plot_area+
            yield_stationary_windmachine[i]*plot_area*harvest_and_following_costs[i]}}
  
  #total costs stationary windmachine
  costs_stationary_windmachine<-initial_cost_stationary_windmachine+
    running_cost_stationary_windmachine+
    wage_costs_stationary_windmachine+
    fuel_costs_stationary_windmachine+
    production_costs_stationary_windmachine+
    establishment_costs
  
  
  #Benefits
  benefits_stationary_windmachine<-income_stationary_windmachine/plot_area-
    costs_stationary_windmachine/plot_area
  
  #NPV
  NPV_stationary_windmachine<-
    discount(benefits_stationary_windmachine,
             discount_rate, calculate_NPV = TRUE)
  
  
  #overhead irrigation####
  #with irrigation pond and own well
  #called ov_irrigation in the code
  #setup empty vectors and lists for "overhead irrigation"
  technical_failure_ov_irrigation <- rep(NA, n_years_protection) 
  protection_stop_wind_ov_irrigation <- vector(mode = "list", length = n_years_protection)
  frozen_water_line_ov_irrigation<-vector(mode = "list", length = n_years_protection)
  n_protection_stop_missing_water_ov_irrigation<-rep(NA, n_years_protection)
  prot_stop_missing_water_harmful_ov_irrigation<-rep(NA, n_years_protection) 
  no_water_ov_irrigation<-vector(mode = "list", length = n_years_protection)
  pseudomonas_ov_irrigation_cost<-rep(NA, n_years_protection)
  waterlogging_ov_irrigation_cost<-rep(NA, n_years_protection)
  branch_fracture_ov_irrigation_cost<-rep(NA, n_years_protection)
  yield_ov_irrigation<-rep(NA, n_years)
  income_ov_irrigation<- rep(NA, n_years)
  add_plant_protection_costs_ov_irrigation<-rep(NA, n_years_protection)
  initial_cost_ov_irrigation<-rep(0,n_years)
  pond_construction_cost_ov_irrigation<-rep(0,n_years)
  residual_value_ov_irrigation<-rep(0,n_years)
  workload_ov_irrigation<-rep(NA,n_years_protection)
  operating_costs_ov_irrigation<-rep(NA,n_years_protection)
  production_costs_ov_irrigation<-rep(NA, n_years)
  
  #irrigation pond
  pond_size_ov_irrigation<-plot_area*pond_dimensioning_irrigation*water_need_ov_irrigation
  n_nights_water_pond_ov_irrigation<-rep(pond_dimensioning_irrigation/mean_frost_duration, n_years_protection)
  
  
  #risks
  for (i in 1:n_years_protection){
    #risk of technical failure
    if (harmful_frost_event[i+(first_protection_year-1)]!=0){
      technical_failure_ov_irrigation[[i]]<-sum(chance_event(tech_failure_risk_ov_irrigation,
                                                             n = frost_event[i]))}else{
                                                               technical_failure_ov_irrigation[[i]]<-0}
    #risk of to much wind for protection
    if (harmful_frost_event[i+(first_protection_year-1)]!=0){
      protection_stop_wind_ov_irrigation[[i]]<-chance_event(wind_risk_ov_irrigation, n= harmful_frost_event[i+(first_protection_year-1)])}else{
        protection_stop_wind_ov_irrigation[[i]]<-0}
    #risk of frozen water lines or sprinklers 
    if (harmful_frost_event[i+(first_protection_year-1)]!=0){
      frozen_water_line_ov_irrigation[[i]]<-chance_event(risk_frozen_water_line_ov_irrigation, n= harmful_frost_event[i+(first_protection_year-1)])}else{
        frozen_water_line_ov_irrigation[[i]]<-0}
    
    #risk of no water availability
    if (n_nights_water_pond_ov_irrigation[i]-(n_protection_no_prep_need[i]+n_unnecessary_start[i]) < 0){
      n_protection_stop_missing_water_ov_irrigation[i] <- ceiling((n_nights_water_pond_ov_irrigation[i]-(n_protection_no_prep_need[i]+n_unnecessary_start[i]))*(-1))} else {
        n_protection_stop_missing_water_ov_irrigation[i] <- 0}
    
    if(n_protection_stop_missing_water_ov_irrigation[i] != 0){
      prot_stop_missing_water_harmful_ov_irrigation[i]<-pmin(sum(chance_event(share_harmful_of_protection[i], n= n_protection_stop_missing_water_ov_irrigation[i])), harmful_frost_event[i+(first_protection_year-1)])} else{
        prot_stop_missing_water_harmful_ov_irrigation[i]<-0}
    
    if (prot_stop_missing_water_harmful_ov_irrigation[i] != 0){
      no_water_ov_irrigation[[i]]<-c(rep(0, (harmful_frost_event[i+(first_protection_year-1)]-prot_stop_missing_water_harmful_ov_irrigation[i])), rep(1, prot_stop_missing_water_harmful_ov_irrigation[i]))}else{
        no_water_ov_irrigation[[i]]<-0}
    
    
    if(harmful_frost_event[i+(first_protection_year-1)]!=0){
      #risk of pseudomonas infection
      pseudomonas_ov_irrigation_cost[i]<-chance_event(risk_pseudomonas_ov_irrigation,
                                                      value_if = vv(mean_costs_handle_pseudomonas,
                                                                    var_CV = var_cv_costs_pseudomonas,
                                                                    n=n_years_protection,
                                                                    lower_limit = 0)*plot_area,
                                                      value_if_not = rep(0, n_years_protection),
                                                      n=n_years_protection)[i]}else{
                                                        pseudomonas_ov_irrigation_cost[i]<-0}
    
    if(harmful_frost_event[i+(first_protection_year-1)]!=0){
      #risk of branch fracture
      branch_fracture_ov_irrigation_cost[i]<-chance_event(risk_branch_fracture_ov_irrigation,
                                                          value_if = vv(mean_costs_handle_branch_fracture,
                                                                        var_CV = var_cv_costs_branch_fracture,
                                                                        n=n_years_protection,
                                                                        lower_limit = 0)*plot_area,
                                                          value_if_not = rep(0, n_years_protection),
                                                          n=n_years_protection)[i]}else{
                                                            branch_fracture_ov_irrigation_cost[i]<-0}
    
    
    if(harmful_frost_event[i+(first_protection_year-1)]!=0){
      #risk of waterlogging
      waterlogging_ov_irrigation_cost[i]<-chance_event(risk_waterlogging_ov_irrigation,
                                                       value_if = vv(mean_costs_handle_waterlogging,
                                                                     var_CV = var_cv_costs_waterlogging,
                                                                     n=n_years_protection,
                                                                     lower_limit = 0)*plot_area,
                                                       value_if_not = rep(0, n_years_protection),
                                                       n=n_years_protection)[i]}else{
                                                         waterlogging_ov_irrigation_cost[i]<-0}
    
  }
  waterlogging_ov_irrigation_cost<-c(rep(0,(first_protection_year-1)),waterlogging_ov_irrigation_cost)
  pseudomonas_ov_irrigation_cost<-c(rep(0,(first_protection_year-1)),pseudomonas_ov_irrigation_cost)
  branch_fracture_ov_irrigation_cost<-c(rep(0,(first_protection_year-1)),branch_fracture_ov_irrigation_cost)
  
  protection_stop_wind_ov_irrigation_matrix<-t(sapply(protection_stop_wind_ov_irrigation, `length<-`, max(2,max(harmful_frost_event))))
  protection_stop_wind_ov_irrigation_matrix_na_0<-protection_stop_wind_ov_irrigation_matrix %>% replace(is.na(.), 0)
  
  frozen_water_line_ov_irrigation_matrix<-t(sapply(frozen_water_line_ov_irrigation, `length<-`, max(2,max(harmful_frost_event))))
  frozen_water_line_ov_irrigation_matrix_na_0<-frozen_water_line_ov_irrigation_matrix %>% replace(is.na(.), 0)
  
  no_water_ov_irrigation_matrix<-t(sapply(no_water_ov_irrigation, `length<-`, max(2,max(harmful_frost_event))))
  no_water_ov_irrigation_matrix_na_0<-no_water_ov_irrigation_matrix %>% replace(is.na(.), 0)
  
  
  #nights with unsuccessful
  unsuccessful_protection_ov_irrigation = (missed_start_matrix_na_0+
                                             protection_stop_wind_ov_irrigation_matrix_na_0+
                                             no_water_ov_irrigation_matrix_na_0> 0) * 1
  #nights reduced protection effect
  reduced_protection_ov_irrigation = (too_late_start_matrix_na_0+
                                        frozen_water_line_ov_irrigation_matrix_na_0> 0) * 1
  
  reduced_protection_ov_irrigation<-reduced_protection_ov_irrigation- unsuccessful_protection_ov_irrigation
  reduced_protection_ov_irrigation[reduced_protection_ov_irrigation==-1]<-0
  
  #sum it up to calculate unsuccessful protection nights
  n_unsuccessful_protection_ov_irrigation<-rowSums(unsuccessful_protection_ov_irrigation)
  n_reduced_protection_ov_irrigation<-rowSums(reduced_protection_ov_irrigation)
  
  #Efficiency of overhead irrigation 
  efficiency_ov_irrigation<-vv(mean_efficiency_ov_irrigation,
                               var_CV = var_cv_efficiency_ov_irrigation,
                               n=n_years_protection,
                               lower_limit = 0)
  #Reduced efficincy in case of late start or frozen water lines
  efficiency_reduce_smaller_problem_ov_irrigation<-vv(mean_efficiency_reduce_smaller_problem_ov_irrigation,
                                                      var_CV = var_cv_efficiency_reduce_smaller_problem_ov_irrigation,
                                                      n=n_years_protection,
                                                      lower_limit = 0)
  
  #Yield
  for (i in first_protection_year : n_years){
    if (harmful_frost_event[i] != 0){
      yield_ov_irrigation[i] <- fruit_yield[i]*
        ((1-(1-efficiency_ov_irrigation[i-(first_protection_year-1)])*
            yield_loss_per_frost_night[i])^(harmful_frost_event[i]-
                                              n_unsuccessful_protection_ov_irrigation[i-(first_protection_year-1)]-
                                              n_reduced_protection_ov_irrigation[i-(first_protection_year-1)])*
           (1-yield_loss_per_frost_night[i])^n_unsuccessful_protection_ov_irrigation[i-(first_protection_year-1)]*
           (1-(1-efficiency_reduce_smaller_problem_ov_irrigation[i-(first_protection_year-1)])*
              yield_loss_per_frost_night[i])^n_reduced_protection_ov_irrigation[i-(first_protection_year-1)])} else {
                yield_ov_irrigation[i] <- fruit_yield[i]}}
  for (i in 1 : (first_protection_year-1)){
    if (harmful_frost_event[i] != 0){
      yield_ov_irrigation[i] <- fruit_yield[i]*
        yield_loss_per_frost_night[i]} else {
          yield_ov_irrigation[i] <- fruit_yield[i]}}
  
  #Initial costs ov_irrigation
  pond_construction_cost_ov_irrigation[first_protection_year]<-(well_drilling_costs+
                                                                  pond_costs*pond_size_ov_irrigation+
                                                                  pump_and_filter_costs_ov_irrigation)*(1-percentage_funding_irrigation_infrastructure)
  #depending on the existence or new building of a pond
  if(existing_irrigation_pond==1){
    initial_cost_ov_irrigation[first_protection_year]<-water_lines_on_field_costs_ov_irrigation*plot_area+
      planning_cost_frost_protection}else{
        initial_cost_ov_irrigation[first_protection_year]<-water_lines_on_field_costs_ov_irrigation*plot_area+
          pond_construction_cost_ov_irrigation[first_protection_year]+
          planning_cost_frost_protection
      }
  
  
  
  #residual value irrigation (no value from the material on the field, only the pond)
  if(existing_irrigation_pond==0){
    residual_value_ov_irrigation[n_years]<-pond_construction_cost_ov_irrigation[first_protection_year]*
      percentage_residual_value_pond}else{
        residual_value_ov_irrigation[n_years]<-0
      }
  #Income:
  income_ov_irrigation<-price*yield_ov_irrigation*plot_area+residual_value_ov_irrigation
  
  
  #Costs#
  #running costs 
  running_cost_ov_irrigation<-vv(mean_maintenance_costs_ov_irrigation,
                                 var_CV = var_cv_maintenance_ov_irrigation,
                                 n=n_years_protection,
                                 lower_limit = 0)*
    plot_area+
    frost_monitoring_costs
  running_cost_ov_irrigation<-c(rep(0,(first_protection_year-1)),running_cost_ov_irrigation)
  #costs in case of irrigation
  
  
  for(i in 1:n_years_protection){
    #workload overhead irrigation
    workload_ov_irrigation[i]<-(n_protection_no_prep_need[i]- rowSums(protection_stop_wind_ov_irrigation_matrix_na_0)[i])*
      frost_duration_hours[i+(first_protection_year-1)]
    #operating costs overhead irrigation
    operating_costs_ov_irrigation[i]<-(n_protection_no_prep_need[i]- rowSums(protection_stop_wind_ov_irrigation_matrix_na_0)[i])*
      frost_duration_hours[i+(first_protection_year-1)]*
      (hourly_wage_worker+
         vv(mean_pump_operating_costs,
            var_CV = var_cv_pump_operating_costs,
            n=n_years_protection,
            lower_limit = 0)[i]+
         water_need_ov_irrigation*
         plot_area*
         water_costs)
    
    #additional plant protection cost with ov irrigation (due to run-off)
    if(harmful_frost_event[i+(first_protection_year-1)]!=0){
      add_plant_protection_costs_ov_irrigation[i]<-vv(mean_costs_additional_plant_protection_ov_irrigation,
                                                      var_CV = var_cv_plant_protection_ov_irrigation,
                                                      n=n_years_protection,
                                                      lower_limit = 0)[i]*plot_area} else{
                                                        add_plant_protection_costs_ov_irrigation[i]<-0
                                                      }}
  
  workload_ov_irrigation<-c(rep(0,(first_protection_year-1)),workload_ov_irrigation)
  operating_costs_ov_irrigation<-c(rep(0,(first_protection_year-1)),operating_costs_ov_irrigation)
  add_plant_protection_costs_ov_irrigation<-c(rep(0,(first_protection_year-1)),add_plant_protection_costs_ov_irrigation)
  
  
  n_unsuccessful_no_protection_ov_irrigation<-c(rep(1,(first_protection_year-1)),n_unsuccessful_protection_ov_irrigation)
  technical_failure_ov_irrigation<-c(rep(0,(first_protection_year-1)),technical_failure_ov_irrigation)
  
  for (i in 1 : n_years){
    #Production costs
    if(n_unsuccessful_no_protection_ov_irrigation[i] == 1|technical_failure_ov_irrigation[i]==1){
      production_costs_ov_irrigation[i]<- production_costs[i]*(1+cost_change_frost[i])*plot_area+
        yield_ov_irrigation[i]*plot_area*harvest_and_following_costs[i]} else{
          production_costs_ov_irrigation[i] <- production_costs[i]*plot_area+
            yield_ov_irrigation[i]*plot_area*harvest_and_following_costs[i]}
    
  }
  
  
  
  #total costs ov_irrigation
  costs_ov_irrigation<-initial_cost_ov_irrigation+
    running_cost_ov_irrigation+
    operating_costs_ov_irrigation+
    production_costs_ov_irrigation+
    pseudomonas_ov_irrigation_cost+
    branch_fracture_ov_irrigation_cost+
    add_plant_protection_costs_ov_irrigation+
    waterlogging_ov_irrigation_cost+
    establishment_costs
  
  #Benefits
  benefits_ov_irrigation<-income_ov_irrigation/plot_area-costs_ov_irrigation/plot_area
  
  #NPV
  NPV_ov_irrigation<-
    discount(benefits_ov_irrigation, discount_rate, calculate_NPV = TRUE)
  
  #below canopy irrigation####
  #with irrigation pond and own well
  #called ut_irrigation in the code
  #setup empty vectors and lists for "under tree irrigation"
  technical_failure_ut_irrigation <- vector(mode = "list", length = n_years_protection)
  protection_stop_wind_ut_irrigation <- vector(mode = "list", length = n_years_protection)
  frozen_water_line_ut_irrigation<-vector(mode = "list", length = n_years_protection)
  n_protection_stop_missing_water_ut_irrigation<-rep(NA, n_years_protection)
  prot_stop_missing_water_harmful_ut_irrigation<-rep(NA, n_years_protection) 
  no_water_ut_irrigation<-vector(mode = "list", length = n_years_protection)
  waterlogging_ut_irrigation_cost<-rep(NA, n_years_protection)
  yield_ut_irrigation<-rep(NA, n_years)
  income_ut_irrigation<- rep(NA, n_years)
  add_fertilizer_costs_ut_irrigation<-rep(NA, n_years_protection)
  initial_cost_ut_irrigation<-rep(0,n_years)
  pond_construction_cost_ut_irrigation<-rep(0,n_years)
  operating_costs_ut_irrigation<-rep(NA, n_years_protection)
  production_costs_ut_irrigation<-rep(NA, n_years_protection)
  workload_ut_irrigation<-rep(NA,n_years_protection)
  
  #irrigation pond
  #possible details: refill and evaporation
  pond_size_ut_irrigation<-plot_area*pond_dimensioning_irrigation*water_need_ut_irrigation
  n_nights_water_pond_ut_irrigation<-rep(pond_dimensioning_irrigation/mean_frost_duration, n_years_protection)
  
  
  #risks
  for (i in 1:n_years_protection){
    #risk of technical failure
    if (harmful_frost_event[i+(first_protection_year-1)]!=0){
      technical_failure_ut_irrigation[[i]]<-chance_event(tech_failure_risk_ut_irrigation,
                                                         n = harmful_frost_event[i+(first_protection_year-1)])}else{
                                                           technical_failure_ut_irrigation[[i]]<-0}
    #risk of to much wind for protection
    if (harmful_frost_event[i+(first_protection_year-1)]!=0){
      protection_stop_wind_ut_irrigation[[i]]<-chance_event(wind_risk_ut_irrigation, n= harmful_frost_event[i+(first_protection_year-1)])}else{
        protection_stop_wind_ut_irrigation[[i]]<-0}
    #risk of frozen water lines or sprinklers 
    if (harmful_frost_event[i+(first_protection_year-1)]!=0){
      frozen_water_line_ut_irrigation[[i]]<-chance_event(risk_frozen_water_line_ut_irrigation, n= harmful_frost_event[i+(first_protection_year-1)])}else{
        frozen_water_line_ut_irrigation[[i]]<-0}
    
    #risk of no water availability
    if (n_nights_water_pond_ut_irrigation[i]-(n_protection_no_prep_need[i]+n_unnecessary_start[i]) < 0){
      n_protection_stop_missing_water_ut_irrigation[i] <- ceiling((n_nights_water_pond_ut_irrigation[i]-(n_protection_no_prep_need[i]+n_unnecessary_start[i]))*(-1))} else {
        n_protection_stop_missing_water_ut_irrigation[i] <- 0}
    
    if(n_protection_stop_missing_water_ut_irrigation[i] != 0){
      prot_stop_missing_water_harmful_ut_irrigation[i]<-pmin(sum(chance_event(share_harmful_of_protection[i], n= n_protection_stop_missing_water_ut_irrigation[i])), harmful_frost_event[i+(first_protection_year-1)])} else{
        prot_stop_missing_water_harmful_ut_irrigation[i]<-0}
    
    if (prot_stop_missing_water_harmful_ut_irrigation[i] != 0){
      no_water_ut_irrigation[[i]]<-c(rep(0, (harmful_frost_event[i+(first_protection_year-1)]-prot_stop_missing_water_harmful_ut_irrigation[i])), rep(1, prot_stop_missing_water_harmful_ut_irrigation[i]))}else{
        no_water_ut_irrigation[[i]]<-0}
    
    
    if(harmful_frost_event[i+(first_protection_year-1)]!=0){
      #risk of waterlogging
      waterlogging_ut_irrigation_cost[i]<-chance_event(risk_waterlogging_ut_irrigation,
                                                       value_if = vv(mean_costs_handle_waterlogging,
                                                                     var_CV = var_cv_costs_waterlogging,
                                                                     n=n_years_protection,
                                                                     lower_limit = 0)*plot_area,
                                                       value_if_not = rep(0, n_years_protection),
                                                       n=n_years_protection)[i]}else{
                                                         waterlogging_ut_irrigation_cost[i]<-0}
    
  }
  
  protection_stop_wind_ut_irrigation_matrix<-t(sapply(protection_stop_wind_ut_irrigation, `length<-`, max(2,max(harmful_frost_event))))
  protection_stop_wind_ut_irrigation_matrix_na_0<-protection_stop_wind_ut_irrigation_matrix %>% replace(is.na(.), 0)
  
  frozen_water_line_ut_irrigation_matrix<-t(sapply(frozen_water_line_ut_irrigation, `length<-`, max(2,max(harmful_frost_event))))
  frozen_water_line_ut_irrigation_matrix_na_0<-frozen_water_line_ut_irrigation_matrix %>% replace(is.na(.), 0)
  
  no_water_ut_irrigation_matrix<-t(sapply(no_water_ut_irrigation, `length<-`, max(2,max(harmful_frost_event))))
  no_water_ut_irrigation_matrix_na_0<-no_water_ut_irrigation_matrix %>% replace(is.na(.), 0)
  
  technical_failure_ut_irrigation_matrix<-t(sapply(technical_failure_ut_irrigation,
                                                   `length<-`, max(2,max(harmful_frost_event))))
  technical_failure_ut_irrigation_matrix_na_0<-technical_failure_ut_irrigation_matrix %>% replace(is.na(.), 0)
  
  waterlogging_ut_irrigation_cost<-c(rep(0,(first_protection_year-1)),waterlogging_ut_irrigation_cost)
  #nights equals to no protection
  unsuccessful_protection_ut_irrigation = (missed_start_matrix_na_0+
                                             protection_stop_wind_ut_irrigation_matrix_na_0+
                                             no_water_ut_irrigation_matrix_na_0+
                                             technical_failure_ut_irrigation_matrix_na_0> 0) * 1
  #nights reduced protection effect
  reduced_protection_ut_irrigation = (too_late_start_matrix_na_0+
                                        frozen_water_line_ut_irrigation_matrix_na_0> 0) * 1
  
  reduced_protection_ut_irrigation<-reduced_protection_ut_irrigation- unsuccessful_protection_ut_irrigation
  reduced_protection_ut_irrigation[reduced_protection_ut_irrigation==-1]<-0
  
  #sum it up to calculate unsuccessful protection nights
  n_unsuccessful_protection_ut_irrigation<-rowSums(unsuccessful_protection_ut_irrigation)
  n_reduced_protection_ut_irrigation<-rowSums(reduced_protection_ut_irrigation)
  
  #protection efficiency of below-canopy irrigation
  efficiency_ut_irrigation<-vv(mean_efficiency_ut_irrigation,
                               var_CV = var_cv_efficiency_ut_irrigation,
                               n=n_years_protection,
                               lower_limit = 0)
  efficiency_reduce_smaller_problem_ut_irrigation<-vv(mean_efficiency_reduce_smaller_problem_ut_irrigation,
                                                      var_CV = var_cv_efficiency_reduce_smaller_problem_ut_irrigation,
                                                      n=n_years_protection,
                                                      lower_limit = 0)
  #Yield
  for (i in first_protection_year : n_years){
    if (harmful_frost_event[i] != 0){
      yield_ut_irrigation[i] <- fruit_yield[i]*
        ((1-(1-efficiency_ut_irrigation[i-(first_protection_year-1)])*
            yield_loss_per_frost_night[i])^(harmful_frost_event[i]-
                                              n_unsuccessful_protection_ut_irrigation[i-(first_protection_year-1)]-
                                              n_reduced_protection_ut_irrigation[i-(first_protection_year-1)])*
           (1-yield_loss_per_frost_night[i])^n_unsuccessful_protection_ut_irrigation[i-(first_protection_year-1)]*
           (1-(1-efficiency_reduce_smaller_problem_ut_irrigation[i-(first_protection_year-1)])*
              yield_loss_per_frost_night[i])^n_reduced_protection_ut_irrigation[i-(first_protection_year-1)])} else {
                yield_ut_irrigation[i] <- fruit_yield[i]}}
  for (i in 1 : (first_protection_year-1)){
    if (harmful_frost_event[i] != 0){
      yield_ut_irrigation[i] <- fruit_yield[i]*
        yield_loss_per_frost_night[i]} else {
          yield_ut_irrigation[i] <- fruit_yield[i]}}
  
  
  #Initial costs ut_irrigation
  pond_construction_cost_ut_irrigation[first_protection_year]<-(well_drilling_costs+
                                                                  pond_costs*pond_size_ut_irrigation+
                                                                  pump_and_filter_costs_ut_irrigation)*(1-percentage_funding_irrigation_infrastructure)
  #initial costs depending of existence or building of a pond
  if(existing_irrigation_pond==1){
    initial_cost_ut_irrigation[first_protection_year]<-water_lines_on_field_costs_ut_irrigation*plot_area+
      planning_cost_frost_protection}else{
        initial_cost_ut_irrigation[first_protection_year]<-water_lines_on_field_costs_ut_irrigation*plot_area+
          pond_construction_cost_ut_irrigation[first_protection_year]+
          planning_cost_frost_protection
      }
  
  #residual value irrigation
  residual_value_ut_irrigation<-rep(0,n_years)
  #residual value irrigation (no value from the material on the field, only the pond)
  residual_value_ut_irrigation<-rep(0,n_years)
  if(existing_irrigation_pond==0){
    residual_value_ut_irrigation[n_years]<-pond_construction_cost_ut_irrigation[first_protection_year]*
      percentage_residual_value_pond}else{
        residual_value_ut_irrigation[n_years]<-0
      }
  
  #Income:
  income_ut_irrigation<-price*yield_ut_irrigation*plot_area+residual_value_ut_irrigation
  
  #Costs#
  #running costs 
  running_cost_ut_irrigation<-vv(mean_maintenance_costs_ut_irrigation,
                                 var_CV = var_cv_maintenance_costs_ut_irrigation,
                                 n=n_years_protection,
                                 lower_limit = 0)*
    plot_area+
    frost_monitoring_costs
  
  running_cost_ut_irrigation<-c(rep(0,(first_protection_year-1)),running_cost_ut_irrigation)
  
  
  for (i in 1 : n_years_protection){
    #costs in case of irrigation
    #workload
    workload_ut_irrigation<-(n_protection_no_prep_need[i]- rowSums(protection_stop_wind_ut_irrigation_matrix_na_0)[i])*
      frost_duration_hours[i+(first_protection_year-1)]
    #operating costs
    operating_costs_ut_irrigation[i]<-(n_protection_no_prep_need[i]- rowSums(protection_stop_wind_ut_irrigation_matrix_na_0)[i])*
      frost_duration_hours[i+(first_protection_year-1)]*
      (hourly_wage_worker+vv(mean_pump_operating_costs,
                             var_CV = var_cv_pump_operating_costs,
                             n=n_years_protection,
                             lower_limit = 0)[i]+water_need_ut_irrigation*plot_area*water_costs)
    
    #additional plant protection cost with ov irrigation (due to run-off)
    if(harmful_frost_event[i]!=0){
      add_fertilizer_costs_ut_irrigation[i]<-vv(mean_costs_additional_fertilizer_ut_irrigation,
                                                var_CV = var_cv_costs_additional_fertilizer_ut_irrigation,
                                                n=n_years_protection,
                                                lower_limit = 0)[i]*plot_area} else{
                                                  add_fertilizer_costs_ut_irrigation[i]<-0
                                                }
  }
  operating_costs_ut_irrigation<-c(rep(0,(first_protection_year-1)),operating_costs_ut_irrigation)
  workload_ut_irrigation<-c(rep(0,(first_protection_year-1)),workload_ut_irrigation)
  add_fertilizer_costs_ut_irrigation<-c(rep(0,(first_protection_year-1)),add_fertilizer_costs_ut_irrigation)
  
  n_unsuccessful_no_protection_ut_irrigation<-c(rep(1,(first_protection_year-1)),n_unsuccessful_protection_ut_irrigation)
  for(i in 1:n_years){
    #Production costs
    if(n_unsuccessful_no_protection_ut_irrigation[i] == 1){
      production_costs_ut_irrigation[i]<- production_costs[i]*(1+cost_change_frost[i])*plot_area+
        yield_ut_irrigation[i]*plot_area*harvest_and_following_costs[i]} else{
          production_costs_ut_irrigation[i] <- production_costs[i]*plot_area+
            yield_ut_irrigation[i]*plot_area*harvest_and_following_costs[i]}}
  
  #total costs ut_irrigation
  costs_ut_irrigation<-initial_cost_ut_irrigation+
    running_cost_ut_irrigation+
    operating_costs_ut_irrigation+
    production_costs_ut_irrigation+
    add_fertilizer_costs_ut_irrigation+
    waterlogging_ut_irrigation_cost+
    establishment_costs
  
  #Benefits
  benefits_ut_irrigation<-income_ut_irrigation/plot_area-costs_ut_irrigation/plot_area
  #NPV
  NPV_ut_irrigation<-
    discount(benefits_ut_irrigation, discount_rate, calculate_NPV = TRUE)
  
  #Heaters####
  #Pellet heaters e.g. Voen-heater or Pelliheat
  
  #setup empty vectors and lists for "heaters"
  fire_costs_heaters<-rep(NA, n_years_protection)
  ineffective_protection_heaters_list <- vector(mode = "list", length = n_years_protection)
  n_protection_stop_missing_pellets<-rep(NA, n_years_protection)
  prot_stop_missing_harmful_pellets<-rep(NA, n_years_protection)
  protection_stop_missing_pellets_list<- vector(mode = "list", length = n_years_protection)
  yield_heaters<-rep(NA, n_years)
  income_heaters<- rep(NA, n_years)
  running_costs_heaters<-rep(NA, n_years_protection)
  preparation_deconstruction_costs_heaters<-rep(NA, n_years_protection)
  preparation_deconstruction_time_heaters<-rep(NA, n_years_protection)
  pellets_needed_per_heater<-rep(NA, n_years_protection)
  residual_value_heater<-rep(0,n_years)
  production_costs_heaters<-rep(NA, n_years)
  
  #Risks
  #Fire damage on trees or skin burn of workers
  fire_damage_heaters<-chance_event(fire_damage_risk_heaters, n = n_years_protection)
  
  for (i in 1 : n_years_protection){
    if (fire_damage_heaters[i] == 1){
      fire_costs_heaters[i] <- costs_fire_damage} else {
        fire_costs_heaters[i] <- 0}
    
    #ineffective_protection e.g. due to very cold temperature or wind
    if (harmful_frost_event[i+(first_protection_year-1)]!=0){
      ineffective_protection_heaters_list[[i]]<-chance_event(risk_ineffective_protection_heaters,
                                                             n= harmful_frost_event[i])}else{
                                                               ineffective_protection_heaters_list[[i]]<-0}
  }
  
  ineffective_protection_heaters_matrix<-t(sapply(ineffective_protection_heaters_list,
                                                  `length<-`, max(2,max(harmful_frost_event))))
  ineffective_protection_heaters_matrix_na_0<-ineffective_protection_heaters_matrix %>% replace(is.na(.), 0)
  fire_costs_heaters<-c(rep(0,(first_protection_year-1)),fire_costs_heaters)
  
  #pellets sold out in spring
  pellets_sold_out_spring<-chance_event(sold_out_risk_pellets, n=n_years_protection)
  
  #pellets needed per year
  for(i in 1:n_years_protection){
    pellets_needed_per_heater[i]<-ceiling((frost_duration_hours[i+(first_protection_year-1)]*
                                             n_prep_and_protection_done[i]*
                                             pellet_need_heaters)+
                                            (mean_frost_duration*
                                               n_unnecessary_start_prep[i]*
                                               pellet_need_heaters))}
  
  #initial pellet purchase = amount in storage
  initial_pellet_purchase<-plot_area*
    storage_recommendation_pellets
  #number of nights which could be protected with stored pellets
  n_nights_pellets_storage<-initial_pellet_purchase/
    (heaters_needed_per_ha*
       plot_area*
       pellets_in_heater)
  
  remaining_nights_pellets_heaters<-n_nights_pellets_storage-
    (n_prep_and_protection_done+
       n_unnecessary_start_prep)
  
  for (i in 1 : n_years_protection){
    #number and point in time of a protection stop due to missing pellets
    if (remaining_nights_pellets_heaters[i] < 0 & pellets_sold_out_spring[i] == 1){
      n_protection_stop_missing_pellets[i] <- ceiling(remaining_nights_pellets_heaters[i]*(-1))} else {
        n_protection_stop_missing_pellets[i] <- 0}
    
    if(harmful_frost_event[i+(first_protection_year-1)] != 0 & n_protection_stop_missing_pellets[i] > 0){
      prot_stop_missing_harmful_pellets[i]<-pmin(sum(chance_event(share_harmful_of_protection_prep[i],
                                                                  n= n_protection_stop_missing_pellets[i])),
                                                 harmful_frost_event[i+(first_protection_year-1)])} else{
                                                   prot_stop_missing_harmful_pellets[i]<-0}
    
    if (n_protection_stop_missing_pellets[i] != 0){
      protection_stop_missing_pellets_list[[i]]<-c(rep(0, (harmful_frost_event[i+(first_protection_year-1)]-
                                                             prot_stop_missing_harmful_pellets[i])),
                                                   rep(1, prot_stop_missing_harmful_pellets[i]))}else{
                                                     protection_stop_missing_pellets_list[[i]]<-0}
  }
  
  protection_stop_missing_pellets_matrix<-t(sapply(protection_stop_missing_pellets_list,
                                                   `length<-`, max(2,max(harmful_frost_event))))
  protection_stop_missing_pellets_matrix_na_0<-protection_stop_missing_pellets_matrix %>% replace(is.na(.), 0)
  
  #combine risks
  unsuccessful_protection_heaters = (missed_start_matrix_na_0 + 
                                       not_prepared_matrix_na_0 + 
                                       ineffective_protection_heaters_matrix_na_0 + 
                                       protection_stop_missing_pellets_matrix_na_0 > 0) * 1
  #nights reduced protection effect
  reduced_protection_heaters = (too_late_start_matrix_na_0 > 0) * 1
  
  reduced_protection_heaters<-reduced_protection_heaters- unsuccessful_protection_heaters
  reduced_protection_heaters[reduced_protection_heaters==-1]<-0
  
  #sum it up to calculate unsuccessful protection nights
  n_unsuccessful_protection_heaters<-rowSums(unsuccessful_protection_heaters)
  n_reduced_protection_heaters<-rowSums(reduced_protection_heaters)
  #time needed for heater preparation and deconstruction
  preparation_time_heaters<-heaters_needed_per_ha*vv(mean_heater_preparation_time,
                                                     var_CV = var_cv_heater_preparation,
                                                     n= n_years_protection,
                                                     lower_limit = 0.0001)
  
  deconstruction_time_heaters<-heaters_needed_per_ha*vv(mean_heater_deconstruction_time,
                                                        var_CV = var_cv_heater_deconstruction,
                                                        n= n_years_protection,
                                                        lower_limit = 0.0001)
  #Protection efficiency of pellet heaters
  efficiency_heaters<-vv(mean_efficiency_heaters,
                         var_CV = var_cv_efficiency_heaters,
                         n=n_years_protection,
                         lower_limit = 0)
  #reduced efficiency in case of to late ignition 
  efficiency_reduce_smaller_problem_heaters<-vv(mean_efficiency_reduce_smaller_problem_heaters,
                                                var_CV = var_cv_efficiency_reduce_smaller_problem_heaters,
                                                n=n_years_protection,
                                                lower_limit = 0)
  #Yield
  for (i in first_protection_year : n_years){
    if (harmful_frost_event[i] != 0){
      yield_heaters[i] <- fruit_yield[i]*
        ((1-(1-efficiency_heaters[i-(first_protection_year-1)])*
            yield_loss_per_frost_night[i])^(harmful_frost_event[i]-
                                              n_unsuccessful_protection_heaters[i-(first_protection_year-1)]-
                                              n_reduced_protection_heaters[i-(first_protection_year-1)])*
           (1-yield_loss_per_frost_night[i])^n_unsuccessful_protection_heaters[i-(first_protection_year-1)]*
           (1-(1-efficiency_reduce_smaller_problem_heaters[i-(first_protection_year-1)])*
              yield_loss_per_frost_night[i])^n_reduced_protection_heaters[i-(first_protection_year-1)])} else {
                yield_heaters[i] <- fruit_yield[i]}}
  for (i in 1 : (first_protection_year-1)){
    if (harmful_frost_event[i] != 0){
      yield_heaters[i] <- fruit_yield[i]*
        yield_loss_per_frost_night[i]} else {
          yield_heaters[i] <- fruit_yield[i]}}
  
  #residual value heaters
  residual_value_heater[n_years]<-heaters_needed_per_ha*
    plot_area*
    heater_price*
    percentage_residual_value_heaters
  
  #Income:
  income_heaters<-price*yield_heaters*plot_area+residual_value_heater
  
  #Costs###
  #Initial cost (number of heaters to buy plus the general planning costs)
  pellet_price<-vv(mean_pellet_price,
                   var_CV = var_cv_pellet_price,
                   n= n_years_protection,
                   lower_limit = 0)
  initial_cost_heaters <- rep(0,n_years)
  initial_cost_heaters[first_protection_year]<-heaters_needed_per_ha*plot_area*heater_price+
    initial_pellet_purchase*pellet_price[first_protection_year]+
    planning_cost_frost_protection
  
  
  #running costs (storage costs)
  running_costs_heaters<-heaters_needed_per_ha*plot_area*
    storage_cost_per_heater+
    frost_monitoring_costs+
    vv(mean_maintenance_costs_heaters, var_CV = var_cv_maintenance_heaters, n=n_years_protection, lower_limit = 0)*plot_area
  running_costs_heaters<-c(rep(0,(first_protection_year-1)),running_costs_heaters)
  
  #preparation and deconstruction costs
  #transport costs *2: setting up an second transport for deconstruction
  for(i in 1:n_years_protection){
    if (preparation_done[i] == 1){
      preparation_deconstruction_costs_heaters[i] <-(preparation_time_heaters[i]
                                                     +deconstruction_time_heaters[i])*
        hourly_wage_worker*plot_area+
        (transport_costs_field_heaters*heaters_needed_per_ha*plot_area)*2} else {
          preparation_deconstruction_costs_heaters[i] <- 0}
    
    if (preparation_done[i] == 1){
      preparation_deconstruction_time_heaters[i] <-preparation_time_heaters[i]+deconstruction_time_heaters[i]} else {
        preparation_deconstruction_time_heaters[i] <- 0}
  }
  
  preparation_deconstruction_costs_heaters<-c(rep(0,(first_protection_year-1)),preparation_deconstruction_costs_heaters)
  
  #costs in case of heater use
  
  #pellet refill costs for every additional frost night
  pellet_refill_time_heaters<-vv(mean_pellet_refill_time_heaters,
                                 var_CV = var_cv_pellet_refill_time_heaters,
                                 n=n_years_protection,
                                 lower_limit = 0)
  refill_time_heaters<-heaters_needed_per_ha*
    n_prep_and_protection_done*
    pellet_refill_time_heaters[i]
  pellet_refill_costs<-refill_time_heaters*hourly_wage_worker*plot_area
  pellet_refill_costs<-c(rep(0,(first_protection_year-1)),pellet_refill_costs)
  #ignite the heaters and remove the storage cap to ensure a full burning
  fire_control_time_heaters<-((heaters_needed_per_ha*
                                 plot_area)/
                                vv(mean_heaters_ignited_per_h,
                                   var_CV = var_cv_heater_ignition,
                                   n= n_years_protection,
                                   lower_limit = 0.0001)+
                                (heaters_needed_per_ha*plot_area)/
                                vv(mean_heater_covers_removed_per_h,
                                   var_CV = var_cv_heater_cover_removed,
                                   n= n_years_protection,
                                   lower_limit = 0.0001))/plot_area
  
  fire_control_cost_heaters<-fire_control_time_heaters*
    hourly_wage_worker*
    n_prep_and_protection_done*
    plot_area
  fire_control_cost_heaters<-c(rep(0,(first_protection_year-1)),fire_control_cost_heaters)
  #Pellet Repurchase
  repurchase_costs_pellets<-n_prep_and_protection_done*
    heaters_needed_per_ha*
    plot_area*
    pellets_in_heater*
    pellet_price
  
  repurchase_costs_pellets<-c(rep(0,(first_protection_year-1)),repurchase_costs_pellets)
  #Production costs
  n_unsuccessful_no_protection_heaters<-c(rep(1,(first_protection_year-1)),n_unsuccessful_protection_heaters)
  for (i in 1 : n_years){
    if(n_unsuccessful_no_protection_heaters[i] == 1){
      production_costs_heaters[i]<- production_costs[i]*(1+cost_change_frost[i])*
        plot_area+
        yield_heaters[i]*plot_area*harvest_and_following_costs[i]} else{
          production_costs_heaters[i] <- production_costs[i]*plot_area+
            yield_heaters[i]*plot_area*harvest_and_following_costs[i]}}
  
  #workload heater
  workload_heaters<-refill_time_heaters+
    fire_control_time_heaters*
    n_prep_and_protection_done+
    preparation_deconstruction_time_heaters
  
  #heater costs
  costs_heaters<-initial_cost_heaters+
    running_costs_heaters[i]+
    preparation_deconstruction_costs_heaters+
    pellet_refill_costs[i]+
    fire_control_cost_heaters+
    repurchase_costs_pellets+
    production_costs_heaters+
    fire_costs_heaters+
    establishment_costs
  
  #Benefits
  benefits_heaters<-income_heaters/plot_area-costs_heaters/plot_area
  
  
  #NPV
  NPV_heaters<-
    discount(benefits_heaters, discount_rate, calculate_NPV = TRUE)
  
  #No frost and protection####
  #theoretical scenario for reality check and comparisons
  #setup empty vectors for "no protection"
  yield_no_frost_no_protection<-fruit_yield
  income_no_frost_no_protection<-rep(NA, n_years)
  costs_no_frost_no_protection<- rep(NA, n_years)
  
  for (i in 1 : n_years){
    #Income:
    #more possible details: price frost dependent, yield_loss time and temperature dependent, yield tree age dependent
    income_no_frost_no_protection[i]<-price[i]*yield_no_frost_no_protection[i]*plot_area
    
    #Costs
    #more possible details: cost change event (temperature/duration) dependent
    costs_no_frost_no_protection[i] <- production_costs[i]*
      plot_area+
      establishment_costs[i]+
      harvest_and_following_costs[i]*
      yield_no_frost_no_protection[i]*
      plot_area
  }
  
  #Benefits
  benefits_no_frost_no_protection<-income_no_frost_no_protection/plot_area-costs_no_frost_no_protection/plot_area
  
  #NPV
  NPV_no_frost_no_protection<-
    discount(benefits_no_frost_no_protection, discount_rate, calculate_NPV = TRUE)
  
  
  
  #Model return####
  return(list(result_no_protection = NPV_no_protection,
              result_candles = NPV_candles,
              result_frostbuster = NPV_frostbuster,
              result_frostguard = NPV_frostguard,
              result_mobile_windmachine = NPV_mobile_windmachine,
              result_stationary_windmachine = NPV_stationary_windmachine,
              result_ov_irrigation = NPV_ov_irrigation,
              result_ut_irrigation = NPV_ut_irrigation,
              result_heaters = NPV_heaters,
              result_no_frost_no_protection = NPV_no_frost_no_protection,
              advantage_candles = NPV_candles-NPV_no_protection,
              advantage_frostbuster = NPV_frostbuster - NPV_no_protection,
              advantage_frostguard = NPV_frostguard - NPV_no_protection,
              advantage_mobile_windmachine = NPV_mobile_windmachine - NPV_no_protection,
              advantage_stationary_windmachine = NPV_stationary_windmachine - NPV_no_protection,
              advantage_ov_irrigation = NPV_ov_irrigation - NPV_no_protection,
              advantage_ut_irrigation = NPV_ut_irrigation - NPV_no_protection,
              advantage_heaters = NPV_heaters - NPV_no_protection,
              cashflow_no_protection = benefits_no_protection,
              cashflow_candles = benefits_candles,
              cashflow_frostbuster= benefits_frostbuster,
              cashflow_frostguard = benefits_frostguard,
              cashflow_mobile_windmachine = benefits_mobile_windmachine,
              cashflow_stationary_windmachine = benefits_stationary_windmachine,
              cashflow_ov_irrigation = benefits_ov_irrigation,
              cashflow_ut_irrigation = benefits_ut_irrigation,
              cashflow_heaters = benefits_heaters,
              cashflow_no_frost_no_protection = benefits_no_frost_no_protection,
              yieldflow_no_protection = yield_no_protection,
              yieldflow_candles = yield_candles,
              yieldflow_frostbuster = yield_frostbuster,
              yieldflow_frostguard = yield_frostguard,
              yieldflow_mobile_windmachine = yield_mobile_windmachine,
              yieldflow_stationary_windmachine = yield_stationary_windmachine,
              yieldflow_ov_irrigation = yield_ov_irrigation,
              yieldflow_ut_irrigation = yield_ut_irrigation,
              yieldflow_heaters = yield_heaters,
              yieldflow_no_frost_no_protection = yield_no_frost_no_protection,
              incomeflow_no_protection = income_no_protection/plot_area,
              incomeflow_candles = income_candles/plot_area,
              incomeflow_frostbuster = income_frostbuster/plot_area,
              incomeflow_frostguard = income_frostguard/plot_area,
              incomeflow_mobile_windmachine = income_mobile_windmachine/plot_area,
              incomeflow_stationary_windmachine = income_stationary_windmachine/plot_area,
              incomeflow_ov_irrigation = income_ov_irrigation/plot_area,
              incomeflow_ut_irrigation = income_ut_irrigation/plot_area,
              incomeflow_heaters = income_heaters/plot_area,
              incomeflow_no_frost_no_protection = income_no_frost_no_protection/plot_area,
              yield_advantage_mobile_windmachine = sum(yield_mobile_windmachine)-sum(yield_no_protection),
              yield_advantage_stationary_windmachine = sum(yield_stationary_windmachine)-sum(yield_no_protection),
              yield_advantage_candles = sum(yield_candles)-sum(yield_no_protection),
              yield_advantage_ut_irrigation = sum(yield_ut_irrigation)-sum(yield_no_protection),
              yield_advantage_frostbuster = sum(yield_frostbuster)-sum(yield_no_protection),
              yield_advantage_frostguard = sum(yield_frostguard)-sum(yield_no_protection),
              yield_advantage_ov_irrigation = sum(yield_ov_irrigation)-sum(yield_no_protection),
              yield_advantage_heaters = sum(yield_heaters)-sum(yield_no_protection),
              yield_no_protection = sum(yield_no_protection),
              yield_mobile_windmachine = sum(yield_mobile_windmachine),
              yield_stationary_windmachine = sum(yield_stationary_windmachine),
              yield_candles = sum(yield_candles),
              yield_ut_irrigation = sum(yield_ut_irrigation),
              yield_frostbuster = sum(yield_frostbuster),
              yield_frostguard = sum(yield_frostguard),
              yield_ov_irrigation = sum(yield_ov_irrigation),
              yield_heaters = sum(yield_heaters),
              yield_no_frost_no_protection = sum (yield_no_frost_no_protection),
              n_frost_nights=sum(frost_event),
              n_harmful_frost_nights=sum(harmful_frost_event),
              n_harmful_frost_nights_per_year=harmful_frost_event,
              yieldflow_without_frost=fruit_yield,
              n_unsuccessful_protection_mobile_windmachine = sum(n_unsuccessful_protection_mobile_windmachine),
              n_unsuccessful_protection_stationary_windmachine = sum(n_unsuccessful_protection_stationary_windmachine),
              n_unsuccessful_protection_candles = sum(n_unsuccessful_protection_candles),
              n_unsuccessful_protection_ut_irrigation = sum(n_unsuccessful_protection_ut_irrigation),
              n_unsuccessful_protection_frostbuster = sum(n_unsuccessful_protection_frostbuster),
              n_unsuccessful_protection_frostguard = sum(n_unsuccessful_protection_frostguard),
              n_unsuccessful_protection_ov_irrigation = sum(n_unsuccessful_protection_ov_irrigation),
              n_unsuccessful_protection_heaters = sum(n_unsuccessful_protection_heaters),
              protection_costs_candles=(costs_candles-establishment_costs-production_costs_candles)/plot_area,
              protection_costs_frostbuster=(costs_frostbuster-establishment_costs-production_costs_frostbuster)/plot_area,
              protection_costs_frostguard=(costs_frostguard-establishment_costs-production_costs_frostguard)/plot_area,
              protection_costs_mobile_windmachine=(costs_mobile_windmachine-establishment_costs-production_costs_mobile_windmachine)/plot_area,
              protection_costs_stationary_windmachine=(costs_stationary_windmachine-establishment_costs-production_costs_stationary_windmachine)/plot_area,
              protection_costs_ov_irrigation=(costs_ov_irrigation-establishment_costs-production_costs_ov_irrigation)/plot_area,
              protection_costs_ut_irrigation=(costs_ut_irrigation-establishment_costs-production_costs_ut_irrigation)/plot_area,
              protection_costs_heaters=(costs_heaters-establishment_costs-production_costs_heaters)/plot_area,
              expensesflow_no_protection = costs_no_protection/plot_area,
              expensesflow_candles = costs_candles/plot_area,
              expensesflow_frostbuster = costs_frostbuster/plot_area,
              expensesflow_frostguard = costs_frostguard/plot_area,
              expensesflow_mobile_windmachine = costs_mobile_windmachine/plot_area,
              expensesflow_stationary_windmachine = costs_stationary_windmachine/plot_area,
              expensesflow_ov_irrigation = costs_ov_irrigation/plot_area,
              expensesflow_ut_irrigation = costs_ut_irrigation/plot_area,
              expensesflow_heaters = costs_heaters/plot_area,
              expensesflow_no_frost_no_protection = costs_no_frost_no_protection/plot_area,
              cost_per_kg_no_protection_full_bearing = sum(costs_no_protection[5:n_years])/(sum(yield_no_protection[5:n_years]*plot_area)),
              cost_per_kg_no_protection_whole_time = sum(costs_no_protection)/(sum(yield_no_protection*plot_area)),
              cost_per_kg_candles_full_bearing = sum(costs_candles[5:n_years])/(sum(yield_candles[5:n_years]*plot_area)),
              cost_per_kg_candles_whole_time = sum(costs_candles)/(sum(yield_candles*plot_area)),
              cost_per_kg_frostbuster_full_bearing = sum(costs_frostbuster[5:n_years])/(sum(yield_frostbuster[5:n_years]*plot_area)),
              cost_per_kg_frostbuster_whole_time = sum(costs_frostbuster)/(sum(yield_frostbuster*plot_area)),
              cost_per_kg_frostguard_full_bearing = sum(costs_frostguard[5:n_years])/(sum(yield_frostguard[5:n_years]*plot_area)),
              cost_per_kg_frostguard_whole_time = sum(costs_frostguard)/(sum(yield_frostguard*plot_area)),
              cost_per_kg_mobile_windmachine_full_bearing = sum(costs_mobile_windmachine[5:n_years])/(sum(yield_mobile_windmachine[5:n_years]*plot_area)),
              cost_per_kg_mobile_windmachine_whole_time = sum(costs_mobile_windmachine)/(sum(yield_mobile_windmachine*plot_area)),
              cost_per_kg_stationary_windmachine_full_bearing = sum(costs_stationary_windmachine[5:n_years])/(sum(yield_stationary_windmachine[5:n_years]*plot_area)),
              cost_per_kg_stationary_windmachine_whole_time = sum(costs_stationary_windmachine)/(sum(yield_stationary_windmachine*plot_area)),
              cost_per_kg_ov_irrigation_full_bearing = sum(costs_ov_irrigation[5:n_years])/(sum(yield_ov_irrigation[5:n_years]*plot_area)),
              cost_per_kg_ov_irrigation_whole_time = sum(costs_ov_irrigation)/(sum(yield_ov_irrigation*plot_area)),
              cost_per_kg_ut_irrigation_full_bearing = sum(costs_ut_irrigation[5:n_years])/(sum(yield_ut_irrigation[5:n_years]*plot_area)),
              cost_per_kg_ut_irrigation_whole_time = sum(costs_ut_irrigation)/(sum(yield_ut_irrigation*plot_area)),
              cost_per_kg_heaters_full_bearing = sum(costs_heaters[5:n_years])/(sum(yield_heaters[5:n_years]*plot_area)),
              cost_per_kg_heaters_whole_time = sum(costs_heaters)/(sum(yield_heaters*plot_area)),
              cost_per_kg_no_frost_no_protection_full_bearing = sum(costs_no_frost_no_protection[5:n_years])/(sum(yield_no_frost_no_protection[5:n_years]*plot_area)),
              cost_per_kg_no_frost_no_protection_whole_time = sum(costs_no_frost_no_protection)/(sum(yield_no_frost_no_protection*plot_area)),
              investment_candles = initial_cost_candles[first_protection_year]/plot_area,
              investment_frostbuster = initial_cost_frostbuster[first_protection_year]/plot_area,
              investment_frostguard = initial_cost_frostguard[first_protection_year]/plot_area,
              investment_stationary_windmachine = initial_cost_stationary_windmachine[first_protection_year]/plot_area,
              investment_mobile_windmachine = initial_cost_mobile_windmachine[first_protection_year]/plot_area,
              investment_ov_irrigation = initial_cost_ov_irrigation[first_protection_year]/plot_area,
              investment_ut_irrigation = initial_cost_ut_irrigation[first_protection_year]/plot_area,
              investment_heaters = initial_cost_heaters[first_protection_year]/plot_area,
              irrigation_pond_ov_irrigation_costs = pond_construction_cost_ov_irrigation,
              irrigation_pond_ut_irrigation_costs = pond_construction_cost_ut_irrigation,
              acc_protection_costs_candles=sum((costs_candles-establishment_costs-production_costs_candles)/plot_area),
              acc_protection_costs_frostbuster=sum((costs_frostbuster-establishment_costs-production_costs_frostbuster)/plot_area),
              acc_protection_costs_frostguard=sum((costs_frostguard-establishment_costs-production_costs_frostguard)/plot_area),
              acc_protection_costs_mobile_windmachine=sum((costs_mobile_windmachine-establishment_costs-production_costs_mobile_windmachine)/plot_area),
              acc_protection_costs_stationary_windmachine=sum((costs_stationary_windmachine-establishment_costs-production_costs_stationary_windmachine)/plot_area),
              acc_protection_costs_ov_irrigation=sum((costs_ov_irrigation-establishment_costs-production_costs_ov_irrigation)/plot_area),
              acc_protection_costs_ut_irrigation=sum((costs_ut_irrigation-establishment_costs-production_costs_ut_irrigation)/plot_area),
              acc_protection_costs_heaters=sum((costs_heaters-establishment_costs-production_costs_heaters)/plot_area),
              add_income_candles=sum(income_candles/plot_area)-sum(income_no_protection/plot_area),
              add_income_frostbuster=sum(income_frostbuster/plot_area)-sum(income_no_protection/plot_area),
              add_income_frostguard=sum(income_frostguard/plot_area)-sum(income_no_protection/plot_area),
              add_income_stationary_windmachine=sum(income_stationary_windmachine/plot_area)-sum(income_no_protection/plot_area),
              add_income_mobile_windmachine=sum(income_mobile_windmachine/plot_area)-sum(income_no_protection/plot_area),
              add_income_ov_irrigation=sum(income_ov_irrigation/plot_area)-sum(income_no_protection/plot_area),
              add_income_ut_irrigation=sum(income_ut_irrigation/plot_area)-sum(income_no_protection/plot_area),
              add_income_heaters=sum(income_heaters/plot_area)-sum(income_no_protection/plot_area)
  ))
  
}


# frost_protection_apple<-frost_protection_apple(plot_area<-input$plot_area[1],
#                                                terrain_correction_needed<-1,
#                                                existing_irrigation_pond<-input$existing_irrigation_pond,
#                                                groundwater_well<-0)
# 
# 
# 
# #Monte Carlo####
# # apple_quality_and_yield_mc_simulation_tp4 <- mcSimulation(estimate = as.estimate(Apple_prediction_input_raw),
# #                                                           model_function = tp_4_quality_and_yield_prediction,
# #                                                           numberOfModelRuns = input$runs,
# #                                                           functionSyntax = "plainNames")
# frost_protection_mc_simulation <- mcSimulation(estimate = as.estimate(Frost_input_all),
#                                                model_function = frost_protection_apple,
#                                                numberOfModelRuns = input$runs,
#                                                functionSyntax = "plainNames")
