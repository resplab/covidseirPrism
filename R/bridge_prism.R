model_run<-function(model_input = NULL)
{

  model_input_default <- get_default_input() #TODO REMOVE!

  input<-model_input

  model <- fit_seir(
    daily_cases            = model_input$daily_cases           ,
    obs_model              = model_input$obs_model             ,
    forecast_days          = model_input$fit_forecast_days     ,
    time_increment         = model_input$time_increment        ,
    samp_frac_fixed        = model_input_default$samp_frac_fixed       , #TODO Correct
    samp_frac_type         = model_input$samp_frac_type        ,
    samp_frac_seg          = model_input_default$samp_frac_seg         ,
    #TODO weird error caused by f_seq
#    f_seg                  = model_input$f_seg                 ,
     days_back              = model_input$days_back             ,

     R0_prior               = model_input$R0_prior              ,
     phi_prior              = model_input$phi_prior             ,
     f_prior                = model_input$f_prior               ,
     e_prior                = model_input$e_prior               ,
     samp_frac_prior        = model_input$samp_frac_prior       ,
     start_decline_prior    = model_input$start_decline_prior   ,
     end_decline_prior      = model_input$end_decline_prior     ,
     f_ramp_rate            = model_input$f_ramp_rate           ,
     rw_sigma               = model_input$rw_sigma              ,
     seed                   = model_input$seed                  ,
     chains                 = model_input$chains                ,
     iter                   = model_input$fit_iter              ,
     N_pop                  = model_input$N_pop                 ,
     pars                   = model_input_default$pars                  , #TODO Correct
     i0_prior               = model_input$i0_prior              ,
     state_0                = model_input_default$state_0               , #TODO Correct
     save_state_predictions = model_input$save_state_predictions,
     delay_scale            = model_input$delay_scale           ,
     delay_shape            = model_input$delay_shape           ,
     ode_control            = model_input$ode_control
  )

  model

  obs_dat <- data.frame(day = seq_along(model_input$daily_cases), value = model_input$daily_cases)

  projection <- project_seir(model,
                              forecast_days   = model_input$project_forecast_days,
                              f_fixed_start   = model_input$f_fixed_start        ,
                              f_fixed         = model_input$f_fixed              ,
                              iter            = model_input$forecast_iter        ,
                              return_states   = model_input$return_states        ,
                              imported_cases  = model_input$imported_cases       ,
                              imported_window = model_input$imported_window
  )

  projection

  plot <- tidy_seir(projection) %>% plot_projection(obs_dat = obs_dat)
  print(plot)

  states_with_Rt <- get_rt(model)
  states_with_Rt

  plot2 <- ggplot(states_with_Rt, aes(time, Rt, group = .iteration)) +
    geom_line(alpha = 0.5)

  print(plot2)

  results <- list(model$post)
  return(results)
}


get_default_input <- function() {

  cases <- c(
    0, 0, 1, 3, 1, 8, 0, 6, 5, 0, 7, 7, 18, 9, 22, 38, 53, 45, 40,
    77, 76, 48, 67, 78, 42, 66, 67, 92, 16, 70, 43, 53, 55, 53, 29,
    26, 37, 25, 45, 34, 40, 35)

  model_input <- list (
    daily_cases            = cases,
    obs_model              = c("NB2", "Poisson"),
    fit_forecast_days          = 0,
    time_increment         = 0.25,
    samp_frac_fixed        = c(rep(0.1, 13), rep(0.2, length(cases) - 13)),
    samp_frac_type         = c("fixed", "estimated", "rw", "segmented"),
    samp_frac_seg          = NULL,
#    f_seg                  = c(0, rep(1, nrow(daily_cases) + forecast_days - 1)),
    days_back              = 45,
    R0_prior               = c(log(2.6), 0.2),
    phi_prior              = 1,
    f_prior                = c(0.4, 0.2),
    e_prior                = c(0.8, 0.05),
    samp_frac_prior        = c(0.4, 0.2),
    start_decline_prior    = c(log(15), 0.05),
    end_decline_prior      = c(log(22), 0.05),
    f_ramp_rate            = 0,
    rw_sigma               = 0.1,
    seed                   = 42,
    chains                 = 1,
    fit_iter               = 100,
    N_pop                  = 5100000,
    pars                   = c(D = 5, k1 = 1/5, k2 = 1, q = 0.05, ud = 0.1, ur = 0.02, f0 = 1),
    i0_prior               = c(log(8), 1),
    state_0                = c(E1_frac = 0.4, E2_frac = 0.1, I_frac = 0.5, Q_num = 0, R_num = 0,
                               E1d_frac = 0.4, E2d_frac = 0.1, Id_frac = 0.5, Qd_num = 0, Rd_num = 0),
    save_state_predictions = FALSE,
    delay_scale            = 9.85,
    delay_shape            = 1.73,
    ode_control            = c(1e-07, 1e-06, 1e+06),

    #forecast requirements
    project_forecast_days   = 100,
    f_fixed_start           = 53,
    f_fixed                 = c(rep(0.7, 60), rep(0.2, 30)),
    forecast_iter           = 1:25,
    return_states           = FALSE,
    imported_cases          = 0,
    imported_window          = 1
  )

 # model_input$f_seg  <- c(0, rep(1, length(model_input$daily_cases) + model_input$forecast_days - 1))

  return(model_input)
}


#Gets a hierarchical named list and flattens it; updating names accordingly
flatten_list<-function(lst,prefix="")
{
  if(is.null(lst)) return(lst)
  out<-list()
  if(length(lst)==0)
  {
    out[prefix]<-NULL
    return(out)
  }

  for(i in 1:length(lst))
  {
    nm<-names(lst[i])

    message(nm)

    if(prefix!="")  nm<-paste(prefix,nm,sep=".")

    if(is.list(lst[[i]]))
      out<-c(out,flatten_list(lst[[i]],nm))
    else
    {
      out[nm]<-lst[i]
    }
  }
  return(out)
}



#Gets a hierarchical named list and flattens it; updating names accordingly
unflatten_list<-function(lst)
{
  if(is.null(lst)) return(lst)
  out<-list()

  nms<-names(lst)

  for(nm in nms)
  {
    path<-paste(strsplit(nm,'.',fixed=T)[[1]],sep="$")
    eval(parse(text=paste("out$",paste(path,collapse="$"),"<-lst[[nm]]",sep="")))
  }

  return(out)
}
