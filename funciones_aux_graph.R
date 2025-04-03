## Funciones para crear gráficos ####

# Función para graficar modelo conjugado binomial
fy_bin <- function(nobs=0,datos=NULL,n_ensayos,y=0,a,b,escenario){
  
  # Parámetros de entrada ---------
  # n_ensayos: número de ensayos
  # a: primer parámetro de forma de la distribución a priori
  # b: segundo parámetro de forma de la distribución a priori
  # y: número de éxitos en n ensayos bernoulli - valor necesario para calcular theta 
  # nobs: cantidad de valores para simular
  # datos: columna de datos cargada a la aplicación
  # escenario: valor que toma el radioButtons "esce_datos". (Simular o ingresar datos)
  
  if(escenario == "ing_datos"){
    # VALIDACIÓN: 
    validate(need(datos, "Por favor ingrese un conjunto de datos"))
    # Datos distribución de verosimilitud ----
    y_val <- datos 
    theta_est <- sum(y_val) / (length(y_val)*n_ensayos) #Estimación muestral
    
    # Datos distribución a priori y posterior ----
    theta_val_apriori <- rbeta(length(datos), shape1=a, shape2=b)
    a_pos <- mean(y_val) + a
    b_pos <- n_ensayos - mean(y_val) + b
    theta_val_posterior <- rbeta(length(datos), shape1=a_pos, shape2=b_pos)
    
  }else if(escenario == "sim_datos"){
    # Datos distribución de verosimilitud ----
    theta_est <- y/n_ensayos #Estimación muestral
    y_val <- rbinom(nobs, n_ensayos, theta_est)
    
    # Datos distribución a priori y posterior----
    theta_val_apriori <- rbeta(nobs, shape1=a, shape2=b)
    
    a_pos <- mean(y_val) + a
    b_pos <- n_ensayos - mean(y_val) + b
    theta_val_posterior <- rbeta(nobs, shape1=a_pos, shape2=b_pos)
  }
  
  # Conjunto de valores para graficar distribución de verosimilitud ---
  fy1 <- dbinom(y_val,n_ensayos,theta_est) #Densidades para la distribución de verosilimitud
  df1 <- data.frame(y_val, fy1 = fy1) #Base de datos para distribución de verosimilitud: valores en y - densidades
  
  # Calculo del número de éxitos máximo y minimo que se graficará
  limInf_vero <- floor((n_ensayos*theta_est) - 3*sqrt(n_ensayos*theta_est*(1-theta_est)))
  limSupe_vero <- ceiling((n_ensayos*theta_est) + 3*sqrt(n_ensayos*theta_est*(1-theta_est)))
  
  # VALIDACIÓN: garantizar que el límite inferior no sea menor a cero y el límite superior no supere 
  # el máximo de éxitos simulado o ingresado
  if(limInf_vero < 0){
    limInf_vero <- min(y_val)
  }else if(limSupe_vero > n_ensayos){
    limSupe_vero <- max(y_val)
  }
  
  # Gráfico de la distribución de verosimilitud ----
  g_vero <- ggplot(data=df1,
                   aes(x=y_val, y=fy1, xend = y_val, yend = 0)) +
    geom_point(linewidth = 0.8, col = 4) +
    geom_segment(col = 4)+
    labs(title ="Distribución de verosimilitud", y= "Densidad", x= "y") +
    scale_x_continuous(limits = c(limInf_vero,limSupe_vero)) +
    scale_y_continuous(limits = c(0,(max(fy1)+0.02))) +
    theme_bw()
  
  
  # Conjunto de valores para graficar distribución de a priori ---
  fy2 <- dbeta(x = theta_val_apriori, shape1 = a, shape2 = b) #Densidades para la distribución a priori
  df2 <- data.frame(theta_val_apriori,fy2) #Base de datos para distribucón a priori : probabilidades - densidades
  
  # Gráfico de la distribución a priori ----
  g_1 <- ggplot(data=df2,
                aes(x=theta_val_apriori, y=fy2)) +
    geom_line(linewidth=0.8,col = 2) +
    labs(title = "Distribución a priori",y= "Densidad", x= "\u03b8") +
    scale_x_continuous(limits = c(0,(max(theta_val_apriori))))+
    scale_y_continuous(
      limits = c(0,(max(fy2)+0.05))) +
    theme_bw()
  
  
  # Conjunto de valores para graficar distribución posterior ---
  fy3 <- dbeta(x = theta_val_posterior, shape1 = a_pos, shape2 = b_pos)
  df3 <- data.frame(theta_val_posterior,fy3)
  
  
  media_pos <- a_pos / (a_pos+b_pos) #media de la distribución posterior
  varianza_pos <- (a_pos*b_pos)/((a_pos+b_pos)^2 * (a_pos+b_pos+1)) # Varianza de la distribución posterior
  
  # Calculo del valor de theta máximo y minimo que se graficará
  limInf_pos <- media_pos-3*sqrt(varianza_pos)
  limSupe_pos <- media_pos+3*sqrt(varianza_pos)
  
  # VALIDACIÓN: garantizar que el límite inferior no sea menor a cero y el límite superior no supere 
  # el valor máximo de theta simulado o ingresado
  if(limInf_pos < 0){
    limInf_pos <- min(theta_val_posterior)
  }else if(limSupe_pos > 1){
    limSupe_pos <- max(theta_val_posterior)
  }
  
  # Gráfico de la distribución posterior ---
  g_2 <- ggplot(data = df3,
                aes(x=theta_val_posterior, y=fy3)) +
    geom_line(linewidth=0.8, col = 3) +
    labs(title = "Distribución posterior", y= "Densidad", x= "\u03b8") +
    scale_x_continuous(limits = c(limInf_pos,limSupe_pos)) +
    scale_y_continuous(
      limits = c(0,max(fy3))) +
    theme_bw()
  
  return(list(g_vero,g_1,g_2,c(n_ensayos,round(theta_est,3),round(a,3),round(b,3),round(a_pos,3),round(b_pos,3))))
}

# Función para graficar modelo conjugado poisson
fx_pois <- function(nobs=0,datos=NULL,theta_m=0,alpha_0,beta_0,escenario){
  
  # Parámetros de entrada ----
  # nobs: número de valores a simular
  # datos: columna de datos cargada a la aplicación
  # theta_m: estimación del parámetro de interés (tasa de ocurrencia promedio)
  # alpha_0: parámetro de escala de la distribución a priori
  # beta_0: parámetro de escala inversa de la distribución a priori
  # escenario: valor que toma el radioButtons "esce_datos". (Simular o ingresar datos)
  
  if(escenario == "sim_datos"){
    like_pois_original <- rpois(n = nobs, lambda = theta_m)
    datos_like <- data.frame(x = like_pois_original, y = dpois(like_pois_original,theta_m))
    
    # Calculo de parámetros y conjunto de valores para graficar distribución posterior ---
    alpha_pos <- sum(like_pois_original) + alpha_0
    beta_pos <- length(like_pois_original) + beta_0
    post_pois <- rgamma(n = nobs, shape = alpha_pos, scale = 1/beta_pos)
    prior_pois <- rgamma(n = nobs, shape = alpha_0, scale = 1/beta_0)
    
  }else if(escenario == "ing_datos"){
    # VALIDACIÓN: 
    validate(need(datos, "Por favor ingrese un conjunto de datos"))
    theta_m <- mean(datos)
    datos_like <- data.frame(x = datos, y = dpois(datos,lambda = theta_m))
    
    # Calculo de parámetros y conjunto de valores para graficar distribución posterior ---
    alpha_pos <- sum(datos) + alpha_0 
    beta_pos <- length(datos) + beta_0
    post_pois <- rgamma(n = length(datos), shape = alpha_pos, scale = 1/beta_pos)
    prior_pois <- rgamma(n = length(datos), shape = alpha_0, scale = 1/beta_0)
  }
  
  # Gráficos de las distribuciones ----
  
  # Gráfico para distribución de verosimilitud
  likelihood <- ggplot(data = datos_like,
                       aes(x = x, y = y, xend = x, yend = 0)) +
    geom_point(color = 4) +
    geom_segment(color = 4) +
    labs(title = "Distribución de verosimilitud", y= "Densidad", x= "y") +
    theme_bw()
  
  # Gráfico para distribución a priori
  data_prior <- data.frame(x = prior_pois, y = dgamma(prior_pois,shape = alpha_0, scale=1/beta_0))
  
  prior <- ggplot(data = data_prior,aes(x = x, y = y)) +
    geom_line(color = 2, linewidth = 0.8) +  
    labs(title = "Distribución a priori", y = "Densidad", x="\u03b8") +
    theme_bw()
  
  # Gráfico para distribución psoterior
  data_pos <- data.frame(x = post_pois, y = dgamma(post_pois,shape = alpha_pos, scale = 1/beta_pos))
  
  
  posterior <- ggplot(data = data_pos,aes(x = x, y = y)) +
    geom_line(color = 3, linewidth = 0.8) + 
    labs(title = "Distribución posterior", y = "Densidad", x="\u03b8") +
    theme_bw()
  return(list(likelihood, prior,posterior, c(round(theta_m,3),round(alpha_0,3),round(beta_0,3),round(alpha_pos,3),round(beta_pos,3))))
  
}

## Función para crear cuadros informativos (infoBox) ####
summaryBox <- function(title, value, width = 4, icon = "fas fa-chart-bar", style = "info", border = "left") {
  
  infoBox  <-  tags$div(
    class = paste0("col-md-", width),
    tags$div(
      class = paste0("card border-", border, "-", style, " shadow h-100 py-2"),
      tags$div(
        class = "card-body",
        tags$div(
          class = "row no-gutters align-items-center",
          tags$div(
            class = "col mr-2",
            tags$div(
              class = paste0("text-xs font-weight-bold text-", style, " text-uppercase mb-1"),
              toupper(title)
            ),
            tags$div(
              class = "h5 mb-0 font-weight-bold text-gray-800",
              value
            )
          ),
          tags$div(
            class = "col-auto",
            tags$i(class = paste(icon, "fa-2x text-gray-300"))
          )
        )
      )
    )
  )
  
  htmltools::browsable(infoBox)
  
}

## Función para crear campos de entrada con tooltip personalizado ####
inputWithTooltip <- function(inputId, label, value, min, tooltip_message) {
  tag <- tags$div(
    numericInput(
      inputId,
      label,
      value = value,
      min = min
    ),
    tags$script(
      HTML(paste0('
        $(document).ready(function() {
          $("#', inputId, '").tooltip({
            title: "', tooltip_message, '",
            placement: "right",
            trigger: "hover",
            container: "body",
            html: true,
            template: \'<div class="tooltip" role="tooltip"><div class="tooltip-arrow"></div><div class="tooltip-inner"></div></div>\',
            arrowSelector: ".tooltip-arrow"
          });
        });
      '))
    ),
    tags$style(HTML(paste0('
      .tooltip-inner {
        color: #ffffff;
        background-color: #3A959D;
        border: 1px solid #3A959D;
        padding: 5px;
        border-radius: 3px;
        font-weight: bold;
      }
      .tooltip-arrow::before {
        border-right-color:  #3A959D;  
      }
      .tooltip-arrow {
        border-width: 0 8px 8px 8px;
      }
    ')))
  )
  return(tag)
}
