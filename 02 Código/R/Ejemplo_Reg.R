

library(plotly)

# Ejemplo de Precios y Cantidades

P = c( 3, 3, 7, 6, 10, 15, 16, 13, 9, 15, 9 )

Q = c( 18, 16, 17, 12, 15, 15, 4, 13, 11, 8, 8)
  

# Gráfica
fig <- plot_ly(x = P, y = Q, type = 'scatter', mode = 'markers',
               marker = list( size = 10, 
                              color = 'rgba(255, 182, 193, .9)', 
                              line = list(color = 'rgba(152, 0, 0, .8)', width = 2)))

fig <- fig %>% layout(title = 'Relación Precios y Cantidades',
                      yaxis = list(zeroline = FALSE, title = 'Q'),
                      xaxis = list(zeroline = FALSE, title = 'P') )

#fig <- fig %>% layout(title = 'Relación de precios y cantidades')

fig

# Formamos Matrices

ones = rep(1, times = 11)

X <- matrix( c( ones, P ), ncol = 2)

X

Y <- matrix( Q, ncol = 1 )

Y

# Betas



Beta <- ( solve(t(X) %*% X) ) %*% ( t(X) %*% Y )

Beta

# Recta de regresión:
# Q = 18.10 - 0.58 * P

Y_e <- X %*% Beta

Y_e

Q_e <- c( Y_e )

Q_e

# Gráfica
fig <- plot_ly(x = P, y = Q, name = 'Datos', type = 'scatter', mode = 'markers',
               marker = list( size = 10, 
                              color = 'rgba(255, 182, 193, .9)', 
                              line = list(color = 'rgba(152, 0, 0, .8)', width = 2)))

fig <- fig %>% add_trace( y = Q_e, name = 'Recta de regresión', type = 'scatter', mode = 'lines+markers',
                          marker = list( size = 5, color = 'darkblue', 
                                         line = list(color = 'darkblue', width = 1)),
                          line = list( color = 'darkblue') )

fig <- fig %>% layout(title = 'Styled Scatter',
                      yaxis = list(zeroline = FALSE, title = 'Q'),
                      xaxis = list(zeroline = FALSE, title = 'P') )

fig <- fig %>% layout(title = 'Relación de precios y cantidades')

fig



e = Y - Y_e

e

e = Y - X %*% Beta

e

t(e) %*% e

sigma2 = (t(e) %*% e) / (11 - 2)

sigma2


sqrt(sigma2 * solve(t(X) %*% X)[2,2])


