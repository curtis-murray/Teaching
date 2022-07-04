library(tidyverse)

#  -----------------------------Real data (HIDDEN) -----------------------------
height <- runif(100,0,100)

a_true = 53.612
b_true = 0

noise = rnorm(n = length(height), 0,80)

weight <- a_true*height + b_true + noise

tibble(weight, height) %>% 
write_csv(file = "hippos.csv")

# ----------------------------- Real data plot -------------------------

df <- tibble(height,weight)

df %>% 
	ggplot() +
	geom_point(aes(x = height, y = weight)) + 
	theme_bw() + 
	labs(x = "Height (cm)", y = "Weight (kg)", 
			 title = "Alien hippo height and weight")

a = a_calc_method

weight_guess = a*height

df %>% 
	ggplot() +
	geom_point(aes(x = height, y = weight)) + 
	theme_bw() + 
	labs(x = "Height (cm)", y = "Weight (kg)", 
			 title = "Alien hippo height and weight") +
	geom_line(aes(x = height, y = weight_guess), colour = "blue") + 
	geom_point(aes(x = height, y = weight_guess), colour = "blue") + 
	geom_segment(aes(x = height, xend = height, y = weight, 
									 yend = weight_guess), color = "red", 
							 linetype = "dotted")

errors = weight_guess - weight

df %>% 
	mutate(weight_guess = weight_guess, errors = errors) %>% 
	mutate(squared_errors = errors^2) %>% 
	summarise(mse = mean(squared_errors)) %>% 
	pull(mse)

find_mse <- function(a){
	weight_guess = a*height
	error = weight_guess - weight
	mse = mean(error^2)
	return(mse)
}

find_mse(25)


a_seq = seq(20,30,0.001)

length(a_seq)

df_mse <- tibble(a = a_seq) %>% 
	mutate(mse = sapply(a, find_mse))



df_mse %>% 
	ggplot() + 
	geom_point(aes(x = a, y = mse)) + 
	lims(y = c(0,NA))


best_guess <- df_mse %>% 
	filter(mse == min(mse)) 
best_guess %>% 
	pull(a)
best_guess

a_calc_method = sum(weight*height)/sum(height^2)

# --------

res <- lm(formula = weight~height, data = df)

res$coefficients
a_calc_method



a*x^2/(b*e^(x)+
















