library(MASS)
library(tidyverse)

df <- "data.csv" %>% 
	read_csv() %>% 
	mutate(
		Adductor = (Adductor0 + Adductor45 + Adductor90) / 3,
		KTW = (KTWL + KTWR) / 2,
		Hams = (LHams30 + LHams90 + RHams30 + RHams90) / 4
	)

# independent & target variables
x <- 2:11
y <- 22:27

my_lm <- function(data) {
	y <- names(data)[1]

	if (sd(data[[y]], na.rm = TRUE) > 0) {
		fit <- lm(as.formula(paste(y, "~", "0 + .")), data) %>% 
			stepAIC(trace = 0) %>% 
			.[["terms"]] %>% 
			lm(formula = ., data)
		
		if (length(coefficients(fit)) > 0) tibble(
			target = y,
			model = as.formula(
				paste(
					"y",
					"~",
					paste(
						sprintf(
							" %+.2f*%s ",
							coefficients(fit),
							names(coefficients(fit))
						),
						collapse = ""
					)
				)
			) %>% 
				as.character() %>% 
				.[3],
			adj.r.squared = summary(fit)$adj.r.squared
		)
	}
}

my_lm_all <- function(data) lapply(
	y,
	function(i) data %>% 
		select(i, x) %>% 
		my_lm()
) %>% 
	do.call(rbind, .)

df %>% 
	count(PlayerName) %>% 
	arrange(n) %>% 
	filter(n > length(x) + 1) %>% 
	pull(PlayerName) %>% 
	lapply(
		function(pn) df %>% 
			filter(PlayerName == pn) %>% 
			my_lm_all() %>% 
			cbind(player = pn, .)
	) %>% 
	do.call(rbind, .) %>% 
	write_csv("lm.csv")
