library(extrafont)
library(ggthemes)
library(MASS)
library(plotly)
library(shiny)
library(tidyverse)

ui <- fluidPage(
	plotlyOutput("heatmap"),

	h1(textOutput("header")),
	
	fluidRow(
		column(6, plotlyOutput("time_series")),
		column(6, plotlyOutput("scatter"))
	)
)

server <- function(input, output) {
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
	
	fit <- read_csv("lm.csv")
	metrics <- unique(fit$target)
	players <- rev(unique(fit$player))

	output$heatmap <- renderPlotly({
		fit %>% 
			mutate(player = factor(player, levels = players)) %>% 
			plot_ly(
				x = ~player, y = ~target, z = ~adj.r.squared,
				source = "hm",
				type = "heatmap"
			)
	})
	
	output$header <- renderPrint({
		s <- event_data("plotly_click", source = "hm")
		if (length(s)) {
			sprintf("%s | %s", s$x, s$y)
		} else {
			"Click on the heatmap"
		}
	})
	
	my_fit <- function(player, target) df %>% 
		filter(PlayerName == player) %>% 
		select(one_of("GameDate", target)) %>% 
		filter(complete.cases(.)) %>% 
		cbind(
			model = lm(
				formula = as.formula(paste(target, "~", "0 + .")),
				data = df %>% 
					filter(PlayerName == player) %>% 
					select(one_of(target, names(df)[x]))
			) %>% 
				stepAIC(trace = 0) %>% 
				.[["terms"]] %>% 
				lm(
					formula = .,
					data = df %>% 
						filter(PlayerName == player) %>% 
						select(one_of(target, names(df)[x]))
				) %>% 
				.$fitted.values
		) %>% 
		rename_(actual = target)
	
	output$time_series <- renderPlotly({
		s <- event_data("plotly_click", source = "hm")
    	if (length(s)) {
			my_fit(s$x, s$y) %>% 
				gather(key, value, -GameDate) %>% 
				plot_ly(
					x = ~GameDate, y = ~value, color = ~key,
					type = "scatter"
				)
    	} else {
    		plotly_empty()
    	}
	})
	
	output$scatter <- renderPlotly({
		s <- event_data("plotly_click", source = "hm")
    	if (length(s)) {
			my_fit(s$x, s$y) %>% 
				plot_ly(
					x = ~model, y = ~actual,
					type = "scatter"
				)
    	} else {
    		plotly_empty()
    	}
	})
}

shinyApp(ui, server)
