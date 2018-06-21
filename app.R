library(extrafont)
#library(ggthemes)
library(MASS)
library(plotly)
library(shiny)
library(tidyverse)

ui <- fillPage(
	h2("Adjusted R squared"),
	fillRow(
		plotlyOutput("heatmap", height = "100%"),
		height = "50%"
	),

	h2(textOutput("header")),
	textOutput("formula"),
	fillRow(
		plotlyOutput("time_series", width = "95%", height = "100%"),
		plotlyOutput("scatter", width = "95%", height = "100%"),
		height = "35%"
	),
	
	padding = c(0, 0, 0, 20)
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
	
	my_plotly <- function(...) plot_ly(...) %>% 
		config(displayModeBar = FALSE) %>% 
		layout(
			font = list(family = "Fira Code"),
			xaxis = list(fixedrange = TRUE),
			yaxis = list(fixedrange = TRUE)
		)

	output$heatmap <- renderPlotly({
		fit %>% 
			mutate(player = factor(player, levels = players)) %>% 
			my_plotly(
				x = ~player, y = ~target, z = ~adj.r.squared,
				text = ~sprintf("%s, %s: %.2f", player, target, adj.r.squared),
				colors = colorRamp(c("#063672", "#ec0c1c")),
				hoverinfo = "text",
				source = "hm",
				type = "heatmap"
			) %>% 
				hide_colorbar() %>% 
				layout(
					margin = list(l = 75, r = 50, b = 125),
					xaxis = list(title = ""),
					yaxis = list(title = "")
				)
	})
	
	output$header <- renderPrint({
		s <- event_data("plotly_click", source = "hm")
		if (length(s)) {
			cat(s$x, "|", s$y)
		} else {
			cat("click on the heatmap")
		}
	})
	
	output$formula <- renderPrint({
		s <- event_data("plotly_click", source = "hm")
		if (length(s)) {
			fit %>% 
				filter(player == s$x, target == s$y) %>% 
				pull(model) %>% 
				cat()
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
    			arrange(GameDate) %>% 
				gather(key, value, -GameDate) %>% 
				my_plotly(
					x = ~GameDate, y = ~value, color = ~key,
					colors = c("#9b814e", "#d80919"),
					line = list(dash = "dash", width = .5),
					mode = "lines+markers",
					type = "scatter"
				) %>% 
    				layout(
    					hovermode = "x",
    					legend = list(
    						orientation = "h",
    						x = .5, y = 1
    					),
						xaxis = list(title = ""),
						yaxis = list(hoverformat = ".2f", range = c(-5, 5), title = "")
    				)
    	} else {
    		plotly_empty() %>% config(displayModeBar = FALSE)
    	}
	})
	
	output$scatter <- renderPlotly({
		s <- event_data("plotly_click", source = "hm")
    	if (length(s)) {
			my_fit(s$x, s$y) %>% 
				my_plotly(
					x = ~model, y = ~actual, color = I("#063672"),
					hoverinfo = "x+y",
					type = "scatter"
				) %>% 
    				add_trace(
    					x = c(-5, 5), y = c(-5, 5),
    					line = list(color = "grey", dash = "dash", width = .5),
						mode = "lines",
						type = "scatter"
    				) %>% 
    				layout(
    					showlegend = FALSE,
    					xaxis = list(hoverformat = ".2f", range = c(-5, 5)),
						yaxis = list(hoverformat = ".2f", range = c(-5, 5))
    				)
    	} else {
    		plotly_empty() %>% config(displayModeBar = FALSE)
    	}
	})
}

shinyApp(ui, server)
