library(extrafont)
library(ggthemes)
library(MASS)
library(shiny)
library(tidyverse)

ui <- fluidPage(
	titlePanel("From Match to Screening"),
	
	plotOutput("heatmap", height = "800px"),

	fluidRow(
		column(
			3,
			selectInput(
				"player",
				label = NULL,
				choices = players
			),
			selectInput(
				"metric",
				label = NULL,
				choices = metrics
			)
		),
		column(
			9,
			plotOutput("time_series"),
			plotOutput("scatter")
		)
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

	output$heatmap <- renderPlot({
		fit %>% 
			mutate(player = factor(player, levels = players)) %>% 
			ggplot(aes(player, target)) +
				coord_equal() +
				geom_tile(aes(fill = adj.r.squared)) +
				geom_text(aes(label = sprintf("%.2f", adj.r.squared))) +
				labs(
					title = "Adjusted R squared",
					x = "<- fewer  |  more games played ->",
					y = NULL
				) +
				scale_fill_gradient2() +
				theme_tufte(ticks = FALSE) +
				theme(
					axis.text.x = element_text(angle = 90, hjust = 1),
					legend.position = "none",
					text = element_text(family = "Fira Code")
				)
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
	
	output$time_series <- renderPlot({
		my_fit(input$player, input$metric) %>% 
			gather(key, value, -GameDate) %>% 
			ggplot(aes(GameDate, value, colour = key)) +
				geom_line(alpha = .1) +
				geom_point() +
				labs(
					x = NULL,
					y = NULL
				) +
				scale_colour_manual(values = c("red", "black")) +
				scale_y_continuous(limits = c(-3, 3)) +
				theme_tufte(ticks = FALSE) +
				theme(
					legend.position = "top",
					legend.title = element_blank(),
					text = element_text(family = "Fira Code")
				)
	})
	
	output$scatter <- renderPlot({
		my_fit(input$player, input$target) %>% 
			ggplot(aes(model, actual)) +
				coord_fixed() +
				geom_abline(slope = 1) +
				geom_point() +
				scale_x_continuous(limits = c(-3, 3)) +
				scale_y_continuous(limits = c(-3, 3)) +
				theme_tufte(ticks = FALSE) +
				theme(
					legend.position = "top",
					legend.title = element_blank(),
					text = element_text(family = "Fira Code")
				)
	})
}

shinyApp(ui, server)
