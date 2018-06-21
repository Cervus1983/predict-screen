library(lubridate)
library(tidyverse)

# raw data ----
MatchData <- "match.csv" %>% 
	read_csv() %>% 
	transmute(
		PlayerID,
		PlayerData,
		GameDate,
		AgeInDays = as.integer(GameDate - DateBirthday),
		MinutesPlayed,
		Distance,
		RunningDistance,
		HiSpeedRunDistance,
		SprintingDistance
	)

ScreeningData <- "screen.csv" %>% 
	read_csv(guess_max = 1e+4) %>% 
	select(
		ScreeningDate,
		PlayerName,
		contains("Adductor"),
		contains("KTW"),
		contains("Hams"),
		Readiness,
		Sleep,
		SAR,
		PlayerID = PlayerId
	) %>% 
	# fix mismatching IDs
	mutate(
		PlayerID = recode(
			PlayerID,
			`12678` = 11414L,
			`20552` = 8467L,
			.default = PlayerID
		)
	)

# check for mismatching PlayerIDs ----
if (FALSE) full_join(
	select(MatchData, PlayerID, PlayerData),
	select(ScreeningData, PlayerID, PlayerName) 
) %>% 
	unique() %>% 
	filter(!complete.cases(.)) %>% 
	View()

# generate features ----
z_score <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE) # https://en.wikipedia.org/wiki/Standard_score

MatchData %>% 
	# calculate & scale month of the season
	mutate(MonthOfSeason = if_else(month(GameDate) < 8, month(GameDate) + 5, month(GameDate) - 7)) %>% 
	mutate(MonthOfSeason = (MonthOfSeason - 5.5) / 3.02765) %>% 
	# previous 7 days
	inner_join(
		MatchData %>% 
			select(PlayerID, GameDate) %>% 
			inner_join(MatchData, by = "PlayerID") %>% 
			filter(as.integer(GameDate.x - GameDate.y) %in% 0:7) %>% 
			group_by(PlayerID, GameDate = GameDate.x) %>% 
			summarise(
				GamesLast7Days = sum(GameDate > GameDate.y),
				MinutesPlayedLast7Days = sum(if_else(GameDate > GameDate.y, MinutesPlayed, 0L)),
				RestDays = min(if_else(GameDate > GameDate.y, as.integer(GameDate.x - GameDate.y), 7L))
			) %>% 
			ungroup()
	) %>% 
	# add screening data
	mutate(ScreeningDate = GameDate + 2) %>% 
	inner_join(ScreeningData) %>% 
	# only (nearly) completed games
	#filter(MinutesPlayed > 80) %>% 
	#select(-MinutesPlayed) %>% 
	# global metrics
	mutate_at(
		vars(
			AgeInDays,
			MinutesPlayed,
			GamesLast7Days,
			MinutesPlayedLast7Days,
			RestDays
		),
		z_score
	) %>% 
	# individual metrics
	group_by(PlayerID) %>% 
	mutate_at(
		vars(
			contains("Distance"),
			contains("Adductor"),
			contains("KTW"),
			contains("Hams"),
			Readiness,
			Sleep,
			SAR
		),
		z_score
	) %>% 
	ungroup() %>% 
	# replace NaN (division by 0) with 0
	mutate_if(
		is.numeric,
		function(x) if_else(is.nan(x), 0, as.numeric(x))
	) %>% 
	# save
	select(-PlayerID, -PlayerData, -ScreeningDate) %>% 
	write_csv("data.csv")
