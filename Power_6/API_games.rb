require 'httparty'
require 'csv'

url = "https://api.collegefootballdata.com/games"
year = "2018"
## use following query to get bowl games/postseason games
# query = {"year" => year, "seasonType" => "postseason"}
query = {"year" => year}

page_data = HTTParty.get(url, :query => query)
responses = page_data.parsed_response

keys = ["id", "season", "week", "season_type", "start_date", "neutral_site",
  "conference_game", "attendance", "venue_id", "venue", "home_team",
  "home_conference", "home_points", "home_q1", "home_q2", "home_q3", "home_q4",
  "away_team", "away_conference", "away_points", "away_q1", "away_q2", "away_q3",
  "away_q4"]

games_filename = "#{year}_games.csv"
CSV.open(games_filename, "ab") do |csv|
  csv << keys
  responses.each do |response|
    csv << [response["id"], response["season"], response["week"],
            response["season_type"], response["start_date"],
            response["neutral_site"], response["conference_game"],
            response["attendance"], response["venue_id"], response["venue"],
            response["home_team"], response["home_conference"],
            response["home_points"], response["home_line_scores"][0],
            response["home_line_scores"][1], response["home_line_scores"][2],
            response["home_line_scores"][3], response["away_team"],
            response["away_conference"], response["away_points"],
            response["away_line_scores"][0], response["away_line_scores"][1],
            response["away_line_scores"][2], response["away_line_scores"][3]]
  end
end
