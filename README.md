# pollen-forecast-notifications
The following repository will run Github actions once a day to check the pollen forecast for tomorrow in the relevant area, such as Oslo. The forecast is based on three different forecasts, including NAAF (specific to Norway and requires an API key), Open-Meteo, and Google Pollen. The notifications are sent to Slack, only if the forecast for the next day is high or extreme. That way, it is possible to adjust meds or plans for the next day based on the forecasted high pollen levels.

To get the necessary API information to get the code to run, you need to add the following Github secrets:
SLACK_WEBHOOK_URL
NAAF_API_KEY
NAAF_API_URL
GOOGLE_POLLEN_KEY (optional)

The Google Pollen API is set as optional in the code, so if that fails or is missing, the script will still run as normal.

Open-Meteo has an API that is open. NAAF requires you to pay for their API. You do this through becoming a member of NAAF and then you should get the access information to the API through the member sites. This is currently at 500 kr per year (as of 2026).

The code used in the repository is largely created by Claude, Opus 5.5, at medium effort.
