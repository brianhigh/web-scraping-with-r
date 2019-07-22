# Filename: get_pollen_history.sh
# Copyright (c) University of Washington
# License: MIT https://opensource.org/licenses/MIT (See LICENSE file.)
# Repository: https://github.com/brianhigh/web-scraping-with-r
#
# Get daily historical pollen index for a specific zip code and number of days.
# Requires: bash, curl (curl.haxx.se), jq (stedolan.github.io/jq/)
#
# Note: Maximum "days" provided by data source appears to be 365.

# Configure
BASE_URL='https://www.pollen.com'
FORECAST_PATH='forecast/historic/pollen'
ZIP=98101
DAYS=30
DATA_DIR='data'
FILE_NAME='pollen.csv'
OUT_PATH="${DATA_DIR}/${FILE_NAME}"

# Get data, clean-up, and save as a CSV file
mkdir -p "$DATA_DIR"
echo '"date","pollen_index","zip"' > "$OUT_PATH"
curl "${BASE_URL}/api/${FORECAST_PATH}/${ZIP}/${DAYS}" \
  -H 'User-Agent: Mozilla/5.0' \
  -H "Referer: ${BASE_URL}/${FORECAST_PATH}/${ZIP}" | \
  jq -r \
  '.Location.periods | map([.Period, .Index|tostring] | join(",")) | join("\n")' | \
  sed -e 's/T[^,]*//g' -e "s/$/,${ZIP}/" >> "$OUT_PATH"
