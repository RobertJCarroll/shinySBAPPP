##Download data from SBA
#Data current as of 2020-08-08
# Information can be found here: https://home.treasury.gov/policy-issues/cares-act/assistance-for-small-businesses/sba-paycheck-protection-program-loan-level-data
# Note that the box url is here: https://sba.app.box.com/s/ahn2exwfebgqruk714v3hnf75qdap3du
# Download links do not last, unfortunately.
data_file="all_data.zip"
if(!file.exists(data_file)) {
  download.file(url="",
                destfile=data_file)
}

#Unzip the data
unzip(zipfile = data_file, files = "All Data 0808/150k plus 0808/PPP Data 150k plus 080820.csv", exdir = "all_data")

##NAICS Codes
#Snag the NAICS code list from the census bureau
if(!file.exists("all_data/naics.xlsx")) {
  download.file(url="https://www.census.gov/eos/www/naics/2017NAICS/6-digit_2017_Codes.xlsx",
                destfile = "all_data/naics.xlsx", mode = "wb")
}
