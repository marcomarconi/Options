
scrape_dir=$1
instruments_file=$2

today=`date +"%Y%m%d"`
today_dash=`date +"%Y-%m-%d"`

### Download specific instruments from yahoo
start_date=1009843200
end_date=`date +"%s"`
for f in `cat $instruments_file`; do
  echo $f
  scrape_tmp=_scrape.$f.html
  curl -sS "https://query1.finance.yahoo.com/v7/finance/download/$f?period1="$start_date"&period2="$end_date"&interval=1d&events=history&includeAdjustedClose=true" > $scrape_tmp
  sed -i 's/null/NA/g' $scrape_tmp
  mv $scrape_tmp $scrape_dir/$f.csv
done 

