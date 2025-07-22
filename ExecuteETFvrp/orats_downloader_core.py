import asyncio
import os
import sys
import logging
from typing import List
import aiohttp
import pandas as pd
import gzip
import shutil
from aiohttp import ClientSession
from datetime import datetime, timedelta


# update file paths as necessary
LOG_FILE = "downloader.log"
DATE_FORMAT = "%Y%m%d"
TOKEN = "6e9bc89e-3c30-46b3-a28e-3182d46a5b64"
BASE_URL = 	"https://api.orats.io/datav2/hist/cores.csv?token="+TOKEN+"&tradeDate="
BASE_NAME = "core"
CORE_DIR = "core"				
LOCAL_PATH = os.path.dirname(os.path.abspath(__file__))

# configure the logging module
logging.basicConfig(
    level=logging.INFO,  # log messages with this level or higher
    format="%(asctime)s--%(levelname)s--%(message)s",  # double hyphen makes it easier to parse
    datefmt="%Y-%m-%d %H:%M:%S",
    handlers=[
        logging.FileHandler(LOG_FILE),  # log to file
        logging.StreamHandler(sys.stdout),  # log to stdout
    ],
)


async def get_data(date: str, session: ClientSession) -> None:
    """
    Asynchronously get data from ORATS core API and save it to local file system
    """
    logging.info("Doing %s", date)
    async with session.get(f"{BASE_URL}{date}") as response:
        try:
            assert response.status == 200
        except AssertionError:
            logging.error("%s status code for %s, skipping", response.status, date)
            return None

        await asyncio.sleep(1)  # should keep us under the rate limit of 1000/min

        csv_data = await response.text()
        # replace \r\n (carriage return and newline) with \n
        csv_data = csv_data.replace("\r\n", "\n")

        with open(f"{LOCAL_PATH}/orats_{BASE_NAME}_{date}.csv", "w", encoding="utf-8") as f:
            f.write(csv_data)

        logging.info("%s downloaded successfully", date)
        return None


async def main(date_list: List[str]):
    """
    Creates an aiohttp ClientSession and start the download process
    """

    # chunk the calls into batches (API seems to disconnect if you do too many requests in a single session)
    chunk_size = 10
    chunks = [
        date_list[i : i + chunk_size] for i in range(0, len(date_list), chunk_size)
    ]
    for chunk in chunks:
        async with aiohttp.ClientSession() as session:
            tasks = []
            for date in chunk:  # dates:
                tasks.append(get_data(date, session))
            # Wait for all tasks to complete
            await asyncio.gather(*tasks)
            # Wait some more time between chunks
            await asyncio.sleep(1)


def generate_dates(start_date: str, end_date: str) -> list:
    """
    Get a list of business days between start_date and end_date inclusive
    """
    # convert strings to datetime
    start_date = pd.to_datetime(start_date, format="%Y%m%d")
    end_date = pd.to_datetime(end_date, format="%Y%m%d")

    # list of dates excluding weekends (holidays will still return a 404)
    dates = pd.bdate_range(start=start_date, end=end_date)

    # convert back to strings
    dates = [date.strftime("%Y%m%d") for date in dates]

    return dates

def gzip_compress_files(file_list):
    for file_path in file_list:
        if not os.path.isfile(file_path):
            print(f"Skipping {file_path}, not a file.")
            continue

        compressed_file_path = file_path + '.gz'
        
        with open(file_path, 'rb') as f_in:
            with gzip.open(compressed_file_path, 'wb') as f_out:
                shutil.copyfileobj(f_in, f_out)
        
        print(f"Compressed {file_path} to {compressed_file_path}")

def move_files_to_folder(file_list, destination_folder):
    # Create destination folder if it doesn't exist
    os.makedirs(destination_folder, exist_ok=True)

    for file_path in file_list:
        if not os.path.isfile(file_path):
            print(f"Skipping {file_path}, not a file.")
            continue

        filename = os.path.basename(file_path)
        dest_path = os.path.join(destination_folder, filename)
        
        shutil.move(file_path, dest_path)
        print(f"Moved {file_path} to {dest_path}")

def delete_files(file_list):
    for file_path in file_list:
        try:
            os.remove(file_path)
            print(f"Deleted: {file_path}")
        except FileNotFoundError:
            print(f"File not found: {file_path}")
        except Exception as e:
            print(f"Error deleting {file_path}: {e}")

if __name__ == "__main__":
	os.chdir(LOCAL_PATH)
	#today = datetime.now().strftime("%Y%m%d")
	#yesterday = datetime.now() - timedelta(days=15)
	#yesterday_str = yesterday.strftime("%Y%m%d")
	#date_list = generate_dates(yesterday, today)
	#asyncio.run(main(date_list))	
	csv_files = [f for f in os.listdir(LOCAL_PATH) if f.endswith('.csv')]
	print(csv_files)	
	gzip_compress_files(csv_files)
	#gz_files = [f for f in os.listdir(LOCAL_PATH) if f.endswith('.csv.gz')]
	#move_files_to_folder(gz_files, CORE_DIR)
	#delete_files(csv_files)

