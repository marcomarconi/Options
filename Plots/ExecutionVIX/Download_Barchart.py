# this script as it is downloads future contract from barchart for this year and the next one

from bcutils.bc_utils import get_barchart_downloads, create_bc_session
from datetime import datetime
import sys

# argments: save_dir 

if(len(sys.argv) < 2):
	print("ERROR: Provide a save directory")
	exit()

save_dir=sys.argv[1]

MONTH_LIST = ['F', 'G', 'H', 'J', 'K', 'M', 'N', 'Q', 'U', 'V', 'X', 'Z']

months_to_download = "".join(MONTH_LIST)

CONTRACTS={
    "VI":{"code":"VI","cycle":months_to_download,"tick_date":"2009-11-24"}
}

session = create_bc_session(config_obj=dict(
    barchart_username="marco.marconi@gmail.com",
    barchart_password = "Idh047760")
)

get_barchart_downloads(
    session,
    contract_map=CONTRACTS,
    save_directory=sys.argv[1],
    start_year=datetime.now().year,
    end_year=datetime.now().year+2,
	force_daily=True, overwrite=True
)


