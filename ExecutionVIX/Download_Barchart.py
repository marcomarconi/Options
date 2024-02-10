from bcutils.bc_utils import get_barchart_downloads, create_bc_session
from datetime import datetime
import sys

if(len(sys.argv) < 2):
	print("ERROR: Provide a save directory")
	exit()

save_dir=sys.argv[1]

CONTRACTS={
    "VI":{"code":"VI","cycle":"FGHJKMNQUVXZ","tick_date":"2009-11-24"}
}

session = create_bc_session(config_obj=dict(
    barchart_username="marco.marconi@gmail.com",
    barchart_password = "Idh047760")
)

get_barchart_downloads(
    session,
    contract_map=CONTRACTS,
    save_directory=sys.argv[1],
    start_year=datetime.now().year-1,
    end_year=datetime.now().year
)


