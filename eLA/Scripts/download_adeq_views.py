import sqlalchemy
import pandas as pd
import sys

len_args = len(sys.argv)
assert len_args == 2, 'Directory to write is required'
dir_ = sys.argv[1]

username = 'waq_readonly'
password = 'waqr3ad'
tns = 'com'

'''
MATS_WBID_NUTRIENT - verified by gh
REF_WATERBODY - not yet verified by gh
'''
views = dict(
        MATS_WBID_NUTRIENT="MATS_WBID_NUTRIENT",
        )

# create sql connection
oracle_db = sqlalchemy.create_engine('oracle://MATS_READONLY:Matsr3ad@com')
connection = oracle_db.connect()

# Loop to dump csv
for k,v in views.items():
    df = pd.read_sql(f'select * from MATS_SCHEMA.{v}', connection)
    df.to_csv(f'{dir_}/{k}.csv', index=True)
    print(f'read from table {v} and wrote to {k}.csv file')

views = dict(
        designated_use="VW_STA_STATION_DESIGNATED_USE",
        criteria_standards="VW_ADEQ_STANDARDS",
        huc_reach = "VW_WATERBODY_HUC_REACH",
        huc_usgs = "VW_USGS_WATERBODY_HUC_REACH",
        ref_waterbody="ref_waterbody",
        )
# create sql connection
oracle_db = sqlalchemy.create_engine('oracle://waq_readonly:waqr3ad@com')
connection = oracle_db.connect()

# Loop to dump csv
for k,v in views.items():
    df = pd.read_sql(f'select * from {v}', connection)
    df.to_csv(f'{dir_}/{k}.csv', index=True)
    print(f'read from table {v} and wrote to {k}.csv file')



if __name__ == '__main__':
    len_args = len(sys.argv)
    assert len_args == 2, 'Directory to write is required'
