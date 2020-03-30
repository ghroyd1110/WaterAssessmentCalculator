import sys
import sqlalchemy
import urllib
import pandas as pd

len_args = len(sys.argv)
assert len_args == 2, 'Directory to write is required'
dir_ = sys.argv[1]

'''
Application	ADEQ Surface Water Assessment	bpgvgzxmb
Table #1	Critical Conditions & Improvements	bpgvgzxmw
Table #2	Standards	bpgvgzxm3
Table #3	2018 Historical ATTAINS	bpgvgzxm8
Table #4	Water Body Decision Summary	bpgvgzxnb
Table #5	Exceedance Decision Summary	bpgvgzxnh
Table #6	impairedDaily	bpgvgzxni

Application     ADEQ AZ Water Assessment    bpk5e968v
Table #1    Critical Conditions & Improvements  bpk5e969c
Table #2    Standards   bpk5e969e
Table #3    2018 Current ATTAINS     bpk5e969i
Table #4    ATTAINS History     bpk5e969u
Table #5    Decision Summary    bpk5e969x
'''
appid = 'bpgvgzxmb' #'bnxyy8g96' #'bnjdne3t4'
s = 'Driver={QuickBase};UID=ramineni.kiran@azdeq.gov;PWD=fighter1;QUICKBASESERVER=arizonadeq.quickbase.com;APPTOKEN=bfuydqacxgv8tvdtsdg3bqi2xyc;LOGAPI=1;'

db = dict(standards='bpk5e969e', critical_condtions_improvements='bpk5e969c', attains_history='bpk5e969u')
#db = dict(standards='bpgvgzxm3', critical_condtions_improvements='bpgvgzxmw', attains='bpgvgzxm8', Exceedance_Decision_Summary='bpgvgzxnh', decision_summary='bpgvgzxnb', daily='bpgvgzxni') #, parameters='bpkwjteft')
quoted = urllib.parse.quote_plus(s)
con = sqlalchemy.create_engine('mssql+pyodbc:///?odbc_connect={}'.format(quoted)) #, echo=True)
for k,v in db.items():
    print(f'processing {k}')
    df = pd.read_sql(f'select * from {v}', con)
    df.to_csv(f"{dir_}/{k}.csv", index=False)
