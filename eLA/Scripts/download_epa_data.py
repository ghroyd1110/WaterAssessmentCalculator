#!/usr/bin/env python
# -*- coding: utf-8 -*-

import requests
import sys
from datetime import datetime, timedelta

# https://www.waterqualitydata.us/data/Result/search?countrycode=US&statecode=US%3A04&mimeType=csv&sorted=yes&startDateLo=11-21-2018&startDateHi=12-21-2018
hmap = {
'Sample_results_chemical':'Result',
'Sampling_Activity':'Activity',
'Sampling_Activity_Metrics':'ActivityMetric',
'Project_data':'Project',
'Site_data_only':'Station',
'Result_Detection_Quantitation_Limit_Data':'ResultDetectionQuantitationLimit'
}

url_ = 'https://www.waterqualitydata.us/data/'
params = dict(countrycode='US', statecode='US:04', mimeType='csv', sorted='yes')


def download_epa(dir_, days):

    today = datetime.now() #.strftime('%m-%d-%Y')
    today = today - timedelta(days=14)
    start = today - timedelta(days=days)
    start = start.strftime('%m-%d-%Y')
    end = today.strftime('%m-%d-%Y')
    params['startDateLo'] = start
    params['startDateHi'] = end
    print(f'downloading from {start} to {end}')

    for k,v in hmap.items():
        url = f'{url_}{v}/search'
        print(f'Downloading {k}')
        fname = f'{dir_}/{k}_from_{start}_to_{end}.csv'
        f = open(fname, 'wb')
        r = requests.get(url, params)
        # print('url is: ', r.url)
        if r.status_code == 200:
            f.write(r.content)
            print('download successful. Written to: ', fname)
        else:
            print('download failed')
            sys.exit(-1)


if __name__ == '__main__':
    len_args = len(sys.argv)
    download_epa('./' if len_args < 2 else sys.argv[1], 7 if len_args < 3 else int(sys.argv[2]))
# python download_epa_data.py ../data/archive 2364
