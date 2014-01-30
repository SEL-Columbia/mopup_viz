import json
import csv
import os

ROOT_DIR = os.path.join(os.path.expanduser('~'), 'Dropbox/Nigeria/Nigeria 661 Baseline Data Cleaning/in_process_data/mop_up_matching_result')

def my_file_path(relative_path):
    my_file = os.path.join(ROOT_DIR, relative_path)
    return my_file


def nmis_id_json_parser(infile):
    """
    takes a file, parse the json file by line
    return the uuid pairs of each facility
    """
    with open(infile) as f:
        my_dat = [json.loads(row) for row in f]
    
    id_list = []
    for current in my_dat:
        if not current.has_key("matched"):
            try:
                uuid = current["long_id"]
                facility_name = current["facility_name"]
                lga_id = current["lga_id"]
                community = current["community"]
                ward = current["ward"]
                short_id = current["short_id"]
                facility_type = current["facility_type"]
                
            except Exception, e:
                print str(e)
        
            obj = {"uuid": uuid,
                   "facility_name": facility_name,
                   "lga_id": lga_id,
                   "community": community,
                   "ward": ward,
                   "short_id": short_id,
                   "facility_type": facility_type}
            id_list.append(obj)
    return id_list


def run(input_file, output_file):
    id_list = nmis_id_json_parser(input_file)
    output_file = open(output_file, 'w')
    writer = csv.DictWriter(output_file, fieldnames = id_list[0].keys())
    writer.writeheader()
    writer.writerows(id_list)
    output_file.close()
    

    
my_file = my_file_path('jsondumps/lga_edu.json')
out_file = my_file_path("facility_missing_list_edu.csv")
try:
    run(my_file, out_file)
except Exception,e:
    print str(e)

my_file = my_file_path('jsondumps/lga_health.json')
out_file = my_file_path("facility_missing_list_health.csv")
try:
    run(my_file, out_file)
except Exception,e:
    print str(e)
