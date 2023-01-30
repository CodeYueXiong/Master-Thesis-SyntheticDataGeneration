import pandas as pd
import pyreadr
import os

from sdmetrics.single_table import NewRowSynthesis

# set the working directory
wd = "/Users/Echo/Documents/MasterThesisYue/Master-Thesis-DifferentialPrivacy"
os.chdir(wd)

ods_dataset = pyreadr.read_r("bindori_dataset_preprocessed_factor.rda")["bindori_dataset_threshold_chr"]
# load the cart synthetic datasets
cart_sample_sds = pyreadr.read_r("./SyntheticData/Yue/syn1_cart/cart_sample_syn.rda")["syn"]
cart_norm_sds = pyreadr.read_r("./SyntheticData/Yue/syn1_cart/cart_norm_syn.rda")["syn"]
cart_normrank_sds = pyreadr.read_r("./SyntheticData/Yue/syn1_cart/cart_normrank_syn.rda")["syn"]
# create the metadata dictionary
single_table_metadata_dict = {"primary_key": "weight",
                              "fields": {
                                "weight": {
                                    "type": "numerical",
                                    "subtype": "float",
                                },
                                "B1_1": {
                                    "type": "numerical",
                                    "subtype": "float",
                                },
                                "B1_2": {
                                    "type": "numerical",
                                    "subtype": "float",
                                },
                                "B1_3": {
                                    "type": "numerical",
                                    "subtype": "float",
                                }, 
                                "B1_4": {
                                    "type": "numerical",
                                    "subtype": "float",
                                },
                                "B1_5": {
                                    "type": "numerical",
                                    "subtype": "float",
                                },
                                "B1_6": {
                                    "type": "numerical",
                                    "subtype": "float",
                                },
                                "B1_7": {
                                    "type": "numerical",
                                    "subtype": "float",
                                },
                                "B1_8": {
                                    "type": "numerical",
                                    "subtype": "float",
                                },
                                "B1_9": {
                                    "type": "numerical",
                                    "subtype": "float",
                                },
                                "B1_10": {
                                    "type": "numerical",
                                    "subtype": "float",
                                },
                                "B1_11": {
                                    "type": "numerical",
                                    "subtype": "float",
                                },
                                "B1_12": {
                                    "type": "numerical",
                                    "subtype": "float",
                                },
                                "B1_13": {
                                    "type": "numerical",
                                    "subtype": "float",
                                },
                                "B3": {
                                    "type": "numerical",
                                    "subtype": "float",
                                },
                                "B5": {
                                    "type": "numerical",
                                    "subtype": "float",
                                },
                                "B6": {
                                    "type": "numerical",
                                    "subtype": "float",
                                },
                                "B7": {
                                    "type": "numerical",
                                    "subtype": "float",
                                },
                                "B8": {
                                    "type": "numerical",
                                    "subtype": "float",
                                },
                                "B9": {
                                    "type": "numerical",
                                    "subtype": "float",
                                },
                                "B10": {
                                    "type": "numerical",
                                    "subtype": "float",
                                },
                                "B11": {
                                    "type": "numerical",
                                    "subtype": "float",
                                },
                                "B12_1": {
                                    "type": "numerical",
                                    "subtype": "float",
                                },
                                "B12_2": {
                                    "type": "numerical",
                                    "subtype": "float",
                                },
                                "B12_3": {
                                    "type": "numerical",
                                    "subtype": "float",
                                },
                                "B12_4": {
                                    "type": "numerical",
                                    "subtype": "float",
                                },
                                "B12_5": {
                                    "type": "numerical",
                                    "subtype": "float",
                                },
                                "B12_6": {
                                    "type": "numerical",
                                    "subtype": "float",
                                },
                                "C1_m": {
                                    "type": "numerical",
                                    "subtype": "float",
                                },
                                "C2": {
                                    "type": "numerical",
                                    "subtype": "float",
                                },
                                "C3": {
                                    "type": "numerical",
                                    "subtype": "float",
                                },
                                "C5": {
                                    "type": "numerical",
                                    "subtype": "float",
                                },
                                "C6": {
                                    "type": "numerical",
                                    "subtype": "float",
                                },
                                "C7": {
                                    "type": "numerical",
                                    "subtype": "float",
                                },
                                "C8": {
                                    "type": "numerical",
                                    "subtype": "float",
                                },
                                "D1": {
                                    "type": "numerical",
                                    "subtype": "float",
                                },
                                "D2": {
                                    "type": "numerical",
                                    "subtype": "float",
                                },
                                "D3": {
                                    "type": "numerical",
                                    "subtype": "float",
                                },
                                "D4": {
                                    "type": "numerical",
                                    "subtype": "float",
                                },
                                "D5": {
                                    "type": "numerical",
                                    "subtype": "float",
                                },
                                "D7": {
                                    "type": "numerical",
                                    "subtype": "float",
                                },
                                "D8": {
                                    "type": "numerical",
                                    "subtype": "float",
                                },
                                "D9": {
                                    "type": "numerical",
                                    "subtype": "float",
                                },
                                "E2": {
                                    "type": "numerical",
                                    "subtype": "float",
                                },
                                "E3": {
                                    "type": "numerical",
                                    "subtype": "float",
                                },
                                "E4": {
                                    "type": "numerical",
                                    "subtype": "float",
                                },
                                "E7": {
                                    "type": "numerical",
                                    "subtype": "float",
                                },
                                "F1": {
                                    "type": "numerical",
                                    "subtype": "float",
                                },
                                "F2_1": {
                                    "type": "numerical",
                                    "subtype": "float",
                                },
                                "F2_2": {
                                    "type": "numerical",
                                    "subtype": "float",
                                },
                                "B2": {
                                    "type": "numerical",
                                    "subtype": "float",
                                },
                                "B4": {
                                    "type": "numerical",
                                    "subtype": "float",
                                },
                                "E5": {
                                    "type": "numerical",
                                    "subtype": "float",
                                },
                                "E6": {
                                    "type": "numerical",
                                    "subtype": "float",
                                }
                            }}


def load_ods_preprocess():
    # preprocess the dataset to keep only 54 variables
    ods_dataset.drop(["B13_1", "B13_2", "B13_3", "B13_4",
                    "B13_5", "B13_6", "B13_7", "B14_1", 
                    "B14_2", "B14_3", "B14_4", "B14_5",
                    "D6_1", "D6_2", "D6_3", "F3_de",
                    "B1b_x1", "B1b_x2", "B1b_x3", "B1b_x4", 
                    "B1b_x5", "B1b_x6", "B1b_x7", "B1b_x8", 
                    "B1b_x9", "B1b_x10", "B1b_x11","B1b_x12", 
                    "B1b_x13", "D10", "C0_1", "C0_2", 
                    "C0_3", "C0_4", "C0_5", "C0_6"], inplace=True, axis=1)
    # encode var B2, B4, E5, E6 as integers
    # prepare the category mapping
    category_mapping_B2 = {'-1': 1, '-99': 2, '[0, 1)': 3, '[1, 3)': 4}
    category_mapping_B4 = {'-99': 1, '[0, 1)': 2, '[1, 5)': 3}
    category_mapping_E5 = {'-99': 1, '[0, 1)': 2, '[1, 2)': 3}
    category_mapping_E6 = {'-99': 1, '[0, 9)': 2}

    ods_dataset['B2'] = ods_dataset['B2'].map(category_mapping_B2)
    ods_dataset['B4'] = ods_dataset['B4'].map(category_mapping_B4)
    ods_dataset['E5'] = ods_dataset['E5'].map(category_mapping_E5)
    ods_dataset['E6'] = ods_dataset['E6'].map(category_mapping_E6)

    # change from column 1-54 as float variables
    ods_dataset[ods_dataset.columns[1:54]] = ods_dataset[ods_dataset.columns[1:54]].astype(float)

    return ods_dataset


def load_sds_cart_preprocess():

    # change from column 1-54 as float variables
    cart_sample_sds[cart_sample_sds.columns[1:54]] = cart_sample_sds[cart_sample_sds.columns[1:54]].astype(float)
    cart_norm_sds[cart_norm_sds.columns[1:54]] = cart_norm_sds[cart_norm_sds.columns[1:54]].astype(float)
    cart_normrank_sds[cart_normrank_sds.columns[1:54]] = cart_normrank_sds[cart_normrank_sds.columns[1:54]].astype(float)

    return cart_sample_sds, cart_norm_sds, cart_normrank_sds

def new_row_synthesis_cart():

    cart_sample_nrs = NewRowSynthesis.compute(real_data=ods_dataset,
                                              synthetic_data=cart_sample_sds,
                                              metadata=single_table_metadata_dict,
                                              numerical_match_tolerance=0.01,
                                              synthetic_sample_size=50_000)
    print("!!!cart_sample_nrs done!!!")
    cart_norm_nrs = NewRowSynthesis.compute(real_data=ods_dataset,
                                            synthetic_data=cart_norm_sds,
                                            metadata=single_table_metadata_dict,
                                            numerical_match_tolerance=0.01,
                                            synthetic_sample_size=50_000)
    print("!!!cart_norm_nrs done!!!")                        
    cart_normrank_nrs = NewRowSynthesis.compute(real_data=ods_dataset,
                                                synthetic_data=cart_normrank_sds,
                                                metadata=single_table_metadata_dict,
                                                numerical_match_tolerance=0.01,
                                                synthetic_sample_size=50_000)
    print("!!!cart_normrank_nrs done!!!")
    with open("./SyntheticData/Yue/risk_analysis_cart.txt", "w") as file:
        file.write("cart_sample_nrs: " + str(cart_sample_nrs) + "\n")
        file.write("cart_norm_nrs: " + str(cart_norm_nrs) + "\n")
        file.write("cart_normrank_nrs: " + str(cart_normrank_nrs) + "\n")
    
if __name__ == "__main__":
   ods_dataset = load_ods_preprocess()
   cart_sample_sds, cart_norm_sds, cart_normrank_sds = load_sds_cart_preprocess()
   new_row_synthesis_cart()



