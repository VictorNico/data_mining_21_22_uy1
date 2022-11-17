
from csv import reader
from csv import DictReader


def main():
    print('*** Read csv file line by line using csv module reader object ***')
    print('*** Iterate over each row of a csv file as list using reader object ***')
    # open file in read mode
    with open('/Users/cedricdonalddobou/Documents/SCHOOL_M1_2021_2022/INF4117_Fouille_des_donnees_II_TSOPZE/TP/TP-INF4117/sources/Simulated_Data_set_of_Iraqi_tourism_places.csv', 'r') as read_obj:
        # pass the file object to reader() to get the reader object
        csv_reader = reader(read_obj)
        # Iterate over each row in the csv using reader object
        for row in csv_reader:
            # row variable is a list that represents a row in csv
            print(row)
    print('*** Read csv line by line without header ***')
    # skip first line i.e. read header first and then iterate over each row od csv as a list
    with open('/Users/cedricdonalddobou/Documents/SCHOOL_M1_2021_2022/INF4117_Fouille_des_donnees_II_TSOPZE/TP/TP-INF4117/sources/Simulated_Data_set_of_Iraqi_tourism_places.csv', 'r') as read_obj:
        csv_reader = reader(read_obj)
        header = next(csv_reader)
        # Check file as empty
        if header != None:
            # Iterate over each row after the header in the csv
            for row in csv_reader:
                # row variable is a list that represents a row in csv
                print(row)
    print('Header was: ')
    print(header)
    print('*** Read csv file line by line using csv module DictReader object ***')
    # open file in read mode
    with open('/Users/cedricdonalddobou/Documents/SCHOOL_M1_2021_2022/INF4117_Fouille_des_donnees_II_TSOPZE/TP/TP-INF4117/sources/Simulated_Data_set_of_Iraqi_tourism_places.csv', 'r') as read_obj:
        # pass the file object to DictReader() to get the DictReader object
        csv_dict_reader = DictReader(read_obj)
        # iterate over each line as a ordered dictionary
        for row in csv_dict_reader:
            # row variable is a dictionary that represents a row in csv
            print(row)
    print('*** select elements by column name while reading csv file line by line ***')
    # open file in read mode
    with open('/Users/cedricdonalddobou/Documents/SCHOOL_M1_2021_2022/INF4117_Fouille_des_donnees_II_TSOPZE/TP/TP-INF4117/sources/Simulated_Data_set_of_Iraqi_tourism_places.csv', 'r') as read_obj:
        # pass the file object to DictReader() to get the DictReader object
        csv_dict_reader = DictReader(read_obj)
        # iterate over each line as a ordered dictionary
        for row in csv_dict_reader:
            # row variable is a dictionary that represents a row in csv
            print(row['ID'], ' is from ', row['City'],' and he is studying ', row['Place names'])
    print('*** Get column names from header in csv file ***')
    # open file in read mode
    with open('/Users/cedricdonalddobou/Documents/SCHOOL_M1_2021_2022/INF4117_Fouille_des_donnees_II_TSOPZE/TP/TP-INF4117/sources/Simulated_Data_set_of_Iraqi_tourism_places.csv', 'r') as read_obj:
        # pass the file object to DictReader() to get the DictReader object
        csv_dict_reader = DictReader(read_obj)
        # get column names from a csv file
        column_names = csv_dict_reader.fieldnames
        print(column_names)
    print('*** Read specific columns from a csv file while iterating line by line ***')
    print('*** Read specific columns (by column name) in a csv file while iterating row by row ***')
    # iterate over each line as a ordered dictionary and print only few column by column name
    with open('/Users/cedricdonalddobou/Documents/SCHOOL_M1_2021_2022/INF4117_Fouille_des_donnees_II_TSOPZE/TP/TP-INF4117/sources/Simulated_Data_set_of_Iraqi_tourism_places.csv', 'r') as read_obj:
        csv_dict_reader = DictReader(read_obj)
        for row in csv_dict_reader:
            print(row['Id'], row['Place names'])
    print('*** Read specific columns (by column Number) in a csv file while iterating row by row ***')
    # iterate over each line as a ordered dictionary and print only few column by column Number
    with open('/Users/cedricdonalddobou/Documents/SCHOOL_M1_2021_2022/INF4117_Fouille_des_donnees_II_TSOPZE/TP/TP-INF4117/sources/Simulated_Data_set_of_Iraqi_tourism_places.csv', 'r') as read_obj:
        csv_reader = reader(read_obj)
        for row in csv_reader:
            print(row[1], row[2])


if __name__ == '__main__':
    main()
