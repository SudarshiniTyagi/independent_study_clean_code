import glob, os
import json
os.chdir("/Users/sudarshinityagi/PycharmProjects/independentStudy/Python2Code")
input_files = []
for file in glob.glob("*.vals"):
    input_files.append(file)
print(input_files)

for file in input_files:
    infile = open(file)
    data = {}
    column_names = []
    matrix = []
    for line in infile:
        if not line.isspace() and not line.startswith('>'):
            # this is one row
            row = list(map(float, line.split()))
            matrix.append(row)

        elif line.startswith('>'):
            column_name = line[1:]
            column_names.append(column_name.rstrip())
    for col, row in zip(column_names, matrix):
        data[col] = row

    file_to_create = file.split(".")[0]+".json"
    path = "/Users/sudarshinityagi/PycharmProjects/independentStudy/RCode/generated_files"

    with open(os.path.join(path, file_to_create), 'w') as fp:
        json.dump(data, fp)
    print("Created: ", file_to_create)


