import os, stat

##########################################################################
# Settings
number_runs = 20

namespace = "replace-space-with-newline"

selection = "lexicase"

clojush_directory = "/home/thelmuth/Autoconstruction/"
output_directory = "/home/thelmuth/Results/autoconstruction/" + namespace + "/csv-printing-better-parent-tracking/"

example_file = "clojush.problems.software." + namespace

title_string = "Autoconstruction | %s | CSV Printing - Better Parent Tracking" % (namespace.replace("-", " ").title())

description = """A first try at systematic runs of benchark problems using autoconstruction
The problem is %s
These runs will print CSVs to load into the graph DB's we've been using.
""" % namespace.replace("-", " ").title()

# Push args                                                                                                                                   
example_file += " :autoconstructive true"
example_file += " :print-homology-data true"
example_file += " :max-generations 2000"

# Clustering experiment stuff
example_file += " :print-csv-logs true"
example_file += """ :csv-columns '[:generation :location :parent-uuids :genetic-operators :is-random-replacement :push-program-size :plush-genome-size :plush-genome :total-error :test-case-errors]'"""


##########################################################################
# Uncomment the following if you want to print timings in the logs
example_file += " :print-timings true"

##########################################################################
# Probably don't change these
output_prefix = "log"
output_postfix = ".txt"

command = "/share/apps/bin/lein with-profiles production trampoline run " + example_file

service_tag = "tom"

##########################################################################
# You don't need to change anything below here

# Check to make sure directory doesn't exist; if not, create it
if output_directory[-1] != "/":
    output_directory += "/"
if os.path.isdir(output_directory):
    raise RuntimeError, "Output directory already exists"

os.mkdir(output_directory)
os.mkdir(output_directory + "logs/")
os.mkdir(output_directory + "csv/")

# Make description file
description_file_string = output_directory + "description.txt"
description_f = open(description_file_string, "w")

description_f.writelines("COMMAND:\n" + command + "\n\nTRACTOR TITLE:\n" + title_string + "\n\nDESCRIPTION:\n" + description)
description_f.close()

# Make alf file
alf_file_string = output_directory + "clojush_runs.alf"
alf_f = open(alf_file_string, "w")

alfcode = """##AlfredToDo 3.0
Job -title {%s} -subtasks {
""" % (title_string)

for run in range(0, number_runs):
    intro_command = "echo Starting run;export PATH=$PATH:/usr/java/latest/bin; cd %s;" % (clojush_directory)
    csv_command = """ :csv-log-filename '"%scsv/data%i.csv"'""" % (output_directory, run)
    output_command = " > %slogs/%s%i%s;" % (output_directory, output_prefix, run, output_postfix)
    tar_command = " echo gzip CSV; cd %scsv/; gzip data%i.csv; cd %s;" % (output_directory, run, clojush_directory)
    outro_command = " echo Finished Run"

    full_command = intro_command + command + csv_command + output_command + tar_command + outro_command

    alfcode += """    Task -title {%s - run %i} -cmds {
        RemoteCmd {/bin/sh -c {%s}} -service {%s}
    }
""" % (title_string, run, full_command, service_tag)

alfcode += "}\n"

alf_f.writelines(alfcode)
alf_f.close()

# Run tractor command
source_string = "source /etc/sysconfig/pixar"
pixar_string = "/opt/pixar/tractor-blade-1.7.2/python/bin/python2.6 /opt/pixar/tractor-blade-1.7.2/tractor-spool.py --engine=fly:8000"

os.system("%s;%s %s" % (source_string, pixar_string, alf_file_string))
