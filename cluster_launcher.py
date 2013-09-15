import os, stat

# Settings
number_runs = 20

clojush_directory = "/home/thelmuth/ExperimentalClojush/"
output_directory = "../Results/thesis/change-exploratory/"

title_string = "Change problem (timed)"

example_file = "clojush.examples.change"

description = """An exploratory run to see how some initial settings do for the change problem.
"""

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
if os.path.isdir(clojush_directory + output_directory):
    raise RuntimeError, "Output directory already exists"

os.mkdir(clojush_directory + output_directory)

# Make alf file
alf_file_string = output_directory + "clojush_runs.alf"
alf_f = open(alf_file_string, "w")

alfcode = """##AlfredToDo 3.0
Job -title {%s} -subtasks {
""" % (title_string)

for run in range(0, number_runs):
    intro_command = "echo Starting run;export PATH=$PATH:/usr/java/latest/bin; cd %s;" % (clojush_directory)
    outro_command = " > %s%s%i%s; echo Finished Run" % (output_directory, output_prefix, run, output_postfix)

    full_command = intro_command + command + outro_command

    alfcode += """    Task -title {%s - run %i} -cmds {
        RemoteCmd {/bin/sh -c "%s"} -service {cluster}
    }
""" % (title_string, run, full_command)

alfcode += "}\n"

alf_f.writelines(alfcode)
alf_f.close()

# Run tractor command
source_string = "source /etc/sysconfig/pixar"
pixar_string = "/opt/pixar/tractor-blade-1.6.3/python/bin/python2.6 /opt/pixar/tractor-blade-1.6.3/tractor-spool.py --engine=fly:8000"

os.system("%s;%s %s" % (source_string, pixar_string, alf_file_string))
