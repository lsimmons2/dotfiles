#!/usr/bin/python3

import subprocess
import time
import datetime

def log(to_log):
    with open("/home/leo/startup.log", "a") as f:
        f.write("%s\n" % to_log)

def run_command_in_background(command):
    # command is list of strings
    subprocess.Popen(command)

def get_wmctrl_lines():
    return subprocess.run(["wmctrl", "-l"], capture_output=True, text=True).stdout.splitlines()

def move_window_to_desktop(window_id, desktop_number):
    # desktop = workspace??
    log("moving window %s to desktop %s" % (window_id, desktop_number))
    run_command_in_background(["wmctrl", "-i", "-r", str(window_id), "-t", str(desktop_number)])

log_datetime = datetime.datetime.now().strftime("%m-%d-%y %H:%M:%S")
log("Running startup script at %s" % log_datetime)

run_command_in_background(["xmodmap", "/home/leo/.Xmodmap"])

# START APPS
for _ in range(3):
    run_command_in_background(["terminator"])
run_command_in_background(["vivaldi"])
run_command_in_background(["google-chrome"])
run_command_in_background(["snap","run","spotify"])
time.sleep(5)

# MOVE APPS
terminator_count = 0
for line in get_wmctrl_lines():
    if "leo@fiona" in line:
        window_id = line.split(" ")[0]
        move_window_to_desktop(window_id, terminator_count+3)
        run_command_in_background(["wmctrl", "-i", "-r", str(window_id), "-b", "toggle,fullscreen"])
        terminator_count += 1


# CREATE AND MOVE OTHER APPS
app_map = {
        "vivaldi":2,
        "spotify":6,
        "chrome":8,
        }

lines = get_wmctrl_lines()
log("%d many lines" % len(lines))
for app_search_string, desktop_number in app_map.items():
    for line in lines:
        if app_search_string in line.lower():
            log("this string")
            log(line)
            window_id = line.split(" ")[0]
            #if app_search_string == "spotify":
            #    log("aqui quey")
            #    run_command_in_background(["wmctrl", "-a", window_id])
            #    time.sleep(5)
            move_window_to_desktop(window_id, desktop_number)
            break
