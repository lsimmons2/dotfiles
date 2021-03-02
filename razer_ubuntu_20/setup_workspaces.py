#!/usr/bin/python3

import subprocess
import time
import datetime
import sys


def log(to_log):
    print(to_log)
    with open("/home/leo/startup.log", "a") as f:
        f.write("%s\n" % to_log)

def run_command_in_background(command):
    # command is list of strings
    subprocess.Popen(command)

def get_wmctrl_lines():
    return subprocess.run(["wmctrl", "-l"], capture_output=True, text=True).stdout.splitlines()


finito_ids = set()
def get_running_app_window_id_desktop_nb(search_term):
    for line in get_wmctrl_lines():
        if search_term in line.lower():
            split = line.split()
            window_id = split[0]
            if window_id not in finito_ids:
                return split[0], int(split[1])
    return None, None

def move_window_to_desktop(window_id, desktop_number):
    # desktop = workspace??
    log("moving window %s to desktop %s" % (window_id, desktop_number))
    run_command_in_background(["wmctrl", "-i", "-r", str(window_id), "-t", str(desktop_number)])

log_datetime = datetime.datetime.now().strftime("%m-%d-%y %H:%M:%S")
log("Running startup script at %s" % log_datetime)

for i in range(1,9):
    command = ["gsettings", "set", "org.gnome.shell.keybindings", "switch-to-application-%s" % i, "[]"]
    #run_command_in_background(command)

run_command_in_background(["xmodmap", "/home/leo/.Xmodmap"])

apps = [
        # app_command, desktop_nb, search_term, should_be_full_screen, has_been_spawned, has_been_formatted
        (["vivaldi"],2,"vivaldi",False,False,False),
        (["terminator"],3,"/bin/bash",True,False,False),
        (["terminator"],4,"/bin/bash",True,False,False),
        (["terminator"],5,"/bin/bash",True,False,False),
        (["snap","run","spotify"],6,"spotify",False,False,False),
        (["google-chrome"],8,"chrome",False,False,False),
        ]


# MOVE APPS
while False in [a[-1] for a in apps]:
    new_apps = []
    for app in apps:
        app_command, desktop_nb, search_term, should_be_full_screen, has_been_spawned, has_been_formatted = app
        if not has_been_spawned:
            log("spawning app %s" % search_term)
            run_command_in_background(app_command)
            has_been_spawned = True
        else:
            app_window_id, current_desktop_nb = get_running_app_window_id_desktop_nb(search_term)
            if app_window_id is not None:
                log("moving app %s in window %s to desktop %s" % (search_term, app_window_id, desktop_nb))
                run_command_in_background(["wmctrl", "-i", "-r", app_window_id, "-t", str(desktop_nb)])
                if should_be_full_screen:
                    run_command_in_background(["wmctrl", "-i", "-r", app_window_id, "-b", "toggle,fullscreen"])
                has_been_formatted = True
                finito_ids.add(app_window_id)
            else:
                print("app %s still not running" % search_term)
        new_apps.append((app_command, desktop_nb, search_term, should_be_full_screen, has_been_spawned, has_been_formatted))
    apps = new_apps
    time.sleep(1)

run_command_in_background(["notify-send", "Hola we", "apps/workspaces are set up"])
