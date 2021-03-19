import sys
import subprocess

def run_command_in_background(command):
    # command is list of strings
    subprocess.Popen(command)

  
color_map = {
	"light":{
		"background_color":"#fdf6e3",
		"foreground_color":"#657b83",
	},
	"dark":{
		"background_color":"#002b36",
		"foreground_color":"#839496",
	}
}

CONFIG_FP = "/home/leo/.config/terminator/config"
def change_color_scheme(color_scheme):
	new_file_lines = []
	with open(CONFIG_FP, "r") as f:
		for line in f.read().splitlines():
			new_line = line
			for key, value in color_map[color_scheme].items():
				if line.strip().startswith(key):
					new_line = "    %s = %s" % (key, value)
			new_file_lines.append(new_line)
	with open(CONFIG_FP, "w") as f:
		for line in new_file_lines:
			f.write("%s\n" % line)


if __name__ == "__main__":
	acceptable_schemes = list(color_map.keys())
	try:
		color_scheme = sys.argv[1]
	except IndexError:
		raise Exception("Call with something in %s" % str(acceptable_schemes))
	if color_scheme not in acceptable_schemes:
		raise Exception("Call with something in %s" % str(acceptable_schemes))
	change_color_scheme(color_scheme)
