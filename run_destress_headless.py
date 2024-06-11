import os
import os.path as p
import argpass
from subprocess import call
##############################################################################
# get inputs
def read_inputs():
    ## create an argpass parser, read config file, 
    parser = argpass.ArgumentParser()
    parser.add_argument("--i")
    args = parser.parse_args()

    inputPath=args.i

    return inputPath
##############################################################################
def main():
    inputPath = read_inputs()

    if not p.isabs(inputPath):
        ascii_splash("ERROR")
        print("Input Path must be Absolute!")
        return

    ascii_splash("DESTRESS")
    deStressDir = os.getcwd()
    dependanciesDir = p.join(deStressDir,"dependencies_for_de-stress")
    call(["docker", "run", "-it", "--rm",
          "--env-file", ".env-headless",
          "-v", f"{dependanciesDir}:/dependencies_for_de-stress",
          "-v", f"{inputPath}:/input_path",
          "de-stress-big-structure:latest",
        "poetry", "run", "headless_destress", "/input_path"])
##############################################################################
def ascii_splash(id):
    splashDict = {"DESTRESS":
"""
██████╗ ███████╗    ███████╗████████╗██████╗ ███████╗███████╗███████╗
██╔══██╗██╔════╝    ██╔════╝╚══██╔══╝██╔══██╗██╔════╝██╔════╝██╔════╝
██║  ██║█████╗█████╗███████╗   ██║   ██████╔╝█████╗  ███████╗███████╗
██║  ██║██╔══╝╚════╝╚════██║   ██║   ██╔══██╗██╔══╝  ╚════██║╚════██║
██████╔╝███████╗    ███████║   ██║   ██║  ██║███████╗███████║███████║
╚═════╝ ╚══════╝    ╚══════╝   ╚═╝   ╚═╝  ╚═╝╚══════╝╚══════╝╚══════╝                                                                                                                 
""", 

"ERROR":
"""
▓█████  ██▀███   ██▀███   ▒█████   ██▀███  
▓█   ▀ ▓██ ▒ ██▒▓██ ▒ ██▒▒██▒  ██▒▓██ ▒ ██▒
▒███   ▓██ ░▄█ ▒▓██ ░▄█ ▒▒██░  ██▒▓██ ░▄█ ▒
▒▓█  ▄ ▒██▀▀█▄  ▒██▀▀█▄  ▒██   ██░▒██▀▀█▄  
░▒████▒░██▓ ▒██▒░██▓ ▒██▒░ ████▓▒░░██▓ ▒██▒
░░ ▒░ ░░ ▒▓ ░▒▓░░ ▒▓ ░▒▓░░ ▒░▒░▒░ ░ ▒▓ ░▒▓░
 ░ ░  ░  ░▒ ░ ▒░  ░▒ ░ ▒░  ░ ▒ ▒░   ░▒ ░ ▒░
   ░     ░░   ░   ░░   ░ ░ ░ ░ ▒    ░░   ░ 
   ░  ░   ░        ░         ░ ░     ░     
                                                      
"""
    }
    print(splashDict[id])

##############################################################################
main()