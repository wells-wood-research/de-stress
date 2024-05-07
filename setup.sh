#!/bin/bash

# Ask the user which version of DE-STRESS they want to install
echo "Which version of DE-STRESS do you want to install? Submit 1 for the webserver and 2 for the headless version of DE-STRESS."
read option1

# Installing the webserver version of DE-STRESS
if [ "$option1" == "1" ]; then
    # Asking the user if they want to install Rosetta and checking if they have a licence.
    echo "Do you want to install Rosetta? A licence is required to use Rosetta which is free for academics but not for commercial users. Submit 1 if you still want to install Rosetta and 2 if you want to install a version without Rosetta."
    read option2
    # Installing Rosetta
    if [ "$option2" == "1" ]; then
        rosetta_install_command1="cd dependencies_for_de-stress/"
        $rosetta_install_command1
        rosetta_install_command2="git clone --branch v2024.18-dev62107 https://github.com/RosettaCommons/rosetta.git"
        $rosetta_install_command2
        echo "Rosetta has been successfully downloaded."

    # Installing a version of DE-STRESS without Rosetta
    elif [ "$option2" == "2" ]; then
        echo "A version without Rosetta is not available yet."
        exit 1
    else
        echo "Invalid option. Please choose either 1 or 2."
        exit 1
    fi

    # Changing directory 
    change_dir_command="cd ../"
    $change_dir_command

    # Ask the user which version of DE-STRESS they want to install
    echo "Are you installing the websever in a development or production environment. Submit 1 for development or 2 for production."
    read option3

    if [ "$option3" == "1" ]; then

        # Building the docker image for development webserver version of DE-STRESS
        echo "Building docker image for the development version of the DE-STRESS webserver."
        docker_command1="docker compose -f development-compose.yml build"
        $docker_command1


    elif [ "$option3" == "2" ]; then

        # Building the docker image for production webserver version of DE-STRESS
        echo "Building docker image for the production version of the DE-STRESS webserver."
        docker_command1="docker compose -f production-compose.yml build"
        $docker_command1

        exit 1
    else
        echo "Invalid option. Please choose either 1 or 2."
        exit 1
    fi

    # Get the current working directory
    current_dir=$(pwd)
    echo "Current dir: $current_dir"

    # Building the dependencies for DE-STRESS
    echo "Building DE-STRESS dependencies. Rosetta will take a few hours to compile. "
    docker_command2="docker run -it --rm -v $current_dir/dependencies_for_de-stress/:/dependencies_for_de-stress de-stress-big-structure:latest sh build_dependencies.sh"
    $docker_command2

    # Asking the user how many cpus they want to use for the webserver
    echo "How many CPUs do you want to use for the DE-STRESS webserver?"
    read webserver_num_cpus
    echo "Launching DE-STRESS webserver"

    if [ "$option3" == "1" ]; then

        # Launching the development version of DE-STRESS web server
        docker_command3="docker compose -f development-compose.yml --env-file .env up -d --scale rq-worker=$webserver_num_cpus"
        $docker_command3


    elif [ "$option3" == "2" ]; then

        # Launching the production version of DE-STRESS web server
        docker_command3="docker compose -f production-compose.yml --env-file .env up -d --scale rq-worker=$webserver_num_cpus"
        $docker_command3

        exit 1
    else
        echo "Invalid option. Please choose either 1 or 2."
        exit 1
    fi

    

# Installing the headless version of DE-STRESS
elif [ "$option1" == "2" ]; then
    # Asking the user if they want to install Rosetta and checking if they have a licence.
    echo "Do you want to install Rosetta? A licence is required to use Rosetta which is free for academics but not for commercial users. Submit 1 if you still want to install Rosetta and 2 if you want to install a version without Rosetta."
    read option2
    # Installing Rosetta
    if [ "$option2" == "1" ]; then
        rosetta_install_command1="cd dependencies_for_de-stress/"
        $rosetta_install_command1
        rosetta_install_command2="git clone --branch v2024.18-dev62107 https://github.com/RosettaCommons/rosetta.git"
        $rosetta_install_command2
        echo "Rosetta has been successfully downloaded."

    # Installing a version of DE-STRESS without Rosetta
    elif [ "$option2" == "2" ]; then
        echo "A version without Rosetta is not available yet."
        exit 1
    else
        echo "Invalid option. Please choose either 1 or 2."
        exit 1
    fi

    # Changing directory 
    change_dir_command="cd ../"
    $change_dir_command

    # Building the docker image for headless version of DE-STRESS
    echo "Building docker image for headless DE-STRESS."
    docker_command1="docker compose -f headless-compose.yml build"
    $docker_command1

    # Get the current working directory
    current_dir=$(pwd)
    echo "Current dir: $current_dir"

    # Building the dependencies for DE-STRESS
    echo "Building DE-STRESS dependencies. Rosetta will take a few hours to compile. "
    docker_command2="docker run -it --rm -v $current_dir/dependencies_for_de-stress/:/dependencies_for_de-stress de-stress-big-structure:latest sh build_dependencies.sh"
    $docker_command2
else
    echo "Invalid option. Please choose either 1 or 2."
    exit 1
fi
