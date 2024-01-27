## TALA-ADLAWAN INSTRUCTION MANUAL

### I. INTRODUCTION
#### Program Overview
This program is a simple task scheduler that allows you to add, edit, delete, and view your tasks. It also allows you to create an account and login to your account. This program is written in COBOL and uses a text file as a database.

------------------------------------------

#### Purpose of the Manual
This manual is created to help the users of the program to understand how to use the program. This manual will also help the users to understand the purpose of the program and how it works.

------------------------------------------

#### Scope and Limitations
This program is only limited to the following:
- Creating an account
- Logging in to an account
- Exiting the program
- Viewing a task
- Adding a task
- Editing a task
- Editing a specified part of a task (e.g. date, day, description, and status)
- Deleting a task
- Viewing an event
- Adding an event
- Editing an event
- Edting a specified part of an event (e.g. date, day, description, location, and status)
- Deleting an event

------------------------------------------

### II. GETTING STARTED
#### Prerequisites
1. Install VSCode
2. Install the following extensions:
    - COBOL (Intellisense, Syntax Highlighting, Linting and Debugging)
    - Micro Focus COBOL (Syntax Highlighting, Linting and Debugging)
3. Enable wsl in windows
    - Open PowerShell as administrator
    - Type in the following command:
    ```bash
    $ dism.exe /online /enable-feature /featurename:Microsoft-Windows-Subsystem-Linux /all /norestart
    ```
    - Restart your computer
3. Install Ubuntu or any Linux Distribution in Microsoft Store
    - Open Microsoft Store
    - Search for Ubuntu
    - Click Install
    - Or use the following command in PowerShell:
    ```bash
    $ wsl --install -d Ubuntu
    ```
4. Update and upgrade the packages of your Linux Distribution
    ```bash
    $ sudo apt-get update
    $ sudo apt-get upgrade
    ```
5. Install GnuCOBOL
    ```bash
    $ sudo apt-get install gnucobol
    ```

------------------------------------------
#### Running the program
1. Open VSCode and open the folder containing the program
2. Open the terminal and type in the following commands:
3. Use Windows Subsystem for Linux (WSL) to run the program
    ```bash
    $ wsl -d Ubuntu
    ```
4. Compile the program using GnuCOBOL
    ```bash
    $ cobc -x sched.cob
    ```
5. Run the program
    ```bash
    $ ./sched
    ```
6. To exit the program, press Ctrl + C


## III. MAIN FEATURES
------------------------------------------
### Using the program
#### Logging in to an account
1. Run the program and you will be prompted to the account menu
2. Type in your username and password
3. Make sure that your username and password are correct
4. If not, you will be prompted to the account menu again
5. If correct, you will be prompted to the main menu

#### Creating an account 
1. Run the program and you will be prompted to the account menu
2. Type in your username and password that you want to create
3. After creating an account, you will be prompted to the account menu again where you can login to your account

#### Exiting the program
1. Run the program and you will be prompted to the account menu
2. Choose the exit option (option 3) and the program will exit

#### Viewing a task
1. Login to your account and you will be prompted to the main menu
2. Choose the option for viewing a schedule (option 1)
3. After that, you will be prompted to choose between viewing a task or an event
4. Choose the option for viewing a task (option 1)
5. The program will display the list of tasks

#### Viewing an event
1. Login to your account and you will be prompted to the main menu
2. Choose the option for viewing a schedule (option 1)
3. After that, you will be prompted to choose between viewing a task or an event
4. Choose the option for viewing an event (option 2)
5. The program will display the list of events

#### Adding a task
1. Login to your account and you will be prompted to the main menu
2. Choose the option for adding a schedule (option 2)
3. After that, you will be prompted to choose between adding a task or an event
4. Choose the option for adding a task (option 1)
5. You will be prompted to enter the date, day, description, and status of the task
6. The program will display that the task has been added

#### Adding an event
1. Login to your account and you will be prompted to the main menu
2. Choose the option for adding a schedule (option 2)
3. After that, you will be prompted to choose between adding a task or an event
4. Choose the option for adding an event (option 2)
5. You will be prompted to enter the date, day, description, location, and status of the event
6. The program will display that the event has been added

#### Editing the date of a task
1. Login to your account and you will be prompted to the main menu
2. Choose the option for editing a schedule (option 3)
3. After that, you will be prompted to choose between editing a task or an event
4. Choose the option for editing a task (option 1)
5. You will be prompted to enter the unique TaskID of the task that you want to edit
6. Choose the option for editing the date of the task (option 1)
7. Enter the new date of the task
8. The program will display that the date of the task has been edited

#### Editing the day of a task
1. Login to your account and you will be prompted to the main menu
2. Choose the option for editing a schedule (option 3)
3. After that, you will be prompted to choose between editing a task or an event
4. Choose the option for editing a task (option 1)
5. You will be prompted to enter the unique TaskID of the task that you want to edit
6. Choose the option for editing the day of the task (option 2)
7. Enter the new day of the task
8. The program will display that the day of the task has been edited

#### Editing the description of a task
1. Login to your account and you will be prompted to the main menu
2. Choose the option for editing a schedule (option 3)
3. After that, you will be prompted to choose between editing a task or an event
4. Choose the option for editing a task (option 1)
5. You will be prompted to enter the unique TaskID of the task that you want to edit
6. Choose the option for editing the description of the task (option 3)
7. Enter the new description of the task
8. The program will display that the description of the task has been edited

#### Editing the due date of a task
1. Login to your account and you will be prompted to the main menu
2. Choose the option for editing a schedule (option 3)
3. After that, you will be prompted to choose between editing a task or an event
4. Choose the option for editing a task (option 1)
5. You will be prompted to enter the unique TaskID of the task that you want to edit
6. Choose the option for editing the due date of the task (option 4)
7. Enter the new due date of the task
8. The program will display that the due date of the task has been edited

#### Editing the status of a task
1. Login to your account and you will be prompted to the main menu
2. Choose the option for editing a schedule (option 3)
3. After that, you will be prompted to choose between editing a task or an event
4. Choose the option for editing a task (option 1)
5. You will be prompted to enter the unique TaskID of the task that you want to edit
6. Choose the option for editing the status of the task (option 4)
7. Enter the new status of the task
8. The program will display that the status of the task has been edited

#### Editing the date of an event
1. Login to your account and you will be prompted to the main menu
2. Choose the option for editing a schedule (option 3)
3. After that, you will be prompted to choose between editing a task or an event
4. Choose the option for editing an event (option 2)
5. You will be prompted to enter the unique EventID of the event that you want to edit
6. Choose the option for editing the date of the event (option 1)
7. Enter the new date of the event
8. The program will display that the date of the event has been edited

#### Editing the day of an event
1. Login to your account and you will be prompted to the main menu
2. Choose the option for editing a schedule (option 3)
3. After that, you will be prompted to choose between editing a task or an event
4. Choose the option for editing an event (option 2)
5. You will be prompted to enter the unique EventID of the event that you want to edit
6. Choose the option for editing the day of the event (option 2)
7. Enter the new day of the event
8. The program will display that the day of the event has been edited

#### Editing the description of an event
1. Login to your account and you will be prompted to the main menu
2. Choose the option for editing a schedule (option 3)
3. After that, you will be prompted to choose between editing a task or an event
4. Choose the option for editing an event (option 2)
5. You will be prompted to enter the unique EventID of the event that you want to edit
6. Choose the option for editing the description of the event (option 3)
7. Enter the new description of the event
8. The program will display that the description of the event has been edited

#### Editing the location of an event
1. Login to your account and you will be prompted to the main menu
2. Choose the option for editing a schedule (option 3)
3. After that, you will be prompted to choose between editing a task or an event
4. Choose the option for editing an event (option 2)
5. You will be prompted to enter the unique EventID of the event that you want to edit
6. Choose the option for editing the location of the event (option 4)
7. Enter the new location of the event
8. The program will display that the location of the event has been edited

#### Editing the status of an event
1. Login to your account and you will be prompted to the main menu
2. Choose the option for editing a schedule (option 3)
3. After that, you will be prompted to choose between editing a task or an event
4. Choose the option for editing an event (option 2)
5. You will be prompted to enter the unique EventID of the event that you want to edit
6. Choose the option for editing the status of the event (option 5)
7. Enter the new status of the event
8. The program will display that the status of the event has been edited

#### Deleting a task
1. Login to your account and you will be prompted to the main menu
2. Choose the option for deleting a schedule (option 4)
3. After that, you will be prompted to choose between deleting a task or an event   
4. Choose the option for deleting a task (option 1)
5. You will be prompted to enter the unique TaskID of the task that you want to delete
6. The program will confirm if you want to delete the task
7. If yes, the program will display that the task has been deleted
8. If no, the program will display that the task has not been deleted

#### Deleting an event
1. Login to your account and you will be prompted to the main menu
2. Choose the option for deleting a schedule (option 4)
3. After that, you will be prompted to choose between deleting a task or an event
4. Choose the option for deleting an event (option 2)
5. You will be prompted to enter the unique EventID of the event that you want to delete
6. The program will confirm if you want to delete the event
7. If yes, the program will display that the event has been deleted
8. If no, the program will display that the event has not been deleted