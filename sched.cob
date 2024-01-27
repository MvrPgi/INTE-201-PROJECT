      ********************************************************************
      *                        COBOLPROJECT                              *
      *                        TALA-ADLAWAN                              *
      *Our Note Taker, crafted by the talented team of  Brian Sebastian, *
      *Franchezka Tingal, Heart Trinidad, Jermagne Nofre, John Vincent   *
      *Agas Modelo, Lhera Ashlyn B. Puning, Alwyn John Mercene, and      *
      *Maurice Lago in the Cobol programming language, is a robust and   *
      *efficient solution for capturing and organizing notes. With their *
      *collaborative effort, this note-taking system offers a seamless   *
      *experience for users, combining the power of Cobol with the unique*
      *skills and insights of its creators.                              *
      ********************************************************************


      *******************THE METADATA FOR OUR PROGRAM*******************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TALA-ADLAWAN.
       AUTHOR. WRITTEN BY GROUP 1.
       DATE-WRITTEN. 12-01-2023.
       DATE-COMPILED. 12-18-2023.
       SECURITY. ONLY AVAILABLE FOR PEOPLE GRANTED ACCESS BY OUR GROUP.
       REMARKS. CONTACT OUR TEAM FOR ANY BUGS OR ERRORS ENCOUNTERED.

      *******FILES USED FOR PROCESSING INPUT AND GENERATING OUTPUT******
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TaskFile ASSIGN TO "TaskFile.dat"                    
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT EventFile ASSIGN TO "EventFile.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TempTaskFile ASSIGN TO "TempTaskFile.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TempEventFile ASSIGN TO "TempEventFile.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT AccountFile ASSIGN TO "AccountFile.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OutputFile ASSIGN TO "OutputFile.dat"
               ORGANIZATION IS LINE SEQUENTIAL.                            
               
      *************FILE DEFINITION FOR ALL THE FILES NEEDED*************
       DATA DIVISION.
       FILE SECTION.
       FD TaskFile.                                                     *> FD FOR THE TASK FILE
       01 TaskRecord.
           05 TaskID               PIC 9(3).
           05 FILLER               PIC X(1) VALUE SPACES.
           05 TaskDate             PIC X(8).
           05 FILLER               PIC X(1) VALUE SPACES.
           05 TaskDay              PIC X(9).
           05 FILLER               PIC X(1) VALUE SPACES.
           05 TaskDescription      PIC X(25).
           05 FILLER               PIC X(1) VALUE SPACES.
           05 TaskDueDate          PIC X(8).
           05 FILLER               PIC X(1) VALUE SPACES.
           05 TaskStatus           PIC X(10).

       FD EventFile.                                                    *> FD FOR THE EVENT FILE
       01 EventRecord.
           05 EventID              PIC 9(3).
           05 FILLER               PIC X(1) VALUE SPACES.
           05 EventDate            PIC X(8).
           05 FILLER               PIC X(1) VALUE SPACES.
           05 EventDay             PIC X(9).
           05 FILLER               PIC X(1) VALUE SPACES.
           05 EventDescription     PIC X(25).
           05 FILLER               PIC X(1) VALUE SPACES.
           05 EventLocation        PIC X(20).
           05 FILLER               PIC X(1) VALUE SPACES.
           05 EventStatus          PIC X(10).

       FD TempTaskFile.                                                 *> FD FOR THE TEMPORARY TASK FILE
       01 TempTaskRecord.
           05 TempTaskID           PIC 9(3).
           05 FILLER               PIC X(1) VALUE SPACES.
           05 TempTaskDate         PIC X(8).
           05 FILLER               PIC X(1) VALUE SPACES.
           05 TempTaskDay          PIC X(9).
           05 FILLER               PIC X(1) VALUE SPACES.
           05 TempTaskDescription  PIC X(25).
           05 FILLER               PIC X(1) VALUE SPACES.
           05 TempTaskDueDate      PIC X(8).
           05 FILLER               PIC X(1) VALUE SPACES.
           05 TempTaskStatus       PIC X(10).
       
       FD TempEventFile.                                                *> FD FOR THE TEMPORARY EVENT FILE
       01 TempEventRecord.
           05 TempEventID          PIC 9(3).
           05 FILLER               PIC X(1) VALUE SPACES.
           05 TempEventDate        PIC X(8).
           05 FILLER               PIC X(1) VALUE SPACES.
           05 TempEventDay         PIC X(9).
           05 FILLER               PIC X(1) VALUE SPACES.
           05 TempEventDescription PIC X(25).
           05 FILLER               PIC X(1) VALUE SPACES.
           05 TempEventLocation    PIC X(20).
           05 FILLER               PIC X(1) VALUE SPACES.
           05 TempEventStatus      PIC X(10).
       
       FD AccountFile.                                                  *> FD FOR THE ACCOUNT FILE
       01 AccountFileRecord.
           05 UsernameSignup       PIC X(20).
           05 PasswordSignup       PIC X(20).

       FD OutputFile.                                                   *> FD FOR THE GENERATED OUTPUT
       01 OutputRecord             PIC X(900).

      *******************VARIABLES USED ON OUR PROGRAM******************
       WORKING-STORAGE SECTION.
       01  UserChoice              PIC X(1).
       01  EOF                     PIC X VALUE 'N'.
       01  Username                PIC X(20).
       01  Password                PIC X(20).
       01  AccountFound            PIC X VALUE 'N'.
       01  TaskIDInput             PIC X(3).
       01  EventIDInput            PIC X(3).
       01  ConfirmDeletion         PIC X(1).
       01  EditOption              PIC X(1).
       01  LoginAttempts           PIC 9(1) VALUE '1'.

      ****************VARIABLES FOR GENERATING THE OUTPUT***************
       01 Newline.                                                      
           05 FILLER               PIC X(1) VALUE SPACES.

       01 TaskTitle.
           05 FILLER               PIC X(22) VALUE SPACES.
           05 TaskTitle-TT         PIC X(25) VALUE 
           "TASK SCHEDULE REPORT".

       01 TaskCol.
           05 TaskIDCol            PIC X(6) VALUE "ID".
           05 TaskDateCol          PIC X(11) VALUE "DATE".
           05 TaskDayCol           PIC X(13) VALUE "DAY".
           05 TaskDescriptionCol   PIC X(28) VALUE "DESCRIPTION".
           05 TaskDueDateCol       PIC X(11) VALUE "DUE DATE".
           05 TaskStatusCol        PIC X(10) VALUE "STATUS".

       01 TaskBody.
           05 TaskIDBody           PIC 9(3).
           05 FILLER               PIC X(3) VALUE SPACES.
           05 TaskDateBody         PIC X(8).
           05 FILLER               PIC X(3) VALUE SPACES.
           05 TaskDayBody          PIC X(10).
           05 FILLER               PIC X(3) VALUE SPACES.
           05 TaskDescriptionBody  PIC X(25).
           05 FILLER               PIC X(3) VALUE SPACES.
           05 TaskDueDateBody      PIC X(8).
           05 FILLER               PIC X(3) VALUE SPACES.
           05 TaskStatusBody       PIC X(10).

       01 EventTitle.
           05 FILLER               PIC X(36) VALUE SPACES.
           05 EventTitle-TT        PIC X(25) VALUE 
           "EVENT SCHEDULE REPORT".

       01 EventCol.
           05 EventIDCol           PIC X(6) VALUE "ID".
           05 EventDateCol         PIC X(11) VALUE "DATE".
           05 EventDayCol          PIC X(13) VALUE "DAY".
           05 EventDescriptionCol  PIC X(28) VALUE "DESCRIPTION".
           05 EventLocationCol     PIC X(28) VALUE "LOCATION".
           05 EventStatusCol       PIC X(10) VALUE "STATUS".

       01 EventBody.
           05 EventIDBody          PIC 9(3).
           05 FILLER               PIC X(3) VALUE SPACES.
           05 EventDateBody        PIC X(8).
           05 FILLER               PIC X(3) VALUE SPACES.
           05 EventDayBody         PIC X(10).
           05 FILLER               PIC X(3) VALUE SPACES.
           05 EventDescriptionBody PIC X(25).
           05 FILLER               PIC X(3) VALUE SPACES.
           05 EventLocationBody    PIC X(25).
           05 FILLER               PIC X(3) VALUE SPACES.
           05 EventStatusBody      PIC X(10).
           

      ********************MAIN PROCESS OF THE PROGRAM*******************
       PROCEDURE DIVISION.
           DISPLAY "****************************************"           *>
           DISPLAY "*             TALA-ADLAWAN             *"           *> TITLE OF OUR PROGRAM
           DISPLAY "****************************************"           *>
           DISPLAY "CREATED BY: GROUP 1"

           PERFORM GenerateReport
           
           PERFORM AccountMenu

           STOP RUN.

       GenerateReport.                                                  *> GENERATE A REPORT OF THE WHOLE SCHEDULE
           PERFORM WriteTaskTitle
           PERFORM WriteNewline
           PERFORM WriteTaskCol
           PERFORM WriteTaskBody

           PERFORM WriteNewline 5 TIMES

           PERFORM WriteEventTitle
           PERFORM WriteNewline
           PERFORM WriteEventCol
           PERFORM WriteEventBody.

       WriteNewline.                                                    *> ADD A NEW LINE FOR THE GENERATED OUTPUT
           OPEN EXTEND OutputFile.
           MOVE Newline TO OutputRecord.
           WRITE OutputRecord.
           CLOSE OutputFile.
           
       WriteTaskTitle.                                                  *> ADD THE TASK TITLE TO THE GENERATED OUPUT
           OPEN OUTPUT OutputFile.
           MOVE TaskTitle TO OutputRecord.
           WRITE OutputRecord.
           CLOSE OutputFile.

       WriteTaskCol.                                                    *> ADD THE COLUMN TITLE FOR THE TASK SCHEDULE
           OPEN EXTEND OutputFile.
           MOVE TaskCol TO OutputRecord.
           WRITE OutputRecord.
           CLOSE OutputFile.
          
       WriteTaskBody.                                                   *> ADD THE CONTENT OF THE TASK SCHEDULE
           OPEN EXTEND OutputFile.
           OPEN INPUT TaskFile.
           MOVE 'N' TO EOF
           PERFORM UNTIL EOF = 'Y'
               READ TaskFile
                   AT END
                       MOVE 'Y' TO EOF
                   NOT AT END
                       MOVE TaskID TO TaskIDBody
                       MOVE TaskDate TO TaskDateBody
                       MOVE TaskDay TO TaskDayBody
                       MOVE TaskDescription TO TaskDescriptionBody
                       MOVE TaskDueDate TO TaskDueDateBody
                       MOVE TaskStatus TO TaskStatusBody

                       MOVE TaskBody TO OutputRecord
                       WRITE OutputRecord
               END-READ
           END-PERFORM
           CLOSE TaskFile.
           CLOSE OutputFile.

       WriteEventTitle.                                                 *> ADD THE EVENT TITLE TO THE GENERATED OUPUT
           OPEN EXTEND OutputFile.
           MOVE EventTitle TO OutputRecord.
           WRITE OutputRecord.
           CLOSE OutputFile.

       WriteEventCol.                                                   *> ADD THE COLUMN TITLE FOR THE EVENT SCHEDULE
           OPEN EXTEND OutputFile.
           MOVE EventCol TO OutputRecord.
           WRITE OutputRecord.
           CLOSE OutputFile.
          
       WriteEventBody.                                                  *> ADD THE CONTENT OF THE EVENT SCHEDULE
           OPEN EXTEND OutputFile.
           OPEN INPUT EventFile.
           MOVE 'N' TO EOF
           PERFORM UNTIL EOF = 'Y'
               READ EventFile
                   AT END
                       MOVE 'Y' TO EOF
                   NOT AT END
                       MOVE EventID TO EventIDBody
                       MOVE EventDate TO EventDateBody
                       MOVE EventDay TO EventDayBody
                       MOVE EventDescription TO EventDescriptionBody
                       MOVE EventLocation TO EventLocationBody
                       MOVE EventStatus TO EventStatusBody

                       MOVE EventBody TO OutputRecord
                       WRITE OutputRecord
               END-READ
           END-PERFORM
           CLOSE EventFile.
           CLOSE OutputFile.


       AccountMenu.                                                     *> ACCOUNT MENU TO CREATE AN ACCOUNT AND LOGIN
           DISPLAY " "
           DISPLAY "--------------ACCOUNT MENU--------------"
           DISPLAY "[1] Create an account"
           DISPLAY "[2] Login"
           DISPLAY "[3] Exit the program"
           DISPLAY "Enter your choice: " WITH NO ADVANCING.
           ACCEPT UserChoice

           EVALUATE UserChoice
               WHEN '1' PERFORM Signup
               WHEN '2' PERFORM Login
               WHEN '3' PERFORM ConfirmExit
               WHEN OTHER 
                   DISPLAY " " 
                   DISPLAY "Invalid Choice"
                   PERFORM AccountMenu
           END-EVALUATE.
       

       Signup.                                                          *> CREATES AN ACOUNT IN THE ACCOUNTS FILE
           DISPLAY " "
           DISPLAY 'Create a username: ' WITH NO ADVANCING.
           ACCEPT UsernameSignup.

           DISPLAY 'Create a password: ' WITH NO ADVANCING.
           ACCEPT PasswordSignup.

           OPEN EXTEND AccountFile.
           WRITE AccountFileRecord.
           CLOSE AccountFile.

           PERFORM AccountMenu.


       Login.                                                           *> CHECKS THE INPUTTED USERNAME AND PASSWORD ON THE ACCOUNTS FILE
           DISPLAY ' '
           DISPLAY 'Enter username: ' WITH NO ADVANCING.
           ACCEPT Username.

           DISPLAY 'Enter password: ' WITH NO ADVANCING.
           ACCEPT Password.
       
           OPEN INPUT AccountFile
           
      *    TO INITIALIZE THE EOF AND ACCCOUNFOUND VARIABLES
           MOVE 'N' TO EOF
           MOVE 'N' TO AccountFound

           PERFORM UNTIL EOF = 'Y'
               READ AccountFile
                   AT END
                       MOVE 'Y' TO EOF
                   NOT AT END
                       IF Username = UsernameSignup 
                       AND Password = PasswordSignup
                           DISPLAY " "
                           DISPLAY 'Login successful.'
                           MOVE 'Y' TO AccountFound
                           CLOSE AccountFile
                           PERFORM MainMenu
                       END-IF
           END-PERFORM

           CLOSE AccountFile.

      *    To make the login attempts only 3 then it will go back to acount menu 
           IF AccountFound = 'N'
               DISPLAY " "
               DISPLAY "Incorrect username or password."
               IF LoginAttempts < 3
                   ADD 1 TO LoginAttempts
                   PERFORM Login
           ELSE
               PERFORM AccountMenu
           END-IF.
           

       MainMenu.                                                        *> MAIN MENU TO VIEW, ADD, EDIT, AND DELETE EITHER TASK OR EVENT SCHEDULE
           DISPLAY " ".
           DISPLAY "---------------MAIN MENU---------------".
           DISPLAY "[1] View Schedule".
           DISPLAY "[2] Add Schedule".
           DISPLAY "[3] Edit Schedule".
           DISPLAY "[4] Delete Schedule".
           DISPLAY "[5] Logout".
           DISPLAY "Enter your choice: " WITH NO ADVANCING.
           ACCEPT UserChoice.

           PERFORM ProcessOption.
           
       ProcessOption.                                                   *> PROCESS THE INPUT OF THE USER
           EVALUATE UserChoice
               WHEN '1' PERFORM ViewSched
               WHEN '2' PERFORM AddSched
               WHEN '3' PERFORM EditSched
               WHEN '4' PERFORM DeleteSched
               WHEN '5' PERFORM AccountMenu
               WHEN OTHER DISPLAY "Invalid Choice"
           END-EVALUATE.
      
       TypeOption.                                                      *> TO CHOOSE BETWEEN TASK OR EVENT 
           DISPLAY " "
           DISPLAY "Select the type you want:"
           DISPLAY "[1] Task"
           DISPLAY "[2] Event"
           DISPLAY "[3] Back to MAIN MENU"
           DISPLAY "Enter your choice: " WITH NO ADVANCING
           Accept UserChoice.
       
                
       ViewSched.
           PERFORM TypeOption.                                          *> LETS THE USER CHOOSE IF TASK OR EVENT

           EVALUATE UserChoice
               WHEN '1' PERFORM ViewTask                                
               WHEN '2' PERFORM ViewEvent
               WHEN '3' PERFORM MainMenu
               WHEN OTHER DISPLAY "Invalid Choice"
           END-EVALUATE. 

           PERFORM MainMenu.


       ViewTask.                                                        *> LET THE USER SEE THE TASK SCHEDULE
           MOVE 'N' TO EOF                                              *> SETTING THE INDICATOR FOR END OF THE FILE
           OPEN INPUT TaskFile.                                      

           DISPLAY " ".
           DISPLAY "Schedule:".        
           PERFORM UNTIL EOF = 'Y'                                     
               READ TaskFile                                            *> IT WILL READ THE WHOLE FILE UNTIL THE EOF IS Y    
                   AT END
                       MOVE 'Y' TO EOF                                  
                   NOT AT END
                       DISPLAY "Task ID: " TaskID                       
                               "  Date: " TaskDate
                               "  Day: " TaskDay
                               "  Task: " TaskDescription
                               "  Due Date: " TaskDueDate
                               "  Status: " TaskStatus
           END-PERFORM.

           CLOSE TaskFile.                                            
           PERFORM MainMenu.                                          

       ViewEvent.                                                       *> LET THE USER SEE THE EVENT SCHEDULE
           MOVE 'N' TO EOF                                                 
           OPEN INPUT EventFile.                                        

           DISPLAY " ".
           DISPLAY "Schedule:".
           PERFORM UNTIL EOF = 'Y'                                     
               READ EventFile                                           
                   AT END
                       MOVE 'Y' TO EOF                                 
                   NOT AT END
                       DISPLAY "Event ID: " EventID                     
                               "  Date: " EventDate
                               "  Day: " EventDay
                               "  Event: " EventDescription
                               "  Location: " EventLocation
                               "  Status: " EventStatus
           END-PERFORM.

           CLOSE EventFile.                                             

           PERFORM MainMenu.                                            


       AddSched.                                                        *> ADD SCHEDULE PROMPT
           PERFORM TypeOption.                                          *> LET THE USER DECIDE IF THEY WILL ADD TO TASK OR EVENT SCHEDULE

           EVALUATE UserChoice
               WHEN '1' PERFORM AddTask                                 
               WHEN '2' PERFORM AddEvent
               WHEN '3' PERFORM MainMenu
               WHEN OTHER DISPLAY "Invalid Choice"
           END-EVALUATE. 

           PERFORM MainMenu.                                            


       AddTask.                                                         *> ADD TASK FUNCTION
           MOVE 'N' TO EOF                                              *> SETTING THE INDICATOR FOR END OF THE FILE
           OPEN INPUT TaskFile.                                 
           PERFORM UNTIL EOF = 'Y'                                     
               READ TaskFile                                            *> IT WILL READ THE WHOLE FILE UNTIL THE EOF IS Y             
                   AT END
                       MOVE 'Y' TO EOF                                  *> ENDING THE LOOP FOR AS IT IS THE END OF THE FILE
                       ADD 1 TO TaskID                                  *> THE LOOP CHECKS THE LAST TASK ID AND INCREMENT IT BY ONE 
           END-PERFORM.                                                 *> WHICH WILL BE THE NEW UNIQUE NAME OF OUR NEW TASK
           CLOSE TaskFile.                                              

           DISPLAY "Enter Task Date (MM-DD-YY):".                          *> USER INPUTS THE DATE, DAY, TASK DESCRIPTION,AND STATUS
           ACCEPT TaskDate.
       
           DISPLAY "Enter Task Day (Monday):".
           ACCEPT TaskDay.

           DISPLAY "Enter Task Description (Math Assignment):".
           ACCEPT TaskDescription.

           DISPLAY "Enter Task Due Date (MM-DD-YY):".
           ACCEPT TaskDueDate.

           DISPLAY "Enter Task Status (Completed):".
           ACCEPT TaskStatus.

           OPEN EXTEND TaskFile.                                        *> ALLOWS THE DATA TO ADD TO THE EXISTING DATA WHILE PRESERVING IT
           WRITE TaskRecord.
           CLOSE TaskFile.                                         

           DISPLAY "Task Added Successfully".

           PERFORM MainMenu.


       AddEvent.                                                        *> ADD EVENT FUNCTION
           MOVE 'N' TO EOF                                              
           OPEN INPUT EventFile.                                       
           PERFORM UNTIL EOF = 'Y'                                     
               READ EventFile                                           
                   AT END
                       MOVE 'Y' TO EOF                                  
                       ADD 1 TO EventID                                 
           END-PERFORM.                                                 
           CLOSE EventFile.                                           

           DISPLAY "Enter Event Date (MM-DD-YY):".     
           ACCEPT EventDate.                    
       
           DISPLAY "Enter Event Day (Monday):".
           ACCEPT EventDay.

           DISPLAY "Enter Event Description (Attending an AWS event.):".
           ACCEPT EventDescription.

           DISPLAY "Enter Event Location (BGC):".
           ACCEPT EventLocation.

           DISPLAY "Enter Event Status (Completed):".
           ACCEPT EventStatus.

           OPEN EXTEND EventFile.                                       
           WRITE EventRecord.
           CLOSE EventFile.                                             

           DISPLAY "Event Added Successfully".

           PERFORM MainMenu.


       EditSched.                                                       *> EDIT SCHED FUNCTION
           PERFORM TypeOption.                                          *> ASK THE USER EITHER TASK OR EVENT

           EVALUATE UserChoice
               WHEN '1' PERFORM EditTask
               WHEN '2' PERFORM EditEvent
               WHEN '3' PERFORM MainMenu
               WHEN OTHER DISPLAY "Invalid Choice"
           END-EVALUATE. 

           PERFORM MainMenu.
      

       EditTask.                                                        *> EDIT TASK FUNCTION
      *    WRITING THE RECONRDS FROM TASK FILE TO TEMPTASKFILE
           DISPLAY "Enter Task ID (999) to edit: "                      *> THE TASK ID WILL ACT AS THE UNIQUE ID OF A RECORD
           ACCEPT TaskIDInput

           DISPLAY "Select the field to edit:"
           DISPLAY "1. Task Date"
           DISPLAY "2. Task Day"
           DISPLAY "3. Task Description"
           DISPLAY "4. Task Due Date"
           DISPLAY "5. Task Status"
           ACCEPT EditOption

           MOVE 'N' TO EOF                                              *> INITIALIZE THE EOF VARIABLE TO REPEAT THE PROCESS

           OPEN INPUT TaskFile.                                         *> INPUT = READING
           OPEN OUTPUT TempTaskFile.                                    *> OUTPUT = WRITING

           PERFORM UNTIL EOF = 'Y'                                      *> READ ALL THE RECORDS IN THE FILE
               READ TaskFile
                   AT END
                       MOVE 'Y' TO EOF
                   NOT AT END
                       IF TaskIDInput = TaskID                          *> IF THE RECORD MATCHES THE INPUT PERFORM THE EDITTASKOPTIONS
                           PERFORM EditTaskOptions
                           WRITE TempTaskRecord
                       ELSE
                           MOVE TaskID TO TempTaskID
                           MOVE TaskDate TO TempTaskDate
                           MOVE TaskDay TO TempTaskDay
                           MOVE TaskDescription TO TempTaskDescription
                           MOVE TaskDueDate TO TempTaskDueDate
                           MOVE TaskStatus TO TempTaskStatus
                           WRITE TempTaskRecord
                       END-IF
           END-PERFORM.

           CLOSE TaskFile.                                              *> CLOSE THE 2 FILES
           CLOSE TempTaskFile.
      
      *    WRITING THE RECORDS FROM THE TEMPTASKFILE TO TASKFILE
           MOVE 'N' TO EOF
           
           OPEN OUTPUT TaskFile.
           OPEN INPUT TempTaskFile.

           PERFORM UNTIL EOF = 'Y'                                      *> COPY ALL THE RECORDS INSIDE THE TEMPFILE BACK TO THE ORIGINAL FILE
               READ TempTaskFile
                   AT END
                       MOVE 'Y' TO EOF                         
                   NOT AT END          
                       MOVE TempTaskID TO TaskID
                       MOVE TempTaskDate TO TaskDate
                       MOVE TempTaskDay TO TaskDay
                       MOVE TempTaskDescription TO TaskDescription
                       MOVE TempTaskDueDate TO TaskDueDate
                       MOVE TempTaskStatus TO TaskStatus
                       WRITE TaskRecord
           END-PERFORM.

           CLOSE TempTaskFile.
           CLOSE TaskFile.

           DISPLAY "Task Updated Successfully".

           PERFORM MainMenu.

       EditTaskOptions.                                                 *> OPTIONS FOR EDITING THE TASK 
           EVALUATE EditOption
               WHEN 1                                                   *> EDIT THE TASK DATE
                   DISPLAY "Enter updated Task Date:"
                   ACCEPT TempTaskDate
                   MOVE TaskID TO TempTaskID
                   MOVE TaskDay TO TempTaskDay
                   MOVE TaskDescription TO TempTaskDescription
                   MOVE TaskDueDate TO TempTaskDueDate
                   MOVE TaskStatus TO TempTaskStatus 
               WHEN 2                                                   *> EDIT THE TASK DAY
                   DISPLAY "Enter updated Task Day:"
                   ACCEPT TempTaskDay
                   MOVE TaskID TO TempTaskID
                   MOVE TaskDate TO TempTaskDate
                   MOVE TaskDescription TO TempTaskDescription
                   MOVE TaskDueDate TO TempTaskDueDate
                   MOVE TaskStatus TO TempTaskStatus 
               WHEN 3                                                   *> EDIT THE TASK DESCRIPTION
                   DISPLAY "Enter updated Task Description:"
                   ACCEPT TempTaskDescription
                   MOVE TaskID TO TempTaskID
                   MOVE TaskDate TO TempTaskDate
                   MOVE TaskDay TO TempTaskDay
                   MOVE TaskDueDate TO TempTaskDueDate
                   MOVE TaskStatus TO TempTaskStatus 
               WHEN 4
                   DISPLAY "Enter updated Task Due Date:"
                   ACCEPT TempTaskDueDate
                   MOVE TaskStatus TO TempTaskStatus
                   MOVE TaskID TO TempTaskID
                   MOVE TaskDate TO TempTaskDate
                   MOVE TaskDay TO TempTaskDay
                   MOVE TaskDescription TO TempTaskDescription
               WHEN 5                                                   *> EDIT THE TASK STATUS
                   DISPLAY "Enter updated Task Status:"
                   ACCEPT TempTaskStatus
                   MOVE TaskID TO TempTaskID
                   MOVE TaskDate TO TempTaskDate
                   MOVE TaskDay TO TempTaskDay
                   MOVE TaskDescription TO TempTaskDescription
                   MOVE TaskDueDate TO TempTaskDueDate
               WHEN OTHER
                   DISPLAY "Invalid option. No updates performed."
           END-EVALUATE.

       
       EditEvent.                                                       *> EDIT THE EVENT    
      *    WRITING THE RECORDS FROM EVENT FILE TO TEMPEVENTFILE    
           DISPLAY "Enter Event ID (999) to edit: "
           ACCEPT EventIDInput

           DISPLAY "Select the field to edit:"
           DISPLAY "1. Event Date"
           DISPLAY "2. Event Day"
           DISPLAY "3. Event Description"
           DISPLAY "4. Event Location"
           DISPLAY "5. Event Status"
           ACCEPT EditOption

           MOVE 'N' TO EOF

           OPEN INPUT EventFile. 
           OPEN OUTPUT TempEventFile.

           PERFORM UNTIL EOF = 'Y'                                      *> READ ALL THE RECORDS FROM THE EVENTFILE
               READ EventFile
                   AT END
                       MOVE 'Y' TO EOF
                   NOT AT END
                       IF EventIDInput = EventID                        *> IF THE INPUT MATCHES THE ID IN THE RECORD PERFORM EDIT OPTIONS
                           PERFORM EditEventOptions
                           WRITE TempEventRecord
                       ELSE
                           MOVE EventID TO TempEventID
                           MOVE EventDate TO TempEventDate
                           MOVE EventDay TO TempEventDay
                           MOVE EventDescription TO TempEventDescription
                           MOVE EventLocation TO TempEventLocation
                           MOVE EventStatus TO TempEventStatus
                           WRITE TempEventRecord
                       END-IF
           END-PERFORM.

           CLOSE EventFile.
           CLOSE TempEventFile.
      
      *    WRITING THE RECORDS FROM TEMPEVENTFILE TO EVENTFILE
           MOVE 'N' TO EOF
           
           OPEN OUTPUT EventFile.
           OPEN INPUT TempEventFile.

           PERFORM UNTIL EOF = 'Y'                                      *> COPY ALL THE RECORDS FROM THE TEMPORARY FILE BACK TO THE ORIGINAL FILE
               READ TempEventFile
                   AT END
                       MOVE 'Y' TO EOF
                   NOT AT END
                       MOVE TempEventID TO EventID
                       MOVE TempEventDate TO EventDate
                       MOVE TempEventDay TO EventDay
                       MOVE TempEventDescription TO EventDescription
                       MOVE TempEventLocation TO EventLocation
                       MOVE TempEventStatus TO EventStatus
                       WRITE EventRecord
           END-PERFORM.

           CLOSE TempEventFile.
           CLOSE EventFile.

           DISPLAY "Event Updated Successfully".

           PERFORM MainMenu.

       EditEventOptions.                                                *> ASK THE USER WHAT THEY WANT TO EDIT IN THE RECORD
           EVALUATE EditOption
               WHEN 1                                                   *> EDIT EVENT DATE
                   DISPLAY "Enter updated Event Date:"
                   ACCEPT TempEventDate
                   MOVE EventID TO TempEventID
                   MOVE EventDay TO TempEventDay
                   MOVE EventDescription TO TempEventDescription
                   MOVE EventLocation TO TempEventLocation
                   MOVE EventStatus TO TempEventStatus 
               WHEN 2                                                   *> EDIT EVENT DAY
                   DISPLAY "Enter updated Event Day:"
                   ACCEPT TempEventDay
                   MOVE EventID TO TempEventID
                   MOVE EventDate TO TempEventDate
                   MOVE EventDescription TO TempEventDescription
                   MOVE EventLocation TO TempEventLocation
                   MOVE EventStatus TO TempEventStatus 
               WHEN 3                                                   *> EDIT EVENT DESCRIPTION
                   DISPLAY "Enter updated Event Description:"
                   ACCEPT TempEventDescription
                   MOVE EventID TO TempEventID
                   MOVE EventDate TO TempEventDate
                   MOVE EventDay TO TempEventDay
                   MOVE EventLocation TO TempEventLocation
                   MOVE EventStatus TO TempEventStatus 
               WHEN 4                                                   *> EDIT EVENT LOCATION
                   DISPLAY "Enter updated Event Location:"
                   ACCEPT TempEventLocation
                   MOVE EventID TO TempEventID
                   MOVE EventDate TO TempEventDate
                   MOVE EventDay TO TempEventDay
                   MOVE EventDescription TO TempEventDescription
                   MOVE EventStatus TO TempEventStatus 
               WHEN 5                                                   *> EDIT EVENT STATUS
                   DISPLAY "Enter updated Event Status:"
                   ACCEPT TempEventStatus
                   MOVE EventID TO TempEventID
                   MOVE EventDate TO TempEventDate
                   MOVE EventDay TO TempEventDay
                   MOVE EventDescription TO TempEventDescription
                   MOVE EventLocation TO TempEventLocation
               WHEN OTHER
                   DISPLAY "Invalid option. No updates performed."
           END-EVALUATE.

   
       DeleteSched.                                                     *> DELETE SCHEDULE PROMPT
           PERFORM TypeOption.                                          *> A REDIRECTION PROMPT TO SEE IF THE USER WOULD LIKE TO DELETE TASK OR EVENT

           EVALUATE UserChoice 
               WHEN '1' PERFORM DeleteTaskConfirmation                  
               WHEN '2' PERFORM DeleteEventConfirmation                 
               WHEN '3' PERFORM MainMenu                                
               WHEN OTHER DISPLAY "Invalid Choice"
           END-EVALUATE. 

           PERFORM MainMenu.


       DeleteTaskConfirmation.                                          
           DISPLAY "Enter Task ID to delete:".                          *> ASK THE TASK ID FROM THE USER
           ACCEPT TaskIDInput.                                          *> ACCEPT THE ID TO BE DELETED

           DISPLAY "Do you want to continue with the deletion? (Y/N): ".
           ACCEPT UserChoice.

           IF UserChoice = 'Y' OR UserChoice = 'y'                      
               PERFORM DeleteTask                                       
           ELSE
               DISPLAY "Deletion canceled."                             
           END-IF.                         

       DeleteTask.                                                      *> DELETE TASK FUNCTION
           OPEN INPUT TaskFile.                                         
           OPEN OUTPUT TempTaskFile.

           MOVE 'N' TO EOF.                                             *> SETTING THE INDICATOR FOR END OF THE FILE

           PERFORM UNTIL EOF = 'Y'                                     
               READ TaskFile                                            *> IT WILL READ THE WHOLE FILE UNTIL THE EOF IS Y
           AT END
               MOVE 'Y' TO EOF                                          *> ENDING THE LOOP FOR AS IT IS THE END OF THE FILE
           NOT AT END
               IF TaskIDInput = TaskID                                  *> CHECKS IF THE TASK INPUT MATCH TO THE RECORDS
                   DISPLAY "Task Deleted: " TaskIDInput
               ELSE
                   MOVE TaskID TO TempTaskID                            *> MOVING THE TASK DETAILS TO TEMPORARY VARIABLES
                   MOVE TaskDate TO TempTaskDate
                   MOVE TaskDay TO TempTaskDay
                   MOVE TaskDescription TO TempTaskDescription
                   MOVE TaskDueDate TO TempTaskDueDate
                   MOVE TaskStatus TO TempTaskStatus
                   WRITE TempTaskRecord                                 *> WRITING THE NON-DELETED VARIABLE(task) TO TEMPORARY FILE
               END-IF
           END-PERFORM.

           CLOSE TaskFile.                                             
           CLOSE TempTaskFile.

           MOVE 'N' TO EOF.                                             *> TO ENSURE THE RESET OF END OF FILE INDICATOR

           OPEN OUTPUT TaskFile.                                        
           OPEN INPUT TempTaskFile.

           PERFORM UNTIL EOF = 'Y'                                     
               READ TempTaskFile                                        *> IT WILL READ THE WHOLE FILE UNTIL THE EOF IS Y AT END
                   AT END
                       MOVE 'Y' TO EOF                                  *> ENDING THE LOOP FOR AS IT IS THE END OF THE FILE
                   NOT AT END
                       MOVE TempTaskID TO TaskID                        *> TRANSFERING THE NON-DELETED VARIABLE(task) TO TASKFILE(original)
                       MOVE TempTaskDate TO TaskDate
                       MOVE TempTaskDay TO TaskDay
                       MOVE TempTaskDescription TO TaskDescription
                       MOVE TempTaskDueDate TO TaskDueDate
                       MOVE TempTaskStatus TO TaskStatus
                       WRITE TaskRecord
           END-PERFORM.

           CLOSE TempTaskFile.                                        
           CLOSE TaskFile.

           DISPLAY "Task Deleted Successfully".                        

           PERFORM MainMenu.


       DeleteEventConfirmation.                                         *> ASK THE USER FOR CONFIRMATION ON THE DELETE
           DISPLAY "Enter Event ID to delete:".
           ACCEPT EventIDInput.                                         

           DISPLAY "Do you want to continue with the deletion? (Y/N): ".
           ACCEPT UserChoice.

           IF UserChoice = 'Y' OR UserChoice = 'y'                      *
               PERFORM DeleteEvent                                      
           ELSE
               DISPLAY "Deletion canceled."                             
           END-IF.
           
       DeleteEvent.                                                     *> DELETE EVENT FUNCTION
           OPEN INPUT EventFile.                                      
           OPEN OUTPUT TempEventFile.

           MOVE 'N' TO EOF.                                             
       
           PERFORM UNTIL EOF = 'Y'                                      
               READ EventFile                                           
           AT END
               MOVE 'Y' TO EOF                                          
           NOT AT END
               IF EventIDInput = EventID                                *> DELETE THE RECORD IF IT MATCHES THE INPUTTED ID
                   DISPLAY "Event Deleted: " EventIDInput          
               ELSE
                   MOVE EventID TO TempEventID                          
                   MOVE EventDate TO TempEventDate
                   MOVE EventDay TO TempEventDay
                   MOVE EventDescription TO TempEventDescription
                   MOVE EventLocation TO TempEventLocation
                   MOVE EventStatus TO TempEventStatus
                   WRITE TempEventRecord                                
               END-IF
           END-PERFORM.

           CLOSE EventFile.                                             
           CLOSE TempEventFile.

           MOVE 'N' TO EOF.                                             

           OPEN OUTPUT EventFile.                                      
           OPEN INPUT TempEventFile.

           PERFORM UNTIL EOF = 'Y'                                      *> WRITE THE TEMPORARY FILE WITH OMMITED RECORD BACK TO THE ORIGINAL FILE
               READ TempEventFile                                       
                   AT END
                       MOVE 'Y' TO EOF                                                           
                   NOT AT END
                       MOVE TempEventID TO EventID                      
                       MOVE TempEventDate TO EventDate
                       MOVE TempEventDay TO EventDay
                       MOVE TempEventDescription TO EventDescription
                       MOVE TempEventLocation TO EventLocation
                       MOVE TempEventStatus TO EventStatus
                       WRITE EventRecord
           END-PERFORM.

           CLOSE TempEventFile.                                         
           CLOSE EventFile.

           DISPLAY "Event Deleted Successfullly".                       

           PERFORM MainMenu.                                            

      
       ConfirmExit.                                                     *> THIS LET THE USER IF THE FUNCTION WILL CONTINUE OR NOT
           DISPLAY "Do you want to exit? (Y/N):".                       
           ACCEPT UserChoice.                                           
        
           IF UserChoice = 'Y' OR UserChoice = 'y'                      
               PERFORM GenerateReport.                                  *> UPDATE THE OUTPUT FILE
               DISPLAY "Exiting Schedule Maker. Thank you!"
               STOP RUN                                                                 
           EXIT.


      *    features to add:
      *    -3 attempts only for logging in (done)
      *    -add spaces dun sa input file (pra maganda tignan) (done)
      *    -add due date in task schedule (done)
      *    -change the date of the schedules, add year (done)


