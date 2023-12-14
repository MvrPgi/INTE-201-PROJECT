       IDENTIFICATION DIVISION.
       PROGRAM-ID. ScheduleMaker.
       AUTHOR. GROUP 1.
       DATE-WRITTEN. DECEMBER 1, 2023.

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
               

       DATA DIVISION.
       FILE SECTION.
       FD TaskFile.
       01 TaskRecord.
           05 TaskID               PIC 9(3).
           05 TaskDate             PIC X(5).
           05 TaskDay              PIC X(10).
           05 TaskDescription      PIC X(25).
           05 TaskStatus           PIC X(10).

       FD EventFile.
       01 EventRecord.
           05 EventID              PIC 9(3).
           05 EventDate            PIC X(5).
           05 EventDay             PIC X(10).
           05 EventDescription     PIC X(25).
           05 EventLocation        PIC X(25).
           05 EventStatus          PIC X(10).

       FD TempTaskFile.
       01 TempTaskRecord.
           05 TempTaskID           PIC 9(3).
           05 TempTaskDate         PIC X(5).
           05 TempTaskDay          PIC X(10).
           05 TempTaskDescription  PIC X(25).
           05 TempTaskStatus       PIC X(10).
       
       FD TempEventFile.
       01 TempEventRecord.
           05 TempEventID          PIC 9(3).
           05 TempEventDate        PIC X(5).
           05 TempEventDay         PIC X(10).
           05 TempEventDescription PIC X(25).
           05 TempEventLocation    PIC X(25).
           05 TempEventStatus      PIC X(10).
       
       FD AccountFile.
       01 AccountFileRecord.
           05 UsernameSignup       PIC X(20).
           05 PasswordSignup       PIC X(20).

       FD OutputFile.
       01 OutputRecord             PIC X(900).

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

       01 Newline.
           05 FILLER               PIC X(1) VALUE SPACES.

       01 TaskTitle.
           05 FILLER               PIC X(22) VALUE SPACES.
           05 TaskTitle-TT         PIC X(25) VALUE 
           "TASK SCHEDULE REPORT".

       01 TaskCol.
           05 TaskIDCol            PIC X(6) VALUE "ID".
           05 TaskDateCol          PIC X(8) VALUE "DATE".
           05 TaskDayCol           PIC X(13) VALUE "DAY".
           05 TaskDescriptionCol   PIC X(28) VALUE "DESCRIPTION".
           05 TaskStatusCol        PIC X(10) VALUE "STATUS".

       01 TaskBody.
           05 TaskIDBody           PIC 9(3).
           05 FILLER               PIC X(3) VALUE SPACES.
           05 TaskDateBody         PIC X(5).
           05 FILLER               PIC X(3) VALUE SPACES.
           05 TaskDayBody          PIC X(10).
           05 FILLER               PIC X(3) VALUE SPACES.
           05 TaskDescriptionBody  PIC X(25).
           05 FILLER               PIC X(3) VALUE SPACES.
           05 TaskStatusBody       PIC X(10).

       01 EventTitle.
           05 FILLER               PIC X(36) VALUE SPACES.
           05 EventTitle-TT        PIC X(25) VALUE 
           "EVENT SCHEDULE REPORT".

       01 EventCol.
           05 EventIDCol           PIC X(6) VALUE "ID".
           05 EventDateCol         PIC X(8) VALUE "DATE".
           05 EventDayCol          PIC X(13) VALUE "DAY".
           05 EventDescriptionCol  PIC X(28) VALUE "DESCRIPTION".
           05 EventLocationCol     PIC X(28) VALUE "LOCATION".
           05 EventStatusCol       PIC X(10) VALUE "STATUS".

       01 EventBody.
           05 EventIDBody          PIC 9(3).
           05 FILLER               PIC X(3) VALUE SPACES.
           05 EventDateBody        PIC X(5).
           05 FILLER               PIC X(3) VALUE SPACES.
           05 EventDayBody         PIC X(10).
           05 FILLER               PIC X(3) VALUE SPACES.
           05 EventDescriptionBody PIC X(25).
           05 FILLER               PIC X(3) VALUE SPACES.
           05 EventLocationBody    PIC X(25).
           05 FILLER               PIC X(3) VALUE SPACES.
           05 EventStatusBody      PIC X(10).
           
           
       PROCEDURE DIVISION.
           DISPLAY "****************************************"
           DISPLAY "*             TALA-ADLAWAN             *"
           DISPLAY "****************************************"
           DISPLAY "CREATED BY: GROUP 1"

           PERFORM GenerateReport
           
           PERFORM AccountMenu

           STOP RUN.

       GenerateReport.
           PERFORM WriteTaskTitle
           PERFORM WriteNewline
           PERFORM WriteTaskCol
           PERFORM WriteTaskBody

           PERFORM WriteNewline 5 TIMES

           PERFORM WriteEventTitle
           PERFORM WriteNewline
           PERFORM WriteEventCol
           PERFORM WriteEventBody.

       WriteNewline.
           OPEN EXTEND OutputFile.
           MOVE Newline TO OutputRecord.
           WRITE OutputRecord.
           CLOSE OutputFile.
           
       WriteTaskTitle.
           OPEN OUTPUT OutputFile.
           MOVE TaskTitle TO OutputRecord.
           WRITE OutputRecord.
           CLOSE OutputFile.

       WriteTaskCol.
           OPEN EXTEND OutputFile.
           MOVE TaskCol TO OutputRecord.
           WRITE OutputRecord.
           CLOSE OutputFile.
          
       WriteTaskBody.
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
                       MOVE TaskStatus TO TaskStatusBody

                       MOVE TaskBody TO OutputRecord
                       WRITE OutputRecord
               END-READ
           END-PERFORM
           CLOSE TaskFile.
           CLOSE OutputFile.

       WriteEventTitle.
           OPEN EXTEND OutputFile.
           MOVE EventTitle TO OutputRecord.
           WRITE OutputRecord.
           CLOSE OutputFile.

       WriteEventCol.
           OPEN EXTEND OutputFile.
           MOVE EventCol TO OutputRecord.
           WRITE OutputRecord.
           CLOSE OutputFile.
          
       WriteEventBody.
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


       AccountMenu.
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
       

       Signup.
           DISPLAY " "
           DISPLAY 'Create a username: ' WITH NO ADVANCING.
           ACCEPT UsernameSignup.

           DISPLAY 'Create a password: ' WITH NO ADVANCING.
           ACCEPT PasswordSignup.

           OPEN EXTEND AccountFile.
           WRITE AccountFileRecord.
           CLOSE AccountFile.

           PERFORM AccountMenu.


       Login.
           DISPLAY ' '
           DISPLAY 'Enter username: ' WITH NO ADVANCING.
           ACCEPT Username.

           DISPLAY 'Enter password: ' WITH NO ADVANCING.
           ACCEPT Password.
       
           OPEN INPUT AccountFile
           
      *    PARA MAINITIALIZE UNG VALUES
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

           IF AccountFound = 'N'
               DISPLAY " "
               DISPLAY "Incorrect username or password."
               PERFORM Login
           END-IF.

           

       MainMenu.
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
           
       ProcessOption.
           EVALUATE UserChoice
               WHEN '1' PERFORM ViewSched
               WHEN '2' PERFORM AddSched
               WHEN '3' PERFORM EditSched
               WHEN '4' PERFORM DeleteSched
               WHEN '5' PERFORM AccountMenu
               WHEN OTHER DISPLAY "Invalid Choice"
           END-EVALUATE.
      
       TypeOption.
           DISPLAY " "
           DISPLAY "Select the type you want:"
           DISPLAY "[1] Task"
           DISPLAY "[2] Event"
           DISPLAY "[3] Back to MAIN MENU"
           DISPLAY "Enter your choice: " WITH NO ADVANCING
           Accept UserChoice.
       
                
       ViewSched.
           PERFORM TypeOption.

           EVALUATE UserChoice
               WHEN '1' PERFORM ViewTask
               WHEN '2' PERFORM ViewEvent
               WHEN '3' PERFORM MainMenu
               WHEN OTHER DISPLAY "Invalid Choice"
           END-EVALUATE. 

           PERFORM MainMenu.


       ViewTask.
           MOVE 'N' TO EOF
           OPEN INPUT TaskFile.

           DISPLAY " ".
           DISPLAY "Schedule:".
           PERFORM UNTIL EOF = 'Y'
               READ TaskFile
                   AT END
                       MOVE 'Y' TO EOF
                   NOT AT END
                       DISPLAY "Task ID: " TaskID
                               " Date: " TaskDate
                               " Day: " TaskDay
                               " Task: " TaskDescription
                               " Status: " TaskStatus
           END-PERFORM.

           CLOSE TaskFile.

           PERFORM MainMenu.


       ViewEvent.
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
                               " Date: " EventDate
                               " Day: " EventDay
                               " Event: " EventDescription
                               "Location: " EventLocation
                               " Status: " EventStatus
           END-PERFORM.

           CLOSE EventFile.

           PERFORM MainMenu.


       AddSched.
           PERFORM TypeOption.

           EVALUATE UserChoice
               WHEN '1' PERFORM AddTask
               WHEN '2' PERFORM AddEvent
               WHEN '3' PERFORM MainMenu
               WHEN OTHER DISPLAY "Invalid Choice"
           END-EVALUATE. 

           PERFORM MainMenu.


       AddTask.
      *    Get the last id in the list and increment 1
           MOVE 'N' TO EOF
           OPEN INPUT TaskFile.
           PERFORM UNTIL EOF = 'Y'
               READ TaskFile
                   AT END
                       MOVE 'Y' TO EOF
                       ADD 1 TO TaskID              
           END-PERFORM.
           CLOSE TaskFile.

           DISPLAY "Enter Task Date (MM-DD):".
           ACCEPT TaskDate.
       
           DISPLAY "Enter Task Day (Monday):".
           ACCEPT TaskDay.

           DISPLAY "Enter Task Description:".
           ACCEPT TaskDescription.

           DISPLAY "Enter Task Status:".
           ACCEPT TaskStatus.

           OPEN EXTEND TaskFile.
           WRITE TaskRecord.
           CLOSE TaskFile.

           DISPLAY "Task Added Successfully".

           PERFORM MainMenu.


       AddEvent.
      *    Get the last id in the list and increment 1
           MOVE 'N' TO EOF
           OPEN INPUT EventFile.
           PERFORM UNTIL EOF = 'Y'
               READ EventFile
                   AT END
                       MOVE 'Y' TO EOF
                       ADD 1 TO EventID              
           END-PERFORM.
           CLOSE EventFile.

           DISPLAY "Enter Event Date (MM-DD):".
           ACCEPT EventDate.
       
           DISPLAY "Enter Event Day (Monday):".
           ACCEPT EventDay.

           DISPLAY "Enter Event Description (Attending an AWS event.):".
           ACCEPT EventDescription.

           DISPLAY "Enter Event Location (BGC):".
           ACCEPT EventLocation.

           DISPLAY "Enter Event Status:".
           ACCEPT EventStatus.

           OPEN EXTEND EventFile.
           WRITE EventRecord.
           CLOSE EventFile.

           DISPLAY "Event Added Successfully".

           PERFORM MainMenu.


       EditSched.
           PERFORM TypeOption.

           EVALUATE UserChoice
               WHEN '1' PERFORM EditTask
               WHEN '2' PERFORM EditEvent
               WHEN '3' PERFORM MainMenu
               WHEN OTHER DISPLAY "Invalid Choice"
           END-EVALUATE. 

           PERFORM MainMenu.
      

       EditTask.
      *    Writing the records from TaskFile to TempTaskFile
           DISPLAY "Enter Task ID (999) to edit: "
           ACCEPT TaskIDInput

           DISPLAY "Select the field to edit:"
           DISPLAY "1. Task Date"
           DISPLAY "2. Task Day"
           DISPLAY "3. Task Description"
           DISPLAY "4. Task Status"
           ACCEPT EditOption

           MOVE 'N' TO EOF

           OPEN INPUT TaskFile. 
           OPEN OUTPUT TempTaskFile.

           PERFORM UNTIL EOF = 'Y'
               READ TaskFile
                   AT END
                       MOVE 'Y' TO EOF
                   NOT AT END
                       IF TaskIDInput = TaskID
                           PERFORM EditTaskOptions
                           WRITE TempTaskRecord
                       ELSE
                           MOVE TaskID TO TempTaskID
                           MOVE TaskDate TO TempTaskDate
                           MOVE TaskDay TO TempTaskDay
                           MOVE TaskDescription TO TempTaskDescription
                           MOVE TaskStatus TO TempTaskStatus
                           WRITE TempTaskRecord
                       END-IF
           END-PERFORM.

           CLOSE TaskFile.
           CLOSE TempTaskFile.
      
      *    Writing the records from Temptaskfile to TaskFile
           MOVE 'N' TO EOF
           
           OPEN OUTPUT TaskFile.
           OPEN INPUT TempTaskFile.

           PERFORM UNTIL EOF = 'Y'
               READ TempTaskFile
                   AT END
                       MOVE 'Y' TO EOF
                   NOT AT END
                       MOVE TempTaskID TO TaskID
                       MOVE TempTaskDate TO TaskDate
                       MOVE TempTaskDay TO TaskDay
                       MOVE TempTaskDescription TO TaskDescription
                       MOVE TempTaskStatus TO TaskStatus
                       WRITE TaskRecord
           END-PERFORM.

           CLOSE TempTaskFile.
           CLOSE TaskFile.

           DISPLAY "Task Updated Successfully".

           PERFORM MainMenu.

       EditTaskOptions.
           EVALUATE EditOption
               WHEN 1
                   DISPLAY "Enter updated Task Date:"
                   ACCEPT TempTaskDate
                   MOVE TaskID TO TempTaskID
                   MOVE TaskDay TO TempTaskDay
                   MOVE TaskDescription TO TempTaskDescription
                   MOVE TaskStatus TO TempTaskStatus 
               WHEN 2
                   DISPLAY "Enter updated Task Day:"
                   ACCEPT TempTaskDay
                   MOVE TaskID TO TempTaskID
                   MOVE TaskDate TO TempTaskDate
                   MOVE TaskDescription TO TempTaskDescription
                   MOVE TaskStatus TO TempTaskStatus 
               WHEN 3
                   DISPLAY "Enter updated Task Description:"
                   ACCEPT TempTaskDescription
                   MOVE TaskID TO TempTaskID
                   MOVE TaskDate TO TempTaskDate
                   MOVE TaskDay TO TempTaskDay
                   MOVE TaskStatus TO TempTaskStatus 
               WHEN 4
                   DISPLAY "Enter updated Task Status:"
                   ACCEPT TempTaskStatus
                   MOVE TaskID TO TempTaskID
                   MOVE TaskDate TO TempTaskDate
                   MOVE TaskDay TO TempTaskDay
                   MOVE TaskDescription TO TempTaskDescription
               WHEN OTHER
                   DISPLAY "Invalid option. No updates performed."
           END-EVALUATE.

       
       EditEvent.
      *    Writing the records from EventFile to TempEventFile
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

           PERFORM UNTIL EOF = 'Y'
               READ EventFile
                   AT END
                       MOVE 'Y' TO EOF
                   NOT AT END
                       IF EventIDInput = EventID
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
      
      *    Writing the records from TempEventfile to EventFile
           MOVE 'N' TO EOF
           
           OPEN OUTPUT EventFile.
           OPEN INPUT TempEventFile.

           PERFORM UNTIL EOF = 'Y'
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

       EditEventOptions.
           EVALUATE EditOption
               WHEN 1
                   DISPLAY "Enter updated Event Date:"
                   ACCEPT TempEventDate
                   MOVE EventID TO TempEventID
                   MOVE EventDay TO TempEventDay
                   MOVE EventDescription TO TempEventDescription
                   MOVE EventLocation TO TempEventLocation
                   MOVE EventStatus TO TempEventStatus 
               WHEN 2
                   DISPLAY "Enter updated Event Day:"
                   ACCEPT TempEventDay
                   MOVE EventID TO TempEventID
                   MOVE EventDate TO TempEventDate
                   MOVE EventDescription TO TempEventDescription
                   MOVE EventLocation TO TempEventLocation
                   MOVE EventStatus TO TempEventStatus 
               WHEN 3
                   DISPLAY "Enter updated Event Description:"
                   ACCEPT TempEventDescription
                   MOVE EventID TO TempEventID
                   MOVE EventDate TO TempEventDate
                   MOVE EventDay TO TempEventDay
                   MOVE EventLocation TO TempEventLocation
                   MOVE EventStatus TO TempEventStatus 
               WHEN 4
                   DISPLAY "Enter updated Event Location:"
                   ACCEPT TempEventLocation
                   MOVE EventID TO TempEventID
                   MOVE EventDate TO TempEventDate
                   MOVE EventDay TO TempEventDay
                   MOVE EventDescription TO TempEventDescription
                   MOVE EventStatus TO TempEventStatus 
               WHEN 5
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

   
       DeleteSched.
           PERFORM TypeOption.

           EVALUATE UserChoice
               WHEN '1' PERFORM DeleteTaskConfirmation
               WHEN '2' PERFORM DeleteEventConfirmation
               WHEN '3' PERFORM MainMenu
               WHEN OTHER DISPLAY "Invalid Choice"
           END-EVALUATE. 

           PERFORM MainMenu.


       DeleteTaskConfirmation.
           DISPLAY "Enter Task ID to delete:".
           ACCEPT TaskIDInput.

           DISPLAY "Do you want to continue with the deletion? (Y/N): ".
           ACCEPT UserChoice.

           IF UserChoice = 'Y' OR UserChoice = 'y'
               PERFORM DeleteTask
           ELSE
               DISPLAY "Deletion canceled."
           END-IF.

       DeleteTask.
           OPEN INPUT TaskFile.
           OPEN OUTPUT TempTaskFile.

           MOVE 'N' TO EOF.

           PERFORM UNTIL EOF = 'Y'
               READ TaskFile
           AT END
               MOVE 'Y' TO EOF
           NOT AT END
               IF TaskIDInput = TaskID
                   DISPLAY "Task Deleted: " TaskIDInput
               ELSE
                   MOVE TaskID TO TempTaskID
                   MOVE TaskDate TO TempTaskDate
                   MOVE TaskDay TO TempTaskDay
                   MOVE TaskDescription TO TempTaskDescription
                   MOVE TaskStatus TO TempTaskStatus
                   WRITE TempTaskRecord
               END-IF
           END-PERFORM.

           CLOSE TaskFile.
           CLOSE TempTaskFile.

           MOVE 'N' TO EOF.

           OPEN OUTPUT TaskFile.
           OPEN INPUT TempTaskFile.

           PERFORM UNTIL EOF = 'Y'
               READ TempTaskFile
                   AT END
                       MOVE 'Y' TO EOF
                   NOT AT END
                       MOVE TempTaskID TO TaskID
                       MOVE TempTaskDate TO TaskDate
                       MOVE TempTaskDay TO TaskDay
                       MOVE TempTaskDescription TO TaskDescription
                       MOVE TempTaskStatus TO TaskStatus
                       WRITE TaskRecord
           END-PERFORM.

           CLOSE TempTaskFile.
           CLOSE TaskFile.

           DISPLAY "Task Deleted Successfullly".

           PERFORM MainMenu.


       DeleteEventConfirmation.
           DISPLAY "Enter Event ID to delete:".
           ACCEPT EventIDInput.

           DISPLAY "Do you want to continue with the deletion? (Y/N): ".
           ACCEPT UserChoice.

           IF UserChoice = 'Y' OR UserChoice = 'y'
               PERFORM DeleteEvent
           ELSE
               DISPLAY "Deletion canceled."
           END-IF.
           
       DeleteEvent.
           OPEN INPUT EventFile.
           OPEN OUTPUT TempEventFile.

           MOVE 'N' TO EOF.

           PERFORM UNTIL EOF = 'Y'
               READ EventFile
           AT END
               MOVE 'Y' TO EOF
           NOT AT END
               IF EventIDInput = EventID
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

           PERFORM UNTIL EOF = 'Y'
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


       ConfirmExit.
           DISPLAY "Do you want to exit? (Y/N):".
           ACCEPT UserChoice.
        
           IF UserChoice = 'Y' OR UserChoice = 'y'
               PERFORM GenerateReport.
               DISPLAY "Exiting Schedule Maker. Thank you!"
               STOP RUN
           EXIT.

