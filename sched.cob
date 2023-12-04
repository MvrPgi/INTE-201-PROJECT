       IDENTIFICATION DIVISION.
       PROGRAM-ID. ScheduleMaker.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TaskFile ASSIGN TO "TaskFile.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT EventFile ASSIGN TO "EventFile.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TempTaskFile ASSIGN TO "Temp.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD TaskFile.
       01 TaskRecord.
           05 TaskID      PIC X(3).
           05 TaskDate       PIC X(5).
           05 TaskDay      PIC X(10).
           05 TaskDescription PIC X(25).
           05 TaskStatus    PIC X(10).

       FD EventFile.
       01 EventRecord.
           05 EventID      PIC X(3).
           05 EventDate       PIC X(5).
           05 EventDay         PIC X(10).
           05 EventDescription PIC X(25).
           05 EventLocation PIC X(25).
           05 EventStatus      PIC X(10).

       FD TempTaskFile.
       01 TempTaskRecord.
           05 TempTaskID      PIC X(3).
           05 TempTaskDate       PIC X(5).
           05 TempTaskDay      PIC X(10).
           05 TempTaskDescription PIC X(25).
           05 TempTaskStatus   PIC X(10).

       WORKING-STORAGE SECTION.
       01 UserChoice PIC X.
       01 EOF        PIC X VALUE 'N'.
       01 Username PIC X(20).
       01 Password PIC X(20).
       01 ValidUsername PIC X(20) VALUE 'user'.
       01 ValidPassword PIC X(20) VALUE 'pass'.
       01 TaskIDInput PIC X(3).
       01 TaskDateInput PIC X(10).
       01 TaskDescriptionInput PIC X(25).
       01 ConfirmDeletion     PIC X.

       PROCEDURE DIVISION.
           DISPLAY "***************************************"
           DISPLAY "*        NAME NG PROGRAM NATIN        *"
           DISPLAY "***************************************"
           DISPLAY "CREATED BY: GROUP 1"

           PERFORM DisplayLogin Until Username = ValidUsername AND
           Password = ValidPassword

           STOP RUN.


       DisplayLogin.
           DISPLAY 'Enter username: ' WITH NO ADVANCING.
           ACCEPT Username.

           DISPLAY 'Enter password: ' WITH NO ADVANCING.
           ACCEPT Password.

           IF Username = ValidUsername AND Password = ValidPassword
               DISPLAY " "
               DISPLAY 'Login successful.'
               PERFORM DisplayMenu UNTIL UserChoice = '5'.
           IF NOT(Username = ValidUsername AND Password = ValidPassword)
               DISPLAY " "
               DISPLAY 'Invalid username or password.'
               DISPLAY " "
           .


       DisplayMenu.
           DISPLAY " ".
           DISPLAY "---------------MAIN MENU---------------".
           DISPLAY "[1] View Schedule".
           DISPLAY "[2] Add Schedule".
           DISPLAY "[3] Edit Schedule".
           DISPLAY "[4] Delete Schedule".
           DISPLAY "[5] Exit".
           DISPLAY "Enter your choice: " WITH NO ADVANCING.
           ACCEPT UserChoice.

           PERFORM ProcessOption.
        
           
       ProcessOption.
           EVALUATE UserChoice
               WHEN '1' PERFORM ViewSched
               WHEN '2' PERFORM AddSched
               WHEN '3' PERFORM EditSched
               WHEN '4' PERFORM DeleteSched
               WHEN '5' PERFORM ConfirmExit
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


       ConfirmExit.
           DISPLAY "Do you want to exit? (Y/N):".
           ACCEPT UserChoice.
        
           IF UserChoice = 'Y' OR UserChoice = 'y'
               DISPLAY "Exiting Schedule Maker. Thank you!"
               STOP RUN
           EXIT.
       
                
       ViewSched.
           PERFORM TypeOption.

           EVALUATE UserChoice
               WHEN '1' PERFORM ViewTask
               WHEN '2' PERFORM ViewEvent
               WHEN '3' PERFORM DisplayMenu
               WHEN OTHER DISPLAY "Invalid Choice"
           END-EVALUATE. 


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


       AddSched.
           PERFORM TypeOption.

           EVALUATE UserChoice
               WHEN '1' PERFORM AddTask
               WHEN '2' PERFORM AddEvent
               WHEN '3' PERFORM DisplayMenu
               WHEN OTHER DISPLAY "Invalid Choice"
           END-EVALUATE. 


       AddTask.
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

       AddEvent.
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


       EditSched.
           PERFORM TypeOption.

           EVALUATE UserChoice
               WHEN '1' PERFORM EditTask
               WHEN '2' PERFORM EditTask
               WHEN '3' PERFORM DisplayMenu
               WHEN OTHER DISPLAY "Invalid Choice"
           END-EVALUATE. 
      

       EditTask.
      *    Writing the records from TaskFile to TempTaskFile
           DISPLAY "Enter Task ID (999) to edit: "
           ACCEPT TaskIDInput

           MOVE 'N' TO EOF

           OPEN INPUT TaskFile. 
           OPEN OUTPUT TempTaskFile.

           PERFORM UNTIL EOF = 'Y'
               READ TaskFile
                   AT END
                       MOVE 'Y' TO EOF
                   NOT AT END
                       IF TaskIDInput = TaskID
                           MOVE TaskID TO TempTaskID
                           MOVE TaskDate TO TempTaskDate
                           MOVE TaskDay TO TempTaskDay
                           DISPLAY "Enter updated Task Description:"
                           ACCEPT TempTaskDescription
                           MOVE TaskStatus TO TempTaskStatus 
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

       
      *EditEvent.

   
       DeleteSched.
           PERFORM TypeOption.

           EVALUATE UserChoice
               WHEN '1' PERFORM DeleteTaskConfirmation
               WHEN '2' PERFORM DeleteEventConfirmation
               WHEN '3' PERFORM DisplayMenu
               WHEN OTHER DISPLAY "Invalid Choice"
           END-EVALUATE. 

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

           DISPLAY "Task Deleted Successfully".

       DeleteEventConfirmation.

      *DeleteEvent.