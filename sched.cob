       IDENTIFICATION DIVISION.
       PROGRAM-ID. ScheduleMaker.

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

       DATA DIVISION.
       FILE SECTION.
       FD TaskFile.
       01 TaskRecord.
           05 TaskID               PIC X(3).
           05 TaskDate             PIC X(5).
           05 TaskDay              PIC X(10).
           05 TaskDescription      PIC X(25).
           05 TaskStatus           PIC X(10).

       FD EventFile.
       01 EventRecord.
           05 EventID              PIC X(3).
           05 EventDate            PIC X(5).
           05 EventDay             PIC X(10).
           05 EventDescription     PIC X(25).
           05 EventLocation        PIC X(25).
           05 EventStatus          PIC X(10).

       FD TempTaskFile.
       01 TempTaskRecord.
           05 TempTaskID           PIC X(3).
           05 TempTaskDate         PIC X(5).
           05 TempTaskDay          PIC X(10).
           05 TempTaskDescription  PIC X(25).
           05 TempTaskStatus       PIC X(10).
       
       FD TempEventFile.
       01 TempEventRecord.
           05 TempEventID          PIC X(3).
           05 TempEventDate        PIC X(5).
           05 TempEventDay         PIC X(10).
           05 TempEventDescription PIC X(25).
           05 TempEventLocation    PIC X(25).
           05 TempEventStatus      PIC X(10).

      *> WORKING-STORAGE SECTION.
       WORKING-STORAGE SECTION.
       01  UserChoice PIC X.
       01  EOF        PIC X VALUE 'N'.
       01  Username PIC X(20).
       01  Password PIC X(20).
       01  ValidUsername PIC X(20) VALUE 'user'.
       01  ValidPassword PIC X(20) VALUE 'pass'.
       01  TaskIDInput PIC X(3).
       01  TaskDateInput PIC X(10).
       01  TaskDescriptionInput PIC X(25).
       01  EventIDInput PIC X(3).
       01  EventDateInput PIC X(10).
       01  EventDescriptionInput PIC X(25).
       01  ConfirmDeletion     PIC X.
       01  EditOnce PIC X VALUE 'N'.
       01  DeleteOnce  PIC X.

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
           PERFORM TypeOption

           EVALUATE UserChoice
               WHEN '1' PERFORM EditTask
               WHEN '2' PERFORM EditTask
               WHEN '3' PERFORM DisplayMenu
               WHEN OTHER DISPLAY "Invalid Choice"
           END-EVALUATE. 
      
       EditTaskOptions.    
           DISPLAY " "
           DISPLAY "What do you want to edit? "
           DISPLAY "[1] Task Date"
           DISPLAY "[2] Task Day"
           DISPLAY "[3] Task Description"
           DISPLAY "[5] Task Status"
           DISPLAY "[6] Back to MAIN MENU"
           DISPLAY "Enter your choice: " WITH NO ADVANCING
           Accept UserChoice.

           EVALUATE UserChoice
               WHEN '1' PERFORM EditTaskDate
               WHEN '2' PERFORM EditTaskDate
               WHEN '3' PERFORM EditTaskDescription
               WHEN '4' PERFORM EditTaskDescription
               WHEN '5' PERFORM DisplayMenu
               WHEN OTHER DISPLAY "Invalid Choice"
           END-EVALUATE.


       EditTask.
      *    Writing the records from TaskFile to TempTaskFile
           IF EditOnce = 'Y' AND EOF = 'Y'
               DISPLAY "Enter Task ID (999) to edit: "
               ACCEPT TaskIDInput
               PERFORM EditTaskOptions
           END-IF.
      
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

      *    IF EOF = 'Y'
      *        DISPLAY "Task Updated Successfully"
      *    END IF.

       EditTaskDate.
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
                           DISPLAY "Enter updated Task Date:"
                           ACCEPT TempTaskDate
                           MOVE TaskDay TO TempTaskDay
                           MOVE TaskDescription TO TempTaskDescription
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
           
           DISPLAY "Task Updated Successfully."

           MOVE 'Y' TO EditOnce.

           PERFORM EditTask.


       
      *EditTaskDay.
      *    MOVE TaskID TO TempTaskID
      *    MOVE TaskDate TO TempTaskDate
      *    DISPLAY "Enter updated Task Day:"
      *    ACCEPT TempTaskDay
      *    MOVE TaskDescription TO TempTaskDescription
      *    MOVE TaskStatus TO TempTaskStatus 

       EditTaskDescription.
           MOVE 'N' TO EOF

           OPEN INPUT TaskFile. 
           OPEN OUTPUT TempTaskFile.

           PERFORM UNTIL EOF = 'Y'
               READ TaskFile
                   AT END
                       MOVE 'Y' TO EOF
                   NOT AT END
                       IF TaskIDInput = TaskID
      *                    PERFORM EditTaskOptions
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

      *EditTaskStatus.
      *    MOVE TaskID TO TempTaskID
      *    DISPLAY "Enter Updated Status"
      *    ACCEPT TempTaskStatus
      *    MOVE TaskDate TO TempTaskDate
      *    MOVE TaskDay TO TempTaskDay
      *    MOVE TaskDescription TO TempTaskDescription
      *    MOVE TaskStatus TO TempTaskStatus
        
       
      *EditEventOptions.    
      *    DISPLAY " "
      *    DISPLAY "What do you want to edit? "
      *    DISPLAY "[1] Event Date"
      *    DISPLAY "[2] Event Day"
      *    DISPLAY "[3] Event Description"
      *    DISPLAY "[4] Event Location"
      *    DISPLAY "[5] Event Status"
      *    DISPLAY "[6] Back to MAIN MENU"
      *    DISPLAY "Enter your choice: " WITH NO ADVANCING
      *    Accept UserChoice.
      *>       EVALUATE UserChoice
      *            WHEN '1' PERFORM EditEventDate
      *            WHEN '2' PERFORM EditEventDay
      *            WHEN '3' PERFORM EditEventDescription
      *            WHEN '4' PERFORM EditEventLocation
      *            WHEN '5' PERFORM EditEventStatus
      *            WHEN '6' PERFORM DisplayMenu
      *            WHEN OTHER DISPLAY "Invalid Choice"
            

      *EditEvent.
      *    Writing the records from TaskFile to TempTaskFile
      *    DISPLAY "Enter Task ID (999) to edit: "
      *    ACCEPT EventIDInput

      *    MOVE 'N' TO EOF

      *    OPEN INPUT EventFile. 
      *    OPEN OUTPUT TempEventFile.

      *    PERFORM UNTIL EOF = 'Y'
      *        READ EventFile
      *            AT END
      *                MOVE 'Y' TO EOF
      *            NOT AT END
      *                IF EventIDInput = EventID
      *                    PERFORM EditEventOptions.

      *                   
      *                    END-EVALUATE. 
      *                    MOVE EventID TO TempEventID
      *                    MOVE EventDate TO TempEventDate
      *                    MOVE EventDay TO TempEventDay
      *                    DISPLAY "Enter updated Event Description:"
      *                    ACCEPT TempEventDescription
      *                    MOVE EventStatus TO TempEventStatus 
      *                    WRITE TempEventRecord
      *                ELSE
      *                    MOVE EventID TO TempEventID
      *                    MOVE EventDate TO TempEventDate
      *                    MOVE EventDay TO TempEventDay
      *                    MOVE EventDescription TO TempEventDescription
      *                    MOVE EventStatus TO TempEventStatus
      *                    MOVE EventLocation TO TempEventLocation

      *                    WRITE TempEventRecord
      *                END-IF
      *    END-PERFORM.

      *    CLOSE EventFile.
      *    CLOSE TempEventFile.
           
      *EditEventDate.
      *    MOVE EventID TO TempEventID
      *    DISPLAY "Enter updated Task Date:"
      *    ACCEPT TempEventDate
      *    MOVE EventDay TO TempEventDay
      *    MOVE TaskDescription TO TempTaskDescription
      *    MOVE TaskStatus TO TempTaskStatus 
       
      *EditEventDay.
      *    MOVE EventID TO TempEventID
      *    MOVE EventDate TO TempEventDate
      *    DISPLAY "Enter updated Event Day:"
      *    ACCEPT TempEventDay
      *    MOVE EventDescription TO TempEventDescription
      *    MOVE EventStatus TO TempEventStatus 

      *EditEventDescription.
      *    MOVE EventID TO TempEventID
      *    MOVE EventDate TO TempEventDate
      *    MOVE EventDay TO TempEventDay
      *    DISPLAY "Enter updated Task Description:"
      *    ACCEPT TempEventDescription
      *    MOVE EventStatus TO TempEventStatus

      *EditEventStatus.
      *    MOVE EventID TO TempEventID
      *    DISPLAY "Enter Updated Status"
      *    ACCEPT TempEventStatus
      *    MOVE EventDate TO TempEventDate
      *    MOVE EventDay TO TempEventDay
      *    MOVE EventDescription TO TempEventDescription
      *    MOVE EventStatus TO TempEventStatus
   
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

           DISPLAY "Task Deleted Successfullly".

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