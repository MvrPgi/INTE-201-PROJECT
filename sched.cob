       IDENTIFICATION DIVISION.
       PROGRAM-ID. ScheduleMaker.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TaskFile ASSIGN TO "TaskFile.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT EventFile ASSIGN TO "EventFile.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TempFile ASSIGN TO "Temp.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD TaskFile.
       01 ScheduleRecord.
           05 TaskID      PIC X(3).
           05 TaskDate       PIC X(10).
           05 TaskDescription PIC X(50).

       FD EventFile.
       01 EventRecord.
           05 EventID      PIC X(3).
           05 EventDate       PIC X(10).
           05 EventDescription PIC X(50).

       FD TempFile.
       01 TempRecord.
           05 TempTaskID      PIC X(3).
           05 TempTaskDate       PIC X(10).
           05 TempTaskDescription PIC X(50).

       WORKING-STORAGE SECTION.
       01 UserChoice PIC X.
       01 EOF        PIC X VALUE 'N'.
       01 Username PIC X(20).
       01 Password PIC X(20).
       01 ValidUsername PIC X(20) VALUE 'user'.
       01 ValidPassword PIC X(20) VALUE 'pass'.
       01 TaskDateInput PIC X(10).
       01 TaskDescriptionInput PIC X(50).
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
           DISPLAY "[2] Add Task".
           DISPLAY "[3] Edit Task".
           DISPLAY "[4] Delete Task".
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


       ConfirmExit.
           DISPLAY "Do you want to exit? (Y/N):".
           ACCEPT UserChoice.
        
           IF UserChoice = 'Y' 
               DISPLAY "Exiting Schedule Maker. Thank you!"
               STOP RUN
           EXIT.
                

       ViewSched.
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
                               " Task: " TaskDescription
           END-PERFORM.

           CLOSE TaskFile.


       AddSched.
           DISPLAY " "
           DISPLAY "Select the type you want:"
           DISPLAY "[1] Task"
           DISPLAY "[2] Event"
           DISPLAY "[3] Back to MAIN MENU"
           Accept UserChoice.
      
           EVALUATE UserChoice
               WHEN '1' PERFORM AddEvent
               WHEN '2' PERFORM AddTask
               WHEN '3' PERFORM ConfirmExit
               WHEN OTHER DISPLAY "Invalid Choice"
           END-EVALUATE. 


       AddEvent.
      * create an event or a task schedule
           DISPLAY "Enter Task Date (YYYY-MM-DD):".
           ACCEPT TaskDate.

           DISPLAY "Enter Task Description:".
           ACCEPT TaskDescription.

           OPEN EXTEND TaskFile.
           WRITE ScheduleRecord.
           CLOSE TaskFile.

           DISPLAY "Task Added Successfully".

       AddTask.
      * create an event or a task schedule
           DISPLAY "Enter Task Date (YYYY-MM-DD):".
           ACCEPT TaskDate.

           DISPLAY "Enter Task Description:".
           ACCEPT TaskDescription.

           OPEN EXTEND TaskFile.
           WRITE ScheduleRecord.
           CLOSE TaskFile.

           DISPLAY "Task Added Successfully".


       EditSched.
      *    Writing the records from Schedule file to TempFile
           DISPLAY "Enter Task Date (YYYY-MM-DD) to edit:".
           ACCEPT TaskDateInput.

           MOVE 'N' TO EOF

           OPEN INPUT TaskFile. 
           OPEN OUTPUT TempFile.

           PERFORM UNTIL EOF = 'Y'
               READ TaskFile
                   AT END
                       MOVE 'Y' TO EOF
                   NOT AT END
                       IF TaskDateInput = TaskDate
                           DISPLAY "Enter updated Task Description:"
                           ACCEPT TempTaskDescription
                           MOVE TaskID TO TempTaskID
                           MOVE TaskDate TO TempTaskDate
                           WRITE TempRecord
                       ELSE
                           MOVE TaskID TO TempTaskID
                           MOVE TaskDate TO TempTaskDate
                           MOVE TaskDescription TO TempTaskDescription
                           WRITE TempRecord
                       END-IF
           END-PERFORM.

           CLOSE TaskFile.
           CLOSE TempFile.
      
      *    Writing the records from Tempfile to TaskFile
           MOVE 'N' TO EOF
           
           OPEN OUTPUT TaskFile.
           OPEN INPUT TempFile.

           PERFORM UNTIL EOF = 'Y'
               READ TempFile
                   AT END
                       MOVE 'Y' TO EOF
                   NOT AT END
                       MOVE TempTaskID TO TaskID
                       MOVE TempTaskDate TO TaskDate
                       MOVE TempTaskDescription TO TaskDescription
                       WRITE ScheduleRecord
           END-PERFORM.

           CLOSE TempFile.
           CLOSE TaskFile.

           DISPLAY "Task Updated Successfully".

   
       DeleteSched.
           DISPLAY "Enter Task Date (YYYY-MM-DD) to delete:".
           ACCEPT TaskDateInput.
        
           DISPLAY "Do you want to continue with the deletion? (Y/N): ".
           ACCEPT ConfirmDeletion.
        
           IF ConfirmDeletion = 'Y' OR ConfirmDeletion = 'y'
               PERFORM DeletionProcess
           ELSE
               DISPLAY "Deletion canceled. "
           END-IF.

        
       DeletionProcess.   
           OPEN INPUT TaskFile.
           OPEN OUTPUT TempFile.
       
           MOVE 'N' TO EOF.
       
           PERFORM UNTIL EOF = 'Y'
               READ TaskFile
                   AT END
                       MOVE 'Y' TO EOF
                   NOT AT END
                       IF TaskDateInput = TaskDate
                           DISPLAY "Task Deleted:" TaskDate
                       ELSE
                           MOVE TaskID TO TempTaskID
                           MOVE TaskDate TO TempTaskDate
                           MOVE TaskDescription TO TempTaskDescription
                           WRITE TempRecord
                       END-IF
           END-PERFORM.
       
           CLOSE TaskFile.
           CLOSE TempFile.
       
           MOVE 'N' TO EOF.
       
           OPEN OUTPUT TaskFile.
           OPEN INPUT TempFile.
       
           PERFORM UNTIL EOF = 'Y'
               READ TempFile
                   AT END
                       MOVE 'Y' TO EOF
                   NOT AT END
                       MOVE TempTaskID TO TaskID
                       MOVE TempTaskDate TO TaskDate
                       MOVE TempTaskDescription TO TaskDescription
                       WRITE ScheduleRecord
           END-PERFORM.
       
           CLOSE TempFile.
           CLOSE TaskFile.