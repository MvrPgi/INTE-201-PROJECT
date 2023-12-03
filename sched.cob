       IDENTIFICATION DIVISION.
       PROGRAM-ID. ScheduleMaker.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ScheduleFile ASSIGN TO "ScheduleFile.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TempFile ASSIGN TO "Temp.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD ScheduleFile.
       01 ScheduleRecord.
           05 TaskDate       PIC X(10).
           05 TaskDescription PIC X(50).

       FD TempFile.
       01 TempRecord.
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
           DISPLAY " "
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
               WHEN '1' PERFORM ViewSchedule
               WHEN '2' PERFORM AddTask
               WHEN '3' PERFORM EditTask
               WHEN '4' PERFORM DeleteTask
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
                

       ViewSchedule.
           MOVE 'N' TO EOF
           OPEN INPUT ScheduleFile.

           DISPLAY " ".
           DISPLAY "Schedule:".
           PERFORM UNTIL EOF = 'Y'
               READ ScheduleFile
                   AT END
                       MOVE 'Y' TO EOF
                   NOT AT END
                       DISPLAY "Date: " TaskDate
                               " Task: " TaskDescription
           END-PERFORM.

           CLOSE ScheduleFile.


       AddTask.
           DISPLAY "Enter Task Date (YYYY-MM-DD):".
           ACCEPT TaskDate.

           DISPLAY "Enter Task Description:".
           ACCEPT TaskDescription.

           OPEN EXTEND ScheduleFile.
           WRITE ScheduleRecord.
           CLOSE ScheduleFile.

           DISPLAY "Task Added Successfully".


       EditTask.
      *    Writing the records from Schedule file to TempFile    
           DISPLAY "Enter Task Date (YYYY-MM-DD) to edit:".
           ACCEPT TaskDateInput.

           MOVE 'N' TO EOF

           OPEN INPUT ScheduleFile. 
           OPEN OUTPUT TempFile.

           PERFORM UNTIL EOF = 'Y'
               READ ScheduleFile
                   AT END
                       MOVE 'Y' TO EOF
                   NOT AT END
                       IF TaskDateInput = TaskDate
                           DISPLAY "Enter updated Task Description:"
                           ACCEPT TempTaskDescription
                           MOVE TaskDate TO TempTaskDate
                           WRITE TempRecord
                       ELSE
                           MOVE TaskDate TO TempTaskDate
                           MOVE TaskDescription TO TempTaskDescription
                           WRITE TempRecord
                       END-IF
           END-PERFORM.

           CLOSE ScheduleFile.
           CLOSE TempFile.
      
      *    Writing the records from Tempfile to ScheduleFile
           MOVE 'N' TO EOF
           
           OPEN OUTPUT ScheduleFile.
           OPEN INPUT TempFile.

           PERFORM UNTIL EOF = 'Y'
               READ TempFile
                   AT END
                       MOVE 'Y' TO EOF
                   NOT AT END
                       MOVE TempTaskDate TO TaskDate
                       MOVE TempTaskDescription TO TaskDescription
                       WRITE ScheduleRecord
           END-PERFORM.

           CLOSE TempFile.
           CLOSE ScheduleFile.

           DISPLAY "Task Updated Successfully".

   
       DeleteTask.
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
           OPEN INPUT ScheduleFile.
           OPEN OUTPUT TempFile.
       
           MOVE 'N' TO EOF.
       
           PERFORM UNTIL EOF = 'Y'
               READ ScheduleFile
                   AT END
                       MOVE 'Y' TO EOF
                   NOT AT END
                       IF TaskDateInput = TaskDate
                           DISPLAY "Task Deleted:" TaskDate
                       ELSE
                           MOVE TaskDate TO TempTaskDate
                           MOVE TaskDescription TO TempTaskDescription
                           WRITE TempRecord
                       END-IF
           END-PERFORM.
       
           CLOSE ScheduleFile.
           CLOSE TempFile.
       
           MOVE 'N' TO EOF.
       
           OPEN OUTPUT ScheduleFile.
           OPEN INPUT TempFile.
       
           PERFORM UNTIL EOF = 'Y'
               READ TempFile
                   AT END
                       MOVE 'Y' TO EOF
                   NOT AT END
                       MOVE TempTaskDate TO TaskDate
                       MOVE TempTaskDescription TO TaskDescription
                       WRITE ScheduleRecord
           END-PERFORM.
       
           CLOSE TempFile.
           CLOSE ScheduleFile.