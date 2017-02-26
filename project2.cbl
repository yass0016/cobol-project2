      ******************************************************************
      * Author: Saleh Yassin
      * Date: Feb 26 2017
      * Purpose: Read an employee file then output
      * a formated list of employees with their gross and net pay
      * and other information calculated from the input file then
      * create an output of a summary report of the totals
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROJECT2.

       ENVIRONMENT DIVISION.
           CONFIGURATION SECTION.
           INPUT-OUTPUT SECTION.
               FILE-CONTROL.
                   SELECT PAYROLL-FILE ASSIGN TO "EMPFILE2.TXT"
                   ORGANIZATION IS LINE SEQUENTIAL.
                   SELECT REGISTER-FILE ASSIGN TO "PAYROLLREG.TXT"
                   ORGANIZATION IS LINE SEQUENTIAL.
                   SELECT SUMMARY-FILE ASSIGN TO "PAYROLLSUM.TXT"
                   ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
           FILE SECTION.
               FD PAYROLL-FILE.
               01 PAYROLL-RECORD.
                   05 EMPLOYEE-NUMBER      PIC 9(9).
                   05 EMPLOYEE-LASTNAME    PIC X(13).
                   05 EMPLOYEE-INITIAL     PIC X(2).
                   05 HOURLY-PAYRATE       PIC 9(2)V99.
                   05 HOURS-WORKED         PIC 9(2)V99.
                   05 IS_UNIONMEMBER       PIC X(1).

               FD REGISTER-FILE.
               01 PAYROLL-REGISTER     PIC X(512).

               FD SUMMARY-FILE.
               01 PAYROLL-SUMMARY      PIC X(256).

           WORKING-STORAGE SECTION.
               01 REGISTER-OUT.
                   05 REGISTER-HEADING-TITLE.
                       10 FILLER       PIC X(6) VALUE SPACES.
                       10 FILLER       PIC X(14) VALUE 'PAYROLL REGIST'.
                       10 FILLER       PIC X(9) VALUE 'ER REPORT'.

                   05 REGISTER-HEADING-LINE.
                       10 FILLER       PIC X(13) VALUE 'EMPLOYEE NAME'.
                       10 FILLER       PIC X(10) VALUE SPACES.
                       10 FILLER       PIC X(5) VALUE 'GROSS'.
                       10 FILLER       PIC X(7) VALUE SPACES.
                       10 FILLER       PIC X(3) VALUE 'TAX'.
                       10 FILLER       PIC X(10) VALUE SPACES.
                       10 FILLER       PIC X(6) VALUE 'HEALTH'.
                       10 FILLER       PIC X(6) VALUE SPACES.
                       10 FILLER       PIC X(10) VALUE 'UNION DUES'.
                       10 FILLER       PIC X(2) VALUE SPACES.
                       10 FILLER       PIC X(3) VALUE 'NET'.

                   05 REGISTER-DET-LINE.
                       10 EMPLOYEEINITIAL         PIC X(2).
                       10 FILLER                   PIC X(4).
                       10 EMPLOYEELASTNAME        PIC X(13).
                       10 FILLER                   PIC X(2).
                       10 GROSSPAY                 PIC Z,ZZ9.99.
                       10 FILLER                   PIC X(4).
                       10 TAX-DED                  PIC Z,ZZ9.99.
                       10 FILLER                   PIC X(4).
                       10 HEALTHINSURANCE-DED      PIC Z,ZZ9.99.
                       10 FILLER                   PIC X(4).
                       10 UNION-DUES               PIC Z,ZZ9.99.
                       10 FILLER                   PIC X(4).
                       10 NET-PAY                  PIC ZZ,ZZ9.99.

               01 SUMMARY-OUT.
                   05 SUMMARY-TOTAL-GROSS.
                       10 FILLER       PIC X(4) VALUE SPACES.
                       10 FILLER       PIC X(11) VALUE 'TOTAL GROSS'.
                       10 FILLER       PIC X(15) VALUE SPACES.
                       10 TGROSS-OUT   PIC $$,$$9.99.

                   05 SUMMARY-TOTAL-NET.
                       10 FILLER       PIC X(4) VALUE SPACES.
                       10 FILLER       PIC X(9) VALUE 'TOTAL NET'.
                       10 FILLER       PIC X(17) VALUE SPACES.
                       10 TNET-OUT     PIC $$,$$9.99.

                   05 SUMMARY-TOTAL-PAY-RECORDS.
                       10 FILLER       PIC X(4) VALUE SPACES.
                       10 FILLER       PIC X(14) VALUE 'TOTAL PAY RECO'.
                       10 FILLER       PIC X(3) VALUE 'RDS'.
                       10 FILLER       PIC X(7) VALUE SPACES.
                       10 TPREC-OUT    PIC Z(3)9.

                   05 SUMMARY-TOTAL-REGISTER-RECORDS.
                       10 FILLER       PIC X(4) VALUE SPACES.
                       10 FILLER       PIC X(14) VALUE 'TOTAL REGISTER'.
                       10 FILLER       PIC X(8) VALUE ' RECORDS'.
                       10 FILLER       PIC X(2) VALUE SPACES.
                       10 TRREC-OUT    PIC Z(3)9.

               01 EMP-GROSS                PIC 9(3)V99 VALUE ZEROS.
               01 EMP-TAX                  PIC 9(3)V99 VALUE ZEROS.
               01 EMP-HEALTH               PIC 9(3)V99 VALUE ZEROS.
               01 EMP-UNION                PIC 9(3)V99 VALUE ZEROS.
               01 EMP-NET                  PIC 9(3)V99 VALUE ZEROS.

               01 TOTAL-GROSS              PIC 9(5)V99 VALUE ZEROS.
               01 TOTAL-NET                PIC 9(5)V99 VALUE ZEROS.
               01 TOTAL-PAY-RECORDS        PIC 9(5) VALUE ZERO.
               01 TOTAL-REGISTER-RECORDS   PIC 9(5) VALUE ZERO.
               01 EOF                      PIC A(1).

       PROCEDURE DIVISION.
           0000-START.
               PERFORM 0100-INITIALIZE.
               PERFORM 0100-PROCESS.
               PERFORM 0100-CLOSE.
               STOP RUN.

           0100-INITIALIZE.
               PERFORM 0200-OPEN-FILES.
               PERFORM 0200-READ-RECORD.

           0100-PROCESS.
               PERFORM 0200-WRITE-REG-HEADING.
               PERFORM 0200-WRITE-DETAIL-REGISTER UNTIL EOF = 'Y'.
               PERFORM 0200-WRITE-SUMMARY.

           0100-CLOSE.
               CLOSE PAYROLL-FILE.
               CLOSE REGISTER-FILE.
               CLOSE SUMMARY-FILE.

           0200-OPEN-FILES.
               OPEN INPUT PAYROLL-FILE.
               OPEN OUTPUT REGISTER-FILE.
               OPEN OUTPUT SUMMARY-FILE.

           0200-READ-RECORD.
               READ PAYROLL-FILE
                   AT END MOVE 'Y' TO EOF
                   NOT AT END
                       ADD 1 TO TOTAL-PAY-RECORDS
               END-READ.

           0200-WRITE-REG-HEADING.
               WRITE PAYROLL-REGISTER FROM REGISTER-HEADING-TITLE.
               WRITE PAYROLL-REGISTER FROM REGISTER-HEADING-LINE.

           0200-WRITE-DETAIL-REGISTER.
               PERFORM 0300-PROCESS-CALC.
               PERFORM 0300-WRITE-RECORD.
               PERFORM 0200-READ-RECORD.

           0200-WRITE-SUMMARY.
               MOVE TOTAL-GROSS TO TGROSS-OUT.
               MOVE TOTAL-NET TO TNET-OUT.
               MOVE TOTAL-PAY-RECORDS TO TPREC-OUT.
               MOVE TOTAL-REGISTER-RECORDS TO TRREC-OUT.

               WRITE PAYROLL-SUMMARY FROM SUMMARY-TOTAL-GROSS.
               WRITE PAYROLL-SUMMARY FROM SUMMARY-TOTAL-NET.
               WRITE PAYROLL-SUMMARY FROM SUMMARY-TOTAL-PAY-RECORDS.
               WRITE PAYROLL-SUMMARY FROM
                   SUMMARY-TOTAL-REGISTER-RECORDS.

           0300-PROCESS-CALC.
               PERFORM 0400-CALC-REGISTER.
               PERFORM 0400-CALC-SUMMARY.

           0300-WRITE-RECORD.
               MOVE EMPLOYEE-INITIAL TO EMPLOYEEINITIAL.
               MOVE EMPLOYEE-LASTNAME TO EMPLOYEELASTNAME.
               MOVE EMP-GROSS TO GROSSPAY.
               MOVE EMP-TAX TO TAX-DED.
               MOVE EMP-HEALTH TO HEALTHINSURANCE-DED.
               MOVE EMP-UNION TO UNION-DUES.
               MOVE EMP-NET TO NET-PAY.

               WRITE PAYROLL-REGISTER FROM REGISTER-DET-LINE.
               ADD 1 TO TOTAL-REGISTER-RECORDS.

           0400-CALC-REGISTER.
               PERFORM 0500-CALC-GROSS.
               PERFORM 0500-CALC-TAX.
               PERFORM 0500-CALC-HEALTH.
               PERFORM 0500-CALC-UNION.
               PERFORM 0500-CALC-NET.

           0400-CALC-SUMMARY.
               PERFORM 0500-CALC-TGROSS.
               PERFORM 0500-CALC-TNET.

           0500-CALC-GROSS.
               IF HOURS-WORKED > 40
                   COMPUTE
                       EMP-GROSS = HOURLY-PAYRATE *
                           (HOURS-WORKED - 40) * 1.5
               ELSE
                   COMPUTE
                       EMP-GROSS = HOURLY-PAYRATE * HOURS-WORKED.

           0500-CALC-TAX.
               COMPUTE EMP-TAX = EMP-GROSS * 0.20.

           0500-CALC-HEALTH.
               COMPUTE EMP-HEALTH = EMP-GROSS * 0.02.

           0500-CALC-UNION.
               IF IS_UNIONMEMBER = 'U'
                   COMPUTE EMP-UNION = 20.0
               ELSE
                   COMPUTE EMP-UNION = 0.

           0500-CALC-NET.
               COMPUTE EMP-NET =
                   EMP-GROSS - (EMP-TAX + EMP-HEALTH + EMP-UNION).

           0500-CALC-TGROSS.
               COMPUTE TOTAL-GROSS = TOTAL-GROSS + EMP-GROSS.

           0500-CALC-TNET.
               COMPUTE TOTAL-NET = TOTAL-NET + EMP-NET.

       END PROGRAM PROJECT2.
