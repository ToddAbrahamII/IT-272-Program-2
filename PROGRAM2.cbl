      ******************************************************************
      * Author: TODD ABRAHAM
      * Date: 10-3-22
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGRAM2.
       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               SELECT NUMIN ASSIGN TO "program2.txt".
               SELECT NUMOUT ASSIGN TO "output.txt"
                ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
           FD NUMIN.
           01 IN-NUM               PIC XXXX.

           FD NUMOUT.
           01 N1                   PIC 999.


       WORKING-STORAGE SECTION.
           01 HDRLINES             PIC X(40) VALUE ALL '-'.

           01 LASTREC              PIC X VALUE SPACE.

           01 NUM-WS.
               05 WSN1             PIC 999.
               05 WSN2             PIC 999.
               05 WSN3             PIC 999.
               05 WSN4             PIC 99.
               05 WSN5             PIC 9.
               05 TOTALNUM         PIC 9(4).
               05 NUMCOUNT         PIC 9.
               05 AVGNUM           PIC 999V99.

           01 TOTAL-LINE.
               05 TOTALDESC        PIC X(7) VALUE'TOTAL: '.
               05 TOTALNUM-OUT     PIC Z,ZZZ.

           01 AVG-LINE.
              05 AVGDESC           PIC X(9) VALUE 'AVERAGE: '.
              05 AVGNUM-OUT        PIC ZZZ.99.

           01 ENDLINES             PIC X(40) VALUE ALL '-'.

       PROCEDURE DIVISION.
       100-MAIN-PROCEDURE.
           OPEN INPUT NUMIN.
           OPEN OUTPUT NUMOUT.

           PERFORM 200-READ-RECORD
               UNTIL LASTREC = 'Y'.


           DISPLAY HDRLINES
           MOVE TOTALNUM TO TOTALNUM-OUT.
           DISPLAY TOTAL-LINE

           COMPUTE AVGNUM = TOTALNUM / 4.
           MOVE AVGNUM TO AVGNUM-OUT.
           DISPLAY AVG-LINE.
`
           PERFORM 400-CLOSE-FILES.

           DISPLAY ENDLINES.
            STOP RUN.


       200-READ-RECORD.
           READ NUMIN
           AT END MOVE 'Y' TO LASTREC
           NOT AT END PERFORM 300-WRITE-RECORD
           END-READ.

       300-WRITE-RECORD.
           MOVE IN-NUM TO N1
           MOVE N1 TO WSN1.
           COMPUTE TOTALNUM = TOTALNUM + WSN1.
           DISPLAY WSN1.

       400-CLOSE-FILES.
           CLOSE NUMIN.
           CLOSE NUMOUT.

       END PROGRAM PROGRAM2.
