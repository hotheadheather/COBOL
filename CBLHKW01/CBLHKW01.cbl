       IDENTIFICATION DIVISION.
      *THIS PROGRAM CREATES PIZZA SALES REPORTS
       PROGRAM-ID. CBLHKW01.
       AUTHOR. Heather Whittlesey.
      

       ENVIRONMENT DIVISION.
           SELECT PIZZA-SALES
               ASSIGN TO 'C:\COBOLWI19\CBLPIZZA.DAT'
                   ORGANIZATION IS LINE SEQUENTIAL.
           SELECT PRTOUT
               ASSIGN TO 'C:\COBOLWI19\PIZZARPT.PRT'
                   ORGANIZATION IS LINE SEQUENTIAL.


       DATA DIVISION.
       FILE SECTION.
       FD PIZZA-SALES
           LABEL RECORD IS STANDARD
           DATA RECORD IS PIZZA-REC
           RECORD CONTAINS 49 CHARACTERS.


       01 I-PIZZA-REC.
         05 I-PIZZA-ITEM-NO      PIC X(6).
         05 I-PIZZA-CUR-DATE.
           10  I-PIZZA-CUR-YY    PIC 9(4).
           10  I-PIZZA-CUR-MM    PIC 99.
           10  I-PIZZA-CUR-DD    PIC 99.
         05 I-PIZZA-PRICE        PIC 99V99.
         05 I-PIZZA-CUR-QTY      PIC 9(5).
         05 I-PIZZA-PREV-QTY     PIC 9(5).


       FD  PRTOUT
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 132 CHARACTERS
           DATA RECORD IS PRTLINE
           LINAGE IS 60 WITH FOOTING AT 56.

       01 PRTLINE              PIC X(132).




       WORKING-STORAGE SECTION.

       01 MISC.
           05 MORE-RECS          PIC X(3)       VALUE 'YES'.
           05 PAGE-CTR           PIC 99         VALUE 0.
           05 SALES-CTR          PIC 9(3)       VALUE 0.
           05 C-TOTAL-SALES-TOTAL PIC 9(8)V99.


       01 TITLE-LINE.
           05 FILLER             PIC X(6)       VALUE 'DATE'.
           05 TITLE-DATE.
               10 TITLE-MONTH    PIC XX.
               10 FILLER         PIC X          VALUE '/'.
               10 TITLE-DAY      PIC XX.
               10 FILLER         PIC X          VALUE '/'.
               10 TITLE-YEAR     PIC X(4).
           05 FILLER             PIC X(36)      VALUE SPACES.
           05 FILLER             PIC X(26)
               VALUE 'WHITTLESEY S PIZZA REPORT'.
           05 FILLER             PIC X(45)      VALUE SPACES.
           05 TITLE-PAGE         PIC Z9.

       01 COL-HEADING1.
           05 FILLER               PIC X(6)       VALUE SPACES.
           05 FILLER               PIC X(4)       VALUE 'ITEM'.
           05 FILLER               PIC X(23)      VALUE SPACES.
           05 FILLER               PIC X(5)       VALUE 'PRIOR'.
           05 FILLER               PIC X(7)       VALUE SPACES.
           05 FILLER               PIC X(7)       VALUE 'CURRENT'.
           05 FILLER               PIC X(9)       VALUE SPACES.
           05 FILLER               PIC X(14)      VALUE'SALES INCREASE'.
           05 FILLER               PIC X(8)       VALUE SPACES.
           05 FILLER               PIC X(9)       VALUE 'INCR/DECR'.
           05 FILLER               PIC X(10)      VALUE SPACES.
           05 FILLER               PIC X(4)       VALUE 'SALE'.


       01 COL-HEADING2.
           05 FILLER               PIC X(4)      VALUE SPACES.
           05 FILLER               PIC X(6)      VALUE 'NUMBER'.
           05 FILLER               PIC X(4)      VALUE SPACES.
           05 FILLER               PIC X(10)     VALUE 'SALES DATE'.
           05 FILLER               PIC X(9)      VALUE SPACES.
           05 FILLER               PIC X(3)      VALUE 'QTY'.
           05 FILLER               PIC X(10)     VALUE SPACES.
           05 FILLER               PIC X(3)      VALUE 'QTY'.
           05 FILLER               PIC X(11)     VALUE SPACES.
           05 FILLER               PIC X(13)     VALUE '/DECREASE AMT'.
           05 FILLER               PIC X(9)      VALUE SPACES.
           05 FILLER               PIC X(10)     VALUE 'PERCENTAGE'.
           05 FILLER               PIC X(8)      VALUE SPACES.
           05 FILLER               PIC X(5)      VALUE 'PRICE'.
           05 FILLER               PIC X(10)     VALUE SPACES.

         
     

       01 DETAIL-LINE.
           05 D-ITEMNUMBER       PIC X(6).
           05 FILLER             PIC X(4).
           05 D-SALES-DATE       PIC X(10).
           05 FILLER             PIC X(7).
           05 D-PRIOR-QTY        PIC X(6).
           05 FILLER             PIC X(8).
           05 D-CURRENT-QTY      PIC X(6).
           05 FILLER             PIC X(12).
           05 D-INC-DEC-AMT      PIC X(6).
           05 FILLER             PIC X(15).
           05 D-INC-DEC-PERCENT  PIC X(5).
           05 FILLER             PIC X(10).
           05 D-SALE-PRICE       PIC $ZZZ.99.
           05 FILLER             PIC X(7).
           05 D-TOTAL-SALES      PIC $$$,$$$,$$$.99.
           05 FILLER             PIC X(6).

       01 TOTAL-LINE.
           05 FILLER             PIC X(44).
           05 GRAND-TOTALS       PIC X(13) VALUE
                       'GRAND TOTALS:  '.
           05 FILLER             PIC X(2).
           05 TOTAL-COUNT        PIC Z,ZZZ,ZZ9.
           05 FILLER             PIC X(62) VALUE SPACES.

       01 AVG-INC-DEC-AMT.
           05 FILLER             PIC X(25).
           05 AVG-DEC-AMT        PIC X(33) VALUE
               'AVERAGE INCREASE/DECREASE AMOUNT:  '.
           05 FILLER             PIC X(5).
           05 TOTAL-AVG-AMT      PIC ZZ,ZZ9.
           05 FILLER             PIC X(62) VALUE SPACES.

       01 AVG-INC-DEC-PERCENT.
           05 FILLER             PIC X(21).
           05 TOTAL-AVG-PERCENT  PIC X(4).
           05 FILLER             PIC X(60).



       PROCEDURE DIVISION.
       L1-MAIN.
           PERFORM L2-INIT
           PERFORM L2-MAINLINE
             UNTIL MORE-RECS = "NO".
           PERFORM L3-CALCS.
           PERFORM L2-CLOSING.
           STOP RUN.


       L2-INIT.
           OPEN INPUT PIZZA-SALES.
           OPEN OUTPUT PRTOUT.
           MOVE FUNCTION CURRENT DATE TO CURRENT-DATE-AND-TIME.
           MOVE CURRENT-MONTH TO TITLE-MONTH.
           MOVE CURRENT-YEAR TO TITLE-YEAR.
           PERFORM L4-HEADING.
           PERFORM L3-READ-INPUT.

       L2-MAINLINE.
           PERFORM L3-CALCS.
           PERFORM L3-MOVE-PRINT.
           PERFORM L3-READ-INPUT.

       L2-CLOSING.
           PERFORM L3-TOTALS.


       L3-CALCS.
           C-SALES-INC-DEC = PIZZA-CUR-QTY - PIZZA-PREV-QTY.
           C-PERCENT-INC-DEC = C-SALES / PIZZA-PREV-QTY.
           C-TOTAL-SALES = PIZZA-PRICE * PIZZA-CUR-QTY.
           C-TOTAL-SALES-TOTAL =  C-TOTAL-SALES + C-TOTAL-SALE-TOTAL.
           C-SALES-INC-DEC-TOTAL = C-SALES-INC-DEC-TOTAL + 
           COMPUTE C-SALES-INC-DEC
           C-NUM-SALES = C-NUM-SALES +1 
           C-PREV-QTY-TOTAL = C-PREV-QTY-TOTAL + PIZZA-PREV-QTY
           C-AVG-INC-DEC= C-SALES-INC-DEC-TOTAL / C-NUM-SALE
           C-AVG-INC-DEC-PERCENT= C-SALES-INC-DEC-TOTAL / 
           C-PREV-QTY-TOTAL


       L3-MOVE-PRINT.
           MOVE
           WRITE PRTLINE FROM DETAIL-LINE
             AFTER ADVANCING 2 LINES
               AT EOP
                   PERFORM L4-HEADING.

       L-3-READ-INPUT.
           READ PIZZA-SALES
               AT END
                   MOVE 'NO' TO MORE-RECS.

       L3-TOTALS.
           WRITE PRTLINE FROM TOTAL-LINE
               AFTER ADVANCING 3 LINES.

       L4-HEADING.
           ADD 1 TO PAGE-CTR.
           MOVE PAGE-CTR TO TITLE-PAGE.
           WRITE PRTLINE FROM TITLE-LINE
               AFTER ADVANCING PAGE.
           WRITE PRTLINE FROM COL-HEADING1
               AFTER ADVANCING 2 LINES.
           WRITE PRTLINE FROM COL-HEADING2
               AFTER ADVANCING 1 LINE.
           WRITE PRTLINE FROM SPACES.
               AFTER ADVANCING 1 LINE.
          
           
       end program CBLHKW01.