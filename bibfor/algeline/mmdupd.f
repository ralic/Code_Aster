      SUBROUTINE  MMDUPD ( EHEAD, NEQNS, XADJ, ADJNCY, DELTA,
     1                     MDEG, DHEAD, DFORW, DBAKW, QSIZE,
     1                     LLIST, MARKER, MAXINT, TAG )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 07/01/2002   AUTEUR JFBHHUC C.ROSE 
C RESPONSABLE JFBHHUC C.ROSE
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR   
C (AT YOUR OPTION) ANY LATER VERSION.                                 
C
C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT 
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF          
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU    
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.                            
C
C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE   
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,       
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
      IMPLICIT REAL*8 (A-H,O-Z)
C
C--- SPARSPAK-A (ANSI FORTRAN) RELEASE III --- NAME = MMDUPD
C  (C)  UNIVERSITY OF WATERLOO   JANUARY 1984
C***************************************************************
C***************************************************************
C*****     MMDUPD ..... MULTIPLE MINIMUM DEGREE UPDATE     *****
C***************************************************************
C***************************************************************
C
C     PURPOSE - THIS ROUTINE UPDATES THE DEGREES OF NODES
C        AFTER A MULTIPLE ELIMINATION STEP.
C
C     INPUT PARAMETERS -
C        EHEAD  - THE BEGINNING OF THE LIST OF ELIMINATED
C                 NODES (I.E., NEWLY FORMED ELEMENTS).
C        NEQNS  - NUMBER OF EQUATIONS.
C        (XADJ,ADJNCY) - ADJACENCY STRUCTURE.
C        DELTA  - TOLERANCE VALUE FOR MULTIPLE ELIMINATION.
C        MAXINT - MAXIMUM MACHINE REPRESENTABLE (SHORT)
C                 INTEGER.
C
C     UPDATED PARAMETERS -
C        MDEG   - NEW MINIMUM DEGREE AFTER DEGREE UPDATE.
C        (DHEAD,DFORW,DBAKW) - DEGREE DOUBLY LINKED STRUCTURE.
C        QSIZE  - SIZE OF SUPERNODE.
C        LLIST  - WORKING LINKED LIST.
C        MARKER - MARKER VECTOR FOR DEGREE UPDATE.
C        TAG    - TAG VALUE.
C
C***************************************************************
C
         INTEGER    ADJNCY(*), DBAKW(*) , DFORW(*) , DHEAD(*) ,
     1              LLIST(*) , MARKER(*), QSIZE(*)
         INTEGER    XADJ(*)
         INTEGER    DEG   , DEG0  , DELTA , EHEAD , ELMNT ,
     1              ENODE , FNODE , I     , IQ2   , ISTOP ,
     1              ISTRT , J     , JSTOP , JSTRT , LINK  ,
     1              MAXINT, MDEG  , MDEG0 , MTAG  , NABOR ,
     1              NEQNS , NODE  , Q2HEAD, QXHEAD, TAG
C
C***************************************************************
C
         MDEG0 = MDEG + DELTA
         ELMNT = EHEAD
  100    CONTINUE
C            -------------------------------------------------------
C            FOR EACH OF THE NEWLY FORMED ELEMENT, DO THE FOLLOWING.
C            (RESET TAG VALUE IF NECESSARY.)
C            -------------------------------------------------------
             IF  ( ELMNT .LE. 0 )  GOTO 9999
             MTAG = TAG + MDEG0
             IF  ( MTAG .LT. MAXINT )  GO TO 300
                 TAG = 1
                 DO  200  I = 1, NEQNS
                     IF  ( MARKER(I) .LT. MAXINT )  MARKER(I) = 0
  200            CONTINUE
                 MTAG = TAG + MDEG0
  300        CONTINUE
C            ---------------------------------------------
C            CREATE TWO LINKED LISTS FROM NODES ASSOCIATED
C            WITH ELMNT: ONE WITH TWO NABORS (Q2HEAD) IN
C            ADJACENCY STRUCTURE, AND THE OTHER WITH MORE
C            THAN TWO NABORS (QXHEAD).  ALSO COMPUTE DEG0,
C            NUMBER OF NODES IN THIS ELEMENT.
C            ---------------------------------------------
             Q2HEAD = 0
             QXHEAD = 0
             DEG0 = 0
             LINK = ELMNT
  400        CONTINUE
                 ISTRT = XADJ(LINK)
                 ISTOP = XADJ(LINK+1) - 1
                 DO  700  I = ISTRT, ISTOP
                     ENODE = ADJNCY(I)
                     LINK = - ENODE
                     IF  ( ENODE )  400, 800, 500
C
  500                CONTINUE
                     IF  ( QSIZE(ENODE) .EQ. 0 )  GO TO 700
                         DEG0 = DEG0 + QSIZE(ENODE)
                         MARKER(ENODE) = MTAG
C                        ----------------------------------
C                        IF ENODE REQUIRES A DEGREE UPDATE,
C                        THEN DO THE FOLLOWING.
C                        ----------------------------------
                         IF  ( DBAKW(ENODE) .NE. 0 )  GO TO 700
C                            ---------------------------------------
C                            PLACE EITHER IN QXHEAD OR Q2HEAD LISTS.
C                            ---------------------------------------
                             IF  ( DFORW(ENODE) .EQ. 2 )  GO TO 600
                                 LLIST(ENODE) = QXHEAD
                                 QXHEAD = ENODE
                                 GO TO 700
  600                        CONTINUE
                             LLIST(ENODE) = Q2HEAD
                             Q2HEAD = ENODE
  700            CONTINUE
  800        CONTINUE
C            --------------------------------------------
C            FOR EACH ENODE IN Q2 LIST, DO THE FOLLOWING.
C            --------------------------------------------
             ENODE = Q2HEAD
             IQ2 = 1
  900        CONTINUE
                 IF  ( ENODE .LE. 0 )  GO TO 1500
                 IF  ( DBAKW(ENODE) .NE. 0 )  GO TO 2200
                     TAG = TAG + 1
                     DEG = DEG0
C                    ------------------------------------------
C                    IDENTIFY THE OTHER ADJACENT ELEMENT NABOR.
C                    ------------------------------------------
                     ISTRT = XADJ(ENODE)
                     NABOR = ADJNCY(ISTRT)
                     IF  ( NABOR .EQ. ELMNT )  NABOR = ADJNCY(ISTRT+1)
C                    ------------------------------------------------
C                    IF NABOR IS UNELIMINATED, INCREASE DEGREE COUNT.
C                    ------------------------------------------------
                     LINK = NABOR
                     IF  ( DFORW(NABOR) .LT. 0 )  GO TO 1000
                         DEG = DEG + QSIZE(NABOR)
                         GO TO 2100
 1000                CONTINUE
C                        --------------------------------------------
C                        OTHERWISE, FOR EACH NODE IN THE 2ND ELEMENT,
C                        DO THE FOLLOWING.
C                        --------------------------------------------
                         ISTRT = XADJ(LINK)
                         ISTOP = XADJ(LINK+1) - 1
                         DO  1400  I = ISTRT, ISTOP
                             NODE = ADJNCY(I)
                             LINK = - NODE
                             IF  ( NODE .EQ. ENODE )  GO TO 1400
                             IF  ( NODE )  1000, 2100, 1100
C
 1100                        CONTINUE
                             IF  ( QSIZE(NODE) .EQ. 0 )  GO TO 1400
                             IF  ( MARKER(NODE) .GE. TAG )  GO TO 1200
C                                -------------------------------------
C                                CASE WHEN NODE IS NOT YET CONSIDERED.
C                                -------------------------------------
                                 MARKER(NODE) = TAG
                                 DEG = DEG + QSIZE(NODE)
                                 GO TO 1400
 1200                        CONTINUE
C                            ----------------------------------------
C                            CASE WHEN NODE IS INDISTINGUISHABLE FROM
C                            ENODE.  MERGE THEM INTO A NEW SUPERNODE.
C                            ----------------------------------------
                             IF  ( DBAKW(NODE) .NE. 0 )  GO TO 1400
                             IF  ( DFORW(NODE) .NE. 2 )  GO TO 1300
                                 QSIZE(ENODE) = QSIZE(ENODE) +
     1                                          QSIZE(NODE)
                                 QSIZE(NODE) = 0
                                 MARKER(NODE) = MAXINT
                                 DFORW(NODE) = - ENODE
                                 DBAKW(NODE) = - MAXINT
                                 GO TO 1400
 1300                        CONTINUE
C                            --------------------------------------
C                            CASE WHEN NODE IS OUTMATCHED BY ENODE.
C                            --------------------------------------
                             IF  ( DBAKW(NODE) .EQ.0 )
     1                             DBAKW(NODE) = - MAXINT
 1400                    CONTINUE
                         GO TO 2100
 1500            CONTINUE
C                ------------------------------------------------
C                FOR EACH ENODE IN THE QX LIST, DO THE FOLLOWING.
C                ------------------------------------------------
                 ENODE = QXHEAD
                 IQ2 = 0
 1600            CONTINUE
                     IF  ( ENODE .LE. 0 )  GO TO 2300
                     IF  ( DBAKW(ENODE) .NE. 0 )  GO TO 2200
                         TAG = TAG + 1
                         DEG = DEG0
C                        ---------------------------------
C                        FOR EACH UNMARKED NABOR OF ENODE,
C                        DO THE FOLLOWING.
C                        ---------------------------------
                         ISTRT = XADJ(ENODE)
                         ISTOP = XADJ(ENODE+1) - 1
                         DO  2000  I = ISTRT, ISTOP
                             NABOR = ADJNCY(I)
                             IF  ( NABOR .EQ. 0 )  GO TO 2100
                             IF  ( MARKER(NABOR) .GE. TAG )  GO TO 2000
                                 MARKER(NABOR) = TAG
                                 LINK = NABOR
C                                ------------------------------
C                                IF UNELIMINATED, INCLUDE IT IN
C                                DEG COUNT.
C                                ------------------------------
                                 IF  ( DFORW(NABOR) .LT. 0 )  GO TO 1700
                                     DEG = DEG + QSIZE(NABOR)
                                     GO TO 2000
 1700                            CONTINUE
C                                    -------------------------------
C                                    IF ELIMINATED, INCLUDE UNMARKED
C                                    NODES IN THIS ELEMENT INTO THE
C                                    DEGREE COUNT.
C                                    -------------------------------
                                     JSTRT = XADJ(LINK)
                                     JSTOP = XADJ(LINK+1) - 1
                                     DO  1900  J = JSTRT, JSTOP
                                         NODE = ADJNCY(J)
                                         LINK = - NODE
                                         IF  ( NODE )  1700, 2000, 1800
C
 1800                                    CONTINUE
                                         IF  ( MARKER(NODE) .GE. TAG )
     1                                         GO TO 1900
                                             MARKER(NODE) = TAG
                                             DEG = DEG + QSIZE(NODE)
 1900                                CONTINUE
 2000                    CONTINUE
 2100                CONTINUE
C                    -------------------------------------------
C                    UPDATE EXTERNAL DEGREE OF ENODE IN DEGREE
C                    STRUCTURE, AND MDEG (MIN DEG) IF NECESSARY.
C                    -------------------------------------------
                     DEG = DEG - QSIZE(ENODE) + 1
                     FNODE = DHEAD(DEG)
                     DFORW(ENODE) = FNODE
                     DBAKW(ENODE) = - DEG
                     IF  ( FNODE .GT. 0 )  DBAKW(FNODE) = ENODE
                     DHEAD(DEG) = ENODE
                     IF  ( DEG .LT. MDEG )  MDEG = DEG
 2200                CONTINUE
C                    ----------------------------------
C                    GET NEXT ENODE IN CURRENT ELEMENT.
C                    ----------------------------------
                     ENODE = LLIST(ENODE)
                     IF  ( IQ2 .EQ. 1 )  GO TO 900
                         GO TO 1600
 2300        CONTINUE
C            -----------------------------
C            GET NEXT ELEMENT IN THE LIST.
C            -----------------------------
             TAG = MTAG
             ELMNT = LLIST(ELMNT)
             GO TO 100
C
 9999 CONTINUE
      END
