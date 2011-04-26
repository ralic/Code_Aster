      SUBROUTINE MMDELM(MDNODE,XADJ,ADJNCY,DHEAD,DFORW,DBAKW,QSIZE,
     +                  LLIST,MARKER,MAXINT,TAG,PARENT)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
C RESPONSABLE JFBHHUC C.ROSE
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C--- SPARSPAK-A (ANSI FORTRAN) RELEASE III --- NAME = MMDELM
C  (C)  UNIVERSITY OF WATERLOO   JANUARY 1984
C   C.ROSE AJ    DE PARENT + MODIFICA STRUCTURE : IF THEN ELSE ET DO WHI
C***************************************************************
C***************************************************************
C**     MMDELM ..... MULTIPLE MINIMUM DEGREE ELIMINATION     ***
C***************************************************************
C***************************************************************
C
C     PURPOSE - THIS ROUTINE ELIMINATES THE NODE MDNODE OF
C        MINIMUM DEGREE FROM THE ADJACENCY STRUCTURE, WHICH
C        IS STORED IN THE QUOTIENT GRAPH FORMAT.  IT ALSO
C        TRANSFORMS THE QUOTIENT GRAPH REPRESENTATION OF THE
C        ELIMINATION GRAPH.
C
C     INPUT PARAMETERS -
C        MDNODE - NODE OF MINIMUM DEGREE.
C        MAXINT - ESTIMATE OF MAXIMUM REPRESENTABLE (SHORT)
C                 INTEGER.
C        TAG    - TAG VALUE.
C
C     UPDATED PARAMETERS -
C        (XADJ,ADJNCY) - UPDATED ADJACENCY STRUCTURE.
C        (DHEAD,DFORW,DBAKW) - DEGREE DOUBLY LINKED STRUCTURE.
C        QSIZE  - SIZE OF SUPERNODE.
C        MARKER - MARKER VECTOR.
C        LLIST  - TEMPORARY LINKED LIST OF ELIMINATED NABORS.
C
C***************************************************************
C
      INTEGER ADJNCY(*),DBAKW(*),DFORW(*)
      INTEGER LLIST(*),MARKER(*),QSIZE(*),DHEAD(*)
      INTEGER XADJ(*),PARENT(*)
      INTEGER ELMNT,I,ISTOP,ISTRT,J,JSTOP,JSTRT,LINK,MAXINT,MDNODE,
     +        NABOR,NODE,NPV,NQNBRS,NXNODE,PVNODE,RLMT,RLOC,RNODE,TAG,
     +        XQNBR
C
C***************************************************************
C
C        -----------------------------------------------
C        FIND REACHABLE SET AND PLACE IN DATA STRUCTURE.
C        -----------------------------------------------
      MARKER(MDNODE) = TAG
      ISTRT = XADJ(MDNODE)
      ISTOP = XADJ(MDNODE+1) - 1
C        -------------------------------------------------------
C        ELMNT POINTS TO THE BEGINNING OF THE LIST OF ELIMINATED
C        NABORS OF MDNODE, AND RLOC GIVES THE STORAGE LOCATION
C        FOR THE NEXT REACHABLE NODE.
C        -------------------------------------------------------
      ELMNT = 0
      RLOC = ISTRT
      RLMT = ISTOP
      DO 110 I = ISTRT,ISTOP
          NABOR = ADJNCY(I)
          IF (NABOR.EQ.0) GO TO 120
          IF (MARKER(NABOR).LT.TAG) THEN
              MARKER(NABOR) = TAG
              IF (DFORW(NABOR).GE.0) THEN
                  ADJNCY(RLOC) = NABOR
                  RLOC = RLOC + 1
              ELSE
                  LLIST(NABOR) = ELMNT
                  ELMNT = NABOR
                  PARENT(NABOR) = MDNODE
              END IF
          END IF
  110 CONTINUE
  120 CONTINUE
C            -----------------------------------------------------
C            MERGE WITH REACHABLE NODES FROM GENERALIZED ELEMENTS.
C            -----------------------------------------------------
C      DO WHILE (ELMNT.GT.0)
  130 CONTINUE
      IF (ELMNT.GT.0) THEN
          ADJNCY(RLMT) = -ELMNT
          LINK = ELMNT
  140     CONTINUE
          JSTRT = XADJ(LINK)
          JSTOP = XADJ(LINK+1) - 1
          DO 160 J = JSTRT,JSTOP
              NODE = ADJNCY(J)
              LINK = -NODE
              IF (NODE.LT.0) GO TO 140
              IF (NODE.EQ.0) GO TO 170
              IF (MARKER(NODE).LT.TAG .AND. DFORW(NODE).GE.0) THEN
                  MARKER(NODE) = TAG
C                            ---------------------------------
C                            USE STORAGE FROM ELIMINATED NODES
C                            IF NECESSARY.
C                            ---------------------------------
C            DO WHILE (RLOC.GE.RLMT)
  150             CONTINUE
                  IF (RLOC.GE.RLMT) THEN
                      LINK = -ADJNCY(RLMT)
                      RLOC = XADJ(LINK)
                      RLMT = XADJ(LINK+1) - 1
                      GO TO 150
C FIN DO WHILE
                  END IF
                  ADJNCY(RLOC) = NODE
                  RLOC = RLOC + 1
              END IF
  160     CONTINUE
  170     CONTINUE
          ELMNT = LLIST(ELMNT)
          GO TO 130
C FIN DO WHILE
      END IF
      IF (RLOC.LE.RLMT) ADJNCY(RLOC) = 0
C        --------------------------------------------------------
C        FOR EACH NODE IN THE REACHABLE SET, DO THE FOLLOWING ...
C        --------------------------------------------------------
      LINK = MDNODE
  180 CONTINUE
      ISTRT = XADJ(LINK)
      ISTOP = XADJ(LINK+1) - 1
      DO 210 I = ISTRT,ISTOP
          RNODE = ADJNCY(I)
          LINK = -RNODE
          IF (RNODE.EQ.0) GO TO 220
          IF (RNODE.LT.0) GO TO 180
C                --------------------------------------------
C                IF RNODE IS IN THE DEGREE LIST STRUCTURE ...
C                --------------------------------------------
          PVNODE = DBAKW(RNODE)
          IF (PVNODE.NE.0 .AND. PVNODE.NE. (-MAXINT)) THEN
C                    -------------------------------------
C                    THEN REMOVE RNODE FROM THE STRUCTURE.
C                    -------------------------------------
              NXNODE = DFORW(RNODE)
              IF (NXNODE.GT.0) DBAKW(NXNODE) = PVNODE
              IF (PVNODE.GT.0) DFORW(PVNODE) = NXNODE
              NPV = -PVNODE
              IF (PVNODE.LT.0) DHEAD(NPV) = NXNODE
          END IF
C                ----------------------------------------
C                PURGE INACTIVE QUOTIENT NABORS OF RNODE.
C                ----------------------------------------
          JSTRT = XADJ(RNODE)
          JSTOP = XADJ(RNODE+1) - 1
          XQNBR = JSTRT
          DO 190 J = JSTRT,JSTOP
              NABOR = ADJNCY(J)
              IF (NABOR.EQ.0) GO TO 200
              IF (MARKER(NABOR).LT.TAG) THEN
                  ADJNCY(XQNBR) = NABOR
                  XQNBR = XQNBR + 1
              END IF
  190     CONTINUE
  200     CONTINUE
C                ----------------------------------------
C                IF NO ACTIVE NABOR AFTER THE PURGING ...
C                ----------------------------------------
          NQNBRS = XQNBR - JSTRT
          IF (NQNBRS.LE.0) THEN
C                    -----------------------------
C                    THEN MERGE RNODE WITH MDNODE.
C                    -----------------------------
              QSIZE(MDNODE) = QSIZE(MDNODE) + QSIZE(RNODE)
              QSIZE(RNODE) = 0
              MARKER(RNODE) = MAXINT
              DFORW(RNODE) = -MDNODE
              DBAKW(RNODE) = -MAXINT
          ELSE
C                --------------------------------------
C                ELSE FLAG RNODE FOR DEGREE UPDATE, AND
C                ADD MDNODE AS A NABOR OF RNODE.
C                --------------------------------------
              DFORW(RNODE) = NQNBRS + 1
              DBAKW(RNODE) = 0
              ADJNCY(XQNBR) = MDNODE
              XQNBR = XQNBR + 1
              IF (XQNBR.LE.JSTOP) ADJNCY(XQNBR) = 0
          END IF
C
  210 CONTINUE
  220 CONTINUE
      END
