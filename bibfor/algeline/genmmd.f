      SUBROUTINE GENMMD(NEQNS,NEQP1,NADJ,XADJ,ADJNCY,MAXINT,DELTA,INVP,
     +                  PERM,NBSN,SUPND,ADRESS,PARENT,GSSUBS,FCTNZS,
     +                  FCTOPS,DHEAD,QSIZE,LLIST,MARKER)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
C RESPONSABLE JFBHHUC C.ROSE
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT NONE
C
C--- SPARSPAK-A (ANSI FORTRAN) RELEASE III --- NAME = GENMMD
C  (C)  UNIVERSITY OF WATERLOO   JANUARY 1984
C       MODIFIE C.ROSE 24/8/92 : PARENT ET IF THEN ELSE DO WHILE
C         AJ  CALCUL DE GSSUBS,FCTNZS,FCTOPS,ADRESS
C***************************************************************
C***************************************************************
C****     GENMMD ..... MULTIPLE MINIMUM EXTERNAL DEGREE     ****
C***************************************************************
C***************************************************************
C
C     PURPOSE - THIS ROUTINE IMPLEMENTS THE MINIMUM DEGREE
C        ALGORITHM.  IT MAKES USE OF THE IMPLICIT REPRESENTATION
C        OF ELIMINATION GRAPHS BY QUOTIENT GRAPHS, AND THE
C        NOTION OF INDISTINGUISHABLE NODES.  IT ALSO IMPLEMENTS
C        THE MODIFICATIONS BY MULTIPLE ELIMINATION AND MINIMUM
C        EXTERNAL DEGREE.
C        ---------------------------------------------
C        CAUTION - THE ADJACENCY VECTOR ADJNCY WILL BE
C        DESTROYED.
C        ---------------------------------------------
C
C     INPUT PARAMETERS -
C        NEQNS  - NUMBER OF EQUATIONS.
C        (XADJ,ADJNCY) - THE ADJACENCY STRUCTURE.
C        DELTA  - TOLERANCE VALUE FOR MULTIPLE ELIMINATION.
C        MAXINT - MAXIMUM MACHINE REPRESENTABLE (SHORT) INTEGER
C                 (ANY SMALLER ESTIMATE WILL DO) FOR MARKING
C                 NODES.
C
C     OUTPUT PARAMETERS -
C        PERM   - THE MINIMUM DEGREE ORDERING.
C        INVP   - THE INVERSE OF PERM.
C        NOFSUB - AN UPPER BOUND ON THE NUMBER OF NONZERO
C                 SUBSCRIPTS FOR THE COMPRESSED STORAGE SCHEME.
C        LLIST  - VECTOR FOR TEMPORARY LINKED LISTS. DEVIENT INVSUP
C
C     WORKING PARAMETERS -
C        DHEAD  - VECTOR FOR HEAD OF DEGREE LISTS.
C        INVP   - USED TEMPORARILY FOR DEGREE FORWARD LINK.
C        PERM   - USED TEMPORARILY FOR DEGREE BACKWARD LINK.
C        QSIZE  - VECTOR FOR SIZE OF SUPERNODES.
C        MARKER - A TEMPORARY MARKER VECTOR.
C
C     PROGRAMME  ROUTINES -
C        MMDELM, MMDINT, MMDNUM, MMDUPD.
C
C***************************************************************
C
      INTEGER ADJNCY(NADJ),DHEAD(NEQNS),INVP(NEQNS),LLIST(NEQNS),
     +        MARKER(NEQNS),PERM(NEQNS),QSIZE(NEQNS)
      INTEGER XADJ(NEQP1),SUPND(NEQP1),ADRESS(NEQP1)
      INTEGER DELTA,EHEAD,I,MAXINT,MDEG,MDLMT,MDNODE,NEQNS,NEXTMD,
     +        NOFSUB,NUM,TAG
      INTEGER NBSN,PARENT(NEQNS)
      INTEGER NEQP1,NADJ,GSSUBS,FCTNZS
      REAL*8 FCTOPS
      INTEGER IL ,IS ,J ,JDEB ,JFIN ,NABOR ,NBSN1
      INTEGER NCOL ,NLIG
C-----------------------------------------------------------------------
C
      IF (NEQNS.LE.0) GOTO 9999
C
C        ------------------------------------------------
C        INITIALIZATION FOR THE MINIMUM DEGREE ALGORITHM.
C        ------------------------------------------------
      NOFSUB = 0
      CALL MMDINT(NEQNS,XADJ,DHEAD,INVP,PERM,QSIZE,LLIST,MARKER)
C
C        ----------------------------------------------
C        NUM COUNTS THE NUMBER OF ORDERED NODES PLUS 1.
C        ----------------------------------------------
      NUM = 1
C.ROSE AJ
      NBSN = 0
      DO 110 I = 1,NEQNS
      PARENT(I) = 0
  110 CONTINUE
      ADRESS(1) = 1
C.ROSE FIN AJ
C
C        -----------------------------
C        ELIMINATE ALL ISOLATED NODES.
C        -----------------------------
      NEXTMD = DHEAD(1)
C      DO WHILE (NEXTMD.GT.0)
  120 CONTINUE
      IF (NEXTMD.GT.0) THEN
          MDNODE = NEXTMD
          NEXTMD = INVP(MDNODE)
          MARKER(MDNODE) = MAXINT
C.ROSE AJ
          NBSN = NBSN + 1
          SUPND(NBSN) = NUM
          ADRESS(NBSN+1) = 1
C.ROSE FIN AJ
          INVP(MDNODE) = -NUM
          NUM = NUM + 1
          GO TO 120
C FIN DO WHILE
      END IF
C        ----------------------------------------
C        SEARCH FOR NODE OF THE MINIMUM DEGREE.
C        MDEG IS THE CURRENT MINIMUM DEGREE,
C        TAG IS USED TO FACILITATE MARKING NODES.
C        ----------------------------------------
      IF (NUM.GT.NEQNS) GO TO 190
      TAG = 1
      DHEAD(1) = 0
      MDEG = 2
  130 CONTINUE
C      DO WHILE (DHEAD(MDEG).LE.0)
  140 CONTINUE
      IF (DHEAD(MDEG).LE.0) THEN
          MDEG = MDEG + 1
          GO TO 140
C FIN DO WHILE
      END IF
C            -------------------------------------------------
C            USE VALUE OF DELTA TO SET UP MDLMT, WHICH GOVERNS
C            WHEN A DEGREE UPDATE IS TO BE PERFORMED.
C            -------------------------------------------------
      MDLMT = MDEG + DELTA
      EHEAD = 0
C
  150 CONTINUE
      MDNODE = DHEAD(MDEG)
C      DO WHILE (MDNODE.LE.0)
  160 CONTINUE
      IF (MDNODE.LE.0) THEN
          MDEG = MDEG + 1
          IF (MDEG.GT.MDLMT) GO TO 180
          MDNODE = DHEAD(MDEG)
          GO TO 160
C FIN DO WHILE
      END IF
C                ----------------------------------------
C                REMOVE MDNODE FROM THE DEGREE STRUCTURE.
C                ----------------------------------------
      NEXTMD = INVP(MDNODE)
      DHEAD(MDEG) = NEXTMD
      IF (NEXTMD.GT.0) PERM(NEXTMD) = -MDEG
C.ROSE AJ
      NBSN = NBSN + 1
      SUPND(NBSN) = NUM
      ADRESS(NBSN+1) = MDEG + QSIZE(MDNODE) - 1
C.ROSE FIN AJ   .................................................
      INVP(MDNODE) = -NUM
      NOFSUB = NOFSUB + MDEG + QSIZE(MDNODE) - 2
      IF (NUM+QSIZE(MDNODE).GT.NEQNS) GO TO 190
C                ----------------------------------------------
C                ELIMINATE MDNODE AND PERFORM QUOTIENT GRAPH
C                TRANSFORMATION.  RESET TAG VALUE IF NECESSARY.
C                ----------------------------------------------
      TAG = TAG + 1
      IF (TAG.GE.MAXINT) THEN
          TAG = 1
          DO 170 I = 1,NEQNS
              IF (MARKER(I).LT.MAXINT) MARKER(I) = 0
  170     CONTINUE
      END IF
      CALL MMDELM(MDNODE,XADJ,ADJNCY,DHEAD,INVP,PERM,QSIZE,LLIST,MARKER,
     +            MAXINT,TAG,PARENT)
C                                     AJ   ...........................
      NUM = NUM + QSIZE(MDNODE)
      LLIST(MDNODE) = EHEAD
      EHEAD = MDNODE
      IF (DELTA.GE.0) GO TO 150
  180 CONTINUE
C            -------------------------------------------
C            UPDATE DEGREES OF THE NODES INVOLVED IN THE
C            MINIMUM DEGREE NODES ELIMINATION.
C            -------------------------------------------
      IF (NUM.GT.NEQNS) GO TO 190
      CALL MMDUPD(EHEAD,NEQNS,XADJ,ADJNCY,DELTA,MDEG,DHEAD,INVP,PERM,
     +            QSIZE,LLIST,MARKER,MAXINT,TAG)
      GO TO 130
C
  190 CONTINUE
      IF (MDNODE.GT.0) THEN
C        ON TERMINE PARENT NODAL
          DO 200 I = XADJ(MDNODE),XADJ(MDNODE+1) - 1
              NABOR = ADJNCY(I)
              IF (NABOR.EQ.0) GO TO 210
              IF (INVP(NABOR).LT.0) THEN
                  PARENT(NABOR) = MDNODE
              END IF
  200     CONTINUE
      END IF
  210 CONTINUE
      NBSN1 = NBSN + 1
      SUPND(NBSN1) = NEQNS + 1
      GSSUBS = 0
      FCTNZS = 0
      FCTOPS = 0.0D0
      DO 230 IS = 1,NBSN
          JDEB = SUPND(IS)
          JFIN = SUPND(IS+1) - 1
          NCOL = JFIN - JDEB + 1
          NLIG = ADRESS(IS+1)
          GSSUBS = GSSUBS + NLIG
          FCTNZS = FCTNZS + NLIG*NCOL - (NCOL* (NCOL+1))/2
          IL = NLIG
          DO 220 J = JDEB,JFIN
              IL = IL - 1
              FCTOPS = FCTOPS + DBLE(IL* (IL+3))
  220     CONTINUE
          ADRESS(IS+1) = ADRESS(IS) + NLIG
  230 CONTINUE
      CALL MMDNUM(NEQNS,PERM,INVP,QSIZE)
C        CALCUL DE PARENT EN SUPERNOEUDS MMDPAR SUR CRAY UTILISE QSIZE
C                          EN DERNIER ARGUMENT ??? TABLEAU DE TRAVAIL ?
C          CALL MMDPAR(NEQNS,NBSN,NBSN1,SUPND,INVP,PARENT,DHEAD,QSIZE)
      CALL MMDPAR(NEQNS,NBSN,NBSN1,SUPND,INVP,PARENT,DHEAD,LLIST)
      GOTO 9999
C
 9999 CONTINUE
      END
