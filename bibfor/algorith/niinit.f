      SUBROUTINE NIINIT(NOMTE,TYPMOD,NDIM,NNO1,NNO2,NNO3,VU,VG,VP)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 28/08/2012   AUTEUR SFAYOLLE S.FAYOLLE 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
      IMPLICIT NONE

      CHARACTER*8  TYPMOD(*)
      CHARACTER*16 NOMTE,ALIAS
      INTEGER NDIM,NNO1,NNO2,NNO3,VU(3,27),VG(27),VP(27),IRET,IEFM
C ----------------------------------------------------------------------
C        INITIALISATION POUR LES ELEMENTS QUASI-INCOMPRESSIBLES
C ----------------------------------------------------------------------
C IN  NOMTE     NOM DE L'ELEMENT
C IN  TYPMOD    TYPE DE MODELISATION
C IN  NDIM      DIMENSION DE L'ESPACE
C IN  NNO1      NOMBRE DE NOEUDS POUR L'ELEMENT PORTANT LES DEPLACEMENTS
C IN  NNO2      NOMBRE DE NOEUDS POUR L'ELEMENT PORTANT LES GONFELEMENTS
C IN  NNO3      NOMBRE DE NOEUDS POUR L'ELEMENT PORTANT LES PRESSIONS
C OUT VU
C OUT VG
C OUT VP
C LOC IEFM : INDENTIFIANT DE L ELEMENT FINI MIXTE : 1 = P2-P1-P1
C                                                   2 = P2-P1-P2
C                                                   3 = P2-P0-P0
C                                                   4 = P2-P1-CR
C                                                   5 = P2-P2-P2
C                                                   6 = P1-P1-P1
C
C RM : POUR L INSTANT ON NE TRAITE QUE LES ELEMENTS P2-P1-P1 ET P2-P1-P2
C      A TERME IL FAUDRA ACTER DE L ELEMENT LE PLUS EFFICACE
C ----------------------------------------------------------------------
      INTEGER N,OS
C ----------------------------------------------------------------------

C    RECUPERATION DU TYPE D'ELEMENT VIA L'ALIAS
      CALL TEATTR(' ','S','ALIAS8',ALIAS,IRET)

      IF(NNO1 .EQ. NNO3) THEN
C       P2.PX.P2 OU P1.PX.P1
C        IF(NNO1 .EQ. NNO2)THEN
C       P2.P2.P2 OU P1.P1.P1
C          IF (ALIAS(6:8).EQ.'T10')  IEFM = 5
C          IF (ALIAS(6:8).EQ.'H20')  IEFM = 5
C          IF (ALIAS(6:8).EQ.'P15')  IEFM = 5
C          IF (ALIAS(6:8).EQ.'TR6')  IEFM = 5
C          IF (ALIAS(6:8).EQ.'QU8')  IEFM = 5
C          IF (ALIAS(6:8).EQ.'TE4')  IEFM = 6
C          IF (ALIAS(6:8).EQ.'HE8')  IEFM = 6
C          IF (ALIAS(6:8).EQ.'PE6')  IEFM = 6
C          IF (ALIAS(6:8).EQ.'TR3')  IEFM = 6
C          IF (ALIAS(6:8).EQ.'QU4')  IEFM = 6
C        ELSE
C       P2.P1.P2
          IF (ALIAS(6:8).EQ.'T10')  IEFM = 2
          IF (ALIAS(6:8).EQ.'H20')  IEFM = 2
          IF (ALIAS(6:8).EQ.'P15')  IEFM = 2
          IF (ALIAS(6:8).EQ.'TR6')  IEFM = 2
          IF (ALIAS(6:8).EQ.'QU8')  IEFM = 2
C        ENDIF
      ELSEIF(NNO2 .EQ. NNO3)THEN
C       PX.P1.P1 OU PX.P0.P0
        IF (ALIAS(6:8).EQ.'T10')  IEFM = 1
        IF (ALIAS(6:8).EQ.'H20')  IEFM = 1
        IF (ALIAS(6:8).EQ.'P15')  IEFM = 1
        IF (ALIAS(6:8).EQ.'TR6')  IEFM = 1
        IF (ALIAS(6:8).EQ.'QU8')  IEFM = 1
C       2D-P2.P0.P0
C        IF (ALIAS(6:8).EQ.'TR7')  IEFM = 3
C      ELSE
C       3D-P2.P1.CR
C        IF (ALIAS(6:8).EQ.'T14')  IEFM = 4
      ENDIF

      IF(NDIM .EQ. 3)THEN
        IF (IEFM .EQ. 1) THEN
C       3D-P2.P1.P1
          DO 10 N = 1,NNO2
            VU(1,N) = 1 + (N-1)*5
            VU(2,N) = 2 + (N-1)*5
            VU(3,N) = 3 + (N-1)*5
            VP(N)   = 4 + (N-1)*5
            VG(N)   = 5 + (N-1)*5
 10       CONTINUE
          OS = 5*NNO2
          DO 20 N = 1,NNO1-NNO2
            VU(1,N+NNO2) = 1 + (N-1)*3 + OS
            VU(2,N+NNO2) = 2 + (N-1)*3 + OS
            VU(3,N+NNO2) = 3 + (N-1)*3 + OS
 20       CONTINUE
          GOTO 1000
        ELSEIF (IEFM .EQ. 2) THEN
C       3D-P2.P1.P1
          DO 30 N = 1,NNO2
            VU(1,N) = 1 + (N-1)*5
            VU(2,N) = 2 + (N-1)*5
            VU(3,N) = 3 + (N-1)*5
            VP(N)   = 4 + (N-1)*5
            VG(N)   = 5 + (N-1)*5
 30       CONTINUE
          OS = 5*NNO2
          DO 40 N = 1,NNO1-NNO2
            VU(1,N+NNO2) = 1 + (N-1)*4 + OS
            VU(2,N+NNO2) = 2 + (N-1)*4 + OS
            VU(3,N+NNO2) = 3 + (N-1)*4 + OS
            VP(  N+NNO2) = 4 + (N-1)*4 + OS
 40       CONTINUE
          GOTO 1000
C       ELSEIF (IEFM .EQ. 4) THEN
C       3D-P2.P1.CR
C         DO 50 N = 1,NNO2
C           DO 60 I = 1,NDIM
C             VU(I,N) = I + (N-1)*(NDIM+1)
C  60       CONTINUE
C           VG(N) = 1 + NDIM + (N-1)*(NDIM+1)
C  50     CONTINUE
C         OS = (1+NDIM)*NNO2
C         DO 70 N = 1,NNO1-NNO2
C           DO 80 I = 1,NDIM
C             VU(I,N+NNO2) = I + (N-1)*(NDIM) + OS
C  80       CONTINUE
C  70     CONTINUE
C         OS = NNO1*NDIM + NNO2
C         DO 90 N = 1,NNO3
C           VP(N) = OS + N
C  90     CONTINUE
C         GOTO 1000
C        ELSEIF (IEFM .EQ. 5 .OR. IEFM .EQ. 6) THEN
C       3D-P2.P2.P2 ET 3D-P1.P1.P1
C          DO 100 N = 1,NNO1
C            VU(1,N) = 1 + (N-1)*5
C            VU(2,N) = 2 + (N-1)*5
C            VU(3,N) = 3 + (N-1)*5
C            VP(N)   = 4 + (N-1)*5
C            VG(N)   = 5 + (N-1)*5
C 100      CONTINUE
C          GOTO 1000
        END IF
      ELSEIF(NDIM .EQ. 2)THEN
        IF (IEFM .EQ. 1) THEN
C       2D-P2.P1.P1
          DO 110 N = 1,NNO2
            VU(1,N) = 1 + (N-1)*4
            VU(2,N) = 2 + (N-1)*4
            VU(3,N) = 0
            VP(N)   = 3 + (N-1)*4
            VG(N)   = 4 + (N-1)*4
 110      CONTINUE
          OS = 4*NNO2
          DO 120 N = 1,NNO1-NNO2
            VU(1,N+NNO2) = 1 + (N-1)*2 + OS
            VU(2,N+NNO2) = 2 + (N-1)*2 + OS
 120       CONTINUE
          GOTO 1000
        ELSEIF (IEFM .EQ. 2) THEN
C       2D-P2.P1.P2
          DO 130 N = 1,NNO2
            VU(1,N) = 1 + (N-1)*4
            VU(2,N) = 2 + (N-1)*4
            VU(3,N) = 0
            VP(N)   = 3 + (N-1)*4
            VG(N)   = 4 + (N-1)*4
 130      CONTINUE
          OS = 4*NNO2
          DO 140 N = 1,NNO1-NNO2
            VU(1,N+NNO2) = 1 + (N-1)*3 + OS
            VU(2,N+NNO2) = 2 + (N-1)*3 + OS
            VU(3,N) = 0
            VP(N+NNO2)   = 3 + (N-1)*3 + OS
 140      CONTINUE
          GOTO 1000
C       ELSEIF (IEFM .EQ. 3) THEN
C       2D-P2.P0.P0
C         DO 150 N = 1,NNO1
C           VU(1,N) = 1 + (N-1)*2
C           VU(2,N) = 2 + (N-1)*2
C  150    CONTINUE
C         OS = 2*NNO1
C         VP(1) = OS+1
C         VG(1) = OS+2
C         GOTO 1000
C       ELSEIF (IEFM .EQ. 4) THEN
C       2D-P2.P1.CR
C         DO 160 N = 1,NNO2
C           VU(1,N) = 1 + (N-1)*3
C           VU(2,N) = 2 + (N-1)*3
C           VG(N)   = 3 + (N-1)*3
C  160    CONTINUE
C         OS = 3*NNO2
C         DO 170 N = 1,NNO1-NNO2
C           VU(1,N+NNO2) = 1 + (N-1)*3 + OS
C           VU(2,N+NNO2) = 2 + (N-1)*3 + OS
C           VP(N)        = 3 + (N-1)*3 + OS
C  170    CONTINUE
C         GOTO 1000
C        ELSEIF (IEFM .EQ. 5 .OR. IEFM .EQ. 6) THEN
C       2D-P2.P2.P2 ET 2D-P1.P1.P1
C          DO 180 N = 1,NNO1
C            VU(1,N) = 1 + (N-1)*4
C            VU(2,N) = 2 + (N-1)*4
C            VU(3,N) = 0
C            VP(N)   = 3 + (N-1)*4
C            VG(N)   = 4 + (N-1)*4
C 180      CONTINUE
C          GOTO 1000
        END IF
      ENDIF

      CALL U2MESK('F','DVP_4',1,NOMTE)

 1000 CONTINUE

      IF (TYPMOD(1).EQ.'AXIS') THEN
        DO 60 N = 1,NNO1
          VU(3,N) = VU(1,N)
 60     CONTINUE
      END IF

      END
