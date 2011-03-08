      SUBROUTINE PJEFLO(ELREFA,NDIM,IPB,XR2,ALARM,MA2,INO2,MA1,IMA1,
     &                  LEXT)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 08/03/2011   AUTEUR PELLET J.PELLET 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C RESPONSABLE PELLET J.PELLET
      IMPLICIT NONE
      CHARACTER*(*) ALARM,ELREFA
      CHARACTER*8 MA1,MA2
      INTEGER IPB,IMA1,NDIM,INO2
      REAL*8 XR2(NDIM)
      LOGICAL LEXT
C ----------------------------------------------------------------------
C BUT :
C   * EMETTRE UNE ALARME SI INO2 EST TROP LOIN DE IMA1.
C   * DETERMINER SI INO2 EST EXTERIEUR A IMA1
C ----------------------------------------------------------------------
C
C IN  ELREFA : ELREFA DE L'ELEMENT
C IN  NDIM   : DIMENSION DE L'ESPACE
C IN  XR2     : COORDONNEES DU POINT DANS L'ESPACE PARA DE L'ELEMENT
C              (CALCULE PAR REEREG)
C IN  IPBD   : CODE RETOUR DE REEREG
C IN  ALARM  : 'OUI'/'NON' (VEUT-ON IMPRIMER L'ALARME)
C IN  MA1    : NOM DU MAILLAGE "1"
C IN  MA2    : NOM DU MAILLAGE "2"
C IN  INO2   : NUMERO DU NOEUD DANS MA2
C IN  IMA1   : NUMERO DE LA MAILLE DANS MA1
C OUT LEXT   : .TRUE. <=> INO2 EST EXTERIEUR A IMA1
C ----------------------------------------------------------------------
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
      CHARACTER*32 JEXNUM,JEXNOM,JEXR8,JEXATR
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------

      REAL*8 D,TOLALA,TOLEXT,X,Y,Z
      CHARACTER*8 NOMNO,NOMMA
      CHARACTER*24 VALK(5)
      INTEGER K,IBID
C ----------------------------------------------------------------------
      CALL ASSERT(ALARM.EQ.'OUI' .OR. ALARM.EQ.'NON')
      LEXT=.FALSE.


C     -- SI REEREG N'A PAS CONVERGE, ON N'A PAS CONFIANCE DANS XR2 :
      IF (IPB.NE.0) THEN
        D=999.D0
        GOTO 70

      ENDIF

      IF (NDIM.GE.1)X=XR2(1)
      IF (NDIM.GE.2)Y=XR2(2)
      IF (NDIM.GE.3)Z=XR2(3)


C     -- POUR LES HEXA : KSI,ETA,DZETA SONT DANS [-1,1]
C     ----------------------------------------------------
      IF (ELREFA.EQ.'HE8' .OR. ELREFA.EQ.'H20' .OR.
     &    ELREFA.EQ.'H27') THEN
        CALL ASSERT(NDIM.EQ.3)
        IF (ABS(X).GT.1.D0)GOTO 10
        IF (ABS(Y).GT.1.D0)GOTO 10
        IF (ABS(Z).GT.1.D0)GOTO 10

C       -- ON EST INTERIEUR
        GOTO 80

   10   CONTINUE
C       -- ON EST EXTERIEUR. EST-ON LOIN ?
        D=0.D0
        D=MAX(D,ABS(X)-1.D0)
        D=MAX(D,ABS(Y)-1.D0)
        D=MAX(D,ABS(Z)-1.D0)


C     -- POUR LES TETRA :
C     ----------------------------------------------------
      ELSEIF (ELREFA.EQ.'TE4' .OR. ELREFA.EQ.'T10') THEN
        CALL ASSERT(NDIM.EQ.3)
        IF (X.LT.0.D0)GOTO 20
        IF (Y.LT.0.D0)GOTO 20
        IF (Z.LT.0.D0)GOTO 20
        IF (X+Y+Z.GT.1.D0)GOTO 20

C       -- ON EST INTERIEUR
        GOTO 80

   20   CONTINUE
C       -- ON EST EXTERIEUR. EST-ON LOIN ?
        D=0.D0
        D=MAX(D,-X)
        D=MAX(D,-Y)
        D=MAX(D,-Z)
        D=MAX(D,X+Y+Z-1.D0)


C     -- POUR LES PYRAM :
C     ----------------------------------------------------
      ELSEIF (ELREFA.EQ.'PY5' .OR. ELREFA.EQ.'P13') THEN
        CALL ASSERT(NDIM.EQ.3)
        IF (Z.LT.0.D0)GOTO 30
        IF (X+Y+Z.GT.1.D0)GOTO 30
        IF (X-Y+Z.GT.1.D0)GOTO 30
        IF (-X+Y+Z.GT.1.D0)GOTO 30
        IF (-X-Y+Z.GT.1.D0)GOTO 30

C       -- ON EST INTERIEUR
        GOTO 80

   30   CONTINUE
C       -- ON EST EXTERIEUR. EST-ON LOIN ?
        D=0.D0
        D=MAX(D,-Z)
        D=MAX(D,X+Y+Z-1.D0)
        D=MAX(D,X-Y+Z-1.D0)
        D=MAX(D,-X+Y+Z-1.D0)
        D=MAX(D,-X-Y+Z-1.D0)


C     -- POUR LES PENTA :
C     ----------------------------------------------------
      ELSEIF (ELREFA.EQ.'PE6' .OR. ELREFA.EQ.'P15' .OR.
     &        ELREFA.EQ.'P18') THEN
        CALL ASSERT(NDIM.EQ.3)
        IF (X.LT.-1.D0)GOTO 40
        IF (X.GT.+1.D0)GOTO 40
        IF (Y.LT.0.D0)GOTO 40
        IF (Z.LT.0.D0)GOTO 40
        IF (Y+Z.GT.1.D0)GOTO 40

C       -- ON EST INTERIEUR
        GOTO 80

   40   CONTINUE
C       -- ON EST EXTERIEUR. EST-ON LOIN ?
        D=0.D0
        D=MAX(D,ABS(X)-1.D0)
        D=MAX(D,-Y)
        D=MAX(D,-Z)
        D=MAX(D,+Y+Z-1.D0)


C     -- POUR LES TRIA :
C     ----------------------------------------------------
      ELSEIF (ELREFA.EQ.'TR3' .OR. ELREFA.EQ.'TR6' .OR.
     &        ELREFA.EQ.'TR7') THEN
        CALL ASSERT(NDIM.EQ.2)
        IF (X.LT.0.D0)GOTO 50
        IF (Y.LT.0.D0)GOTO 50
        IF (X+Y.GT.1.D0)GOTO 50

C       -- ON EST INTERIEUR
        GOTO 80

   50   CONTINUE
C       -- ON EST EXTERIEUR. EST-ON LOIN ?
        D=0.D0
        D=MAX(D,-X)
        D=MAX(D,-Y)
        D=MAX(D,+X+Y-1.D0)


C     -- POUR LES QUAD :
C     ----------------------------------------------------
      ELSEIF (ELREFA.EQ.'QU4' .OR. ELREFA.EQ.'QU8' .OR.
     &        ELREFA.EQ.'QU9') THEN
        CALL ASSERT(NDIM.EQ.2)
        IF (X.LT.-1.D0)GOTO 60
        IF (Y.LT.-1.D0)GOTO 60
        IF (X.GT.+1.D0)GOTO 60
        IF (Y.GT.+1.D0)GOTO 60

C       -- ON EST INTERIEUR
        GOTO 80

   60   CONTINUE
C       -- ON EST EXTERIEUR. EST-ON LOIN ?
        D=0.D0
        D=MAX(D,-1.D0-X)
        D=MAX(D,-1.D0-Y)
        D=MAX(D,X-1.D0)
        D=MAX(D,Y-1.D0)

      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF


C     -- EST-ON EXTERIEUR ?
C     -------------------------------
   70 CONTINUE
      TOLEXT=1.D-2
      IF (D.GT.TOLEXT)LEXT=.TRUE.

C     -- DOIT-ON EMETTRE UNE ALARME ?
C     -------------------------------
      TOLALA=1.D-1
      IF (D.GT.TOLALA) THEN
        IF (ALARM.EQ.'OUI') THEN
          CALL JENUNO(JEXNUM(MA2//'.NOMNOE',INO2),NOMNO)
          CALL JENUNO(JEXNUM(MA1//'.NOMMAI',IMA1),NOMMA)
          VALK(1)=NOMNO
          VALK(2)=MA2
          VALK(3)=NOMMA
          VALK(4)=MA1
          CALL U2MESK('A','CALCULEL5_7',4,VALK)
        ENDIF
      ENDIF


   80 CONTINUE
      END
