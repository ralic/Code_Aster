      SUBROUTINE HUJINI(MOD,NMAT,MATER,INTG,DEPS,NR,YD,NVI,VIND,SIGD,
     &                  SIGF,BNEWS,MTRAC,DY,INDI,IRET)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 30/04/2013   AUTEUR FOUCAULT A.FOUCAULT 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE FOUCAULT A.FOUCAULT
      IMPLICIT NONE
C     ----------------------------------------------------------------
C     CALCUL DE LA SOLUTION INITIALE ESSAI DY = ( DSIG DVIN )
C     ----------------------------------------------------------------
C     IN   MOD    :  TYPE DE MODELISATION
C          NMAT   :  DIMENSION TABLEAU MATERIAU 
C          MATER  :  PROPRIETES MATERIAU
C          INTG   :  NOMBRE DE TENTATIVES D'INTEGRATION
C          DEPS   :  INCREMENT DEFORMATION TOTALE
C          NR     :  DIMENSION SYSTEME NL A RESOUDRE 
C          YD     :  VECTEUR INITIAL A T
C          NVI    :  NOMBRE DE VARIABLES INTERNES
C          VIND   :  VARIABLES INTERNES A T
C          SIGD   :  ETAT DE CONTRAINTES A T
C          SIGF   :  PREDICTION ELASTIQUE
C          BNEWS  :  GESTION TRACTION AVEC HUJEUX
C          MTRAC  :  GESTION TRACTION AVEC HUJEUX (BIS) 
C          IRET   :  IRET = 2 - RELANCE DU PROCESSUS DE RESOLUTION
C     OUT  DY     :  INCREMENT INITIAL SUR VECTEUR SOLUTION
C          INDI   :  MECANISMES POTENTIELLEMENT ACTIFS
C          NR     :  NR MIS A JOUR SI TRACTION PRESENTE
C          IRET   :  IRET = 0 (OK) - 3 (NON CVG)
C     ----------------------------------------------------------------
      CHARACTER*8    MOD
      REAL*8         MATER(NMAT,2),DEPS(6),YD(18),VIND(NVI),SIGF(6)
      REAL*8         DY(18),SIGD(6)
      INTEGER        NR,NVI,IRET,INDI(7),INTG,NMAT
      LOGICAL        BNEWS(3),MTRAC
C
      REAL*8         I1F,DSIG(6),ZERO,UN,TROIS,PREF,E0,MATERT(22,2)
      LOGICAL        LOOP,NODEF
      INTEGER        NBMECA,NBMECT,I,II,NDT,NDI,INDIS(7),DIFF
C
      PARAMETER     (NDI   = 3   )
      PARAMETER     (NDT   = 6   )
      PARAMETER     (ZERO  = 0.D0)
      PARAMETER     (UN    = 1.D0)
      PARAMETER     (TROIS = 3.D0)
C     ----------------------------------------------------------------
C--------------------------
C ---- PROPRIETES MATERIAU
C -------------------------
      PREF = MATER(8,2)
      E0   = MATER(1,1)

C --- GESTION DES BOUCLES
      IF (IRET.EQ.2)THEN
        LOOP = .TRUE.
        DO 10 I = 1, 7
          INDIS(I) = INDI(I)
  10    CONTINUE
      ELSE
        LOOP = .FALSE.  
        DO 20 I = 1, 7
          INDIS(I) = 0
  20    CONTINUE
      ENDIF
C
      IRET = 0
C
C --- PREPARATION DE L'APPEL A HUJIID (ROUTINE DE L'ALGO SPECIFIQUE)
   1  CONTINUE

      IF(IRET.EQ.3)GOTO 999

C ---  INITIALISATION VECTEUR D'INDICE INDI(I=1,7)
      DO 30 I = 1, 7
        INDI(I) = 0
  30  CONTINUE
  
C ---  DEFINITION DU NOMBRE DE MECANISMES POTENTIELS ACTIFS
      NBMECA = 0
      DO 40 I = 1, 8
        IF (VIND(23+I) .EQ. UN) NBMECA = NBMECA + 1
  40  CONTINUE
C
C --- REMPLISSAGE DES MECANISMES POTENTIELLEMENT ACTIFS 
C
      II = 1
      DO 50 I = 1, 8
        IF (VIND(23+I) .EQ. UN) THEN
        
          IF (I .NE. 4) THEN
            INDI(II)            = I
            YD(NDT+1+II)        = VIND(I)
            YD(NDT+1+NBMECA+II) = ZERO
            II                  = II + 1
          ELSE
            INDI(NBMECA)        = I
            YD(NDT+1+NBMECA)    = VIND(I)
            YD(NDT+1+2*NBMECA)  = ZERO
          ENDIF  
          
        ENDIF
 50   CONTINUE

C --- REDIMENSIONNEMENT DE YD POUR S'ADAPTER A HUJIID
C --- COPIE A PARTIR DU TRAITEMENT DE HUJMID
      DO 60 I = 1, 6
        YD(I) = YD(I)*E0
  60  CONTINUE            

C
C --- PREPARATION DE L'INCREMENT DE CONTRAINTES
C
      DIFF = 0
      DO 70 I = 1, 7
        DIFF = DIFF + INDI(I)-INDIS(I)
  70  CONTINUE
      IF((DIFF.EQ.0).AND.(NBMECA.EQ.1))LOOP=.FALSE.

      IF (LOOP) THEN
        DO 80 I = 1, NDT
          DSIG(I) = SIGF(I) - SIGD(I)
  80    CONTINUE
      ELSE
        DO 90 I = 1, NDT
          DSIG(I) = ZERO
  90    CONTINUE
      ENDIF

      I1F = (SIGF(1)+SIGF(2)+SIGF(3))/TROIS

C
C --- APPEL A HUJIID
C
      DO 100 I = 1, 22 
        MATERT(I,1) = MATER(I,1)
        MATERT(I,2) = MATER(I,2)
 100  CONTINUE 

      CALL HUJIID (MOD, MATERT, INDI, DEPS, I1F, YD, VIND, DY,
     &             LOOP, DSIG, BNEWS, MTRAC, IRET)

C
C --- CONTROLE SUR LA SOLUTION INITIALE PROPOSEE
C
      NBMECT = NBMECA
      DO 110 I = 1, 7
        IF (INDI(I).GT.8) THEN
          NBMECT = NBMECT + 1
        ENDIF
 110  CONTINUE

      NODEF = .FALSE.
      IF (NBMECA.NE.NBMECT) THEN
        DO 120 I = 1, NDI
          IF (ABS(YD(I)+DSIG(I)).GT.PREF**2.D0) NODEF = .TRUE.
 120    CONTINUE 
        IF (NODEF) THEN
          IRET = 3
          IF(INTG.GT.5)THEN
            GOTO 999
          ELSE
            DO 130 I = NBMECA+1, NBMECT
              IF (DY(NDT+1+NBMECA+I).EQ.ZERO) THEN
                BNEWS(INDI(I)-8) = .TRUE.
                IRET = 2
              ENDIF
 130        CONTINUE 
            GOTO 1
          ENDIF
        ENDIF
      ENDIF

C --- REDIMENSIONNEMENT DE YD POUR S'ADAPTER A LCPLNL
C --- COPIE A PARTIR DU TRAITEMENT DE HUJMID
      DO 140 I = 1, 6
        YD(I) = YD(I)/E0
        DY(I) = DY(I)/E0
 140  CONTINUE            

      DO 150 I = 1, NBMECA
        YD(NDT+1+I) = YD(NDT+1+I)/E0*ABS(PREF)
        DY(NDT+1+I) = DY(NDT+1+I)/E0*ABS(PREF)
 150  CONTINUE            

      NR = NDT+1+NBMECA+NBMECT
      
      DO 160 I = NR+1, 18
        DY(I) = ZERO
 160  CONTINUE     

 999  CONTINUE 

      END
