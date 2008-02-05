      SUBROUTINE LCFRGV (NDIM  , TYPMOD, OPTION, MAT , EPSM  ,
     &                   DEPS  , VIM   , NONLOC     , SIG   , VIP   ,
     &                   DSIDEP)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 04/02/2008   AUTEUR GODARD V.GODARD 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
      CHARACTER*8  TYPMOD
      CHARACTER*16 OPTION
      INTEGER      NDIM, MAT
      REAL*8       EPSM(6), DEPS(6), VIM(2),NONLOC(3)
      REAL*8       VIP(2), SIG(6), DSIDEP(6,6,4)

C ----------------------------------------------------------------------
C     ENDOMMAGEMENT FRAGILE A GRADIENT DE VARIABLE INTERNE
C     ENDO_FRAGILE avec GRAD_VARI
C ----------------------------------------------------------------------
C IN  NDIM    DIMENSION DE L'ESPACE
C IN  TYPMOD  TYPE DE MODELISATION
C IN  OPTION  OPTION DE CALCUL
C               RIGI_MECA_TANG, RIGI_MECA_ELAS
C               RAPH_MECA
C               FULL_MECA, FULL_MECA_ELAS
C IN  MAT     NATURE DU MATERIAU
C IN  EPSM    CHAMP DE DEFORMATION EN T- ET PHIM=EPSM(7)
C IN  DEPS    INCREMENT DU CHAMP DE DEFORMATION ET DPHI=DEPS(7)
C IN  VIM     VARIABLES INTERNES EN T-
C IN  NONLOC  VARIABLES NON LOCALES
C OUT VIP     DENSITE DE FISSURATION 
C OUT SIG     CONTRAINTE 
C OUT DSIDEP  MATRICES TANGENTES
C ----------------------------------------------------------------------

      LOGICAL CPLAN, RIGI, RESI, ELAS
      INTEGER NDIMSI, IJ, KL, ETAT, NRAC
      REAL*8  VAL(4), NU, LAMBDA, DEUXMU, KG, SEUIL, GAMMA
      REAL*8  COPLAN, EPS(6), PHI, R, W, TREPS, EPSEPS, SIGEL(6)
      REAL*8  FEL,FSAT,D, FD, Q0, Q1, Q2, RAC(3), B
      REAL*8  DDOT
      CHARACTER*2 K2(4)
      CHARACTER*8 NOM(4)
      REAL*8 KRON(6), RIGMIN
      PARAMETER (RIGMIN = 1.D-5)
      DATA   KRON/1.D0,1.D0,1.D0,0.D0,0.D0,0.D0/
C ----------------------------------------------------------------------


C ----------------------------------------------------------------------
C                          INITIALISATIONS
C ----------------------------------------------------------------------


C -- OPTIONS DE CALCUL

      CPLAN = TYPMOD.EQ.'C_PLAN  '
      ELAS  = OPTION.EQ.'RIGI_MECA_ELAS' .OR. OPTION.EQ.'FULL_MECA_ELAS'
      RIGI  = OPTION(1:9).EQ.'RIGI_MECA' .OR. OPTION(1:9).EQ.'FULL_MECA'
      RESI  = OPTION(1:9).EQ.'FULL_MECA' .OR. OPTION.EQ.'RAPH_MECA'
      NDIMSI = 2*NDIM



C -- LECTURE DES CARACTERISTIQUES MATERIAU

      NOM(1) = 'E'
      NOM(2) = 'NU'
      NOM(3) = 'SY'
      NOM(4) = 'D_SIGM_EPSI'
      CALL RCVALA(MAT,' ','ELAS'     ,0,' ',0.D0,
     &             2,NOM(1),VAL(1),K2,'F ')
      CALL RCVALA(MAT,' ','ECRO_LINE',0,' ',0.D0,
     &             2,NOM(3),VAL(3),K2,'F ')

      NU     = VAL(2)
      LAMBDA = VAL(1)*VAL(2) / (1-2*VAL(2)) / (1+VAL(2))
      DEUXMU = VAL(1) / (1.D0+VAL(2))
      GAMMA  = -VAL(4)/VAL(1)
      KG     = VAL(3)**2 / (2*VAL(1)) * (1+GAMMA)**2


C -- DEFORMATIONS COURANTES

      CALL DCOPY(NDIMSI, EPSM,1, EPS,1)
      IF (RESI) CALL DAXPY(NDIMSI,1.D0,DEPS,1,EPS,1)

C    DEFORMATION HORS PLAN POUR LES CONTRAINTES PLANES
      IF (CPLAN) THEN
        COPLAN  = - NU/(1.D0-NU)
        EPS(3) = COPLAN * (EPS(1)+EPS(2))
      END IF

      R=NONLOC(2)
      PHI = NONLOC(3)
      
C -- ENERGIE DE DEFORMATION ET CONTRAINTE ELASTIQUE

      TREPS  = EPS(1)+EPS(2)+EPS(3)
      EPSEPS = DDOT(NDIMSI,EPS,1,EPS,1)
      W      = 0.5D0 * (LAMBDA*TREPS**2 + DEUXMU*EPSEPS)
      DO 5 IJ = 1,NDIMSI
        SIGEL(IJ) = LAMBDA*TREPS*KRON(IJ) + DEUXMU*EPS(IJ)
 5    CONTINUE        


C ----------------------------------------------------------------------
C                     CALCUL DE L'ENDOMMAGEMENT
C ----------------------------------------------------------------------

      D    = VIM(1)
      ETAT = VIM(2)
      IF (.NOT.RESI) GOTO 5000
      

C    ESTIMATION DU CRITERE
      IF (ETAT.EQ.2) GOTO 2000
      
C    PREDICTION ELASTIQUE
      FEL = W - KG/(1+GAMMA-D)**2 + PHI - R*D
      IF (FEL.LE.0) THEN
        ETAT = 0
        D    = VIM(1)
        GOTO 2000
      END IF
      
C    PREDICTION SATUREE
      FSAT = W - KG/GAMMA**2 + PHI - R
      IF (FSAT.GE.0) THEN
        ETAT = 2
        D    = 1.D0
        GOTO 2000
      END IF
         
C    RESOLUTION DE L'EQUATION SEUIL=0
      Q2 = - (2*R*(1+GAMMA) + W + PHI)/R
      Q1 = (R*(1+GAMMA)**2 + 2*(1+GAMMA)*(W+PHI))/R
      Q0 = (KG - (W+PHI)*(1+GAMMA)**2)/R
      CALL ZEROP3(Q2,Q1,Q0,RAC,NRAC)

      ETAT = 1
      D = RAC(NRAC)
      IF (D.LT.VIM(1)) D = VIM(1)
      IF (D.GT.1.D0)   D = 1.D0


C    STOCKAGE DES CONTRAINTES ET DES VARIABLES INTERNES

 2000 CONTINUE
 
      DO 10 IJ = 1,NDIMSI
        SIG(IJ) = (1-D)*SIGEL(IJ)
 10   CONTINUE

      VIP(1) = D
      VIP(2) = ETAT


 5000 CONTINUE      



C ----------------------------------------------------------------------
C                     CALCUL DES MATRICES TANGENTES
C ----------------------------------------------------------------------

      IF (.NOT. RIGI) GOTO 9999

      CALL R8INIR(36*4, 0.D0, DSIDEP,1)
   
   
C -- CONTRIBUTION ELASTIQUE


      FD = MAX(1-D, RIGMIN)

      DO 80 IJ=1,3
        DO 90 KL=1,3
          DSIDEP(IJ,KL,1) = FD*LAMBDA
 90     CONTINUE
 80   CONTINUE
      DO 100 IJ=1,NDIMSI
        DSIDEP(IJ,IJ,1) = DSIDEP(IJ,IJ,1) + FD*DEUXMU
 100  CONTINUE



C -- CORRECTION POUR LES CONTRAINTES PLANES

      IF (CPLAN) THEN
        DO 130 IJ=1,NDIMSI
          IF (IJ.EQ.3) GOTO 130
          DO 140 KL=1,NDIMSI
            IF (KL.EQ.3) GO TO 140
            DSIDEP(IJ,KL,1)=DSIDEP(IJ,KL,1)
     &      - 1.D0/DSIDEP(3,3,1)*DSIDEP(IJ,3,1)*DSIDEP(3,KL,1)
 140      CONTINUE
 130    CONTINUE
      ENDIF


C -- CORRECTION DISSIPATIVE


      IF (ETAT.NE.1) GOTO 9999
      B = 1.D0 / (R + 2*KG/(1+GAMMA-D)**3)

      DO 200 IJ = 1,NDIMSI
        IF (.NOT.ELAS) THEN
         DO 210 KL = 1,NDIMSI
           DSIDEP(IJ,KL,1) = DSIDEP(IJ,KL,1) - B*SIGEL(IJ)*SIGEL(KL)
 210     CONTINUE
        ENDIF
        DSIDEP(IJ,1,3) = B*SIGEL(IJ)
        DSIDEP(IJ,1,2) = - DSIDEP(IJ,1,3)
 200  CONTINUE         
      DSIDEP(1,1,4) = B

            
 9999 CONTINUE

      END
