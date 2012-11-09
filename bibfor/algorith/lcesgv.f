      SUBROUTINE LCESGV (FAMI  , KPG   , KSP   , NEPS  , TYPMOD,
     &                   OPTION, MAT   , EPSM  , DEPS  , VIM   ,
     &                   ITEMAX, PRECVG, SIG   , VIP   ,
     &                   DSIDEP, IRET  )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
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
      CHARACTER*8   TYPMOD
      CHARACTER*16  OPTION
      CHARACTER*(*) FAMI
      INTEGER       NEPS, MAT, IRET, KPG, KSP,ITEMAX
      REAL*8        EPSM(NEPS), DEPS(NEPS), VIM(*), PRECVG
      REAL*8        VIP(*), SIG(NEPS), DSIDEP(NEPS,NEPS)
C ----------------------------------------------------------------------
C           ENDOMMAGEMENT FRAGILE A GRADIENT DE VARIABLE INTERNE :
C                       ENDO_SCALAIRE AVEC GRAD_VARI
C ----------------------------------------------------------------------
C IN  NEPS    DIMENSION DES DEFORMATIONS GENERALISEES
C IN  TYPMOD  TYPE DE MODELISATION
C IN  OPTION  OPTION DE CALCUL
C               RIGI_MECA_TANG, RIGI_MECA_ELAS
C               RAPH_MECA
C               FULL_MECA, FULL_MECA_ELAS
C IN  MAT     NATURE DU MATERIAU
C IN  EPSM    CHAMP DE DEFORMATION EN T- ET PHIM=EPSM(7)
C IN  DEPS    INCREMENT DU CHAMP DE DEFORMATION ET DPHI=DEPS(7)
C IN  VIM     VARIABLES INTERNES EN T-
C IN  NONLOC  INUTILISE
C IN  ITEMAX  NBR MAXI D'ITERATIONS POUR RESOLUTION EQUATION SCALAIRE
C IN  PRECVG  CRITERE DE CVG : A ET A+PRECVG ENCADRENT LA SOLUTION
C OUT VIP     DENSITE DE FISSURATION
C OUT SIG     CONTRAINTE
C OUT DSIDEP  MATRICE TANGENTE
C OUT IRET    CODE RETOUR (0=OK, 1=ECHEC CVG)
C ----------------------------------------------------------------------
      LOGICAL CPLAN, RIGI, RESI, ELAS
      INTEGER NDIM,NDIMSI,IJ, KL, ETAT
      REAL*8  LCESVF,LCESRF,KRON(6)
      REAL*8  PHI,LAG,APG,GRAD(3)
      REAL*8  COPLAN,COR33,VPLAN(6),EPS(6),SIGEL(6),TREPS,EPSDV(6)
      REAL*8  SIGMA(6),A,DRDA,DRDAE,DRDAS,GEL,GSAT,KTG(6,6,4)
      REAL*8  RA,FD,D2RDA2,DGDA,GAMEPS,DGAMDE(6),COEFG
      CHARACTER*1 POUM
C ----------------------------------------------------------------------
      REAL*8 LAMBDA,DEUXMU,TROISK,RIGMIN,PC,PR,EPSTH
      COMMON /LCEE/ LAMBDA,DEUXMU,TROISK,RIGMIN,PC,PR,EPSTH
C ----------------------------------------------------------------------
      REAL*8 PK,PM,PP
      COMMON /LCES/ PK,PM,PP
C ----------------------------------------------------------------------
      REAL*8 PCT,PCH,PCS
      COMMON /LCER/ PCH,PCT,PCS
C ----------------------------------------------------------------------
      DATA KRON/1.D0,1.D0,1.D0,0.D0,0.D0,0.D0/
C ----------------------------------------------------------------------


C ----------------------------------------------------------------------
C                          INITIALISATIONS
C ----------------------------------------------------------------------


C -- OPTIONS DE CALCUL

      CPLAN = TYPMOD.EQ.'C_PLAN  '
      ELAS  = OPTION(11:14).EQ.'ELAS'
      RIGI  = OPTION(1:4).EQ.'RIGI' .OR. OPTION(1:4).EQ.'FULL'
      RESI  = OPTION(1:4).EQ.'FULL' .OR. OPTION(1:4).EQ.'RAPH'
      NDIM  = (NEPS-2)/3
      NDIMSI = 2*NDIM
      IRET   = 0
      POUM='-'
      IF (RESI) POUM='+'


C -- LECTURE DES CARACTERISTIQUES MATERIAU

      CALL LCERMA(MAT,FAMI,KPG,KSP,POUM)


C -- DEFORMATIONS COURANTES

C    DEFORMATION, ENDOMMAGEMENT, LAGRANGE ET GRADIENT
      CALL DCOPY(NDIMSI, EPSM,          1, EPS,1)
      CALL DCOPY(NDIM,   EPSM(NDIMSI+3),1, GRAD,1)
      APG  = EPSM(NDIMSI+1)
      LAG  = EPSM(NDIMSI+2)

      IF (RESI) THEN
        CALL DAXPY(NDIMSI,1.D0,DEPS,1,EPS,1)
        CALL DAXPY(NDIM,1.D0,DEPS(NDIMSI+3),1,GRAD,1)
        APG  = APG + DEPS(NDIMSI+1)
        LAG  = LAG + DEPS(NDIMSI+2)
      END IF

      PHI= LAG + PR*APG

C    DEFORMATIONS MECANIQUES
      EPS(1) = EPS(1) - EPSTH
      EPS(2) = EPS(2) - EPSTH
      EPS(3) = EPS(3) - EPSTH

C    CONTRAINTES PLANES
      IF (CPLAN) THEN
        COPLAN = -LAMBDA / (LAMBDA + DEUXMU)
        EPS(3) = COPLAN*(EPS(1)+EPS(2))
      END IF


C -- PSEUDO-ENERGIE DE DEFORMATION ET CONTRAINTE ELASTIQUE

      CALL LCERVF(0,NDIMSI,EPS,TREPS,EPSDV,GAMEPS,DGAMDE)

      DO 15 IJ = 1,NDIMSI
        SIGEL(IJ) = LAMBDA*TREPS*KRON(IJ) + DEUXMU*EPS(IJ)
 15   CONTINUE


C ----------------------------------------------------------------------
C                     CALCUL DE L'ENDOMMAGEMENT
C ----------------------------------------------------------------------

      A      = VIM(1)
      ETAT   = NINT(VIM(2))

      IF (.NOT.RESI) GOTO 5000


C    ESTIMATION DU CRITERE
      IF (ETAT.EQ.2) GOTO 2000


C    PREDICTION ELASTIQUE

      DRDAE = LCESVF(1,A)
      GEL   = DRDAE*GAMEPS + PK - PHI + PR*A

      IF (GEL.GE.0) THEN
        ETAT = 0
        GOTO 2000
      END IF


C    PREDICTION SATUREE

      DRDAS = LCESVF(1,1.D0)
      GSAT  = DRDAS*GAMEPS + PK - PHI + PR
      IF (GSAT.LE.0) THEN
        ETAT = 2
        A    = 1.D0
        GOTO 2000
      END IF


C    RESOLUTION DE L'EQUATION G(A)=0
      ETAT = 1
      A = LCESRF(A,GAMEPS,PR,PK-PHI,PRECVG,ITEMAX,IRET)
      IF (IRET.NE.0) GOTO 9999

C    PROJECTION DE A+ ENTRE A- ET 1.D0
      IF (A.LE.VIM(1)) THEN
        A = VIM(1)
      ELSE IF (A.GT.1.D0) THEN
        ETAT = 2
        A    = 1.D0
      END IF

C    STOCKAGE DES CONTRAINTES ET DES VARIABLES INTERNES

 2000 CONTINUE

      RA = LCESVF(0,A)
      DO 20 IJ = 1,NDIMSI
        SIGMA(IJ) = RA*SIGEL(IJ)
 20   CONTINUE

      VIP(1) = A
      VIP(2) = ETAT
      VIP(3) = 1.D0-RA

 5000 CONTINUE


C ----------------------------------------------------------------------
C                     CALCUL DES MATRICES TANGENTES
C ----------------------------------------------------------------------

      IF (.NOT. RIGI) GOTO 8000

      CALL R8INIR(36*4, 0.D0, KTG,1)


C -- CONTRIBUTION ELASTIQUE

      RA = LCESVF(0,A)
      FD = MAX(RA, RIGMIN)
      DO 80 IJ=1,3
        DO 90 KL=1,3
          KTG(IJ,KL,1) = FD*LAMBDA
 90     CONTINUE
 80   CONTINUE
      DO 100 IJ=1,NDIMSI
        KTG(IJ,IJ,1) = KTG(IJ,IJ,1) + FD*DEUXMU
 100  CONTINUE


C -- CORRECTION DISSIPATIVE
      IF (ETAT.EQ.1 .AND. .NOT.ELAS) THEN

        CALL LCERVF(1,NDIMSI,EPS,TREPS,EPSDV,GAMEPS,DGAMDE)
        DRDA   = LCESVF(1,A)
        D2RDA2 = LCESVF(2,A)
        DGDA   = D2RDA2*GAMEPS+PR
        COEFG  = DRDA**2 / DGDA

        DO 200 IJ = 1,NDIMSI
          DO 210 KL = 1,NDIMSI
            KTG(IJ,KL,1) = KTG(IJ,KL,1) - COEFG*SIGEL(IJ)*DGAMDE(KL)
 210      CONTINUE
          KTG(IJ,1,2) =  DRDA/DGDA * SIGEL(IJ)
          KTG(IJ,1,3) = -DRDA/DGDA * DGAMDE(IJ)
 200    CONTINUE
        KTG(1,1,4) = 1/DGDA

      END IF


C -- CORRECTION POUR LES CONTRAINTES PLANES

      IF (CPLAN) THEN

        COR33 = COPLAN**2*KTG(3,3,1)
        DO 130 IJ = 1,NDIMSI
          VPLAN(IJ) = COPLAN * KTG(IJ,3,1)
 130    CONTINUE
        DO 140 IJ = 1,NDIMSI
          KTG(IJ,1,1) = KTG(IJ,1,1) + VPLAN(IJ)
          KTG(IJ,2,1) = KTG(IJ,2,1) + VPLAN(IJ)
          KTG(1,IJ,1) = KTG(1,IJ,1) + VPLAN(IJ)
          KTG(2,IJ,1) = KTG(2,IJ,1) + VPLAN(IJ)
 140    CONTINUE
        KTG(1,1,1) = KTG(1,1,1) + COR33
        KTG(1,2,1) = KTG(1,2,1) + COR33
        KTG(2,1,1) = KTG(2,1,1) + COR33
        KTG(2,2,1) = KTG(2,2,1) + COR33

        KTG(1,1,2) = KTG(1,1,2) + COPLAN*KTG(3,1,2)
        KTG(2,1,2) = KTG(2,1,2) + COPLAN*KTG(3,1,2)
        KTG(1,1,3) = KTG(1,1,3) + COPLAN*KTG(3,1,3)
        KTG(2,1,3) = KTG(2,1,3) + COPLAN*KTG(3,1,3)

      ENDIF


C -- PRISE EN CHARGE DES TERMES DU LAGRANGIEN AUGMENTE

 8000 CONTINUE
      CALL LCGRAD(RESI,RIGI,NDIM,NDIMSI,NEPS,SIGMA,APG,LAG,GRAD,
     &  A,PR,PC,KTG,SIG,DSIDEP)


 9999 CONTINUE
      END
