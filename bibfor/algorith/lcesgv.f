      SUBROUTINE LCESGV (FAMI  , KPG   , KSP   , NDIM  , TYPMOD, 
     &                   OPTION, MAT   , EPSM  , DEPS  , VIM   ,
     &                   NONLOC, SIG   , VIP   , DSIDEP, IRET  )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 20/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
      IMPLICIT NONE
      CHARACTER*8   TYPMOD
      CHARACTER*16  OPTION
      CHARACTER*(*) FAMI
      INTEGER       NDIM, MAT, IRET, KPG, KSP
      REAL*8        EPSM(7), DEPS(7), VIM(*), NONLOC(2)
      REAL*8        VIP(*), SIG(*), DSIDEP(6,6,4)
C ----------------------------------------------------------------------
C           ENDOMMAGEMENT FRAGILE A GRADIENT DE VARIABLE INTERNE :
C                       ENDO_SCALAIRE AVEC GRAD_VARI
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
C IN  R       COEFFICIENT DE PENALISATION
C OUT VIP     DENSITE DE FISSURATION 
C OUT SIG     CONTRAINTE 
C OUT DSIDEP  MATRICES TANGENTES
C OUT IRET    CODE RETOUR (0=OK, 1=PAS DE CVG, 2=INCR ENDO TROP GRAND)
C ----------------------------------------------------------------------
      LOGICAL CPLAN, RIGI, RESI, ELAS, EXTEMP
      INTEGER NDIMSI, IJ, KL, ETAT,VRET
      REAL*8  LCESRF,DDOT,KRON(6),VAL(6),ALPHA,TEMP,TREF,EPSTH
      REAL*8  LAMBDA,DEUXMU,WY,RIGMIN,DDMAXI
      REAL*8   EPS(6), TREPS, EPSEPS, SIGEL(6)
      REAL*8  A,DRDA,DRDAE,DFDAE,DRDAS,DFDAS,GEL,GSAT
      REAL*8  RA,FD,D2RDA2,D2FDA2,DGDA,H
      CHARACTER*1 POUM
      INTEGER K2(6),REP
      CHARACTER*8 NOM(6)
C ----------------------------------------------------------------------
      REAL*8 GAMMA,DRDA0,DFDA0,WEPS,PHI,R
      COMMON /LCES/ GAMMA,DRDA0,DFDA0,WEPS,PHI,R
C ----------------------------------------------------------------------
      DATA KRON/1.D0,1.D0,1.D0,0.D0,0.D0,0.D0/
      DATA NOM/'E','NU','SY','GAMMA','COEF_RIG','DD_MAXI'/
C ----------------------------------------------------------------------


C ----------------------------------------------------------------------
C                          INITIALISATIONS
C ----------------------------------------------------------------------


C -- OPTIONS DE CALCUL

      CPLAN = TYPMOD.EQ.'C_PLAN  '
      ELAS  = OPTION(11:14).EQ.'ELAS'
      RIGI  = OPTION(1:4).EQ.'RIGI' .OR. OPTION(1:4).EQ.'FULL'
      RESI  = OPTION(1:4).EQ.'FULL' .OR. OPTION(1:4).EQ.'RAPH'
      NDIMSI = 2*NDIM
      IRET   = 0

C -- LECTURE DES CARACTERISTIQUES MATERIAU

      PHI = NONLOC(1)   
      R   = NONLOC(2)

      CALL RCVALA(MAT,' ','ELAS',         0,' ',0.D0,2,NOM(1),
     &            VAL(1),K2,2)
      CALL RCVALA(MAT,' ','ENDO_SCALAIRE',0,' ',0.D0,4,NOM(3),
     &            VAL(3),K2,2)

C      NU     = VAL(2)
      LAMBDA = VAL(1)*VAL(2) / (1-2*VAL(2)) / (1+VAL(2))
      DEUXMU = VAL(1) / (1.D0+VAL(2))
      WY     = VAL(3)**2 / (2*VAL(1))
      GAMMA  = VAL(4)
      RIGMIN = VAL(5)
      DDMAXI = VAL(6)

      DRDA0  = LCESRF(2,0.D0)
      DFDA0  = -WY*DRDA0

C    PARAMETRES FACULTATIFS : TEMPERATURES 
      IF (RESI) THEN 
        POUM = '+'
      ELSE
        POUM = '-'
      END IF

      CALL RCVALA(MAT,' ','ELAS',0,' ',0.D0,1,'ALPHA',ALPHA,
     &            REP,0)
      EXTEMP = REP.EQ.0

      IF (EXTEMP) THEN
        CALL RCVARC('F','TEMP','REF',FAMI,KPG,KSP,TREF,VRET)
        CALL RCVARC('F','TEMP',POUM, FAMI,KPG,KSP,TEMP,VRET)
        EPSTH = ALPHA*(TEMP-TREF)
      ELSE
        EPSTH = 0
      END IF
      

C -- DEFORMATIONS COURANTES

      CALL DCOPY(NDIMSI, EPSM,1, EPS,1)
      IF (RESI) CALL DAXPY(NDIMSI,1.D0,DEPS,1,EPS,1)

C    DEFORMATIONS MECANIQUES
      EPS(1) = EPS(1) - EPSTH
      EPS(2) = EPS(2) - EPSTH
      EPS(3) = EPS(3) - EPSTH
      
C -- ENERGIE DE DEFORMATION ET CONTRAINTE ELASTIQUE

      TREPS  = EPS(1)+EPS(2)+EPS(3)
      EPSEPS = DDOT(NDIMSI,EPS,1,EPS,1)
      WEPS   = 0.5D0 * (LAMBDA*TREPS**2 + DEUXMU*EPSEPS)
      DO 5 IJ = 1,NDIMSI
        SIGEL(IJ) = LAMBDA*TREPS*KRON(IJ) + DEUXMU*EPS(IJ)
 5    CONTINUE        


C ----------------------------------------------------------------------
C                     CALCUL DE L'ENDOMMAGEMENT
C ----------------------------------------------------------------------

      A    = VIM(1)
      ETAT = VIM(2)

      IF (.NOT.RESI) GOTO 5000
      

C    ESTIMATION DU CRITERE
      IF (ETAT.EQ.2) GOTO 2000
      

C    PREDICTION ELASTIQUE

      DRDAE = LCESRF(2,A)
      DFDAE = LCESRF(3,A)
      GEL  = DRDAE*WEPS + DFDAE - PHI + R*A
      IF (GEL.GE.0) THEN
        ETAT = 0
        GOTO 2000
      END IF
      

C    PREDICTION SATUREE

      DRDAS = LCESRF(2,1.D0)
      DFDAS = LCESRF(3,1.D0)
      GSAT  = DRDAS*WEPS + DFDAS - PHI + R
      IF (GSAT.LE.0) THEN
        ETAT = 2
        A    = 1.D0
        GOTO 2000
      END IF
         
         
C    RESOLUTION DE L'EQUATION G(A)=0
      ETAT = 1
      A = LCESRF(6,A)
C    PROJECTION DE A+ ENTRE A- ET 1.D0
      IF (A.LE.VIM(1)) THEN
        A = VIM(1)
      ELSE IF (A.GT.1.D0) THEN
        ETAT = 2
        A    = 1.D0
      END IF

C    STOCKAGE DES CONTRAINTES ET DES VARIABLES INTERNES

 2000 CONTINUE
 
      RA = LCESRF(1,A)
      DO 20 IJ = 1,NDIMSI
        SIG(IJ) = RA*SIGEL(IJ)
 20   CONTINUE

      VIP(1) = A
      VIP(2) = ETAT
      VIP(3) = 1.D0-RA

C    TEST DE L'INCREMENT D'ENDOMMAGEMENT
      IF (VIP(1)-VIM(1) .GT. DDMAXI) IRET = 2

 5000 CONTINUE
 
 
C ----------------------------------------------------------------------
C                     CALCUL DES MATRICES TANGENTES
C ----------------------------------------------------------------------

      IF (.NOT. RIGI) GOTO 9999

      CALL R8INIR(36*4, 0.D0, DSIDEP,1)
   
   
C -- CONTRIBUTION ELASTIQUE

      RA = LCESRF(1,A)
      FD = MAX(RA, RIGMIN)
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
      IF (ETAT.NE.1 .OR. ELAS) GOTO 9999
      
      DRDA   = LCESRF(2,A)
C      DFDA   = LCESRF(3,A)
      D2RDA2 = LCESRF(4,A)
      D2FDA2 = LCESRF(5,A)
      DGDA   = D2RDA2*WEPS+D2FDA2+R 
      H      = DRDA**2 / DGDA
      
      DO 200 IJ = 1,NDIMSI
        DO 210 KL = 1,NDIMSI
          DSIDEP(IJ,KL,1) = DSIDEP(IJ,KL,1) - H*SIGEL(IJ)*SIGEL(KL)
 210    CONTINUE
        DSIDEP(IJ,1,2) =  DRDA/DGDA * SIGEL(IJ)
        DSIDEP(IJ,1,3) = -DSIDEP(IJ,1,2)
 200  CONTINUE         
      DSIDEP(1,1,4) = 1/DGDA

            
 9999 CONTINUE
      END
