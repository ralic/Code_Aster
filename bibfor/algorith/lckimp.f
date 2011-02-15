      SUBROUTINE LCKIMP (NDIM  , TYPMOD, OPTION, MAT , EPSM  ,
     &     DEPS  , VIM   , NONLOC     , SIG   , VIP   ,
     &     DSIDEP)
C     
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 15/02/2011   AUTEUR FLEJOU J-L.FLEJOU 
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
C     
      IMPLICIT NONE
      CHARACTER*8  TYPMOD
      CHARACTER*16 OPTION
      INTEGER      NDIM, MAT
      REAL*8       EPSM(6), DEPS(6), VIM(2),NONLOC(3)
      REAL*8       VIP(2), SIG(6), DSIDEP(6,6,4)

C     -----------------------------------------------------------------
C     ENDOMMAGEMENT FRAGILE ENDO_CARRE POUR GVNO
C     -----------------------------------------------------------------
C     IN  NDIM    DIMENSION DE L'ESPACE
C     IN  TYPMOD  TYPE DE MODELISATION
C     IN  OPTION  OPTION DE CALCUL
C     RIGI_MECA_TANG, RIGI_MECA_ELAS
C     RAPH_MECA
C     FULL_MECA, FULL_MECA_ELAS
C     IN  MAT     NATURE DU MATERIAU
C     IN  EPSM    CHAMP DE DEFORMATION EN T- ET PHIM=EPSM(7)
C     IN  DEPS    INCREMENT DU CHAMP DE DEFORMATION ET DPHI=DEPS(7)
C     IN  VIM     VARIABLES INTERNES EN T-
C     IN  NONLOC  VARIABLES NON LOCALES
C     OUT VIP     DENSITE DE FISSURATION 
C     OUT SIG     CONTRAINTE 
C     OUT DSIDEP  MATRICES TANGENTES
C     -----------------------------------------------------------------

      LOGICAL CPLAN, RIGI, RESI, ELAS
      INTEGER NDIMSI, IJ, KL, I
      REAL*8  VAL(3), NU, LAMBDA, DEUXMU 
      REAL*8  ALPHA, SY, E, WY
      REAL*8  COPLAN, EPS(6), PHI, W, TREPS, EPSEPS, SIGEL(6)
      REAL*8  D, FD
      REAL*8  DDOT
      REAL*8  KK, EPSD(6)
      REAL*8  SIGM,W0       
      CHARACTER*2 K2(4)
      CHARACTER*8 NOM(3)
      REAL*8 KRON(6), RIGMIN
      PARAMETER (RIGMIN = 1.D-5)     
      DATA   KRON/1.D0,1.D0,1.D0,0.D0,0.D0,0.D0/
C     -----------------------------------------------------------------


C     -----------------------------------------------------------------
C     INITIALISATIONS
C     -----------------------------------------------------------------


C     -- OPTIONS DE CALCUL

      CPLAN = TYPMOD.EQ.'C_PLAN  '
      RESI=OPTION(1:9).EQ.'FULL_MECA'.OR.OPTION.EQ.'RAPH_MECA'
C      RIGI=OPTION(1:9).EQ.'RIGI_MECA'.OR.OPTION(1:9).EQ.'FULL_MECA'
C      ELAS=OPTION.EQ.'RIGI_MECA_ELAS'.OR.OPTION.EQ.'FULL_MECA_ELAS'
      NDIMSI = 2*NDIM


C     -- LECTURE DES CARACTERISTIQUES MATERIAU

      NOM(1) = 'E'
      NOM(2) = 'NU'
      NOM(3) = 'SY'

      CALL RCVALA(MAT,' ','ELAS'     ,0,' ',0.D0,
     &     2,NOM(1),VAL(1),K2,'F ')

      NU     = VAL(2)
      E      = VAL(1)
      LAMBDA = VAL(1)*VAL(2) / (1-2*VAL(2)) / (1+VAL(2))
      DEUXMU = VAL(1) / (1.D0+VAL(2))
      KK     = LAMBDA + DEUXMU/(3.0D0)   
      
      CALL RCVALA(MAT,' ','ECRO_LINE'     ,0,' ',0.D0,
     &     1,NOM(3),VAL(3),K2,'F ')      
      
      SIGM = VAL(3)
      
      W0 = SIGM**2/(2*E)
      
C     -- DEFORMATIONS COURANTES

      CALL DCOPY(NDIMSI, EPSM,1, EPS,1)
      IF (RESI) CALL DAXPY(NDIMSI,1.D0,DEPS,1,EPS,1)

C     DEFORMATION HORS PLAN POUR LES CONTRAINTES PLANES
      IF (CPLAN) THEN
         COPLAN  = - NU/(1.D0-NU)
         EPS(3) = COPLAN * (EPS(1)+EPS(2))
      END IF

      PHI = NONLOC(1)

C     -- ENERGIE DE DEFORMATION ET CONTRAINTE ELASTIQUE

      TREPS  = EPS(1)+EPS(2)+EPS(3)      
      
C     -- DEVIATEUR DES DEFORMATIONS

      DO 2 I = 1,NDIMSI           
         EPSD(I) = EPS(I) - TREPS*KRON(I)/(3.0D0)
 2    CONTINUE
      
      EPSEPS = DDOT(NDIMSI,EPS,1,EPS,1)
      W      = 0.5D0 * (LAMBDA*TREPS**2 + DEUXMU*EPSEPS)
      DO 5 IJ = 1,NDIMSI
         SIGEL(IJ) = LAMBDA*TREPS*KRON(IJ) + DEUXMU*EPS(IJ)
 5    CONTINUE        
      
C     CORRECTION 1 DE LA DERIVEE PAR RAPPORT A D EN COMPRESSION    
      
      IF (TREPS .LT. 0.D0) THEN
         W = 0.5D0 * DEUXMU*DDOT(NDIMSI,EPSD,1,EPSD,1)
      ENDIF

C     -----------------------------------------------------------------
C     CALCUL DE L'ENDOMMAGEMENT
C     -----------------------------------------------------------------
      
      FD = (1.D0 - PHI)**2 + RIGMIN 
      
      IF (.NOT.RESI) GOTO 5000 
      
      VIP(1) = PHI
      
      IF (VIP(1).GT. VIM(1)) THEN
         VIP(2) = 1
      ELSE
         VIP(2) = 0
      ENDIF
      
C     STOCKAGE DES CONTRAINTES ET DES VARIABLES INTERNES

 2000 CONTINUE
      
C     FORMULATION LOI DE COMPORTMENT AVEC CORRECTION EN COMPRESSION 
      
      IF (TREPS .LT. 0.D0) THEN
         DO 20 IJ = 1,NDIMSI 
            SIG(IJ) = KK*TREPS*KRON(IJ) + 
     &           DEUXMU*EPSD(IJ)*FD
 20      CONTINUE   
      ELSE  
         DO 10 IJ = 1,NDIMSI
            SIG(IJ) = SIGEL(IJ)*FD
 10      CONTINUE
      ENDIF

 5000 CONTINUE      

C     -----------------------------------------------------------------
C     CALCUL DES MATRICES TANGENTES
C     -----------------------------------------------------------------
      
      CALL R8INIR(36*4, 0.D0, DSIDEP,1)
      
      FD = (1.D0 - PHI)**2 + RIGMIN
      
C     -- CONTRIBUTION ELASTIQUE

      DO 80 IJ=1,3
         DO 90 KL=1,3
            IF (TREPS .LT. 0.D0) THEN
               DSIDEP(IJ,KL,1) = LAMBDA+DEUXMU/(3.0D0)*(1.D0-FD)
            ELSE  
               DSIDEP(IJ,KL,1) = FD*LAMBDA
            ENDIF  
 90      CONTINUE
 80   CONTINUE
      DO 100 IJ=1,NDIMSI
         DSIDEP(IJ,IJ,1) = DSIDEP(IJ,IJ,1) + FD*DEUXMU
 100  CONTINUE

C     -- CORRECTION POUR LES CONTRAINTES PLANES

      IF (CPLAN) THEN
         DO 130 IJ=1,NDIMSI
            IF (IJ.EQ.3) GOTO 130
            DO 140 KL=1,NDIMSI
               IF (KL.EQ.3) GO TO 140
               DSIDEP(IJ,KL,1)=DSIDEP(IJ,KL,1)
     &              - 1.D0/DSIDEP(3,3,1)*DSIDEP(IJ,3,1)*DSIDEP(3,KL,1)
 140        CONTINUE
 130     CONTINUE
      ENDIF

C     -- CORRECTION DISSIPATIVE

C     CORRECTION 2 DE LA DERIVEE PAR RAPPORT A D EN COMPRESSION      

      IF (TREPS .LT. 0.D0) THEN
         DO 220 IJ = 1,NDIMSI
            SIGEL(IJ) = DEUXMU*EPSD(IJ)
 220     CONTINUE
      ENDIF

C     DERIVEES CROISEES

      DO 200 IJ = 1,NDIMSI
         DSIDEP(IJ,1,2) = -2.D0*(1.0D0-PHI)*SIGEL(IJ)
 200  CONTINUE 
      
C     DERIVEE SECONDE /ENDO 
 
      DSIDEP(1,1,3) = 2.0D0*W  
      
C    DERIVEE PREMIERE /ENDO      
            
      DSIDEP(1,1,4) = 2.0D0*(W0-(1.0D0-PHI)*W)              

            
 9999 CONTINUE

      END
