      SUBROUTINE PIESGV(NDIM,TAU,MAT,VIM,EPSM,EPSP,EPSD,NONLOC,COPILO)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 12/04/2010   AUTEUR MICHEL S.MICHEL 
C ======================================================================
C COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
      INTEGER  NDIM, MAT
      REAL*8   TAU,EPSM(6),EPSD(6),EPSP(6),NONLOC(4)
      REAL*8   VIM(*),COPILO(2,2)

C ----------------------------------------------------------------------
C     PILOTAGE PRED_ELAS POUR ENDO_SCALAIRE (EN GRAD_VARI)
C ----------------------------------------------------------------------
C
C IN  NDIM    DIMENSION DE L'ESPACE
C IN  MAT     NATURE DU MATERIAU
C IN  VIM     VARIABLES INTERNES EN T-
C IN  EPSM    CHAMP DE DEFORMATION EN T- 
C IN  EPSP    INCREMENT FIXE
C IN  EPSD    INCREMENT PILOTE
C IN  NONLOC  GRANDEURS NON LOCALES PROPRE A GRAD_VARI
C              - NONLOC(1) : CORRECTION NON LOCALE EN T-
C              - NONLOC(2) : INCREMENT FIXE
C              - NONLOC(3) : INCREMENT PILOTE
C              - NONLOC(4) : FACTEUR DE PENALISATION
C
C OUT COPILO  COEFFICIENT DE PILOTAGE : F := A0+A1*ETA = TAU
C ----------------------------------------------------------------------
C REMARQUE :
C  SI PHIP = PHID = R = 0
C  ON RETROUVE LA ROUTINE DE PILOTAGE POUR ENDO_FRAGILE LOCAL
C  PRATIQUEMENT SANS SURCOUT
C ----------------------------------------------------------------------
      INTEGER NDIMSI,I,NRAC
      REAL*8  VAL(6),LAMBDA,DEUXMU,WY,DM,D,DRDA,SD
      REAL*8  EPS(6),PHI,TREPS,TREPSD,EPSPP,EPSPD,EPSDD,Q0,Q1,Q2,RAC(2)
      REAL*8  R,PHIM,PHIP,PHID,ETA,QL1,QL0
      REAL*8  DDOT,LCESRF,R8GAEM
      CHARACTER*2 K2(6)
      CHARACTER*8 NOM(6)
C ----------------------------------------------------------------------
      REAL*8 GAMMA,DRDA0,DFDA0,DUM1,DUM2,DUM3
      COMMON /LCES/ GAMMA,DRDA0,DFDA0,DUM1,DUM2,DUM3
C ----------------------------------------------------------------------
      DATA NOM/'E','NU','SY','GAMMA','COEF_RIGI_MINI','DD_MAXI'/
C ----------------------------------------------------------------------


C ----------------------------------------------------------------------
C                          INITIALISATIONS
C ----------------------------------------------------------------------


C -- INITIALISATION

      NDIMSI = 2*NDIM
      CALL R8INIR(4,0.D0,COPILO,1)
      DM   = VIM(1)
      D    = DM + TAU
      
C -- NON PILOTABLE      
      
      IF (D.GT.1) GOTO 9999
      

C -- LECTURE DES PARAMTERES D'ENTREE NON LOCAUX

      PHIM = NONLOC(1)
      PHIP = NONLOC(2)
      PHID = NONLOC(3)
      R    = NONLOC(4)
      
      
C -- LECTURE DES CARACTERISTIQUES MATERIAU

      CALL RCVALA(MAT,' ','ELAS',         0,' ',0.D0,2,NOM(1),
     &            VAL(1),K2,'F ')
      CALL RCVALA(MAT,' ','ENDO_SCALAIRE',0,' ',0.D0,4,NOM(3),
     &            VAL(3),K2,'F ')
     
C     NU     = VAL(2)
      LAMBDA = VAL(1)*VAL(2) / (1-2*VAL(2)) / (1+VAL(2))
      DEUXMU = VAL(1) / (1.D0+VAL(2))
      WY     = VAL(3)**2 / (2*VAL(1))
      GAMMA  = VAL(4)
      DRDA0  = LCESRF(2,0.D0)
      DFDA0  = -WY*DRDA0
      SD     = LCESRF(3,D) + R*D
      DRDA   = LCESRF(2,D)
      
                  
C -- DEFORMATIONS COURANTES FIXES

      PHI = PHIM + PHIP
      CALL DCOPY(NDIMSI,      EPSM,1, EPS,1)
      CALL DAXPY(NDIMSI,1.D0, EPSP,1, EPS,1)
      TREPS  = EPS(1)+EPS(2)+EPS(3)
      TREPSD = EPSD(1) + EPSD(2) + EPSD(3)
      
            
C -- FORME QUADRATIQUE DE L'ENERGIE DE DEFORMATION : 
C      Q0 + Q1*ETA + Q2*ETA**2 = SD

      EPSPP = DDOT(NDIMSI, EPS,1, EPS,1)
      EPSPD = DDOT(NDIMSI, EPS,1, EPSD,1)
      EPSDD = DDOT(NDIMSI, EPSD,1,EPSD,1)

      Q0 = -DRDA * 0.5D0 * (LAMBDA*TREPS**2 + DEUXMU*EPSPP) + PHI
      Q1 = -DRDA * (LAMBDA*TREPS*TREPSD + DEUXMU*EPSPD)     + PHID
      Q2 = -DRDA * 0.5D0 * (LAMBDA*TREPSD**2 + DEUXMU*EPSDD)
      
      IF (Q2 .LT. 1.D0/R8GAEM()) GOTO 9999


C -- CALCUL DE ETA ET LINEARISATION
 
      CALL ZEROP2(Q1/Q2,(Q0-SD)/Q2,RAC,NRAC)
      
      DO 100 I = 1,NRAC
        ETA = RAC(I)     
        QL1 = 2*Q2*ETA + Q1
        QL0 = SD - QL1*ETA
        
C      SCALING POUR REMPLACER SD PAR TAU (ALGO GLOBAL)
        COPILO(1,I) = QL0*(TAU/SD)
        COPILO(2,I) = QL1*(TAU/SD)
 100  CONTINUE

 
 9999 CONTINUE
      END
