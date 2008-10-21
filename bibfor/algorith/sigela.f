      SUBROUTINE SIGELA(TYPMOD,NDIM,E,NU,EPSE,SIGEL)
                       
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 20/10/2008   AUTEUR MICHEL S.MICHEL 
C ======================================================================
C COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
      CHARACTER*8      TYPMOD(1)
      INTEGER           NDIM
      REAL*8            EPSE(6), E, NU
      REAL*8            SIGEL(6)
C ----------------------------------------------------------------------
C  CALCUL DES CONTRAINTES ELASTIQUES A PARTIR DES DEFORMATIONS 
C	ELASTIQUES POUR UN MATERIAU ISOTROPE
C IN  NDIM    : DIMENSION DE L'ESPACE
C IN  EPSE    : DEFORMATIONS ELASTIQUES
C IN  E       : MODULE ELASTIQUE DE YOUNG
C IN  NU      : COEFFICIENT DE POISSON
C IN TYPMOD   : TYPE DE MODELISATION (C_PLAN...)
C
C OUT SIGEL   : CONTRAINTES ELASTIQUES
C ----------------------------------------------------------------------

      INTEGER     NDIMSI, NPERM, NITJAC, TRIJ, ORDREJ, K
      REAL*8      EPSEP(3), VECPE(3,3), SIGELP(3)
      REAL*8      TOL, TOLDYN, TR(6), TU(6), JACAUX(3)
      REAL*8      RAC2, COPLAN, LAMBDA, DEUXMU

C ======================================================================


C--------------------------------------------------------
C                            INITIALISATION
C--------------------------------------------------------
      
      NDIMSI = 2*NDIM
      RAC2=SQRT(2.D0)

      LAMBDA = E * NU / (1.D0+NU) / (1.D0 - 2.D0*NU)
      DEUXMU = E/(1.D0+NU)      
      
      IF (TYPMOD(1).EQ.'C_PLAN  ') THEN
        COPLAN  = - NU/(1.D0-NU)
        EPSE(3)  = COPLAN * (EPSE(1)+EPSE(2))
      END IF
      
      DO  30 K=4,NDIMSI
        EPSE(K) = EPSE(K)/RAC2
30    CONTINUE

C--------------------------------------------------------
C  -   ON PASSE DANS LE REPERE PROPRE DE EPS
C--------------------------------------------------------
      
        NPERM  = 12
        TOL    = 1.D-10
        TOLDYN = 1.D-2
C       MATRICE  TR = (XX XY XZ YY YZ ZZ) POUR JACOBI)
        TR(1) = EPSE(1)
        TR(2) = EPSE(4)
        TR(3) = EPSE(5)
        TR(4) = EPSE(2)
        TR(5) = EPSE(6)
        TR(6) = EPSE(3)
C     MATRICE UNITE = (1 0 0 1 0 1) (POUR JACOBI)
        TU(1) = 1.D0
        TU(2) = 0.D0
        TU(3) = 0.D0
        TU(4) = 1.D0
        TU(5) = 0.D0
        TU(6) = 1.D0
        TRIJ   = 2
        ORDREJ = 2
C
        CALL JACOBI(3,NPERM,TOL,TOLDYN,TR,TU,VECPE,EPSEP,JACAUX,
     &               NITJAC,TRIJ,ORDREJ)

        
C----------------------------------------------------------------
C     CALCUL DES CONTRAINTES ELASTIQUES (REPERE PRINCIPAL)
C----------------------------------------------------------------
        DO  50 K=1,3
          SIGELP(K) = LAMBDA*(EPSEP(1)+EPSEP(2)+EPSEP(3))
50      CONTINUE
        DO  60 K=1,3
          SIGELP(K) = SIGELP(K) + DEUXMU*EPSEP(K)
60      CONTINUE

C------------------------------------------------------------------
C     ON PASSE DANS LE REPERE INITIAL LES CONTRAINTES ELASTIQUES
C------------------------------------------------------------------
        CALL R8INIR(6, 0.D0, SIGEL,1)
        TR(1) = SIGELP(1)
        TR(2) = SIGELP(2)
        TR(3) = SIGELP(3)
        TR(4) = 0.D0
        TR(5) = 0.D0
        TR(6) = 0.D0
        CALL BPTOBG(TR,SIGEL,VECPE)
        DO  90 K=4,NDIMSI
          SIGEL(K)=RAC2*SIGEL(K)
90      CONTINUE
      END
