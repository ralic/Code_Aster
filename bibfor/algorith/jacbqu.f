      SUBROUTINE JACBQU(NP3,NBSEG,RC,THETA,
     &                  XLOC,KN,CN,IC,JACOBC,JACOBK)
      IMPLICIT NONE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 16/05/2000   AUTEUR KXBADNG T.KESTENS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C-----------------------------------------------------------------------
C DESCRIPTION : CALCUL DES MATRICES JACOBIENNES LIEES A LA FORCE
C -----------   NON-LINEAIRE DE CHOC F(X,DX)
C
C               CAS DE LA BUTEE QUELCONQUE
C
C               APPELANT : CALJAC
C
C-------------------   DECLARATION DES VARIABLES   ---------------------
C
C ARGUMENTS
C ---------
      INTEGER   NP3, NBSEG(*)
      REAL*8    RC(NP3,*), THETA(NP3,*), XLOC(*), KN, CN
      INTEGER   IC
      REAL*8    JACOBC(3,*), JACOBK(3,*)
C
C VARIABLES LOCALES
C -----------------
      INTEGER   NBS
      REAL*8    XJEU, COST, SINT, DNORM, C2, CS, S2
C
C ROUTINES EXTERNES
C -----------------
C     EXTERNAL  DISBUT
C
C-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
C
C  1. CALCUL DU SIN ET DU COS DE L'ANGLE DE LA NORMALE A L'OBSTACLE
C     ET DE LA DISTANCE NORMALE DE CHOC
C     ---------------------------------
      NBS = NBSEG(IC)
      CALL DISBUT(NP3,IC,XLOC,3,XJEU,RC,THETA,NBS,COST,SINT,DNORM)
C
C  2. MATRICE JACOBIENNE DE RAIDEUR
C     -----------------------------
      JACOBK(1,1) = 0.0D0
      JACOBK(1,2) = 0.0D0
      JACOBK(1,3) = 0.0D0
      JACOBK(2,1) = 0.0D0
      JACOBK(3,1) = 0.0D0
C
      C2 = COST * COST
      CS = COST * SINT
      S2 = SINT * SINT
C
      JACOBK(2,2) = - KN * C2
      JACOBK(2,3) = - KN * CS
      JACOBK(3,2) = - KN * CS
      JACOBK(3,3) = - KN * S2
C
C  3. MATRICE JACOBIENNE D'AMORTISSEMENT
C     ----------------------------------
      JACOBC(1,1) = 0.0D0
      JACOBC(1,2) = 0.0D0
      JACOBC(1,3) = 0.0D0
      JACOBC(2,1) = 0.0D0
      JACOBC(3,1) = 0.0D0
C
      JACOBC(2,2) = - CN * C2
      JACOBC(2,3) = - CN * CS
      JACOBC(3,2) = - CN * CS
      JACOBC(3,3) = - CN * S2
C
C --- FIN DE JACBQU.
      END
