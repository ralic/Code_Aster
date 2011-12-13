       SUBROUTINE BURJPL ( NMAT,MATER,NR,DRDY,DSDE )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/12/2011   AUTEUR FOUCAULT A.FOUCAULT 
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
C RESPONSABLE FOUCAULT A.FOUCAULT
C     ----------------------------------------------------------------
C     CALCUL DU JACOBIEN DU SYSTEME NL A RESOUDRE = DRDY(DY) 
C     POUR LE MODELE BETON_BURGER_FP
C     IN  NR	 :  DIMENSION JACOBIEN
C         NMAT   :  DIMENSION MATER
C         MATER  :  COEFFICIENTS MATERIAU
C         NR	 :  DIMENSION MATRICE JACOBIENNE
C         DRDY   :  MATRICE JACOBIENNE
C     OUT DSDE   :  MATRICE TANGENTE EN VITESSE
C     ----------------------------------------------------------------
      IMPLICIT NONE
C     ----------------------------------------------------------------
      COMMON /TDIM/   NDT  , NDI
C     ----------------------------------------------------------------
      INTEGER         NMAT,NR,IRET,NDT,NDI,I,J
      REAL*8          DRDY(NR,NR),DSDE(6,6),MATER(NMAT,2)
      REAL*8          HOOK(6,6),Y0(6,6),Y1(6,6),Y2(6,6),Y3(6,6)
      REAL*8          INVELA(6,6),DRDYT(6,6),DET,R8PREM,MAXI,MINI

C === =================================================================
C --- INITIALISATION MATRICES A ZERO
C === =================================================================
      CALL LCINMA(0.D0,Y0)
      CALL LCINMA(0.D0,Y1)
      CALL LCINMA(0.D0,Y2)
      CALL LCINMA(0.D0,Y3)
C === =================================================================
C --- RECHERCHE DU MAXIMUM DE DRDY
C === =================================================================
      MAXI = 0.D0
      DO 1 I = 1, NR
        DO 2 J = 1, NR
          IF(ABS(DRDY(I,J)).GT.MAXI)MAXI = ABS(DRDY(I,J))
 2    CONTINUE
 1    CONTINUE
C === =================================================================
C --- DIMENSIONNEMENT A R8PREM
C === =================================================================
      MINI = R8PREM()*MAXI
      DO 3 I = 1, NR
        DO 4 J = 1, NR
          IF(ABS(DRDY(I,J)).LT.MINI)DRDY(I,J) = 0.D0
 4    CONTINUE
 3    CONTINUE

C === =================================================================
C --- SEPARATION DES TERMES DU JACOBIEN
C === =================================================================
      DO 5 I = 1, NDT
        DO 6 J = 1, NDT
          Y0(I,J) = DRDY(I,J)
          Y1(I,J) = DRDY(I,J+NDT)
          Y2(I,J) = DRDY(I+NDT,J)
          Y3(I,J) = DRDY(I+NDT,J+NDT)
 6      CONTINUE
 5    CONTINUE  

C === =================================================================
C --- CONSTRUCTION TENSEUR RIGIDITE ELASTIQUE A T+DT
C === =================================================================
      CALL LCOPLI ( 'ISOTROPE', '3D      ', MATER, HOOK)
C === =================================================================
C --- CONSTRUCTION TENSEUR CONSTITUTIF TANGENT DSDE 
C === =================================================================
C --- INVERSION DU TERME Y3
      CALL LCINMA(0.D0,INVELA)
      DO 7 I=1,NDT
        INVELA(I,I) = 1.D0
 7    CONTINUE

      CALL MGAUSS('NCVP',Y3, INVELA, 6, NDT, NDT, DET, IRET)
      IF(IRET.GT.0)CALL LCINMA(0.D0,INVELA)

C --- PRODUIT DU TERME (Y3)^-1 * Y2 = DRDYT
      CALL LCPRMM(INVELA,Y2,DRDYT)

C --- PRODUIT DU TERME (Y1) * DRDYT = INVELA
      CALL LCPRMM(Y1,DRDYT,INVELA)

C --- DIFFERENCE DE MATRICE (DR1DY1 - INVELA) = DRDYT
      CALL LCDIMA(Y0,INVELA,DRDYT)

C --- INVERSION DU TERME DRDYT
      CALL LCINMA(0.D0,DSDE)
      DO 8 I=1,NDT
        DSDE(I,I) = -1.D0
 8    CONTINUE
      CALL MGAUSS('NCVP',DRDYT, DSDE, 6, NDT, NDT, DET, IRET)
      IF(IRET.GT.1)CALL LCEQMA(HOOK,DSDE)

      END
