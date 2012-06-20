       SUBROUTINE LCOPTG ( NMAT,MATER,NR,NVI,DRDY,DSDE ,IRET)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 18/06/2012   AUTEUR PROIX J-M.PROIX 
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
C TOLE CRS_1404
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
      INTEGER         NMAT,NR,IRET,NDT,NDI,I,J,NVI,NORM
      REAL*8          DRDY(NR,NR),DSDE(6,6),MATER(NMAT,2)
      REAL*8          HOOK(6,6),Y0(6,6),Y1(6,NVI),Y2(NVI,6),Y3(NVI,NVI)
      REAL*8          Y4(6,6),Y5(6,6),DET,R8PREM,MAXI,MINI
      CHARACTER*4     CARGAU
C === =================================================================
C --- RECHERCHE DU MAXIMUM DE DRDY
C === =================================================================

      NORM=0
      IF (NORM.EQ.0) GOTO 30
      
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

 30   CONTINUE

C === =================================================================
C --- SEPARATION DES TERMES DU JACOBIEN
C === =================================================================
      DO 5 I = 1, 6
        DO 6 J = 1, 6
          Y0(I,J) = DRDY(I,J)
 6      CONTINUE
 5    CONTINUE  
      DO 51 I = 1, 6
        DO 61 J = 1, NVI
          Y1(I,J) = DRDY(I,J+6)
 61      CONTINUE
 51    CONTINUE  
      DO 52 I = 1, NVI
        DO 62 J = 1, 6
          Y2(I,J) = DRDY(I+6,J)
 62      CONTINUE
 52    CONTINUE  
      DO 53 I = 1, NVI
        DO 63 J = 1, NVI
          Y3(I,J) = DRDY(I+6,J+6)
 63      CONTINUE
 53    CONTINUE  

C === =================================================================
C --- CONSTRUCTION TENSEUR RIGIDITE ELASTIQUE A T+DT
C === =================================================================
      CALL LCOPLI ( 'ISOTROPE', '3D      ', MATER, HOOK)
      
C     CHOIX DES PARAMETRES DE LANCEMENT DE MGAUSS
C     METHODE 'S' : SURE
      CARGAU = 'NCSP'
C     METHODE 'W' : RATEAU
C      CARGAU = 'NCWP'      
C === =================================================================
C --- CONSTRUCTION TENSEUR CONSTITUTIF TANGENT DSDE 
C === =================================================================
C     Y2=INVERSE(Y3)*Y2
      CALL MGAUSS (CARGAU,Y3, Y2, NVI, NVI, 6, DET, IRET )
      IF(IRET.GT.1) THEN
         CALL LCEQMA(HOOK,DSDE)
         GOTO 9999
      ENDIF
C --- PRODUIT DU TERME (Y3)^-1 * Y2 = Y4
      CALL PROMAT(Y1,6,6,NVI,Y2,NVI,NVI,6,Y4)

C --- DIFFERENCE DE MATRICE (DR1DY1 - Y4) = Y5
      CALL LCDIMA(Y0,Y4,Y5)

C --- INVERSION DU TERME Y5
      CALL LCINMA(0.D0,DSDE)
      DO 8 I=1,6
        DSDE(I,I) = 1.D0
 8    CONTINUE
      CALL MGAUSS(CARGAU,Y5, DSDE, 6, 6, 6, DET, IRET)
      
      IF(IRET.GT.1) THEN
         CALL LCEQMA(HOOK,DSDE)
      ENDIF
      
 9999 CONTINUE
      END
