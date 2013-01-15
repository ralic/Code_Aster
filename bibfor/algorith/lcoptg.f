       SUBROUTINE LCOPTG ( NMAT,MATER,NR,NVI,DRDY,DSDE ,IRET)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 14/01/2013   AUTEUR COURTOIS M.COURTOIS 
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
C TOLE CRS_1404
C     ----------------------------------------------------------------
C     CALCUL DU JACOBIEN DU SYSTEME NL A RESOUDRE = DRDY(DY) 
C     POUR LE MODELE BETON_BURGER_FP
C     IN  NR     :  DIMENSION JACOBIEN
C         NMAT   :  DIMENSION MATER
C         MATER  :  COEFFICIENTS MATERIAU
C         NR     :  DIMENSION MATRICE JACOBIENNE
C         DRDY   :  MATRICE JACOBIENNE
C     OUT DSDE   :  MATRICE TANGENTE EN VITESSE
C     ----------------------------------------------------------------
      IMPLICIT NONE
C     ----------------------------------------------------------------
      COMMON /TDIM/   NDT  , NDI
C     ----------------------------------------------------------------
      INTEGER NMAT,NR,IRET,NDT,NDI,I,J,NVI,NORM
      REAL*8  HOOK(6,6),DRDY(NR,NR),DSDE(6,6),MATER(NMAT,2)
      REAL*8  Y0(NDT,NDT),Y1(NDT,NVI),Y2(NVI,NDT),Y3(NVI,NVI)
      REAL*8  Y4(NDT,NDT),Y5(NDT,NDT),DET,R8PREM,MAXI,MINI
      REAL*8  DSDEB(NDT,NDT)
      CHARACTER*4 CARGAU
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
      DO 5 I = 1, NDT
        DO 6 J = 1, NDT
          Y0(I,J) = DRDY(I,J)
 6      CONTINUE
 5    CONTINUE  
      DO 51 I = 1, NDT
        DO 61 J = 1, NVI
          Y1(I,J) = DRDY(I,J+NDT)
 61      CONTINUE
 51    CONTINUE  
      DO 52 I = 1, NVI
        DO 62 J = 1, NDT
          Y2(I,J) = DRDY(I+NDT,J)
 62      CONTINUE
 52    CONTINUE  
      DO 53 I = 1, NVI
        DO 63 J = 1, NVI
          Y3(I,J) = DRDY(I+NDT,J+NDT)
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
      CALL MGAUSS (CARGAU,Y3, Y2, NVI, NVI, NDT, DET, IRET )
      IF(IRET.GT.1) THEN
         CALL LCEQMA(HOOK,DSDE)
         GOTO 9999
      ENDIF
C --- PRODUIT DU TERME (Y3)^-1 * Y2 = Y4
      CALL PROMAT(Y1,NDT,NDT,NVI,Y2,NVI,NVI,NDT,Y4)

C --- DIFFERENCE DE MATRICE (DR1DY1 - Y4) = Y5
      DO 13 I=1,NDT
         DO 14 J=1,NDT
            Y5(I,J)=Y0(I,J)-Y4(I,J)
 14      CONTINUE
 13   CONTINUE

C --- INVERSION DU TERME Y5
      CALL R8INIR(NDT*NDT,0.D0,DSDEB,1)
      DO 8 I=1,NDT
        DSDEB(I,I) = 1.D0
 8    CONTINUE
      CALL MGAUSS(CARGAU,Y5, DSDEB, NDT, NDT, NDT, DET, IRET)
      
      IF(IRET.GT.1) THEN
         CALL LCEQMA(HOOK,DSDE)
      ELSE
         CALL R8INIR(36,0.D0,DSDE,1)
         DO 12 I=1,NDT
            DO 11 J=1,NDT
               DSDE(I,J)=DSDEB(I,J)
 11         CONTINUE
 12      CONTINUE
         
      ENDIF

 9999 CONTINUE
      END
