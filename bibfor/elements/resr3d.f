      SUBROUTINE RESR3D ( ROTA, COOR, FF, RHO, NNO, NPG, FRX, FRY, FRZ )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 07/12/2010   AUTEUR PELLET J.PELLET 
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
      IMPLICIT REAL*8 (A-H,O-Z)          
C        
C ......................................................................
C    - FONCTION REALISEE:  CALCUL DE LA FORCE 3D DUE A UN TERME DE
C                          ROTATION
C                          POUR L'OPTION : 'ERME_ELEM'
C                             (ESTIMATEUR EN RESIDU)
C
C    - ARGUMENTS:
C        DONNEES:  
C                  ROTA      -->  TABLEAU OME , AR , BR , CR
C                  COOR      -->  COORDONNEES DES NOEUDS
C                  FF        -->  FONCTIONS DE FORME AUX POINTS DE GAUSS
C                  RHO       -->  DENSITE
C                  NNO       -->  NOMBRE DE NOEUDS
C                  NPG       -->  NOMBRE DE POINTS DE GAUSS
C                      
C        SORTIE :  FRX       -->  FORCE AU POINT DE GAUSS EN X
C                  FRY       -->  FORCE AU POINT DE GAUSS EN Y
C                  FRZ       -->  FORCE AU POINT DE GAUSS EN Z
C ......................................................................
C
      REAL*8             ROTA(*), COOR(1), FF(1) 
      REAL*8             FX(27), FY(27), FZ(27)  
      REAL*8             FRX(27), FRY(27), FRZ(27) 
      REAL*8             OMO, OMM, OM1, OM2, OM3 
      INTEGER            NPG, NNO, I, K, KP
C
      OMM = ROTA(1) * ROTA(1)
      OM1 = ROTA(1) * ROTA(2)
      OM2 = ROTA(1) * ROTA(3)
      OM3 = ROTA(1) * ROTA(4)
      DO 100 I=1,NNO
        OMO = OM1 * COOR(3*I-2) + OM2 * COOR(3*I-1)+ OM3 * COOR(3*I)
        FX(I) = OMM * COOR(3*I-2) - OMO * OM1
        FY(I) = OMM * COOR(3*I-1) - OMO * OM2
        FZ(I) = OMM * COOR(3*I)   - OMO * OM3
  100 CONTINUE
C    
      DO 200 KP=1, NPG
        K=(KP-1)*NNO
        FRX(KP) = 0.D0
        FRY(KP) = 0.D0
        FRZ(KP) = 0.D0
        DO 150 I=1, NNO
          FRX(KP) = FRX(KP) + FX(I) * FF(K+I)
          FRY(KP) = FRY(KP) + FY(I) * FF(K+I)
          FRZ(KP) = FRZ(KP) + FZ(I) * FF(K+I)
  150   CONTINUE
        FRX(KP) = RHO * FRX(KP)
        FRY(KP) = RHO * FRY(KP)
        FRZ(KP) = RHO * FRZ(KP)
  200 CONTINUE
      END
