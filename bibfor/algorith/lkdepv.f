      SUBROUTINE LKDEPV(NBMAT,MATER,
     &                  DEPSV,DDEPSV,DGAMV,DDGAMV)
C
      IMPLICIT    NONE
      INTEGER     NBMAT
      REAL*8      MATER(NBMAT,2),DEPSV(6),DDEPSV(6)
      REAL*8      DGAMV, DDGAMV(6)
C =================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 29/10/2007   AUTEUR ELGHARIB J.EL-GHARIB 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
C =================================================================
C --- MODELE LETK : LAIGLE VISCOPLASTIQUE--------------------------
C =================================================================
C --- BUT : DERIVEE DE LA DEFORMATION VISQUEUSE ET DU PARAMETRE
C ---- D ECROUISSAGE VISQUEUX
C =================================================================
C IN  : NBMAT :  NOMBRE DE PARAMETRES MATERIAU --------------------
C --- : MATER :  COEFFICIENTS MATERIAU A T+DT ---------------------
C ---         :  MATER(*,1) = CARACTERISTIQUES ELASTIQUES ---------
C ---         :  MATER(*,2) = CARACTERISTIQUES PLASTIQUES ---------
C --- : DT    : PAS DE TEMPS --------------------------------------
C     : DEPSV : DEFORMATIONS VISQUEUSES ---------------------------
C OUT : DDEPSV: DEFORMATIONS DEVIATORIQUES VISQUEUSES -------------
C     : DGAMV : PARAMETRE D ECROUISSAGE VISQUEUX ------------------
C     :DDGAMV : DERIVEE DU PARAMETRE D ECROUISSAGE VISQUEUX PAR  --
C     :         RAPPORT A DEPS ------------------------------------
C =================================================================
      COMMON /TDIM/   NDT , NDI
      INTEGER   I, K, NDI, NDT
      REAL*8    AA(6)
      REAL*8    ZERO, UN, MUN, DEUX, TROIS
      REAL*8    DEVIA(6,6)
C =================================================================
C --- INITIALISATION DE PARAMETRES --------------------------------
C =================================================================
      PARAMETER       ( ZERO    =  0.0D0   )
      PARAMETER       ( UN      =  1.0D0   )
      PARAMETER       ( MUN     = -1.0D0   )
      PARAMETER       ( DEUX    =  2.0D0   )
      PARAMETER       ( TROIS   =  3.0D0   )
C =================================================================
C --- CALCUL DU DEVIATEUR DU TENSEUR DES DEFORMATIONS VISQUEUSES -
C =================================================================

      CALL LCDEVI(DEPSV, DDEPSV)

C =================================================================
C --- CALCUL DE DGAMV ------------------------------------
C =================================================================
      
      DGAMV = 0.D0

      DO 20 I = 1,NDT

      DGAMV = DGAMV + DDEPSV(I)**2 

  20  CONTINUE
      DGAMV = SQRT(DEUX/TROIS * DGAMV)
C =================================================================
C --- MATRICE DE PROJECTION DEVIATORIQUE --------------------------
C =================================================================

      CALL R8INIR(6*6,0.D0,DEVIA,1)

      DO 30 I = 1, 3
      DO 40 K = 1, 3
      DEVIA(I,K) = MUN/TROIS      
  40  CONTINUE
  30  CONTINUE

      DO 50 I = 1, NDT
      DEVIA(I,I) = DEVIA(I,I)+ UN
  50  CONTINUE
     
C =================================================================
C --- CALCUL DE DERIVEE DE DGAMV/DEPS ---------------------------
C =================================================================
      
      CALL R8INIR(6,0.D0,DDGAMV,1)
      
       IF (DGAMV .EQ. ZERO) THEN
        DO 60 I = 1, NDT
        DDGAMV(I) = ZERO
  60  CONTINUE
       ELSE
        CALL LCPRMV(DEVIA,DDEPSV,DDGAMV)
        DO 70 I = 1, NDT
        DDGAMV(I) = DEUX/TROIS*DDGAMV(I)
  70  CONTINUE
       ENDIF
      
C =================================================================
      END
