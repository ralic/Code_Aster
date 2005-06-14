      SUBROUTINE HBMAJS(DG,NBMAT,MATERF,SE,I1E,SIGEQE,ETAP,SIGP)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 14/06/2005   AUTEUR JMBHH01 J.M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
C =====================================================================
      IMPLICIT      NONE
      INTEGER       NBMAT
      REAL*8        DG,MATERF(NBMAT,2),SE(6),I1E,SIGP(6),SIGEQE,ETAP
C ======================================================================
C --- LOI DE HOEK BROWN : MISE A JOUR DES CONTRAINTES A T+ -------------
C ======================================================================
C IN   DG      INCREMENT DE LA VARIABLE GAMMA --------------------------
C IN   NBMAT   NOMBRE DE DONNEES MATERIAU ------------------------------
C IN   MATERF  DONNEES MATERIAU ----------------------------------------
C IN   SE      DEVIATEUR ELASTIQUE -------------------------------------
C IN   ETAP    VALEUR DE ETA A GAMMA_PLUS ------------------------------
C OUT  SIGP    CONTRAINTES A T+ ----------------------------------------
C ======================================================================
      INTEGER  II,NDI,NDT
      REAL*8   K,MU,I1,DEV(6),UN,NEUF,TROIS
C =================================================================
      PARAMETER       ( UN     =  1.0D0  )
      PARAMETER       ( NEUF   =  9.0D0  )
      PARAMETER       ( TROIS  =  3.0D0  )
C ======================================================================
      COMMON /TDIM/   NDT, NDI
C ======================================================================
      K  = MATERF(5,1)
      MU = MATERF(4,1)
      DO 10 II=1,NDT
         DEV(II) = SE(II)*(UN-TROIS*MU*DG/(SIGEQE*(ETAP+UN)))
 10   CONTINUE         
      I1 = I1E - NEUF*K*ETAP*DG/(ETAP+UN)
      DO 20 II=1,NDT
         SIGP(II) = DEV(II)
 20   CONTINUE
      DO 30 II=1,NDI
         SIGP(II) = SIGP(II) + I1/TROIS
 30   CONTINUE
C ======================================================================
      END
