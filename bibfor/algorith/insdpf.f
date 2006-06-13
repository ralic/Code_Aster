      SUBROUTINE INSDPF(NMAT,MATERF,DSDE,MOD)
      IMPLICIT REAL*8 (A-H,O-Z)
C       -----------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 09/02/2004   AUTEUR REZETTE C.REZETTE 
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
C     -----------------------------------------------------------
C     NADAI_B       :  MATRICE DE COMPORTEMENT TANGENT
C                      BETON FISSURE EN VITESSE A T OU T+DT
C     ----------------------------------------------------------------
C     IN  NMAT   :  DIMENSION MATER
C         MATERF  :  COEFFICIENTS MATERIAU
C     OUT DSDE   :  MATRICE DE COMPORTEMENT TANGENT = DSIG / DST
C       -----------------------------------------------------------
      INTEGER   NMAT
      REAL*8    MATERF(NMAT,2)
      REAL*8    DSDE(6,6), TE(3,3), DEPF(3,3), V(3), DSDE3(3,3)
      REAL*8    PHIC, A, B, NU, E, D33, DELS, ETS1, ETS2, PI
      CHARACTER*8     MOD
C       ------------------------------------------------------------
        COMMON /TDIM/   NDT , NDI
C       ------------------------------------------------------------
      CALL LCINMA ( 0.D0 , DSDE )
      CALL LCOPLI ( 'ISOTROPE' , MOD , MATERF(1,1) , DSDE )
C
 9999 CONTINUE
      END
