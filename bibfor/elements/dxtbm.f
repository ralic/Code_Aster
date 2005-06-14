      SUBROUTINE DXTBM  ( R , BM )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 11/03/98   AUTEUR CIBHHLV L.VIVAN 
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
      REAL*8   R(*)
      REAL*8   BM(3,6)
C     ------------------------------------------------------------------
C     MATRICE BM(3,6) EN MEMBRANE POUR LES ELEMENT DKT ET DST
C     ------------------------------------------------------------------
      REAL*8  VJ11 , VJ12 , VJ21 , VJ22
C
C     ------------------ PARAMETRAGE TRIANGLE --------------------------
      INTEGER LJACO
               PARAMETER (LJACO = 2)
C     ------------------------------------------------------------------
      VJ11 = R(LJACO)
      VJ12 = R(LJACO+1)
      VJ21 = R(LJACO+2)
      VJ22 = R(LJACO+3)
C
      BM(1,1) = - VJ11 - VJ12
      BM(1,2) = 0.D0
      BM(1,3) =   VJ11
      BM(1,4) = 0.D0
      BM(1,5) =   VJ12
      BM(1,6) = 0.D0
      BM(2,1) = 0.D0
      BM(2,2) = - VJ21 - VJ22
      BM(2,3) = 0.D021
      BM(2,4) = VJ21
      BM(2,5) = 0.D0
      BM(2,6) = VJ22
      BM(3,1) = BM(2,2)
      BM(3,2) = BM(1,1)
      BM(3,3) = BM(2,4)
      BM(3,4) = BM(1,3)
      BM(3,5) = BM(2,6)
      BM(3,6) = BM(1,5)
      END
