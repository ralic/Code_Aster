      SUBROUTINE DXQNIM ( INT , R , NMI )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 29/04/2000   AUTEUR CIBHHGB G.BERTRAND 
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
      INTEGER  INT
      REAL*8   R(*)
      REAL*8   NMI(4)
C     ------------------------------------------------------------------
C     FONCTIONS D'INTERPOLATION MEMBRANE POUR LES ELEMS DKQ, DSQ ET Q4G
C     ------------------------------------------------------------------
      REAL*8  QSI , ETA , LBD
      REAL*8  X4,X6 , Y4,Y6
C
C     ------------------ PARAMETRAGE QUADRANGLE ------------------------
      INTEGER NPG , NC , NNO
      INTEGER LJACO,LTOR,LQSI,LETA
               PARAMETER (NPG   = 4)
               PARAMETER (NNO   = 4)
               PARAMETER (NC    = 4)
               PARAMETER (LJACO = 2)
               PARAMETER (LTOR  = LJACO + 4)
               PARAMETER (LQSI  = LTOR  + 1)
               PARAMETER (LETA  = LQSI + NPG + NNO + 2*NC)
C     ------------------------------------------------------------------
      UNQUAR = 0.25D0
      UN     = 1.0D0
      QSI = R(LQSI+INT-1)
      ETA = R(LETA+INT-1)
C
      NMI(1) = UNQUAR*(UN-QSI)*(UN-ETA)
      NMI(2) = UNQUAR*(UN+QSI)*(UN-ETA)       
      NMI(3) = UNQUAR*(UN+QSI)*(UN+ETA)               
      NMI(4) = UNQUAR*(UN-QSI)*(UN+ETA)               
C
      END
