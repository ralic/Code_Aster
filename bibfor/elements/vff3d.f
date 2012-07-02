      SUBROUTINE VFF3D ( NNO,POIDS,DFDE,COOR,JAC )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
      IMPLICIT NONE
      INTEGER            NNO
      REAL *8                POIDS,DFDE(NNO),COOR(3*NNO),JAC
C ......................................................................
C    - FONCTION REALISEE:  VALEUR DU POIDS D'INTEGRATION POUR UN SEGMENT
C                          A 2 OU 3 NOEUDS
C    - ARGUMENTS:
C        DONNEES:          NNO      -->  NOMBRE DE NOEUDS
C                          KP       -->  NUMERO DU POINTS DE GAUSS
C                          COOR     -->  COORDONNEES DES NOEUDS
C
C      RESULTATS:          JAC      <--  JACOBIEN ASSOCIE
C ......................................................................
C
      REAL *8            DXDS,DYDS,DZDS
      INTEGER            I
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      DXDS = 0.D0
      DYDS = 0.D0
      DZDS = 0.D0
      DO 1 I = 1,NNO
         DXDS = DXDS + DFDE(I) * COOR(3*I-2)
         DYDS = DYDS + DFDE(I) * COOR(3*I-1)
         DZDS = DZDS + DFDE(I) * COOR(3*I)
 1    CONTINUE
      JAC = POIDS * SQRT(DXDS**2 + DYDS**2 + DZDS**2)
      END
