      SUBROUTINE DILSGA(DIMDEF,DIMUEL,POIDS,POIDS2,B,R,VECTU)
C ======================================================================
       IMPLICIT NONE
       INTEGER  DIMDEF,DIMUEL
       REAL*8   POIDS,POIDS2,R(DIMDEF),B(DIMDEF,DIMUEL)
       REAL*8   VECTU(DIMUEL)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
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
C ======================================================================
C --- BUT : ASSEMBLAGE DES FORCES NODALES POUR LA PARTIE ---------------
C ---       SECOND GRADIENT POUR L ELEMENT CALCULE ---------------------
C ======================================================================
C --- VARIABLES LOCALES ------------------------------------------------
C ======================================================================
      CALL DGEMV('T',DIMDEF,DIMUEL,POIDS,B,DIMDEF,R,1,1.0D0,VECTU,1)
C ======================================================================
      END
