      SUBROUTINE VPRI2D(SIG,SIGI)
C ......................................................................
C .  - FONCTION REALISEE:  CALCUL DU MAX DES VALEURS PROPRES D'UN      .
      IMPLICIT NONE
C .         TENSEUR DE TYPE CONTRAINTE/DEFORMATION 2D                  .
C .  - ARGUMENTS:                                                      .
C .      DONNEES:          SIG      -->                                .
C .      RESULTATS:       SIGI     <--                                .
C ......................................................................

C     ------------------------------------------------------------------
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
C     ------------------------------------------------------------------
C
      INCLUDE 'jeveux.h'
      REAL*8 SIG(4),SIGI
      REAL*8 S,SP,AL1,AL2,DELTA,SQD
C     ------------------------------------------------------------------

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      S = SIG(1)+SIG(2)
      SP= SIG(1)-SIG(2)
      DELTA=SP**2+4.D0*SIG(4)**2
      SQD = SQRT(DELTA)
      AL1 = (S+SQD)/2.D0
      AL2 = (S-SQD)/2.D0
      SIGI = MAX(AL1,AL2,SIG(3),0.D0)
      END
