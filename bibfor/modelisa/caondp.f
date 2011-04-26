      SUBROUTINE CAONDP(CHAR,NOMA,ONDP)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C BUT : STOCKAGE DES CARAC. ONDE PLANE DANS UNE CARTE ALLOUEE SUR LE
C       LIGREL DU MODELE
C
C ARGUMENTS D'ENTREE:
C      CHAR: NOM UTILISATEUR DU RESULTAT DE CHARGE
C      NOMA : NOM DU MAILLAGE
C
      COMPLEX*16 CBID
      REAL*8 R8BID
      CHARACTER*24 ONDP(6)
      CHARACTER*8  CHAR,NOMA,LICMP(6)
      CHARACTER*19 CARTE
C
      CARTE=CHAR//'.CHME.ONDPL'
      LICMP(1)='Z1'
      LICMP(2)='Z2'
      LICMP(3)='Z3'
      LICMP(4)='Z4'
      LICMP(5)='Z5'
      LICMP(6)='Z6'
      CALL MECACT('G',CARTE,'MAILLA',NOMA,'NEUT_K24',6,LICMP,0,R8BID,
     +            CBID,ONDP)
      END
