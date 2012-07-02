      SUBROUTINE ZZLOCA(MODELE,LIGREL,MATEZ,SIGMA,SIGNO,CHVARC,RESU)
      IMPLICIT NONE
      CHARACTER*(*) MODELE,LIGREL,MATEZ,SIGMA,SIGNO,CHVARC,RESU
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.

C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.

C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C
C     BUT:
C         CALCUL DE L'ESTIMATEUR D'ERREUR SUR LES CONTRAINTES
C
C                 OPTION : 'CALC_ESTI_ERRE'
C
C     ENTREES:
C
C     LES NOMS QUI SUIVENT SONT LES PREFIXES UTILISATEUR K8:
C     MODELE : NOM DU MODELE
C     MATEZ  : NOM DU CONCEPT MATERIAU
C     SIGMA  : NOM DU CHAMP DE CONTRAINTES CALCULEES (CHAM_ELEM_SIEF_R)
C     SIGNO  : NOM DU CHAMP DE CONTRAINTES LISSEES
C     CHVARC : NOM DU CHAMP DE VARIABLE DE COMMANDE
C
C     SORTIE :
C
C      RESU   : NOM DU CHAM_ELEM_ERREUR PRODUIT
C               SI RESU EXISTE DEJA, ON LE DETRUIT.
C ......................................................................

      LOGICAL EXIGEO
      CHARACTER*8 LCHAR,LPAIN(5),LPAOUT(1)
      CHARACTER*16 OPTION
      CHARACTER*24 LCHIN(5),LCHOUT(1),CHGEOM,MATE

C DEB-------------------------------------------------------------------

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      MATE = MATEZ
      LCHAR = ' '
      CALL MEGEOM(MODELE,LCHAR,EXIGEO,CHGEOM)
      IF (.NOT.EXIGEO) CALL U2MESS('F','CALCULEL2_81')

      IF (MATE.EQ.' ') CALL U2MESS('F','CALCULEL4_66')

      LPAIN(1) = 'PGEOMER'
      LCHIN(1) = CHGEOM
      LPAIN(2) = 'PMATERC'
      LCHIN(2) = MATE
      LPAIN(3) = 'PSIEF_R'
      LCHIN(3) = SIGMA
      LPAIN(4) = 'PVARCPR'
      LCHIN(4) = CHVARC
      LPAIN(5) = 'PSIGMA'
      LCHIN(5) = SIGNO

      LPAOUT(1) = 'PERREUR'
      LCHOUT(1) = RESU
      OPTION = 'CALC_ESTI_ERRE'
      CALL CALCUL('S',OPTION,LIGREL,5,LCHIN,LPAIN,1,LCHOUT,LPAOUT,'G',
     &               'OUI')

      END
