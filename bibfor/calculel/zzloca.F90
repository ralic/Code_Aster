subroutine zzloca(modele, ligrel, matez, sigma, signo,&
                  chvarc, resu)
    implicit none
#include "asterfort/calcul.h"
#include "asterfort/megeom.h"
#include "asterfort/u2mess.h"
    character(len=*) :: modele, ligrel, matez, sigma, signo, chvarc, resu
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     BUT:
!         CALCUL DE L'ESTIMATEUR D'ERREUR SUR LES CONTRAINTES
!
!                 OPTION : 'CALC_ESTI_ERRE'
!
!     ENTREES:
!
!     LES NOMS QUI SUIVENT SONT LES PREFIXES UTILISATEUR K8:
!     MODELE : NOM DU MODELE
!     MATEZ  : NOM DU CONCEPT MATERIAU
!     SIGMA  : NOM DU CHAMP DE CONTRAINTES CALCULEES (CHAM_ELEM_SIEF_R)
!     SIGNO  : NOM DU CHAMP DE CONTRAINTES LISSEES
!     CHVARC : NOM DU CHAMP DE VARIABLE DE COMMANDE
!
!     SORTIE :
!
!      RESU   : NOM DU CHAM_ELEM_ERREUR PRODUIT
!               SI RESU EXISTE DEJA, ON LE DETRUIT.
! ......................................................................
!
    character(len=8) :: lpain(5), lpaout(1)
    character(len=16) :: option
    character(len=24) :: lchin(5), lchout(1), chgeom, mate
!
! DEB-------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    mate = matez
    call megeom(modele, chgeom)
!
    if (mate .eq. ' ') call u2mess('F', 'CALCULEL4_66')
!
    lpain(1) = 'PGEOMER'
    lchin(1) = chgeom
    lpain(2) = 'PMATERC'
    lchin(2) = mate
    lpain(3) = 'PSIEF_R'
    lchin(3) = sigma
    lpain(4) = 'PVARCPR'
    lchin(4) = chvarc
    lpain(5) = 'PSIGMA'
    lchin(5) = signo
!
    lpaout(1) = 'PERREUR'
    lchout(1) = resu
    option = 'CALC_ESTI_ERRE'
    call calcul('S', option, ligrel, 5, lchin,&
                lpain, 1, lchout, lpaout, 'G',&
                'OUI')
!
end subroutine
