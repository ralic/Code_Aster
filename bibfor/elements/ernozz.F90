subroutine ernozz(modele, sigma, chmat, signo, chvarc,&
                  option, ligrel, iordr, resuco, resuc1,&
                  champ)
    implicit none
#include "asterfort/erglob.h"
#include "asterfort/zzloca.h"
    integer :: iordr
    character(len=*) :: modele, sigma, chmat, signo, option, ligrel
    character(len=*) :: champ, resuco
    character(len=19) :: resuc1, chvarc
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
!         CALCULER LES ESTIMATEURS LOCAUX A PARTIR DES
!         CONTRAINTES.
!         CALCULER LES ESTIMATEURS GLOBAUX A PARTIR DES ESTIMATEURS
!         LOCAUX CONTENUS DANS CHAMP.
!
!
!     ARGUMENTS:
!     ----------
!
!      ENTREE :
!-------------
! IN   MODELE :  NOM DU MODELE
! IN   SIGMA  :  CONTRAINTES AUX POINTS DE GAUSS
! IN   CHMAT  :  NOM DU CHAMP DE MATERIAU
! IN   SIGNO  :  CONTRAINTES AUX NOEUDS
! IN   OPTION :  'ERZ1_ELEM' OU 'ERZ2_ELEM'
! IN   LIGREL :  NOM D'UN LIGREL SUR LEQUEL ON FERA LE CALCUL
! IN   IORDR  :  NUMERO D'ORDRE
! IN   RESUCO :  NOM DE CONCEPT ENTRANT
! IN   RESUC1 :  NOM DE CONCEPT RESULTAT
!
!      SORTIE :
!-------------
! OUT  CHAMP  :  CONTRAINTES AUX NOEUDS
!
! ======================================================================
!
!    CALCUL DE L'ESTIMATEUR D'ERREUR
!
    call zzloca(modele, ligrel, chmat, sigma, signo,&
                chvarc, champ)
    call erglob(champ, .false., .false., option, iordr,&
                resuco, resuc1)
!
end subroutine
