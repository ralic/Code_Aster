subroutine xelgano(modele,sigelga,sigseno)
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!
!     routine utilitaire X-FEM
!
!     - FONCTION REALISEE : PASSAGE D'UN CHAMP DE CONTRAINTES AUX POINTS 
!                           DE GAUSS DES SOUS-ELEMENTS X-FEM A UN CHAMP
!                           DE CONTRAINTES AUX NOEUDS DES SOUS-ELEMENTS
!
!     - ARGUMENTS   :
!
! IN    modele  : modele
! IN    sigelga : champ de contraintes aux points de gauss des sous-elements
! OUT   sigseno : champ de contraintes aux noeuds des sous-elements
!
!----------------------------------------------------------------------
! CORPS DU PROGRAMME
!
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/calcul.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/utmess.h"

!
    character(len=8)  :: modele
    character(len=24) :: sigelga
    character(len=24) :: sigseno
!
!
! DECLARATION VARIABLES LOCALES
!
    character(len=16) :: option

    integer :: nchin, nchout
    parameter (nchin=2)
    parameter (nchout=1)
    character(len=8)  :: lpain(nchin), lpaout(nchout)
    character(len=24) :: lchin(nchin), lchout(nchout)
    character(len=24) :: ligrmo
!
    call jemarq()
!
!   Recuperation du LIGREL
    ligrmo = modele//'.MODELE'

!   Definition de l'option de calcul (anciennement SIEF_SEGA_SENO)
    option = 'SISE_ELNO'

!   liste des champs et parametres en entree/sortie de calcul
    lpain(1) = 'PCONTRR'
    lchin(1) = sigelga
    lpain(2) = 'PLONCHA'
    lchin(2) = modele//'.TOPOSE.LON'

    lpaout(1) = 'PCONTSER'
    lchout(1) = sigseno

    call calcul('S',option,ligrmo,nchin,lchin,lpain,nchout,lchout,lpaout,'V','OUI')

    call jedema()
end subroutine
