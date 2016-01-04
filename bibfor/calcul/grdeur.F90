function grdeur(nompar)

use calcul_module, only : ca_iaopds_, ca_iaoppa_

implicit none

! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.

! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.

! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr

#include "jeveux.h"
#include "asterc/indik8.h"
#include "asterfort/utmess.h"

    character(len=8) :: nompar
    integer :: grdeur
!-----------------------------------------------------------------------
!   entrees:
!      nompar : nom d'1 parametre de l'option que l'on calcule.
!   sorties:
!      grdeur : grandeur associee au parametre
!-----------------------------------------------------------------------
    integer :: jpar, nbpar
!-------------------------------------------------------------------

    nbpar = zi(ca_iaopds_-1+2) + zi(ca_iaopds_-1+3) + zi(ca_iaopds_-1+4)
    jpar = indik8(zk8(ca_iaoppa_),nompar,1,nbpar)
    if (jpar .eq. 0) then
        call utmess('F', 'CALCUL_14', sk=nompar)
    endif
    grdeur = zi(ca_iaopds_-1+4+jpar)
end function
