subroutine cgvedo(ndim, option)
    implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/utmess.h"
    integer :: ndim
    character(len=16) :: option
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: samuel.geniaut at edf.fr
!
!      SOUS-ROUTINE DE L'OPERATEUR CALC_G
!
!      BUT : VERIFICATION DE LA COMPATIBILITE ENTRE NDIM ET OPTION
!
!  IN :
!     NDIM   : DIMENSION DU PROBLEME
!     OPTION : OPTION DE CALC_G
! ======================================================================
!
    integer :: nbop3d, i
    parameter   (nbop3d=4)
    aster_logical :: bool
    character(len=16) :: liop3d(nbop3d)
    data         liop3d / 'CALC_G_GLOB','G_MAX_GLOB','G_BILI_GLOB',&
     &                      'CALC_K_MAX' /
!
!     VERIFICATION DE NDIM VAUT 2 OU 3
    if (.not.(ndim.eq.2.or.ndim.eq.3)) then
        call utmess('F', 'RUPTURE0_2')
    endif
!
!     VERIFICATION DE L'OPTION (NORMALEMENT, C'EST FAIT DANS LE CAPY)
    bool = option .eq. 'CALC_G' .or. option .eq. 'CALC_G_GLOB' .or. option .eq. 'CALC_K_G' .or.&
           option .eq. 'K_G_MODA' .or. option .eq. 'G_BILI' .or. option .eq. 'G_BILI_GLOB' .or.&
           option .eq. 'G_MAX' .or. option .eq. 'G_MAX_GLOB' .or. option .eq. 'CALC_K_MAX' .or.&
           option .eq. 'CALC_GTP'
    ASSERT(bool)
!
!     CERTAINES OPTIONS NE S'UTILISENT (OU NE SONT PROGRAMMEES) QU'EN 3D
    if (ndim .eq. 2) then
        do 10 i = 1, nbop3d
            if (option .eq. liop3d(i)) then
                call utmess('F', 'RUPTURE0_3', sk=option)
            endif
10      continue
    endif
!
end subroutine
