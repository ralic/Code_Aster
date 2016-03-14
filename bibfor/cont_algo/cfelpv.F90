subroutine cfelpv(numlia, sdcont_solv, nbliai, lelpiv)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/jeveuo.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
    aster_logical :: lelpiv
    integer :: numlia
    integer :: nbliai
    character(len=24) :: sdcont_solv

!  SAVOIR SI LA LIAISON EST A PIVOT NUL
!
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  NUMLIA : NUMERO DE LA LIAISON

! IN  NBLIAI : NOMBRE DE LIAISONS DE CONTACT
! OUT CFELPV : .TRUE.  SI LIAISON A PIVOT NUL
!              .FALSE. SINON
!
!
!
!
    integer :: iote
    character(len=19) :: sdcont_liot
    integer, pointer :: v_sdcont_liot(:) => null()
! ======================================================================

    sdcont_liot = sdcont_solv(1:14)//'.LIOT'
    call jeveuo(sdcont_liot, 'L', vi = v_sdcont_liot)
! ======================================================================
    lelpiv = .false.
    do iote = 1, v_sdcont_liot(4*nbliai+1)
        if (v_sdcont_liot(iote) .eq. numlia) then
            lelpiv = .true.
            goto 100
        endif
    end do
100 continue
!
end subroutine
