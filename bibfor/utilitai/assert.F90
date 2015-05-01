subroutine assert(cond, str_cond, fname, line)
    implicit none
#include "asterf_types.h"
#include "asterfort/utmess.h"
    aster_logical :: cond
    character(len=*) :: str_cond
    character(len=*) :: fname
    integer :: line
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
! person_in_charge: mathieu.courtois@edf.fr
!
    character(len=256) :: valk(2)
    integer :: vali(1)
    real(kind=8) :: rbid(1)
    if (.not.cond) then
        valk(1) = str_cond
        valk(2) = fname
        vali(1) = line
        call utmess('F', 'DVP_1', nk=2, valk=valk, si=vali(1),&
                    sr=rbid(1))
    endif
end subroutine
