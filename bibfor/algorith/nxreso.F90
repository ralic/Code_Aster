subroutine nxreso(matass, maprec, solver, cnchci, cn2mbr,&
                  chsolu)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/resoud.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=19), intent(in) :: maprec
    character(len=24), intent(in) :: matass
    character(len=19), intent(in) :: solver
    character(len=24), intent(in) :: cnchci
    character(len=24), intent(in) :: cn2mbr
    character(len=19), intent(in) :: chsolu
!
! --------------------------------------------------------------------------------------------------
!
! THER_NON_LINE
!
! Solve linear system
!
! --------------------------------------------------------------------------------------------------
!
!
! --------------------------------------------------------------------------------------------------
!
    complex(kind=8), parameter :: cbid = dcmplx(0.d0, 0.d0)
    integer :: iret
    character(len=24) :: criter
!
! --------------------------------------------------------------------------------------------------
!
    criter = '&&RESGRA_GCPC'

    call resoud(matass, maprec, solver, cnchci, 0,&
                cn2mbr, chsolu, 'V', [0.d0], [cbid],&
                criter, .true._1, 0, iret)
!

end subroutine
