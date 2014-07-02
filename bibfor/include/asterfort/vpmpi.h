! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
#include "asterf_types.h"
interface
    subroutine vpmpi(option, eigsol, icom1, icom2, lcomod,&
                     mpicou, mpicow, nbvecg, nfreqg, rangl,&
                     omemax, omemin, vpinf, vpmax)
        integer, intent(in) :: option
        character(len=19), intent(in) :: eigsol
!!
        integer, intent(out) :: icom1
        integer, intent(out) :: icom2
        aster_logical , intent(inout) :: lcomod
        mpi_int , intent(inout) :: mpicou
        mpi_int , intent(inout) :: mpicow
        integer, intent(out) :: nbvecg
        integer, intent(out) :: nfreqg
        integer, intent(inout) :: rangl
        real(kind=8), intent(inout) :: omemax
        real(kind=8), intent(inout) :: omemin
        real(kind=8), intent(inout) :: vpinf
        real(kind=8), intent(inout) :: vpmax
    end subroutine vpmpi
end interface
