!
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
    subroutine vppost(vecrer, vecrei, vecrek, vecvp, nbpark, nbpari, nbparr, mxresf,&
                      nconv, nblagr, nfreqg, modes, typcon, compex, eigsol, matopa, matpsc, solveu,&
                      vecblo, veclag, flage,&
                      icom1, icom2, mpicou, mpicow, omemax, omemin, vpinf, vpmax, lcomod)
        character(len=24) , intent(in)    :: vecrer
        character(len=24) , intent(in)    :: vecrei
        character(len=24) , intent(in)    :: vecrek
        character(len=24) , intent(in)    :: vecvp
        integer           , intent(in)    :: nbpark
        integer           , intent(in)    :: nbpari
        integer           , intent(in)    :: nbparr
        integer           , intent(in)    :: mxresf
        integer           , intent(in)    :: nconv
        integer           , intent(in)    :: nblagr
        integer           , intent(in)    :: nfreqg
        character(len=8)  , intent(in)    :: modes
        character(len=16) , intent(in)    :: typcon
        character(len=16) , intent(in)    :: compex
        character(len=19) , intent(in)    :: eigsol
        character(len=19) , intent(in)    :: matopa
        character(len=19) , intent(in)    :: matpsc
        character(len=19) , intent(in)    :: solveu
        character(len=24) , intent(in)    :: vecblo
        character(len=24) , intent(in)    :: veclag
        logical(kind=1)   , intent(in)    :: flage
!!
        integer           , intent(inout) :: icom1
        integer           , intent(inout) :: icom2
        mpi_int           , intent(inout) :: mpicou
        mpi_int           , intent(inout) :: mpicow
        real(kind=8)      , intent(inout) :: omemax
        real(kind=8)      , intent(inout) :: omemin
        real(kind=8)      , intent(inout) :: vpinf
        real(kind=8)      , intent(inout) :: vpmax
        logical(kind=1)   , intent(inout) :: lcomod
    end subroutine vppost
end interface
