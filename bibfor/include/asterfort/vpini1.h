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
interface
    subroutine vpini1(eigsol, modes, solveu, typcon, vecblo, veclag, vecrig,&
                      matpsc, matopa, iretr, nblagr, neqact, npivot, nstoc, omemax, omemin, omeshi,&
                      sigma)
        character(len=19) , intent(in)  :: eigsol
        character(len=8)  , intent(in)  :: modes
        character(len=19) , intent(in)  :: solveu
        character(len=16) , intent(in)  :: typcon
        character(len=24) , intent(in)  :: vecblo
        character(len=24) , intent(in)  :: veclag
        character(len=24) , intent(in)  :: vecrig
!!
        character(len=19) , intent(inout)  :: matpsc
        character(len=19) , intent(inout)  :: matopa
        integer           , intent(out) :: iretr
        integer           , intent(out) :: nblagr
        integer           , intent(out) :: neqact
        integer           , intent(out) :: npivot
        integer           , intent(out) :: nstoc
        real(kind=8)      , intent(out) :: omemax
        real(kind=8)      , intent(out) :: omemin
        real(kind=8)      , intent(out) :: omeshi
        complex(kind=8)   , intent(out) :: sigma
    end subroutine vpini1
end interface
