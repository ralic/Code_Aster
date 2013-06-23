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
    subroutine lkcomp(mod, imate, instam, instap, tm,&
                      tp, tref, deps, sigm, vinm,&
                      option, sigp, vinp, dside, retcom,&
                      invi)
        integer :: invi
        character(len=8) :: mod(*)
        integer :: imate
        real(kind=8) :: instam
        real(kind=8) :: instap
        real(kind=8) :: tm
        real(kind=8) :: tp
        real(kind=8) :: tref
        real(kind=8) :: deps(6)
        real(kind=8) :: sigm(6)
        real(kind=8) :: vinm(invi)
        character(len=16) :: option
        real(kind=8) :: sigp(6)
        real(kind=8) :: vinp(invi)
        real(kind=8) :: dside(6, 6)
        integer :: retcom
    end subroutine lkcomp
end interface
