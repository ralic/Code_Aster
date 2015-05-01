!
! COPYRIGHT (C) 1991 - 2015  EDF R&D                WWW.CODE-ASTER.ORG
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
    subroutine dpvpre(mod, nvi, option, crit, instam,&
                      instap, nbmat, materf, sigm, deps,&
                      vim, vip, sig, nbre, dsidep,&
                      iret)
        integer :: nbmat
        integer :: nvi
        character(len=8) :: mod
        character(len=16) :: option
        real(kind=8) :: crit(3)
        real(kind=8) :: instam
        real(kind=8) :: instap
        real(kind=8) :: materf(nbmat, 2)
        real(kind=8) :: sigm(6)
        real(kind=8) :: deps(6)
        real(kind=8) :: vim(nvi)
        real(kind=8) :: vip(nvi)
        real(kind=8) :: sig(6)
        integer :: nbre
        real(kind=8) :: dsidep(6, 6)
        integer :: iret
    end subroutine dpvpre
end interface
