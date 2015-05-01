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
    subroutine pmfmcf(ip, nbgf, nbfib, nugf, sdcomp,&
                      crit, option, instam, instap,&
                      icdmat, nbvalc, defam, defap, varim,&
                      varimp, contm, defm, defp, epsm,&
                      modf, sigf, varip, codret)
        integer :: ip
        integer :: nbgf
        integer :: nbfib
        integer :: nugf(*)
        character(len=24) :: sdcomp(*)
        real(kind=8) :: crit(*)
        character(len=16) :: option
        real(kind=8) :: instam
        real(kind=8) :: instap
        integer :: icdmat
        integer :: nbvalc
        real(kind=8) :: defam(*)
        real(kind=8) :: defap(*)
        real(kind=8) :: varim(*)
        real(kind=8) :: varimp(*)
        real(kind=8) :: contm(*)
        real(kind=8) :: defm(*)
        real(kind=8) :: defp(*)
        real(kind=8) :: epsm
        real(kind=8) :: modf(*)
        real(kind=8) :: sigf(*)
        real(kind=8) :: varip(*)
        integer :: codret
    end subroutine pmfmcf
end interface
