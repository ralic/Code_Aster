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
    subroutine pmfcom(kpg, debsp, option, compor, crit, &
                  nf,instam, instap, icdmat,nbvalc, &
                  defam, defap, varim, varimp,contm, &
                  defm, ddefp, epsm, modf,sigf, &
                  varip, isecan, codret)
        integer :: nbvalc
        integer :: nf
        integer :: kpg
        integer :: debsp
        character(len=16) :: option
        character(len=24) :: compor(*)
        real(kind=8) :: crit(*)
        real(kind=8) :: instam
        real(kind=8) :: instap
        integer :: icdmat
        real(kind=8) :: defam(*)
        real(kind=8) :: defap(*)
        real(kind=8) :: varim(nbvalc*nf)
        real(kind=8) :: varimp(nbvalc*nf)
        real(kind=8) :: contm(nf)
        real(kind=8) :: defm(nf)
        real(kind=8) :: ddefp(nf)
        real(kind=8) :: epsm
        real(kind=8) :: modf(nf)
        real(kind=8) :: sigf(nf)
        real(kind=8) :: varip(nbvalc*nf)
        integer :: isecan
        integer :: codret
    end subroutine pmfcom
end interface
