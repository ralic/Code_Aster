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
    subroutine pmstab(sigm, sigp, epsm, deps, nbvari,&
                      vim, vip, iforta, instam, instap,&
                      iter, nbpar, nompar, table, vr,&
                      igrad, valimp, imptgt, dsidep, nomvi,&
                      nbvita)
        real(kind=8) :: sigm(6)
        real(kind=8) :: sigp(6)
        real(kind=8) :: epsm(9)
        real(kind=8) :: deps(9)
        integer :: nbvari
        real(kind=8) :: vim(*)
        real(kind=8) :: vip(*)
        integer :: iforta
        real(kind=8) :: instam
        real(kind=8) :: instap
        integer :: iter
        integer :: nbpar
        character(len=16) :: nompar(*)
        character(len=8) :: table
        real(kind=8) :: vr(*)
        integer :: igrad
        real(kind=8) :: valimp(9)
        integer :: imptgt
        real(kind=8) :: dsidep(*)
        character(len=8) :: nomvi(*)
        integer :: nbvita
    end subroutine pmstab
end interface
