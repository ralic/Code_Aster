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
    subroutine cgfint(ndim, nno1, nno2, npg, wref,&
                      vff1, vff2, dffr1, geom, tang,&
                      typmod, option, mat, compor, lgpg,&
                      crit, instam, instap, ddlm, ddld,&
                      iu, iuc, im, a, sigm,&
                      vim, sigp, vip, matr, vect,&
                      codret)
        integer :: lgpg
        integer :: npg
        integer :: nno2
        integer :: nno1
        integer :: ndim
        real(kind=8) :: wref(npg)
        real(kind=8) :: vff1(nno1, npg)
        real(kind=8) :: vff2(nno2, npg)
        real(kind=8) :: dffr1(nno1, npg)
        real(kind=8) :: geom(ndim, nno1)
        real(kind=8) :: tang(*)
        character(len=8) :: typmod(*)
        character(len=16) :: option
        integer :: mat
        character(len=16) :: compor(*)
        real(kind=8) :: crit(*)
        real(kind=8) :: instam
        real(kind=8) :: instap
        real(kind=8) :: ddlm(nno1*(ndim+1)+nno2)
        real(kind=8) :: ddld(nno1*(ndim+1)+nno2)
        integer :: iu(3, 3)
        integer :: iuc(3)
        integer :: im(3)
        real(kind=8) :: a
        real(kind=8) :: sigm(3, npg)
        real(kind=8) :: vim(lgpg, npg)
        real(kind=8) :: sigp(3, npg)
        real(kind=8) :: vip(lgpg, npg)
        real(kind=8) :: matr(*)
        real(kind=8) :: vect(nno1*(ndim+1)+nno2)
        integer :: codret
    end subroutine cgfint
end interface
