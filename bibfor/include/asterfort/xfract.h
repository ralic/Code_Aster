!
! COPYRIGHT (C) 1991 - 2016  EDF R&D                WWW.CODE-ASTER.ORG
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
    subroutine xfract(nvec, nnop, nnops, nddls, nddlm,&
                      ndim, pla, deplp, deplm,&
                      ffp, ffc, dffc, saut, gradpf,&
                      q1, q2, dpf, q1m, q2m, sautm,&
                      gradpfm, pf, ffp2, psup, pinf,&
                      job, jmate, meca, hydr, thmc,&
                      t, dimuel, lamb, jheavn, ncompn,&
                      ifiss, nfiss, nfh, ifa, jheafa,&
                      ncomph, contac, depl0, depl1, lambm, pfm)
                           
        integer, intent(in) :: nvec
        integer, intent(in) :: nddlm
        integer, intent(in) :: nnop
        integer, intent(in) :: nnops
        integer, intent(in) :: nddls
        integer, intent(in) :: ndim
        integer, intent(in) :: pla(27)
        integer, intent(in) :: dimuel
        real(kind=8), intent(in) :: deplm(dimuel)
        real(kind=8), intent(in) :: deplp(dimuel)
        real(kind=8), intent(in) :: ffp(27)
        real(kind=8), intent(in) :: ffc(16)
        real(kind=8), intent(in) :: dffc(16,3)
        real(kind=8), intent(in) :: ffp2(27)
        character(len=8), intent(in) :: job
        integer, intent(in) :: jmate
        character(len=16), intent(in) :: meca
        character(len=16), intent(in) :: hydr
        character(len=16), intent(in) :: thmc
        real(kind=8), intent(out) :: lamb(3)
        real(kind=8), intent(out) :: t
        real(kind=8), intent(out) :: psup
        real(kind=8), intent(out) :: pinf
        real(kind=8), intent(out) :: saut(3)
        real(kind=8), intent(out) :: gradpf(3)
        real(kind=8), intent(out) :: q1
        real(kind=8), intent(out) :: q2
        real(kind=8), intent(out) :: dpf
        real(kind=8), intent(out) :: q1m
        real(kind=8), intent(out) :: q2m
        real(kind=8), intent(out) :: sautm(3)
        real(kind=8), intent(out) :: gradpfm(3)
        real(kind=8), intent(out) :: pf
        integer, intent(in) :: nfiss
        integer, intent(in) :: jheavn
        integer :: ifiss
        integer, intent(in) :: ncompn
        integer :: nfh
        integer :: ifa
        integer :: jheafa
        integer, intent(in) :: ncomph
        integer, intent(in) :: contac
        real(kind=8), optional, intent(in) :: depl0(dimuel)
        real(kind=8), optional, intent(in) :: depl1(dimuel)
        real(kind=8), optional, intent(out) :: lambm(3)
        real(kind=8), optional, intent(out) :: pfm
    end subroutine xfract
end interface
