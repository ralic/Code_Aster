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
    subroutine xcabhm(nddls, nddlm, nnop, nnops, nnopm,&
                      dimuel, ndim, kpi, ff, ff2,&
                      dfdi, dfdi2, b, nmec, yamec,&
                      addeme, yap1, addep1, np1, axi,&
                      ivf, ipoids, idfde, poids, coorse,&
                      nno, geom, yaenrm, adenme, dimenr,&
                      he)
        integer :: dimenr
        integer :: ndim
        integer :: dimuel
        integer :: nnops
        integer :: nnop
        integer :: nddls
        integer :: nddlm
        integer :: nnopm
        integer :: kpi
        real(kind=8) :: ff(nnop)
        real(kind=8) :: ff2(nnops)
        real(kind=8) :: dfdi(nnop, ndim)
        real(kind=8) :: dfdi2(nnops, ndim)
        real(kind=8) :: b(dimenr, dimuel)
        integer :: nmec
        integer :: yamec
        integer :: addeme
        integer :: yap1
        integer :: addep1
        integer :: np1
        logical(kind=1) :: axi
        integer :: ivf
        integer :: ipoids
        integer :: idfde
        real(kind=8) :: poids
        real(kind=8) :: coorse(81)
        integer :: nno
        real(kind=8) :: geom(ndim, nnop)
        integer :: yaenrm
        integer :: adenme
        real(kind=8) :: he
    end subroutine xcabhm
end interface 
