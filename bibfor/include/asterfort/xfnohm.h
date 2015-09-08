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
#include "asterf_types.h"
!
interface 
    subroutine xfnohm(fnoevo, deltat, nno,&
                      npg, ipoids, ivf, idfde,&
                      geom, congem, b, dfdi, dfdi2,&
                      r, vectu, imate, mecani, press1,&
                      dimcon, nddls, nddlm, dimuel, nmec,&
                      np1, ndim, axi, dimenr, nnop,&
                      nnops, nnopm, igeom, jpintt, jpmilt,&
                      jheavn, lonch, cnset, heavt, enrmec, enrhyd,&
                      nfiss, nfh, jfisno)
        integer :: nnops
        integer :: nnop
        integer :: dimenr
        integer :: ndim
        integer :: dimuel
        aster_logical :: fnoevo
        real(kind=8) :: deltat
        integer :: nno
        integer :: npg
        integer :: ipoids
        integer :: ivf
        integer :: idfde
        real(kind=8) :: geom(ndim, nnop)
        real(kind=8) :: congem(*)
        real(kind=8) :: b(dimenr, dimuel)
        real(kind=8) :: dfdi(nnop, ndim)
        real(kind=8) :: dfdi2(nnops, ndim)
        real(kind=8) :: r(1:dimenr)
        real(kind=8) :: vectu(dimuel)
        integer :: imate
        integer :: mecani(5)
        integer :: press1(7)
        integer :: dimcon
        integer :: nddls
        integer :: nddlm
        integer :: nmec
        integer :: np1
        integer :: jheavn
        aster_logical :: axi
        integer :: nnopm
        integer :: igeom
        integer :: jpintt
        integer :: jpmilt
        integer :: lonch(10)
        integer :: cnset(*)
        integer :: heavt(*)
        integer :: enrmec(3)
        integer :: enrhyd(3)
        integer :: nfiss
        integer :: nfh
        integer :: jfisno
    end subroutine xfnohm
end interface 
