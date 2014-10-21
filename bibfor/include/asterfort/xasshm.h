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
#include "asterf_types.h"
!
interface 
    subroutine xasshm(nno, npg, npi, ipoids, ivf,&
                      idfde, igeom, geom, crit, deplm,&
                      deplp, contm, contp, varim,&
                      varip, defgem, defgep, drds,&
                      drdsr, dsde, b, dfdi, dfdi2,&
                      r, sigbar, c, ck, cs,&
                      matuu, vectu, rinstm, rinstp, option,&
                      imate, mecani, press1, press2, tempe,&
                      dimdef, dimcon, dimuel, nbvari, nddls,&
                      nddlm, nmec, np1, ndim,&
                      compor, axi, modint, codret,&
                      nnop, nnops, nnopm, enrmec,&
                      dimenr, heavt, lonch, cnset, jpintt,&
                      jpmilt, jlsn, angmas,dimmat, enrhyd)
        integer :: dimmat
        integer :: dimenr
        integer :: nnops
        integer :: nnop
        integer :: ndim
        integer :: dimuel
        integer :: dimcon
        integer :: dimdef
        integer :: nno
        integer :: npg
        integer :: npi
        integer :: ipoids
        integer :: ivf
        integer :: idfde
        integer :: igeom
        real(kind=8) :: geom(ndim, nnop)
        real(kind=8) :: crit(*)
        real(kind=8) :: deplm(dimuel)
        real(kind=8) :: deplp(dimuel)
        real(kind=8) :: contm(*)
        real(kind=8) :: contp(*)
        real(kind=8) :: varim(*)
        real(kind=8) :: varip(*)
        real(kind=8) :: defgem(dimdef)
        real(kind=8) :: defgep(dimdef)
        real(kind=8) :: drds(dimenr, dimcon)
        real(kind=8) :: drdsr(dimenr, dimcon)
        real(kind=8) :: dsde(dimcon, dimenr)
        real(kind=8) :: b(dimenr, dimuel)
        real(kind=8) :: dfdi(nnop, ndim)
        real(kind=8) :: dfdi2(nnops, ndim)
        real(kind=8) :: r(dimenr)
        real(kind=8) :: sigbar(dimenr)
        real(kind=8) :: c(dimenr)
        real(kind=8) :: ck(dimenr)
        real(kind=8) :: cs(dimenr)
        real(kind=8) :: matuu(dimuel*dimuel)
        real(kind=8) :: vectu(dimuel)
        real(kind=8) :: rinstm
        real(kind=8) :: rinstp
        character(len=16) :: option
        integer :: imate
        integer :: mecani(5)
        integer :: press1(7)
        integer :: press2(7)
        integer :: tempe(5)
        integer :: nbvari
        integer :: nddls
        integer :: nddlm
        integer :: nmec
        integer :: np1
        character(len=16) :: compor(*)
        aster_logical :: axi
        character(len=3) :: modint
        integer :: codret
        integer :: nnopm
        integer :: enrmec(3)
        integer :: heavt(36)
        integer :: lonch(10)
        integer :: cnset(128)
        integer :: jpintt
        integer :: jpmilt
        integer :: jlsn
        real(kind=8) :: angmas(3)
        integer :: enrhyd(3)
    end subroutine xasshm
end interface 
