!
! COPYRIGHT (C) 1991 - 2017  EDF R&D                WWW.CODE-ASTER.ORG
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
    subroutine caethm(axi, perman, vf, typvf,&
                      typmod, modint, mecani, press1, press2,&
                      tempe, dimdep, dimdef, dimcon, nmec,&
                      np1, np2, ndim, nno, nnos,&
                      nnom, nface, npi, npg, nddls,&
                      nddlm, nddlfa, nddlk, dimuel, ipoids,&
                      ivf, idfde, ipoid2, ivf2, idfde2,&
                      npi2, jgano)
        aster_logical, intent(in) :: axi
        aster_logical, intent(in) :: perman
        aster_logical, intent(in) :: vf
        integer, intent(in) :: ndim
        integer, intent(out) :: mecani(5)
        integer, intent(out) :: press1(7)
        integer, intent(out) :: press2(7)
        integer, intent(out) :: tempe(5)
        integer :: typvf
        character(len=8) :: typmod(2)
        character(len=3) :: modint
        integer, intent(out) :: dimdep
        integer, intent(out) :: dimdef
        integer, intent(out) :: dimcon
        integer, intent(out) :: nmec
        integer, intent(out) :: np1
        integer, intent(out) :: np2
        integer :: nno
        integer :: nnos
        integer :: nnom
        integer :: nface
        integer :: npi
        integer :: npg
        integer :: nddls
        integer :: nddlm
        integer :: nddlfa
        integer :: nddlk
        integer :: dimuel
        integer :: ipoids
        integer :: ivf
        integer :: idfde
        integer :: ipoid2
        integer :: ivf2
        integer :: idfde2
        integer :: npi2
        integer :: jgano
    end subroutine caethm
end interface
