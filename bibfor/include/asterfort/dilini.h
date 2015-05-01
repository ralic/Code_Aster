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
    subroutine dilini(option, nomte, ivf, ivf2, idfde,&
                      idfde2, jgano, ndim, ipoids, ipoid2,&
                      icompo, npi, dimdef, nddls, nddlm,&
                      dimcon, typmod, dimuel, nno, nnom,&
                      nnos, regula, axi, interp)
        character(len=16) :: option
        character(len=16) :: nomte
        integer :: ivf
        integer :: ivf2
        integer :: idfde
        integer :: idfde2
        integer :: jgano
        integer :: ndim
        integer :: ipoids
        integer :: ipoid2
        integer :: icompo
        integer :: npi
        integer :: dimdef
        integer :: nddls
        integer :: nddlm
        integer :: dimcon
        character(len=8) :: typmod(2)
        integer :: dimuel
        integer :: nno
        integer :: nnom
        integer :: nnos
        integer :: regula(6)
        aster_logical :: axi
        character(len=2) :: interp
    end subroutine dilini
end interface
