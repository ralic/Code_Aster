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
    subroutine xinils(noma, maiaux, grille, ndim, meth,&
                      nfonf, nfong, geofis, a, b,&
                      r, noeud, cote, vect1, vect2,&
                      cnslt, cnsln)
        character(len=8) :: noma
        character(len=8) :: maiaux
        aster_logical :: grille
        integer :: ndim
        character(len=8) :: meth
        character(len=8) :: nfonf
        character(len=8) :: nfong
        character(len=16) :: geofis
        real(kind=8) :: a
        real(kind=8) :: b
        real(kind=8) :: r
        real(kind=8) :: noeud(3)
        character(len=8) :: cote
        real(kind=8) :: vect1(3)
        real(kind=8) :: vect2(3)
        character(len=19) :: cnslt
        character(len=19) :: cnsln
    end subroutine xinils
end interface
