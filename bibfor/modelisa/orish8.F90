subroutine orish8(coor, ps)
    implicit   none
#include "asterfort/provec.h"
#include "blas/ddot.h"
    integer :: j
    real(kind=8) :: coor(24), ps
    real(kind=8) :: vec1(3), vec2(3), vec3(3), vect(3)
!
!.======================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     ORISH1  --  VERIFIE SI LA NORMALE A LA PREMIER FACE : 12 VECT 13
!                 EST RENTRANTE POUR UNE MAILLE HEXA8
!   ARGUMENT       E/S  TYPE         ROLE
!    COOR         IN    R         COORDONNEES DES 8 NOEUDS
!    PS           OUT   R         PRODUIT SCALAIRE : SI >0, NORMALE
!                                 RENTRANTE.
!
!.======================================================================
!
! VECTEURS 12 14 et 15
    do 100 j = 1, 3
        vec1(j) = coor(3+j) - coor(j)
        vec2(j) = coor(9+j) - coor(j)
        vect(j) = coor(12+j) - coor(j)
100  end do
! CALCUL DU PRODUIT VECTORIEL 12 X 14
    call provec(vec1, vec2, vec3)
!
!  VEC3= PRODUIT VECTORIEL 12 X 14 EST IL DANS LA DIRECTION DE 15
    ps=ddot(3,vec3,1,vect,1)
!
end subroutine
