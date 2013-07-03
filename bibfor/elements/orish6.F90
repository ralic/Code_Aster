subroutine orish6(coor, ps)
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit   none
#include "asterfort/provec.h"
#include "blas/ddot.h"
    integer :: j
    real(kind=8) :: coor(18), ps
    real(kind=8) :: vec1(3), vec2(3), vec3(3), vect(3)
! ======================================================================
!
!    ORISHB6  --  VERIFIE SI LA NORMALE A LA PREMIERE FACE : 12 VECT 13
!                 EST RENTRANTE POUR UNE MAILLE PENTA6
!   ARGUMENT       E/S  TYPE         ROLE
!    COOR         IN    R         COORDONNEES DES 6 NOEUDS
!    PS           OUT   R         PRODUIT SCALAIRE : SI >0, NORMALE
!                                 RENTRANTE.
!
!.======================================================================
!
! VECTEURS 12 13 et 14
    do 10 j = 1, 3
        vec1(j) = coor(3+j) - coor(j)
        vec2(j) = coor(6+j) - coor(j)
        vect(j) = coor(9+j) - coor(j)
10  end do
! CALCUL DU PRODUIT VECTORIEL 12 X 13
    call provec(vec1, vec2, vec3)
!
!  VEC3= PRODUIT VECTORIEL 12 X 13 EST IL DANS LA DIRECTION DE 14
    ps=ddot(3,vec3,1,vect,1)
!
end subroutine
