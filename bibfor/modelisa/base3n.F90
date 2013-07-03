subroutine base3n(x1, mat33)
    implicit none
#include "asterfort/assert.h"
#include "asterfort/normev.h"
#include "asterfort/provec.h"
    real(kind=8) :: x1(3), mat33(3, 3)
    real(kind=8) :: norme
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: jacques.pellet at edf.fr
! ======================================================================
!     BUT : CALCULE UNE MATRICE ORTHORMEE MAT33 DONT LE 1ER VECTEUR
!           EST COLINEAIRE A X1
!     REMARQUE : IL EXISTE DE NOMBREUSES BASES AYANT CETTE PROPRIETE.
! ======================================================================
    real(kind=8) :: v1(3), v2(3), v3(3)
    integer :: k, k1
!
!
!     -- CALCUL DE V1 :
    do 10,k=1,3
    v1(k)=x1(k)
    10 end do
    call normev(v1, norme)
!
!
!     -- CALCUL DE V2 :
!     -- ON CHERCHE UNE COMPOSANTE (K1) PAS TROP PETITE DANS V1 :
    do 20,k=1,3
    if (abs(v1(k)) .ge. 0.5d0) k1=k
    20 end do
!
    if (k1 .eq. 1) then
        v2(2)=1.d0
        v2(3)=0.d0
        v2(1)=-v1(2)
    else if (k1.eq.2) then
        v2(3)=1.d0
        v2(1)=0.d0
        v2(2)=-v1(3)
    else if (k1.eq.3) then
        v2(1)=1.d0
        v2(2)=0.d0
        v2(3)=-v1(1)
    else
        call assert(.false.)
    endif
    call normev(v2, norme)
!
!
!     -- CALCUL DE V3 :
    call provec(v1, v2, v3)
    call normev(v3, norme)
!
!
!     -- RECOPIE DE V1, V2, V3 DANS MAT33 :
    do 30,k=1,3
    mat33(k,1)=v1(k)
    mat33(k,2)=v2(k)
    mat33(k,3)=v3(k)
    30 end do
!
!
end subroutine
