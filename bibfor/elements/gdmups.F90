subroutine gdmups(ne, kp, ajacob, en, enprim,&
                  ups)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
! FONCTION: POUR UN ELEMENT DE POUTRE EN GRAND DEPLACEMENT, CALCULE LA
!           CONTRIBUTION DU NOEUD NE A LA MATRICE UPSILON AU POINT DE
!           GAUSS KP. CETTE MATRICE UPSILON INTERVIENT DANS LA RIGIDITE
!           GEOMETRIQUE COMME LA MATRICE DE DEFORMATION B INTERVIENT
!           DANS LA RIGIDITE MATERIELLE.
!
!     IN  : NE        : NUMERO DU NOEUD
!           KP        : NUMERO DU POINT DE GAUSS
!           AJACOB    : JACOBIEN
!           EN        : FONCTIONS DE FORME
!           ENPRIM    : DERIVEES DES FONCTIONS DE FORME
!
!     OUT : UPS       : MATRICE 9*6
! ------------------------------------------------------------------
    implicit none
    real(kind=8) :: en(3, 2), enprim(3, 2), ups(9, 6)
!
!-----------------------------------------------------------------------
    integer :: i, j, kp, ne
    real(kind=8) :: ajacob, form, formpr, un, unsurj, zero
!-----------------------------------------------------------------------
    zero = 0.d0
    un = 1.d0
    do 2 j = 1, 6
        do 1 i = 1, 9
            ups(i,j) = zero
 1      end do
 2  end do
    unsurj = un / ajacob
    form = en(ne,kp)
    formpr = unsurj * enprim(ne,kp)
    do 3 i = 1, 6
        ups(i,i) = formpr
 3  end do
    do 4 i = 1, 3
        ups(6+i,3+i) = form
 4  end do
end subroutine
