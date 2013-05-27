subroutine gdmb(ne, kp, ajacob, en, enprim,&
                x0pg, b)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
!           CONTRIBUTION DU DEPLACEMENT DU NOEUD NE A LA MATRICE DE
!           DEFORMATION B AU POINT DE GAUSS KP.
!
!     IN  : NE        : NUMERO DU NOEUD
!           KP        : NUMERO DU POINT DE GAUSS
!           AJACOB    : JACOBIEN
!           EN        : FONCTIONS DE FORME
!           ENPRIM    : DERIVEES DES FONCTIONS DE FORME
!           X0PG      : DERIVEES DES COORDONNEES PAR RAP. A L'ABS. CURV.
!
!     OUT : B         : MATRICE DE DEFORMATION 6*6
! ------------------------------------------------------------------
    implicit none
    include 'asterfort/antisy.h'
    real(kind=8) :: en(3, 2), enprim(3, 2), x0pg(3), b(6, 6), amat(3, 3)
!
!-----------------------------------------------------------------------
    integer :: kp, l, m, ne
    real(kind=8) :: ajacob, form, formpr, un, unsurj, zero
!-----------------------------------------------------------------------
    zero = 0.d0
    un = 1.d0
    do 1 m = 1, 6
        do 1 l = 1, 6
            b(l,m) = zero
 1      continue
    unsurj = un / ajacob
    form = en(ne,kp)
    formpr = unsurj * enprim(ne,kp)
    do 2 l = 1, 6
        b(l,l) = formpr
 2  end do
    call antisy(x0pg, un, amat)
    do 5 m = 1, 3
        do 4 l = 1, 3
            b(l,m+3) = form * amat(l,m)
 4      end do
 5  end do
end subroutine
