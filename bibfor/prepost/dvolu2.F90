function dvolu2(coord, norm)
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
!**********************************************************
!              BUT DE CETTE ROUTINE :                     *
! CALCULER LE VOLUME DE L INTERSECTION CYLINDRE-TETRAEDRE *
!**********************************************************
!
! IN   COORD  : COORDONNEES DES NOEUDS DU TETRA
!               ET DES INTERSECTIONS AVEC LE CYLINDRE
! IN   NORM   : POSITION DES NOEUDS PAR RAPPORT AU CYLINDRE
! OUT  DVOLU2 : VOLUME DE L INTERSECTION
!
    implicit none
!
! DECLARATION GLOBALE
!
    integer :: norm(2, 4)
    real(kind=8) :: coord(3, 12), dvolu2
!
! DECLARATION LOCALE
!
    integer :: i, j, k, l, m, n
    real(kind=8) :: vol3, vol4, vol5
!
! 1 - RECHERCHE DES DEUX POINTS INTERNES
!     RQ : 2 POINTS DEDANS ET 4 INTERSECTIONS
!
    i = 0
    do 10 k = 1, 4
        if (norm(1,k) .eq. 1 .and. i .gt. 0) j = k
        if (norm(1,k) .eq. 1 .and. i .eq. 0) i = k
10  end do
!
! 2 - TABLEAU DES INTERSECTIONS
!
    if (i .eq. 1 .and. j .eq. 2) then
        k = 6
        l = 7
        m = 8
        n = 9
    else if (i.eq.1.and.j.eq.3) then
        k = 7
        l = 5
        m = 10
        n = 8
    else if (i.eq.1.and.j.eq.4) then
        k = 5
        l = 6
        m = 9
        n = 10
    else if (i.eq.2.and.j.eq.3) then
        k = 5
        l = 9
        m = 6
        n = 10
    else if (i.eq.2.and.j.eq.4) then
        k = 8
        l = 5
        m = 10
        n = 7
    else if (i.eq.3.and.j.eq.4) then
        k = 6
        l = 8
        m = 7
        n = 9
    endif
!
! 3 - CALCUL DU VOLUME
!
    vol3 =(coord(1,l)-coord(1,i))*&
     &    ((coord(2,j)-coord(2,i))*(coord(3,k)-coord(3,i)) -&
     &      (coord(3,j)-coord(3,i))*(coord(2,k)-coord(2,i)) )
    vol3 = vol3 +(&
           coord(2,l)-coord(2,i))* ((coord(1,k)-coord(1,i))*(coord(3,j)-coord(3,i)) - (coord(3,k)&
           &-coord(3,i))*(coord(1,j)-coord(1,i))&
           )
    vol3 = vol3 +(&
           coord(3,l)-coord(3,i))* ((coord(1,j)-coord(1,i))*(coord(2,k)-coord(2,i)) - (coord(2,j)&
           &-coord(2,i))*(coord(1,k)-coord(1,i))&
           )
!
    if (abs(vol3) .lt. 1.0d-10) vol3 = 0.0d0
    if (vol3 .lt. 0.d0) vol3 = - vol3
!
    vol4 =(coord(1,l)-coord(1,j))*&
     &    ((coord(2,m)-coord(2,j))*(coord(3,k)-coord(3,j)) -&
     &      (coord(3,m)-coord(3,j))*(coord(2,k)-coord(2,j)) )
    vol4 = vol4 +(&
           coord(2,l)-coord(2,j))* ((coord(1,k)-coord(1,j))*(coord(3,m)-coord(3,j)) - (coord(3,k)&
           &-coord(3,j))*(coord(1,m)-coord(1,j))&
           )
    vol4 = vol4 +(&
           coord(3,l)-coord(3,j))* ((coord(1,m)-coord(1,j))*(coord(2,k)-coord(2,j)) - (coord(2,m)&
           &-coord(2,j))*(coord(1,k)-coord(1,j))&
           )
!
    if (abs(vol4) .lt. 1.0d-10) vol4 = 0.0d0
    if (vol4 .lt. 0.d0) vol4 = - vol4
!
    vol5 =(coord(1,l)-coord(1,j))*&
     &    ((coord(2,n)-coord(2,j))*(coord(3,m)-coord(3,j)) -&
     &      (coord(3,n)-coord(3,j))*(coord(2,m)-coord(2,j)) )
    vol5 = vol5 +(&
           coord(2,l)-coord(2,j))* ((coord(1,m)-coord(1,j))*(coord(3,n)-coord(3,j)) - (coord(3,m)&
           &-coord(3,j))*(coord(1,n)-coord(1,j))&
           )
    vol5 = vol5 +(&
           coord(3,l)-coord(3,j))* ((coord(1,n)-coord(1,j))*(coord(2,m)-coord(2,j)) - (coord(2,n)&
           &-coord(2,j))*(coord(1,m)-coord(1,j))&
           )
!
    if (abs(vol5) .lt. 1.0d-6) vol5 = 0.0d0
    if (vol5 .lt. 0.d0) then
        vol5 = - vol5
    endif
    dvolu2 = vol3 + vol4 + vol5
    dvolu2 = dvolu2 / 6.d0
!
end function
