function dvolu4(coord, norm, coord1)
! ======================================================================
! COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
! IN   COORD1 : COORDONNEES DE O1
! OUT  DVOLU4 : VOLUME DE L INTERSECTION
!
    implicit none
#include "asterf_types.h"
!
! DECLARATION GLOBALE
!
    integer :: norm(2, 4)
    real(kind=8) :: coord(3, 12), coord1(3), dvolu4
!
! DECLARATION LOCALE
!
    integer :: i, j, k, l, e
    real(kind=8) :: xo1i, yo1i, zo1i, do1i
    aster_logical :: lnoeu
!
! 1 - RECHERCHE DES DEUX POINTS INTERNES
!     RQ : 2 POINTS DEDANS
!          2 INTERSECTIONS
!          2 POINTS HORS PLAN
!          2 POINTS INFERIEURS A R
!
    i = 0
    do 10 k = 1, 4
        if (norm(1,k) .eq. 1 .and. i .gt. 0) j = k
        if (norm(1,k) .eq. 1 .and. i .eq. 0) i = k
 10 end do
!
! 2 - RECHERCHE DU PREMIER POINT HORS PLAN
!
    e = 0
    do 20 k = 1, 4
        if (norm(2,k) .ne. 0 .and. e .eq. 0) e = k
 20 end do
!
! 3 - NOEU1 ET NOEU2 SONT CONFONDUS AVEC LES 2 SOMMETS I ET J
! RECHERCHE DE LA CORRESPONDANCE SI LNOEU ALORS I=NOEU1 SINON I=NOEU2
!
    xo1i = coord(1,i) - coord1(1)
    yo1i = coord(2,i) - coord1(2)
    zo1i = coord(3,i) - coord1(3)
    do1i =( xo1i**2 + yo1i**2 + zo1i**2 )
    if (do1i .le. 1.0d-6) then
        lnoeu = .true.
    else
        lnoeu = .true.
    endif
!
! 4 - ON DETERMINE DANS LE TABLEAU LES POSITIONS DES INTERSECTIONS
!
    if (i .eq. 1 .and. j .eq. 2) then
        if (lnoeu .and. norm(2,e) .eq. 1 .or. .not.lnoeu .and. norm(2, e) .ne. 1) then
            k = 8
            l = 7
        else
            k = 6
            l = 9
        endif
    else if (i.eq.1.and.j.eq.3) then
        if (lnoeu .and. norm(2,e) .eq. 1 .or. .not.lnoeu .and. norm(2, e) .ne. 1) then
            k = 7
            l = 8
        else
            k = 10
            l = 5
        endif
    else if (i.eq.1.and.j.eq.4) then
        if (lnoeu .and. norm(2,e) .eq. 1 .or. .not.lnoeu .and. norm(2, e) .ne. 1) then
            k = 9
            l = 6
        else
            k = 5
            l = 10
        endif
    else if (i.eq.2.and.j.eq.3) then
        if (lnoeu .and. norm(2,e) .eq. 1 .or. .not.lnoeu .and. norm(2, e) .ne. 1) then
            k = 6
            l = 9
        else
            k = 5
            l = 10
        endif
    else if (i.eq.2.and.j.eq.4) then
        if (lnoeu .and. norm(2,e) .eq. 1 .or. .not.lnoeu .and. norm(2, e) .ne. 1) then
            k = 8
            l = 7
        else
            k = 10
            l = 5
        endif
    else if (i.eq.3.and.j.eq.4) then
        if (lnoeu .and. norm(2,e) .eq. 1 .or. .not.lnoeu .and. norm(2, e) .ne. 1) then
            k = 7
            l = 8
        else
            k = 6
            l = 9
        endif
    endif
!
! 5 - CALCUL DU VOLUME
!
    dvolu4 =(coord(1,l)-coord(1,i))*&
     &    ((coord(2,j)-coord(2,i))*(coord(3,k)-coord(3,i)) -&
     &      (coord(3,j)-coord(3,i))*(coord(2,k)-coord(2,i)) )
    dvolu4 = dvolu4 +(&
             coord(2,l)-coord(2,i))* ((coord(1,k)-coord(1,i))*(coord(3,j)-coord(3,i)) - (coord(3,&
             &k)-coord(3,i))*(coord(1,j)-coord(1,i))&
             )
    dvolu4 = dvolu4 +(&
             coord(3,l)-coord(3,i))* ((coord(1,j)-coord(1,i))*(coord(2,k)-coord(2,i)) - (coord(2,&
             &j)-coord(2,i))*(coord(1,k)-coord(1,i))&
             )
!
    if (dvolu4 .lt. 0.d0) then
        dvolu4 = - dvolu4
    endif
    dvolu4 = dvolu4 / 6.d0
    if (abs(dvolu4) .lt. 1.0d-10) dvolu4 = 0.0d0
!
end function
