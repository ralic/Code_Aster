function dvolu5(numele, coord, norm, volt, coord1,&
                coord2)
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
! IN   NUMELE : NUMERO DE L ELEMENT
! IN   COORD  : COORDONNEES DES NOEUDS DU TETRA
!               ET DES INTERSECTIONS AVEC LE CYLINDRE
! IN   NORM   : POSITION DES NOEUDS PAR RAPPORT AU CYLINDRE
! IN   VOLT   : VOLUME DES ELEMENTS
! IN   COORD1 : COORDONNEE DE NOEU1
! IN   COORD2 : COORDONNEE DE NOEU2
! OUT  DVOLU5 : VOLUME DE L INTERSECTION
!
    implicit none
!
! DECLARATION GLOBALE
!
    integer :: numele, norm(2, 4)
    real(kind=8) :: coord(3, 12), volt(*), coord1(3), coord2(3), dvolu5
!
! DECLARATION LOCALE
!
    integer :: a, b, c, d, e, f, g, h, i, j, k, ex
    real(kind=8) :: vol3, vol4, xo1a, yo1a, zo1a, xo2a, yo2a, zo2a, do1a, do2a
    logical(kind=1) :: lnoeu
!
! 1 - RECHERCHE DES DEUX POINTS INTERNES
!     RQ : 2 POINTS DEDANS
!          4 INTERSECTIONS
!          2 POINTS HORS PLAN
!          3 OU 4 POINTS INFERIEURS A R
!
    a = 0
    do 10 k = 1, 4
        if (norm(1,k) .eq. 1 .and. a .gt. 0) b = k
        if (norm(1,k) .eq. 1 .and. a .eq. 0) a = k
10  end do
!
! 2 - RECHERCHE DU PREMIER POINT HORS PLAN
!
    ex = 0
    do 20 k = 1, 4
        if (norm(2,k) .ne. 0 .and. ex .eq. 0) ex = k
20  end do
!
! 3 - A EST-IL DU COTE DE 01 OU 02
!
    xo1a = coord(1,a) - coord1(1)
    yo1a = coord(2,a) - coord1(2)
    zo1a = coord(3,a) - coord1(3)
    xo2a = coord(1,a) - coord2(1)
    yo2a = coord(2,a) - coord2(2)
    zo2a = coord(3,a) - coord2(3)
    do1a =( xo1a**2 + yo1a**2 + zo1a**2 )
    do2a =( xo2a**2 + yo2a**2 + zo2a**2 )
    if (do1a .lt. do2a) then
        lnoeu = .true.
    else
        lnoeu = .true.
    endif
!
! 4 - DETERMINATION DANS LE TABLEAU DES POSITIONS DES INTERSECTIONS
!
    if (a .eq. 1 .and. b .eq. 2) then
        if (lnoeu .and. norm(2,ex) .eq. 1 .or. .not.lnoeu .and. norm(2, ex) .ne. 1) then
            c = 4
            d = 3
            e = 6
            f = 8
            g = 11
            h = 9
            i = 7
            j = 12
        else
            c = 3
            d = 4
            e = 7
            f = 9
            g = 12
            h = 8
            i = 6
            j = 11
        endif
    else if (a.eq.1.and.b.eq.3) then
        if (lnoeu .and. norm(2,ex) .eq. 1 .or. .not.lnoeu .and. norm(2, ex) .ne. 1) then
            c = 4
            d = 2
            e = 5
            f = 8
            g = 11
            h = 10
            i = 7
            j = 12
        else
            c = 2
            d = 4
            e = 7
            f = 10
            g = 12
            h = 8
            i = 5
            j = 11
        endif
    else if (a.eq.1.and.b.eq.4) then
        if (lnoeu .and. norm(2,ex) .eq. 1 .or. .not.lnoeu .and. norm(2, ex) .ne. 1) then
            c = 3
            d = 2
            e = 5
            f = 9
            g = 11
            h = 10
            i = 6
            j = 12
        else
            c = 2
            d = 3
            e = 6
            f = 10
            g = 12
            h = 9
            i = 5
            j = 11
        endif
    else if (a.eq.2.and.b.eq.3) then
        if (lnoeu .and. norm(2,ex) .eq. 1 .or. .not.lnoeu .and. norm(2, ex) .ne. 1) then
            c = 4
            d = 1
            e = 5
            f = 6
            g = 11
            h = 10
            i = 9
            j = 12
        else
            c = 1
            d = 4
            e = 9
            f = 10
            g = 12
            h = 6
            i = 5
            j = 11
        endif
    else if (a.eq.2.and.b.eq.4) then
        if (lnoeu .and. norm(2,ex) .eq. 1 .or. .not.lnoeu .and. norm(2, ex) .ne. 1) then
            c = 3
            d = 1
            e = 5
            f = 7
            g = 11
            h = 10
            i = 8
            j = 12
        else
            c = 1
            d = 3
            e = 8
            f = 10
            g = 12
            h = 7
            i = 5
            j = 11
        endif
    else if (a.eq.3.and.b.eq.4) then
        if (lnoeu .and. norm(2,ex) .eq. 1 .or. .not.lnoeu .and. norm(2, ex) .ne. 1) then
            c = 2
            d = 1
            e = 6
            f = 7
            g = 11
            h = 9
            i = 8
            j = 12
        else
            c = 1
            d = 2
            e = 8
            f = 9
            g = 12
            h = 7
            i = 6
            j = 11
        endif
    endif
!
! 5 - CALCUL DU VOLUME
!
    vol3 =(coord(1,e)-coord(1,d))*&
     &    ((coord(2,g)-coord(2,d))*(coord(3,f)-coord(3,d)) -&
     &      (coord(3,g)-coord(3,d))*(coord(2,f)-coord(2,d)) )
    vol3 = vol3 +(&
           coord(2,e)-coord(2,d))* ((coord(1,f)-coord(1,d))*(coord(3,g)-coord(3,d)) - (coord(3,f)&
           &-coord(3,d))*(coord(1,g)-coord(1,d))&
           )
    vol3 = vol3 +(&
           coord(3,e)-coord(3,d))* ((coord(1,g)-coord(1,d))*(coord(2,f)-coord(2,d)) - (coord(2,g)&
           &-coord(2,d))*(coord(1,f)-coord(1,d))&
           )
!
    if (abs(vol3) .lt. 1.0d-10) vol3 = 0.0d0
    if (vol3 .lt. 0.d0) then
        vol3 = - vol3
    endif
    vol3 = vol3 / 6.d0
!
    vol4 =(coord(1,i)-coord(1,c))*&
     &    ((coord(2,h)-coord(2,c))*(coord(3,j)-coord(3,c)) -&
     &      (coord(3,h)-coord(3,c))*(coord(2,j)-coord(2,c)) )
    vol4 = vol4 +(&
           coord(2,i)-coord(2,c))* ((coord(1,j)-coord(1,c))*(coord(3,h)-coord(3,c)) - (coord(3,j)&
           &-coord(3,c))*(coord(1,h)-coord(1,c))&
           )
    vol4 = vol4 +(&
           coord(3,i)-coord(3,c))* ((coord(1,h)-coord(1,c))*(coord(2,j)-coord(2,c)) - (coord(2,h)&
           &-coord(2,c))*(coord(1,j)-coord(1,c))&
           )
!
    if (abs(vol4) .lt. 1.0d-10) vol4 = 0.0d0
    if (vol4 .lt. 0.d0) then
        vol4 = - vol4
    endif
    vol4 = vol4 / 6.d0
!
    dvolu5 = volt(numele) - vol3 - vol4
    if (abs(dvolu5) .lt. 1.0d-10) dvolu5 = 0.0d0
!
    if (dvolu5 .lt. 0.0d0) then
        dvolu5 = - dvolu5
    endif
!
end function
