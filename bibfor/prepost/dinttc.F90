subroutine dinttc(coord1, coord2, xo1o2, yo1o2, zo1o2,&
                  do1o2, r, norm, nint, nhop,&
                  npir, coord, nbi)
!
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
!***************************************************************
!              BUT DE CETTE ROUTINE :                          *
! CALCULER LES INTERSECTIONS ENTRE UN TETRAEDRE ET UN CYLINDRE *
!***************************************************************
! IN COORD1         : COORDONNES DES NOEUDS EXTREMES DU CYLINDRE NOEU1
! IN COORD2         : COORDONNES DES NOEUDS EXTREMES DU CYLINDRE NOEU2
! IN *O1O2          : DISTANCE DES NOEUDS DU TETRA A L AXE DU CYLINDRE
! IN DO102          : DISTANCE DES NOEUDS DU TETRA A L AXE DU CYLINDRE
! IN R              : RAYON DE LA SPHERE
! IN NINT,NHOP,NPIR : PLACEMENT DES NOEUDS PAR RAPPORT AU CYLINDRE
! OUT COORD         : COORDONNEES DES NOEUDS DU TETRA/INTERSECTIONS
! OUT NBI           : NBR D INTERSECTION
!
    implicit none
!
! - DECLARATION GLOBALE
!
#include "asterf_types.h"
#include "asterfort/assert.h"
    real(kind=8) :: coord1(3), coord2(3), xo1o2, yo1o2, zo1o2, do1o2
    real(kind=8) :: r, coord(3, 12)
    integer :: norm(2, 4), nint, nhop, npir
    integer :: nbi
!
! - DECLARATION LOCALE
!
    integer :: i, j, k, l, ia, ib
    real(kind=8) :: xo1a, yo1a, zo1a, xo2a, yo2a, zo2a, xab, yab, zab
    real(kind=8) :: xxo1, yxo1, zxo1, xxo2, yxo2, zxo2
    real(kind=8) :: a, b, c, delta, lamda1, lamda2, lambda
    real(kind=8) :: dao1, dao2, dxo1, dxo2, pao1o2, pao2o1, pbo1o2
    real(kind=8) :: cos1, cos2, n2, n3, n4
    aster_logical :: lnoeu(2, 4), lintcy
!
    l = 4
    nbi = 0
!
! VERIFICATION SI UN DES NOEUDS DU TETRA EST CONFONDU
! AVEC NOEU1 OU NOEU2
!
    do 10 j = 1, 4
        lnoeu(1,j) = .false.
        lnoeu(2,j) = .false.
        xo1a = coord(1,j) - coord1(1)
        yo1a = coord(2,j) - coord1(2)
        zo1a = coord(3,j) - coord1(3)
        xo2a = coord(1,j) - coord2(1)
        yo2a = coord(2,j) - coord2(2)
        zo2a = coord(3,j) - coord2(3)
        dao1 = sqrt( xo1a**2 + yo1a**2 + zo1a**2 )
        dao2 = sqrt( xo2a**2 + yo2a**2 + zo2a**2 )
        if (abs(dao1) .le. 1.0d-6) lnoeu(1,j) = .true.
        if (abs(dao2) .le. 1.0d-6) lnoeu(2,j) = .true.
 10 end do
!
! RECHERCHE DES INTERSECTIONS DES ARETES DU TETRA AVEC LE CYLINDRE
    do 20 j = 1, 3
        do 30 k = j+1, 4
            l = l+1
! LES 2 NOEUDS SONT EXTERIEURS OU INTERIEURS
            if ((norm(1,j)*norm(1,k)) .gt. 0) then
                do 35 i = 1, 3
                    coord(i,l) = 1.0d30
 35             continue
            else
! UN POINT EST EXTERIEUR ET L AUTRE INTERIEUR
                nbi = nbi + 1
                lintcy = .true.
! ON PREND COMME REFERENCE LE POINT EXTERNE
                if (norm(1,j) .eq. 1) then
                    ia = k
                    ib = j
                else
                    ia = j
                    ib = k
                endif
! COORDONNES DES VECTEURS ET DISTANCES
                xo1a = coord(1,ia) - coord1(1)
                yo1a = coord(2,ia) - coord1(2)
                zo1a = coord(3,ia) - coord1(3)
                xo2a = coord(1,ia) - coord2(1)
                yo2a = coord(2,ia) - coord2(2)
                zo2a = coord(3,ia) - coord2(3)
                xab = coord(1,ib) - coord(1,ia)
                yab = coord(2,ib) - coord(2,ia)
                zab = coord(3,ib) - coord(3,ia)
                dao1 = sqrt( xo1a**2 + yo1a**2 + zo1a**2 )
                dao2 = sqrt( xo2a**2 + yo2a**2 + zo2a**2 )
                if (abs(dao1) .le. 1.0d-6) dao1 = 0.0d0
                if (abs(dao2) .le. 1.0d-6) dao2 = 0.0d0
                pao1o2 = xo1a*xo1o2 + yo1a*yo1o2 + zo1a*zo1o2
                pao2o1 = -( xo2a*xo1o2 + yo2a*yo1o2 + zo2a*zo1o2 )
                a =(yab*zo1o2-zab*yo1o2)**2 +(zab*xo1o2-xab*zo1o2)**2&
                + (xab*yo1o2-yab*xo1o2)**2
                b =( yab*zo1o2-zab*yo1o2 ) *( yo1a*zo1o2-zo1a*yo1o2 )&
                + ( zab*xo1o2-xab*zo1o2 ) *( zo1a*xo1o2-xo1a*zo1o2 ) +&
                ( xab*yo1o2-yab*xo1o2 ) *( xo1a*yo1o2-yo1a*xo1o2 )
                c =( yo1a*zo1o2-zo1a*yo1o2 )**2 + ( zo1a*xo1o2-xo1a*&
                zo1o2 )**2 + ( xo1a*yo1o2-yo1a*xo1o2 )**2 - r*r *&
                do1o2**2
                delta = b*b - a*c
                ASSERT(delta.ge.0.d0)
                if (a .eq. 0.0d0) then
! AB ET O1O2 SONT COLINEAIRES : INTERSECTION SUR LES FACES
                    lintcy = .false.
                else
! 2 POINTS D INTERSECTION
                    lamda1 =( - b - sqrt(delta) ) / a
                    if (abs(1.d0-lamda1) .le. 1.0d-6) then
                        lamda1 = 1.0d0
                    else if (abs(lamda1).le.1.0d-6) then
                        lamda1 = 0.0d0
                    endif
                    lamda2 =( - b + sqrt(delta) ) / a
                    if (abs(1.d0-lamda2) .le. 1.0d-6) then
                        lamda2 = 1.0d0
                    else if (abs(lamda2).le.1.0d-6) then
                        lamda2 = 0.0d0
                    endif
! CAS OU L INTERSECTION EST SUR LA PARTIE CYLINDRIQUE
                    if (lamda1 .ge. 0.d0 .and. lamda1 .le. 1.d0) then
                        coord(1,l) = lamda1*xab + coord(1,ia)
                        coord(2,l) = lamda1*yab + coord(2,ia)
                        coord(3,l) = lamda1*zab + coord(3,ia)
                        else if ( lamda2.ge.0.d0 .and. lamda2.le.1.d0 )&
                    then
                        coord(1,l) = lamda2*xab + coord(1,ia)
                        coord(2,l) = lamda2*yab + coord(2,ia)
                        coord(3,l) = lamda2*zab + coord(3,ia)
                    else
! L INTERSECTION COUPE UNE DES SURFACES EXTREMITES DU CYLINDRE
                        lintcy = .false.
                    endif
                endif
!
! VERIFICATION SI LA PROJECTION DU POINT D INTERSECTION SUR 0102
! SE TROUVE ENTRE 01 ET 02
!
                if (lintcy) then
                    xxo1 = coord(1,l)-coord1(1)
                    yxo1 = coord(2,l)-coord1(2)
                    zxo1 = coord(3,l)-coord1(3)
                    xxo2 = coord(1,l)-coord2(1)
                    yxo2 = coord(2,l)-coord2(2)
                    zxo2 = coord(3,l)-coord2(3)
                    dxo1 = sqrt( xxo1**2 + yxo1**2 + zxo1**2 )
                    dxo2 = sqrt( xxo2**2 + yxo2**2 + zxo2**2 )
                    cos1 = ( xxo1*xo1o2+yxo1*yo1o2+zxo1*zo1o2 )/( do1o2*dxo1)
                    cos2 = -( xxo2*xo1o2+yxo2*yo1o2+zxo2*zo1o2 )/( do1o2*dxo2)
                    n2 =( dxo1*cos1 ) / do1o2
                    n3 =( dxo2*cos2 ) / do1o2
                    n4 = n2 + n3
                    if (abs(1.d0-n2) .le. 1.0d-6) n2 = 1.0d0
                    if (abs(1.d0-n3) .le. 1.0d-6) n3 = 1.0d0
                    if (abs(1.d0-n4) .le. 1.0d-6) n4 = 1.0d0
                    ASSERT(n4.eq.1.0d0)
                    if (n2 .gt. 1.0d0 .or. n3 .gt. 1.0d0) then
                        lintcy = .false.
                    endif
! SUIVANT LES CAS, ON CHERCHE OU PAS L INTERSECTION SUR LES FACES
                    if (nint .eq. 1) then
                        if (lnoeu(1,ib) .and. n2 .lt. 0.0d0) then
                            lintcy = .true.
                            nbi = nbi-1
                        else if (lnoeu(2,ib) .and. n3.lt.0.0d0) then
                            lintcy = .true.
                            nbi = nbi-1
                        endif
                    else if (nint.eq.2 .and. nhop.le.1) then
                        if (lnoeu(1,ib) .and. n2 .lt. 0.0d0) then
                            lintcy = .true.
                            coord(1,l) = coord1(1)
                            coord(2,l) = coord1(2)
                            coord(3,l) = coord1(3)
                            nbi = nbi-1
                        else if (lnoeu(2,ib) .and. n3.lt.0.0d0) then
                            lintcy = .true.
                            coord(1,l) = coord2(1)
                            coord(2,l) = coord2(2)
                            coord(3,l) = coord2(3)
                            nbi = nbi-1
                        endif
                        else if (nint.eq.2 .and. nhop.eq.2 .and.&
                    npir.eq.2) then
                        if (lnoeu(1,ib) .and. n2 .lt. 0.0d0) then
                            lintcy = .true.
                            coord(1,l) = coord1(1)
                            coord(2,l) = coord1(2)
                            coord(3,l) = coord1(3)
                            nbi = nbi-1
                        else if (lnoeu(2,ib) .and. n3.lt.0.0d0) then
                            lintcy = .true.
                            coord(1,l) = coord2(1)
                            coord(2,l) = coord2(2)
                            coord(3,l) = coord2(3)
                            nbi = nbi-1
                        endif
                        else if (nint.eq.2 .and. nhop.eq.2 .and.&
                    npir.gt.2) then
                        if (lnoeu(1,ib) .and. n2 .lt. 0.0d0) then
                            lintcy = .true.
                            coord(1,l) = coord1(1)
                            coord(2,l) = coord1(2)
                            coord(3,l) = coord1(3)
                        else if (lnoeu(2,ib) .and. n3.lt.0.0d0) then
                            lintcy = .true.
                            coord(1,l) = coord2(1)
                            coord(2,l) = coord2(2)
                            coord(3,l) = coord2(3)
                        endif
                    endif
                endif
!
! CAS OU L INTERSECTION COUPE UNE DES SURFACES EXTREMITES DU CYLINDRE
! IL FAUT CHERCHER DE QUEL COTE DU CYLINDRE EST LE POINT A
!
                if (.not.lintcy) then
                    if (dao1 .gt. dao2 .and. dao2 .ne. 0.0d0) then
                        lambda = pao2o1 /(xab*xo1o2 + yab*yo1o2+ zab* zo1o2)
                    else if (dao2.gt.dao1 .and. dao1.ne.0.0d0) then
                        lambda = -pao1o2 /(xab*xo1o2 + yab*yo1o2 + zab*zo1o2)
                    else if (dao1.eq.0.0d0 .or. dao2.eq.0.0d0) then
                        lambda = 0.0d0
                    else
                        ASSERT(.false.)
                    endif
                    if (abs(1.d0-lambda) .le. 1.0d-10) then
                        lambda = 1.0d0
                    else if (abs(lambda).le.1.0d-10) then
                        lambda = 0.0d0
                    endif
                    ASSERT(lambda.ge.0.d0 .and. lambda.le.1.d0)
                    coord(1,l) = lambda*xab + coord(1,ia)
                    coord(2,l) = lambda*yab + coord(2,ia)
                    coord(3,l) = lambda*zab + coord(3,ia)
                endif
            endif
 30     continue
 20 end do
!
! SUIVANT LES CAS IL RESTE DES CALCULS A FAIRE
    if (nint .eq. 1 .and. nbi .ge. 1) then
        nbi = 3
!
! CALCUL LA PROJECTION DE L INTERSECTION SUR LA FACE
! RECHERCHE DU NUMERO DU POINT INTERNE
!
        do 40 i = 1, 4
            if (norm(1,i) .eq. 1) ib = i
 40     continue
        do 50 i = 1, 4
            if (norm(1,i) .eq. -1 .and. norm(2,i) .ne. 0) then
!
! DE QUEL COTE SE TROUVE LES POINTS EXTERNES 01 OU 02
                if (norm(2,i) .eq. 1 .and. lnoeu(1,ib)) then
                    xo1a = coord(1,i) - coord1(1)
                    yo1a = coord(2,i) - coord1(2)
                    zo1a = coord(3,i) - coord1(3)
                    lambda = -( xo1a*xo1o2 + yo1a*yo1o2 + zo1a*zo1o2 )
                    if (ib .eq. 1) then
                        l = 4+ib+i-2
                    else
                        l = 4+ib+i-1
                    endif
                    coord(1,l) = lambda*xo1o2/do1o2 + coord(1,i)
                    coord(2,l) = lambda*yo1o2/do1o2 + coord(2,i)
                    coord(3,l) = lambda*zo1o2/do1o2 + coord(3,i)
                else if (norm(2,i).eq.-1 .and. lnoeu(2,ib)) then
                    xo2a = coord(1,ia) - coord2(1)
                    yo2a = coord(2,ia) - coord2(2)
                    zo2a = coord(3,ia) - coord2(3)
                    lambda = -( xo2a*xo1o2 + yo2a*yo1o2 + zo2a*zo1o2 )
                    if (ib .eq. 1) then
                        l = 4+i+j-2
                    else
                        l = 4+i+j-1
                    endif
                    coord(1,l) = -lambda*xo1o2/do1o2 + coord(1,i)
                    coord(2,l) = -lambda*yo1o2/do1o2 + coord(2,i)
                    coord(3,l) = -lambda*zo1o2/do1o2 + coord(3,i)
                endif
            endif
 50     continue
    else if (nint.eq.2 .and. nhop.eq.2 .and. npir.ge.3) then
!
! SI LES 2 POINTS EXTERNES SONT DU MEME COTE ON NE FAIT RIEN
! SINON CALCUL DE L INTERSECTION POUR L ARETE AVEC 2 POINTS EXTERNES
! ON CHERCHE LES POINTS EXTERNES
!
        ia = 0
        do 60 i = 1, 4
            if (norm(1,i) .eq. -1 .and. ia .eq. 0) then
                ia = i
            else if (norm(1,i).eq.-1) then
                ib = i
            endif
 60     continue
! SI LES 2 POINTS NE SONT PAS DU MEME COTE
        if (norm(2,ia) .ne. norm(2,ib)) then
            xab = coord(1,ib) - coord(1,ia)
            yab = coord(2,ib) - coord(2,ia)
            zab = coord(3,ib) - coord(3,ia)
            xo1a = coord(1,ia) - coord1(1)
            yo1a = coord(2,ia) - coord1(2)
            zo1a = coord(3,ia) - coord1(3)
            xo2a = coord(1,ia) - coord2(1)
            yo2a = coord(2,ia) - coord2(2)
            zo2a = coord(3,ia) - coord2(3)
            pao1o2 = xo1a*xo1o2 + yo1a*yo1o2 + zo1a*zo1o2
            pbo1o2 = xab*xo1o2 + yab*yo1o2 + zab*zo1o2
            lamda1 = - pao1o2 / pbo1o2
            pao2o1 = xo2a*xo1o2 + yo2a*yo1o2 + zo2a*zo1o2
            lamda2 = - pao2o1 / pbo1o2
            coord(1,11) = lamda1*xab + coord(1,ia)
            coord(2,11) = lamda1*yab + coord(2,ia)
            coord(3,11) = lamda1*zab + coord(3,ia)
            coord(1,12) = lamda2*xab + coord(1,ia)
            coord(2,12) = lamda2*yab + coord(2,ia)
            coord(3,12) = lamda2*zab + coord(3,ia)
        endif
    endif
end subroutine
