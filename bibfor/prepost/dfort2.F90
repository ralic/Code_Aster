subroutine dfort2(nsommx, icnc, noeu1, tbelzo, nbelt,&
                  tbnozo, nbnozo, nbnoe, xy, aire,&
                  energi, pe)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!********************************************************************
!                  BUT DE CETTE ROUTINE :                           *
!       CALCULER LE DEGRE DE LA SINGULARITE PE AU NOEUD NOEU1       *
! 1) CALCUL DE L ENERGIE MOYENNE ENER SUR UN CERCLE DE CENTRE NOEU1 *
!    ET POUR DIFFERENTS RAYONS RAYZ                                 *
! 2) COMPARAISON DE CETTE ENERGIE CALCULEE AVEC L ENERGIE THEORIQUE *
!    EN POINTE DE FISSURE ENER_THEO=K*(RAYZ**(2*(PE-1)))+C          *
!    ET CALCUL DU COEFFICENT PE PAR LA METHODE DES MOINDRES CARRES  *
!********************************************************************
!
! IN  NSOMMX               : NOMBRE DE SOMMETS MAX PAR EF
! IN  ICNC(NSOMMX+2,NELEM) : CONNECTIVITE EF => NOEUDS CONNECTES
!     1ERE VALEUR = NBRE DE NOEUDS SOMMETS CONNECTES A L EF N°X
!     2EME VALEUR = 1 SI EF UTILE 0 SINON
!     CONNECTIVITE  EF N
!     EN 2D EF UTILE = QUAD OU TRIA
!     EN 3D EF UTILE = TETRA OU HEXA
! IN  NDIM                 : DIMENSION DU PROBLEME
! IN  NOEU1                : NOEUD CONSIDERE
! IN  TBELZO(NBELT)        : EFS DES COUCHES 1, 2 ET 3
! IN  NBELT                : NBR D EFS DES COUCHES 1,2,3
! IN  TBNOZO(NBNOE)        : NOEUDS DES COUCHES 1, 2 ET 3
! IN  NBNOZO(3)            : NBR DE NOEUDS DES COUCHES 1, 2 ET 3
! IN  XY(3,NNOEM)          : COORDONNEES DES NOEUDS
! IN  AIRE(NELEM)          : SURFACE DE CHAQUE EF
! IN  ENERGI(NELEM)        : ENERGIE SUR CHAQUE EF
! OUT PE                   : DEGRE DE LA SINGULARITE
!
    implicit none
!
! DECLARATION GLOBALE
!
    include 'asterfort/assert.h'
    include 'asterfort/dcalph.h'
    include 'asterfort/dcqpri.h'
    include 'asterfort/dcspri.h'
    include 'asterfort/dinter.h'
    integer :: nsommx, icnc(nsommx+2, *), noeu1
    integer :: nbnozo(3), nbelt, nbnoe
    integer :: tbelzo(nbelt), tbnozo(nbnoe)
    real(kind=8) :: xy(3, *), aire(*), energi(*), pe
!
! DECLARATION LOCALE
!
    integer :: i, j, inno, iint, inel, nuef, noeu2, nedep, nefin
    integer :: nsomm, nbint
    integer :: ipoi1, ipoi2, ipoi4, nint, ip1
    parameter (nbint = 10)
    real(kind=8) :: coord(2), coor(2, 4), coorin(2, 2)
    real(kind=8) :: delta(3), dist, rayz(nbint), ray
    real(kind=8) :: airtot, sprim
    real(kind=8) :: ener(nbint)
!
! 1 - COORDONNEES DU NOEUD CONSIDERE INNO
!
    do 5 i = 1, 2
        coord(i)=xy(i,noeu1)
 5  end do
!
! 2 - CALCUL DES RAYONS DES COUCHES 1,2 ET 3
!
    nedep=0
    nefin=nbnozo(1)
    do 10 j = 1, 3
        delta(j) = 1.d+10
        do 20 inno = nedep+1, nefin
            noeu2=tbnozo(inno)
            if (noeu2 .ne. noeu1) then
                dist = sqrt( (coord(1)-xy(1,noeu2))**2 + (coord(2)-xy( 2,noeu2))**2)
                delta(j) = min(delta(j),dist)
            endif
20      continue
        if (j .ne. 3) then
            nedep=nefin
            nefin=nedep+nbnozo(j+1)
        endif
10  end do
!
! 3 - CALCUL DE L ENERGIE POUR DIFFERENTS RAYONS RAYZ
!     ENER=SOMME(ENERGI(EF)*SPRIM(EF)/AIRE(EF))/AIRTOT
!     LA SOMME S EFFECTUE SUR TOUS LES EFS DES COUCHES 1,2 ET 3
!     AIRTOT EST L AIRE DU CERCLE DE CENTRE NOEU1 ET DE RAYON RAYZ
!     AIRE(EF) EST LA SURFACE DE L EF CONSIDERE
!     SPRIM(EF) EST LA SURFACE DE L EF CONSIDERE INCLUE DANS LE CERCLE
!     CAS 1 : EF INCLU DANS LE CERCLE => SPRIM(EF)=AIRE(EF)
!     CAS 2 : EF EXCLU DU CERCLE      => SPRIM(EF)=0
!     CAS 3 : EF INCLUE EN PARTIE     => SPRIM(EF) A CALCULER
!
    do 30 iint = 1, nbint
!
        rayz(iint) = delta(1)+(delta(3)-delta(1))*(iint-1)/(nbint-1)
        ener(iint) = 0.d+0
        airtot = 0.d+0
!
! 3.1 - BOUCLE SUR LES EFS
!
        do 40 inel = 1, nbelt
!
            nuef = tbelzo(inel)
            nsomm=icnc(1,nuef)
!
! 3.1.1 - NOMBRE DE NOEUD NINTER DE L EF INEL INCLU DANS LE CERCLE
!
            nint = 0
            ipoi1 = 0
            do 50 inno = 1, nsomm
                coor(1,inno) = xy(1,icnc(inno+2,nuef))
                coor(2,inno) = xy(2,icnc(inno+2,nuef))
                ray = sqrt( (coord(1)-coor(1,inno))**2 +(coord(2)-coor( 2,inno))**2)
                if (ray .le. rayz(iint)) then
                    nint = nint + 1
                    if (ipoi1 .eq. 0) then
                        ipoi1 = inno
                    else
                        ipoi4 = inno
                    endif
                else
                    ipoi2 = inno
                endif
50          continue
!
! 3.1.2 - AIRE DE L INTERSECTION SPRIM SELON LES CAS
!         SI NINT=NSOMM SPRIM = AIRE(EF)
!         SI NINT=0     SPRIM = 0
!         SINON ON CORRIGE
!
            sprim = 0.d+0
            if (nint .eq. nsomm) then
                sprim = aire(nuef)
!
! SI 2 NOEUDS APPARTIENNENT AU CERCLE SPRIM=AIRE - TRIANGLE COUPE
! RQ :DINTER CALCULE LES COORDONNEES DES NOEUDS COUPANT LE CERCLE
!
            else if (nint.eq.(nsomm-1)) then
                ip1 = ipoi2 + 1
                if (ip1 .gt. nsomm) ip1 = ip1 - nsomm
                call dinter(coord, rayz(iint), coor(1, ipoi2), coor(1, ip1), coorin(1, 1))
                ip1 = ipoi2 - 1
                if (ip1 .le. 0) ip1 = ip1 + nsomm
                call dinter(coord, rayz(iint), coor(1, ipoi2), coor(1, ip1), coorin(1, 2))
!
                call dcspri(coor(1, ipoi2), coorin, sprim)
                sprim = aire(nuef) - sprim
!
! SI 1 NOEUD APPARTIENT AU CERCLE SPRIM=TRIANGLE COUPE
!
            else if (nint.eq.1) then
                ip1 = ipoi1 + 1
                if (ip1 .gt. 3) ip1 = ip1 - 3
                call dinter(coord, rayz(iint), coor(1, ipoi1), coor(1, ip1), coorin(1, 1))
                ip1 = ipoi1 + 2
                if (ip1 .gt. 3) ip1 = ip1 - 3
                call dinter(coord, rayz(iint), coor(1, ipoi1), coor(1, ip1), coorin(1, 2))
!
                call dcspri(coor(1, ipoi1), coorin, sprim)
!
! CAS PARTICULIER DES QUADRILATERES
!
            else if (nsomm.eq.4 .and. nint.eq.2) then
                if (ipoi1 .eq. 1 .and. ipoi4 .eq. 4) then
                    call dinter(coord, rayz(iint), coor(1, 1), coor(1, 2), coorin(1, 1))
                    call dinter(coord, rayz(iint), coor(1, 4), coor(1, 3), coorin(1, 2))
                    call dcqpri(coor(1, 1), coor(1, 4), coorin, sprim)
                else if (ipoi1.eq.3 .and. ipoi4.eq.4) then
                    call dinter(coord, rayz(iint), coor(1, 4), coor(1, 1), coorin(1, 1))
                    call dinter(coord, rayz(iint), coor(1, 3), coor(1, 2), coorin(1, 2))
                    call dcqpri(coor(1, 4), coor(1, 3), coorin, sprim)
                else if (ipoi1.eq.2 .and. ipoi4.eq.3) then
                    call dinter(coord, rayz(iint), coor(1, 3), coor(1, 4), coorin(1, 1))
                    call dinter(coord, rayz(iint), coor(1, 2), coor(1, 1), coorin(1, 2))
                    call dcqpri(coor(1, 3), coor(1, 2), coorin, sprim)
                else if (ipoi1.eq.1 .and. ipoi4.eq.2) then
                    call dinter(coord, rayz(iint), coor(1, 2), coor(1, 3), coorin(1, 1))
                    call dinter(coord, rayz(iint), coor(1, 1), coor(1, 4), coorin(1, 2))
                    call dcqpri(coor(1, 2), coor(1, 1), coorin, sprim)
                else
!             IPOI1 et IPOI4 non traité
                    call assert(.false.)
                endif
            endif
!
            call assert(sprim.ge.0.d0)
!
! 3.1.3 - CALCUL DE L ENERGIE
!
            ener(iint) = ener(iint) + energi(nuef) * sprim / aire( nuef)
            airtot = airtot + sprim
!
! 2 - FIN DE LA BOUCLE SUR TOUS LES EF
!
40      continue
!
        call assert(airtot.gt.0.d0.and.ener(iint).gt.0.d0)
        ener(iint) = ener(iint) / airtot
!
! LISSAGE DE LA COURBE ENERGI=F(RAYON) POUR IDENTIFIER PE
!
        if (iint .ge. 2) then
            ener(iint) = min(ener(iint),ener(iint-1))
        endif
!
! 3 - FIN DE LA BOUCLE SUR LE CALCUL DE L ENERGIE
!
30  end do
!
! 4 - CALCUL DU DEGRE DE LA SINGULARITE
!     PAR LA METHODE DES MOINDRES CARRES
!
    call dcalph(rayz, ener, nbint, pe)
!
end subroutine
