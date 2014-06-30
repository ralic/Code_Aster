subroutine dzonfg(nsommx, icnc, nelcom, numeli, inno,&
                  tbelzo, nbelzo, tbnozo, nbnozo)
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
!*********************************************
!              BUT DE CETTE ROUTINE :        *
! RECHERCHER LES EFS ET LES NOEUDS COMPOSANT *
! LES COUCHES 1, 2 ET 3                      *
!*********************************************
!
! IN  NSOMMX                 : NOMBRE DE SOMMETS MAX PAR EF
! IN  ICNC(NSOMMX+2,NELEM)   : CONNECTIVITE EF => NOEUDS CONNECTES
!     1ERE VALEUR = NBRE DE NOEUDS SOMMETS CONNECTES A L EF N°X
!     2EME VALEUR = 1 SI EF UTILE 0 SINON
!     SUITE EF N°X=>N° DE NOEUDS SOMMETS CONNECTES A X
!     EN 2D EF UTILE = QUAD OU TRIA
!     EN 3D EF UTILE = TETRA OU HEXA
! IN  NELCOM                 : NOMBRE MAX D'EF PAR NOEUD
! IN  NUMELI(NELCOM+2,NNOEM) : CONNECTIVITE NOEUD X=>EF CONNECTES A X
!     1ERE VALEUR = NBRE D EFS UTILES CONNECTES AU NOEUD N°X
!     2EME VALEUR = 0 NOEUD MILIEU OU NON CONNECTE A UN EF UTILE
!                   1 NOEUD SOMMET A L INTERIEUR + LIE A UN EF UTILE
!                   2 NOEUD SOMMET BORD + LIE A UN EF UTILE
!     CONNECTIVITE  NOEUD N°X=>N° DES EF UTILE CONNECTES A X
! IN  INNO                   : NOEUD CONSIDER
! OUT TBELZO(SOMME(NBELZO))  : TOUS LES EFS COMPOSANTS LA ZONE
! OUT NBELZO(3)              : NOMBRE D EFS DES COUCHES 1 2 ET 3
!                              NBELZO(1)=NBR D EFS COUCHE 1
!                              NBELZO(2)=NBR D EFS COUCHES 1 ET 2
!                              NBELZO(3)=NBR D EFS COUCHES 1, 2 ET 3
! OUT TBNOZO(SOMME(NBNOZO))  : TOUS LES NOEUDS COMPOSANTS LA ZONE
! OUT NBNOZO(3)              : NOMBRE DE NOEUDS DES COUCHES 1 2 ET 3
!
    implicit none
!
! DECLARATION GLOBALE
!
#include "asterfort/assert.h"
    integer :: nsommx, icnc(nsommx+2, *), nelcom, numeli(nelcom+2, *)
    integer :: inno
    integer :: tbelzo(1000), nbelzo(3), tbnozo(1000), nbnozo(3)
!
! DECLARATION LOCALE
!
    integer :: j, inel, nuef, ind, noeud, iel, elem, n
    integer :: nedep, nefin, nbelco, nbnoco
    logical(kind=1) :: test
!
! 1 - EFS DES COUCHES 1, 2 ET 3
! 1.1 - COUCHE 1
!
    nbelzo(1) = numeli(1,inno)
    ASSERT(nbelzo(1).le.1000)
    do 10 inel = 1, nbelzo(1)
        tbelzo(inel) = numeli(inel+2,inno)
10  end do
!
! 1.2 - COUCHES 2 ET 3
!
    nedep = 1
    nefin = nbelzo(1)
!
    do 20 j = 1, 2
        nbelco = 0
        do 30 inel = nedep, nefin
            nuef = tbelzo(inel)
            do 40 ind = 1, icnc(1, nuef)
                noeud = icnc(ind+2,nuef)
                do 50 iel = 1, numeli(1, noeud)
                    elem = numeli(iel+2,noeud)
                    test = .true.
                    n = 1
60                  continue
                    if (n .le. (nefin+nbelco) .and. test) then
                        if (tbelzo(n) .eq. elem) test = .false.
                        n=n+1
                        goto 60
                    endif
                    if (test) then
                        nbelco = nbelco + 1
                        ASSERT((nefin+nbelco).le.1000)
                        tbelzo(nefin+nbelco) = elem
                    endif
50              continue
40          continue
30      continue
        if (j .eq. 1) then
            nbelzo(2) = nbelco
            nedep = nbelzo(1)+1
            nefin = nbelzo(1)+nbelzo(2)
        else
            nbelzo(3) = nbelco
        endif
20  end do
!
! 2 - NOEUDS DES COUCHES 1 2 ET 3
!
    nbnozo(1) = 1
    nbnozo(2) = 0
    nbnozo(3) = 0
    tbnozo(1) = inno
    nbnoco = 1
!
    nedep=0
    nefin=nbelzo(1)
!
    do 70 j = 1, 3
        do 80 inel = nedep+1, nefin
            nuef = tbelzo(inel)
            do 90 ind = 1, icnc(1, nuef)
                noeud = icnc(ind+2,nuef)
                n = 1
                test = .true.
100              continue
                if (n .le. nbnoco .and. test) then
                    if (tbnozo(n) .eq. noeud) test = .false.
                    n=n+1
                    goto 100
                endif
                if (test) then
                    nbnozo(j) = nbnozo(j) + 1
                    nbnoco = nbnoco + 1
                    ASSERT(nbnoco.le.1000)
                    tbnozo(nbnoco) = noeud
                endif
90          continue
80      continue
        if (j .ne. 3) then
            nedep=nefin
            nefin=nedep+nbelzo(j+1)
        endif
70  end do
!
    nbelzo(2)=nbelzo(1)+nbelzo(2)
    nbelzo(3)=nbelzo(2)+nbelzo(3)
end subroutine
