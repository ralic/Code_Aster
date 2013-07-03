subroutine tri(clef, tab, ntab, n)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
!
! ======================================================================
! A_UTIL
! ----------------------------------------------------------------------
!                     TRI RAPIDE (HOARE / SEDGEWICK)
! ----------------------------------------------------------------------
! VARIABLES D'ENTREE / SORTIE
! INTEGER CLEF(N)         : VECTEUR CLEF
! INTEGER TAB(N,NTAB)     : TABLEAU A TRIER EN MEME TEMPS QUE CLEF
!                           (SI NTAB = 0, PAS PRIS EN COMPTE)
!
! VARIABLES D'ENTREE
! INTEGER NTAB            : NOMBRE DE COLONNES DE TAB
! INTEGER N               : NOMBRE DE LIGNES A TRIER
! ----------------------------------------------------------------------
!
    implicit none
!
! --- PARAMETRES
#include "asterfort/assert.h"
#include "asterfort/triins.h"
#include "asterfort/trirap.h"
    integer :: blocmx, npile
    parameter (blocmx = 14)
    parameter (npile = 59)
!
! --- VARIABLES
    integer :: n, ntab, clef(*), tab(n, *)
    integer :: pile(npile+1), g, d, gs, ds, m, ipile
!
! --- INITIALISATION
!
    if (n .le. blocmx) goto 20
!
    g = 1
    d = n
    ipile = 1
!
10  continue
!
! --- DECOUPAGE
!
    call trirap(clef, tab, ntab, n, g,&
                d, m)
!
    if ((m-g) .gt. (d-m)) then
        gs = g
        ds = m - 1
        g = m + 1
    else
        gs = m + 1
        ds = d
        d = m - 1
    endif
!
    if ((d-g) .ge. blocmx) then
!
! ----- PUSH
!
        if ((ds-gs) .ge. blocmx) then
!
            if (ipile .le. npile) then
!
                pile(ipile) = gs
                ipile = ipile + 1
                pile(ipile) = ds
                ipile = ipile + 1
!
            else
!
                call assert(.false.)
!
            endif
!
        endif
!
        goto 10
!
    else
!
! ----- POP
!
        if (ipile .gt. 2) then
!
            ipile = ipile - 1
            d = pile(ipile)
            ipile = ipile - 1
            g = pile(ipile)
            goto 10
!
        endif
!
    endif
!
! --- TRI PAR INSERTION
!
20  continue
!
    call triins(clef, tab, ntab, n)
!
end subroutine
