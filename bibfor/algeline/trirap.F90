subroutine trirap(clef, tab, ntab, n, g,&
                  d, m)
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
! ----------------------------------------------------------------------
!             TRI RAPIDE : CHOIX PIVOT ET EXPLORATION (CF TRI)
! ----------------------------------------------------------------------
! VARIABLES D'ENTREE / SORTIE
! INTEGER CLEF(N)         : VECTEUR CLEF
! INTEGER TAB(N,NTAB)     : TABLEAU A TRIER EN MEME TEMPS QUE CLEF
!                           (SI NTAB = 0, PAS PRIS EN COMPTE)
! VARIABLES D'ENTREE
! INTEGER NTAB            : NOMBRE DE COLONNES DE TAB
! INTEGER N               : NOMBRE DE LIGNES DE CLEF
! INTEGER G               : INDICE GAUCHE DE CLEF
! INTEGER D               : INDICE DROITE DE CLEF
!
! VARIABLE DE SORTIE
! INTEGER M               : INDICE DU PIVOT
! ----------------------------------------------------------------------
!
    implicit none
!
! --- VARIABLES
    integer :: n, ntab, g, d, m, clef(*), tab(n, *)
    integer :: pivot, gp, dp, i, tmp
!
! --- CHOIX DU PIVOT
!
    m = (d+g)/2
!
    if (clef(g) .lt. clef(m)) then
        if (clef(d) .lt. clef(m)) then
            if (clef(d) .lt. clef(g)) then
                m = g
            else
                m = d
            endif
        endif
    else
        if (clef(m) .lt. clef(d)) then
            if (clef(g) .lt. clef(d)) then
                m = g
            else
                m = d
            endif
        endif
    endif
!
    pivot = clef(m)
    clef(m) = clef(g)
    clef(g) = pivot
!
    do 10 i = 1, ntab
        tmp = tab(m,i)
        tab(m,i) = tab(g,i)
        tab(g,i) = tmp
10  end do
!
! --- EXPLORATION
!
    gp = g
    dp = d + 1
!
20  continue
!
    gp = gp + 1
    if (clef(gp) .lt. pivot) goto 20
!
30  continue
!
    dp = dp - 1
    if (clef(dp) .gt. pivot) goto 30
!
    if (gp .lt. dp) then
!
        tmp = clef(gp)
        clef(gp) = clef(dp)
        clef(dp) = tmp
!
        do 40 i = 1, ntab
            tmp = tab(gp,i)
            tab(gp,i) = tab(dp,i)
            tab(dp,i) = tmp
40      continue
!
        goto 20
!
    endif
!
! --- PLACEMENT DU PIVOT
!
    m = gp - 1
!
    clef(g) = clef(m)
    clef(m) = pivot
!
    do 50 i = 1, ntab
        tmp = tab(g,i)
        tab(g,i) = tab(m,i)
        tab(m,i) = tmp
50  end do
!
end subroutine
