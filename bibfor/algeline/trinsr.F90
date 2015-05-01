subroutine trinsr(clef, tab, ntab, n)
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
! A_UTIL
! ----------------------------------------------------------------------
!                            TRI PAR INSERTION
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
! --- VARIABLES
!
    integer :: n, ntab, clef(*)
    real(kind=8) :: tab(n, *)
    integer :: g, d, i, j, inser
    real(kind=8) :: tmpr
!
! --- TRI PAR INSERTION
!
    do 10 d = 2, n
!
        inser = clef(d)
        g = d
!
! ----- INSERTION DE INSER
!
20      continue
!
        g = g - 1
!
        if (g .gt. 0) then
            if (clef(g) .gt. inser) then
                clef(g+1) = clef(g)
                goto 20
            endif
        endif
!
        g = g + 1
!
        if (g .ne. d) then
!
            clef(g) = inser
!
! ------- DEPLACEMENT TABLEAU
!
            do 30 i = 1, ntab
                tmpr = tab(d,i)
                do 40 j = d-1, g, -1
                    tab(j+1,i) = tab(j,i)
40              continue
                tab(g,i) = tmpr
30          continue
!
        endif
!
10  end do
!
end subroutine
