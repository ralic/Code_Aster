subroutine calajt(j1, j, diag, col, n,&
                  itab, deb, tab, suiv, lt,&
                  ier)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! person_in_charge: olivier.boiteau at edf.fr
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
    integer :: j1, j, n, diag(0:n), col(*), itab, deb(1:n)
    integer :: tab(*), suiv(*), lt, ier
    integer :: k, l, ok, oj, pred, it
!     ON AJOUTE LES NOEUDS COL(J1) A COL(J-1) DANS LA LISTE
!     DES VOISINS DE COL(J)
    oj = col(j)
    do 1 k = j1, j-1
        ok = col(k)
        do 2 l = diag(oj-1)+1, diag(oj)-1
            if (col(l) .eq. ok) goto 3
 2      continue
!     OK N' EST PAS UN VOISIN INITIAL DE OJ ON L INSERE DANS
!     LA LISTE DES VOISINS
        if (deb(oj) .eq. 0) then
            itab = itab + 1
            if (itab .gt. lt) then
                ier=1
                goto 22
            endif
            deb(oj) = itab
            tab(itab) = ok
            suiv(itab) = 0
        else
            it = deb(oj)
            pred =it
10          continue
            if (it .gt. 0) then
                if (tab(it) .eq. ok) goto 9
                pred = it
                it = suiv(it)
                goto 10
            endif
!     OK N'EST PAS DANS TAB
            itab = itab + 1
            if (itab .gt. lt) then
                ier=1
                goto 22
            endif
            tab(itab) = ok
            suiv(pred) = itab
            suiv(itab) = 0
 9          continue
        endif
 3      continue
 1  end do
    ier = -itab
22  continue
end subroutine
