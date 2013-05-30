subroutine uttrii(ivale, nbvale)
    implicit none
    integer :: ivale(*)
!     ------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     TRIE PAR ORDRE CROISSANT, (METHODE DE REMONTEE DES BULLES)
!     ET SUPPRIME LES VALEURS MULTIPLES .
!     ------------------------------------------------------------------
! VAR IVALE  : R8 : TABLEAU A TRIER PAR ORDRE CROISSANT
! VAR NBVALE : IS : NOMBRE DE VALEUR A TRIER PAR ORDRE CROISSANT
!                 : (SORTIE) NOMBRE DE VALEURS DISTINCTES
!     ------------------------------------------------------------------
    integer :: incrs, is9, diff
!
!     --- RIEN A FAIRE SI NBVALE=0 OU 1 (ET NE PAS MODIFIER NBVALE)
!
!     --- TRI BULLE ---
!-----------------------------------------------------------------------
    integer :: i, j, l, nbvale
!-----------------------------------------------------------------------
    if (nbvale .gt. 1) then
!        --- CHOIX DE L'INCREMENT ---
        incrs = 1
        is9 = nbvale / 9
10      continue
        if (incrs .lt. is9) then
            incrs = 3*incrs+1
            goto 10
        endif
!
!        --- REMONTEE DES BULLES ---
120      continue
        do 150 j = incrs+1, nbvale
            l = j-incrs
130          continue
            if (l .gt. 0) then
                if (ivale(l) .gt. ivale(l+incrs)) then
!                 --- PERMUTATION ---
                    diff = ivale(l)
                    ivale(l) = ivale(l+incrs)
                    ivale(l+incrs) = diff
                    l = l - incrs
                    goto 130
                endif
            endif
150      continue
        incrs = incrs/3
        if (incrs .ge. 1) goto 120
!
!        --- SUPPRESSION DES VALEURS MULTIPLES ---
        j=1
        do 301 i = 2, nbvale
            if (ivale(i) .ne. ivale(j)) then
                j = j + 1
                ivale(j) = ivale(i)
            endif
301      continue
        nbvale = j
    endif
!
end subroutine
