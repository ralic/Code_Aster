subroutine uttrir(nbvale, vale, eps)
    implicit none
    integer :: nbvale
    real(kind=8) :: vale(*), eps
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
! VAR VALE   : R8 : TABLEAU A TRIER PAR ORDRE CROISSANT
! VAR NBVALE : IS : NOMBRE DE VALEUR A TRIER PAR ORDRE CROISSANT
!                 : (SORTIE) NOMBRE DE VALEURS DISTINCTES
! IN  EPS    : R8 : VALEUR DE SEPARATION ADMISE ENTRE DEUX VALEURS
!                 : (SI EPS < 0 ALORS ON GARDE LES VALEURS MULTIPLES)
!     ------------------------------------------------------------------
    integer :: incrs, is9
    real(kind=8) :: diff
!
!     --- RIEN A FAIRE SI NBVALE=0 OU 1 (ET NE PAS MODIFIER NBVALE)
!
!     --- TRI BULLE ---
!-----------------------------------------------------------------------
    integer :: i, j, l
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
                if (vale(l) .gt. vale(l+incrs)) then
!                 --- PERMUTATION ---
                    diff = vale(l)
                    vale(l) = vale(l+incrs)
                    vale(l+incrs) = diff
                    l = l - incrs
                    goto 130
                endif
            endif
150      continue
        incrs = incrs/3
        if (incrs .ge. 1) goto 120
!
!        --- SUPPRESSION DES VALEURS MULTIPLES ---
        if (eps .ge. 0.d0) then
            j=1
            do 301 i = 2, nbvale
                diff = vale(i)-vale(j)
                if (diff .gt. eps) then
                    j = j + 1
                    vale(j) = vale(i)
                endif
301          continue
            nbvale = j
        endif
    endif
!
end subroutine
