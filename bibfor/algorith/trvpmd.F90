subroutine trvpmd(np1, n, m, rr, loc,&
                  indxf, npoint, lpoint, tp, rtp)
!
!----------------------------------------------------------------------
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
!----------------------------------------------------------------------
    implicit none
!
! ARGUMENTS
! ---------
#include "asterfort/indexx.h"
#include "asterfort/vecini.h"
    integer :: np1, n, m
    integer :: indxf(*), npoint(*), lpoint(*)
    real(kind=8) :: rr(*), tp(*), rtp(*)
    logical(kind=1) :: loc(*)
!
! VARIABLES LOCALES
! -----------------
    integer :: ideb, ifin, ll, i, j, jmax, npdeb, npfin, lsom, nsaut
!
!**********************************************************************
!                DEBUT DU CODE EXECUTABLE
!**********************************************************************
!
! --- INITIALISATION
!
    ideb=1
    ifin=0
    ll=0
    j=0
    jmax=0
    nsaut = 0
    call vecini(np1, 0.d0, tp)
    call vecini(np1, 0.d0, rtp)
    do i = 1, np1
        npoint(i) = 0
        lpoint(i) = 0
    end do
!
! --- RECHERCHE DU POSITIONNEMENT DES MODES SELECTIONNES
!
    if (ideb .lt. n) then
!
  1     continue
        ideb = ideb + ll
!
        j = j + 1
!
        do i = ideb, n
            if (loc(i)) then
                ideb = i
                goto 3
            endif
        end do
!
  3     continue
!
        do i = ideb, n
            if (loc(i)) then
                ifin = n+1
            else
                ifin = i
                goto 5
            endif
        end do
!
  5     continue
!
        ll = ifin - ideb
        npoint(j) = ideb
        lpoint(j) = ll
!
        if (ll .eq. 0) then
            goto 6
        else
            goto 1
        endif
!
    endif
!
  6 continue
!
    if (j .eq. 1) then
        jmax = j
    else
        jmax = j-1
    endif
!
! --- VERIFICATION DU NOMBRE DE MODES SELECTIONNES
!
    lsom=0
!
    do j = 1, jmax
        lsom = lsom + lpoint(j)
    end do
!
    if (lsom .ne. m) then
!CCC COMMENTAIRE CDURAND :
! TRES CURIEUX, ON IMPRIME UN MESSAGE D ERREUR ... ET ON SORT
! COMME SI DE RIEN N ETAIT, SANS UTMESS, SANS ASSERT
        write(6,*) 'ERREUR SUR LE NOMBRE DE MODES SELECTIONNES'
        goto 999
    endif
!
! --- REMPLISSAGE DU TABLEAU RTP COMPORTANT UNIQUEMENT
!     LES MODES SELECTIONNES
!
!
    do j = 1, jmax
!
        if (j .gt. 1) then
            nsaut = nsaut + lpoint(j-1)
        endif
!
        npdeb = npoint(j)
        npfin = npoint(j) + lpoint(j) - 1
!
        do i = npdeb, npfin
            tp(i-npdeb+nsaut+1) = rr(i)
        end do
!
    end do
!
    do i = 1, m
        rtp(i) = tp(i)
    end do
!
! --- TRI DES MODES SELECTIONNES PAR VALEURS PROPRES CROISSANTES
!
    call indexx(m, rtp, indxf)
    do i = 1, m
        tp(i) = rtp(indxf(i))
    end do
!
    do i = 1, n
        rr(i) = 0.0d0
    end do
!
    do i = 1, m
        rr(i) = tp(i)
    end do
!
999 continue
end subroutine
