subroutine fgordo(nbextr, ext, ord)
    implicit none
    include 'jeveux.h'
    real(kind=8) :: ext(*), ord(*)
    integer :: nbextr
!     ------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!     ----------------------------------------------------------------
!     RANGE LES EXTREMAS PAR AMPLITUDE DECROISSANTE
!     -----------------------------------------------------------------
! IN  NBEXTR : I   : NOMBRE  D'EXTREMUM DE LA FONCTION
! IN  EXT    : R   : VALEURS DES EXTREMA
! OUT ORD    : R   : VALEURS DES EXTREMA REORDONNES
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, j, k
!-----------------------------------------------------------------------
    if (ext(1) .lt. ext(2)) then
        ord(1)=ext(1)
        ord(2)=ext(2)
    else
        ord(1)=ext(2)
        ord(2)=ext(1)
    endif
!
    do 1 i = 3, nbextr
        if (ext(i) .lt. ord(1)) then
            do 31 k = i, 2, -1
                ord(k)=ord(k-1)
31          continue
            ord(1)=ext(i)
            goto 1
        endif
        if (ext(i) .ge. ord(i-1)) then
            ord(i)=ext(i)
            goto 1
        endif
        do 2 j = 1, i-2
            if ((ord(j).le.ext(i)) .and. (ext(i).lt.ord(j+1))) then
                do 3 k = i, j+2, -1
                    ord(k)=ord(k-1)
 3              continue
                ord(j+1)=ext(i)
                goto 1
            endif
 2      continue
 1  end do
!
end subroutine
