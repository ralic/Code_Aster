subroutine i3trip(lstpt, nbpt)
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
    integer :: lstpt(*), nbpt
!
!     ------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ------------------------------------------------------------------
!     TRI DANS  OBJ DE TYPE LISTE_POINT
!     ------------------------------------------------------------------
! IN  LSTPT  : I : POINTEUR SUR LE TYPE LISTE_POINT
! IN  NBPT   : I : NOMBRE DE POINTS DANS OBJ POINTE
!     ------------------------------------------------------------------
!     STRUCT LISTE_POINT
!             ( REEL          ABSC      (NMAXPT)
!               ENTIER        FACE      (NMAXPT)
!               ENTIER        ARETE     (NMAXPT)
!              (PLANE,GAUCHE) TYPE_FACE (NMAXPT)
!               REEL          COORDO_REF(NMAXPT)
!               ENTIER        ORDRE     (NMAXPT)
!             );
!     STRUCT LISTE_POINT LSTPT;
!     ------------------------------------------------------------------
!     INVARIANT : ABSC(ORDRE(J)) < ABSC(ORDRE(J+1)) J := 2,I-1,1
!     ------------------------------------------------------------------
!
!
!
    integer :: i, j, k, aa, ao
    real(kind=8) :: aux
    aster_logical :: insert
!
!======================================================================
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    aa = lstpt(1)
    ao = lstpt(6)
    do 10 i = 1, nbpt, 1
        zi(ao + i-1) = i
 10 end do
    do 100 i = 1, nbpt, 1
        insert = .false.
        aux = zr(aa + i-1)
        j = 1
110     continue
        if ((j .lt. i) .and. (.not. insert)) then
            if (zr(aa + zi(ao + j-1)-1) .gt. aux) then
                insert = .true.
                do 120 k = i, j+1, -1
                    zi(ao + k-1) = zi(ao + k-2)
120             continue
                zi(ao + j-1) = i
            endif
            j = j + 1
            goto 110
        endif
        if (.not. insert) then
            zi(ao + i-1) = i
        endif
100 end do
end subroutine
