subroutine i3nloc(t1, t2, n1, n2, t3)
    implicit none
#include "asterf_types.h"
!
    integer :: t1(*), t2(*), t3(*)
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
!     NUMEROTATION LOCALE DE NOEUD DANS UNE FACE
!     ------------------------------------------------------------------
! IN  T1     : I : TABLE(1..N) D' ENTIERS
! IN  T2     : I : TABLE(1..N) D' ENTIERS
! IN  N1     : I : DIMENSION DE T1
! IN  N2     : I : DIMENSION DE T2
! OUT T3     : I : TABLE(1..N1) D' ENTIERS
!            :   : T3(I) = J  J>0 => T1(I) = T2(J)
!            :   :            J=0 => T1(I) NON DANS T2
!     ------------------------------------------------------------------
    integer :: i, j, i1
    aster_logical :: trouve
    integer :: n1, n2
!-----------------------------------------------------------------------
!
    do 100 i = 1, n1, 1
        i1 = t1(i)
        j = 1
        trouve = .false.
 10     continue
        if ((.not. trouve) .and. (j .le. n2)) then
            if (t2(j) .eq. i1) then
                trouve = .true.
            else
                j = j + 1
            endif
            goto 10
        endif
        if (.not. trouve) then
            j = 0
        endif
        t3(i) = j
100 end do
end subroutine
