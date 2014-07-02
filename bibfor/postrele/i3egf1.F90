subroutine i3egf1(desc, desctm, conek1, conek2, im1,&
                  if1, iao1, iae1, im2, if2,&
                  iao2, legfa)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
    integer :: desc(*), desctm(*), conek1(*), conek2(*), im1, if1, iao1, iae1
    integer :: im2, if2, iao2
    aster_logical :: legfa
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
!
    integer :: decf1, adesc1, nbndf1, decf2, adesc2, nbndf2, nbno2
    integer :: i, numno1(15), numno2(15), i1, j1, i2, j2, nbno1
    integer :: l, nbnt
!     ------------------------------------------------------------------
    call jemarq()
!
    legfa = .false.
!
    decf1 = 8 + if1
    adesc1 = desctm(desc(im1))
    nbndf1 = zi(adesc1-1 + 2 + if1)
    decf2 = 8 + if2
    adesc2 = desctm(desc(im2))
    nbndf2 = zi(adesc2-1 + 2 + if2)
!
    if (iae1 .eq. 0) then
        do 10 i = 1, nbndf1, 1
            numno1(i) = conek1( zi(adesc1-1 + decf1 + (i-1)*6) )
 10     continue
        nbno1 = nbndf1
        nbnt = nbndf1
    else if (iae1 .eq. 1) then
        numno1(1) = conek1( zi(adesc1-1 + decf1 + (1-1)*6) )
        numno1(2) = conek1( zi(adesc1-1 + decf1 + (2-1)*6) )
        nbno1 = 2
        nbnt = 2
        if (iae1 .eq. iao1) nbnt = 1
    else if (iae1 .eq. 2) then
        numno1(1) = conek1( zi(adesc1-1 + decf1 + (2-1)*6) )
        numno1(2) = conek1( zi(adesc1-1 + decf1 + (3-1)*6) )
        nbno1 = 2
        nbnt = 2
        if (iae1 .eq. iao1) nbnt = 1
    else if (iae1 .eq. 3 .and. nbndf1 .eq. 3) then
        numno1(1) = conek1( zi(adesc1-1 + decf1 + (3-1)*6) )
        numno1(2) = conek1( zi(adesc1-1 + decf1 + (1-1)*6) )
        nbno1 = 2
        nbnt = 2
        if (iae1 .eq. iao1) nbnt = 1
    else if (iae1 .eq. 3 .and. nbndf1 .eq. 4) then
        numno1(1) = conek1( zi(adesc1-1 + decf1 + (3-1)*6) )
        numno1(2) = conek1( zi(adesc1-1 + decf1 + (4-1)*6) )
        nbno1 = 2
        nbnt = 2
        if (iae1 .eq. iao1) nbnt = 1
    else if (iae1 .eq. 4) then
        numno1(1) = conek1( zi(adesc1-1 + decf1 + (4-1)*6) )
        numno1(2) = conek1( zi(adesc1-1 + decf1 + (1-1)*6) )
        nbno1 = 2
        nbnt = 2
        if (iae1 .eq. iao1) nbnt = 1
    else
        goto 9999
    endif
!
    if (iao2 .eq. 0) then
        do 12 i = 1, nbndf1, 1
            numno2(i) = conek2( zi(adesc2-1 + decf2 + (i-1)*6) )
 12     continue
        nbno2 = nbndf1
    else if (iao2 .eq. 1) then
        numno2(1) = conek2( zi(adesc2-1 + decf2 + (1-1)*6) )
        numno2(2) = conek2( zi(adesc2-1 + decf2 + (2-1)*6) )
        nbno2 = 2
    else if (iao2 .eq. 2) then
        numno2(1) = conek2( zi(adesc2-1 + decf2 + (2-1)*6) )
        numno2(2) = conek2( zi(adesc2-1 + decf2 + (3-1)*6) )
        nbno2 = 2
    else if (iao2 .eq. 3 .and. nbndf2 .eq. 3) then
        numno2(1) = conek2( zi(adesc2-1 + decf2 + (3-1)*6) )
        numno2(2) = conek2( zi(adesc2-1 + decf2 + (1-1)*6) )
        nbno2 = 2
    else if (iao2 .eq. 3 .and. nbndf2 .eq. 4) then
        numno2(1) = conek2( zi(adesc2-1 + decf2 + (3-1)*6) )
        numno2(2) = conek2( zi(adesc2-1 + decf2 + (4-1)*6) )
        nbno2 = 2
    else if (iao2 .eq. 4) then
        numno2(1) = conek2( zi(adesc2-1 + decf2 + (4-1)*6) )
        numno2(2) = conek2( zi(adesc2-1 + decf2 + (1-1)*6) )
        nbno2 = 2
    else
        goto 9999
    endif
!
    l = 0
    do 20 i1 = 1, nbno1, 1
        j1 = numno1(i1)
        do 22 i2 = 1, nbno2, 1
            j2 = numno2(i2)
            if (j2 .eq. j1) l = l + 1
 22     continue
 20 end do
    if (l .eq. nbnt) legfa = .true.
!
9999 continue
    call jedema()
end subroutine
