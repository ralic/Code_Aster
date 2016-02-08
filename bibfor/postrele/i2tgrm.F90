subroutine i2tgrm(voisn1, voisn2, nbm, stchm, ptchm,&
                  nbchm)
    implicit none
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/i2fccl.h"
#include "asterfort/i2fspl.h"
#include "asterfort/i2gspl.h"
#include "asterfort/jecreo.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/assert.h"
    integer :: nbm, voisn1(*), voisn2(*)
    integer :: nbchm, stchm(*), ptchm(*)
!
    aster_logical :: simple, cycle
    integer :: i, apt, ast, mdpt, aplace
!-----------------------------------------------------------------------
    call jemarq()
    apt = 1
    ast = 1
!
    mdpt = 0
!
    aplace = 0
!
    simple = .true.
    cycle = .true.
!
    call jecreo('&INTPLACE', 'V V L')
    call jeecra('&INTPLACE', 'LONMAX', nbm)
    call jeveuo('&INTPLACE', 'E', aplace)
!
    do 10 i = 1, nbm, 1
!
        zl(aplace + i-1) = .false.
!
 10 end do
!
 20 continue
    if (simple) then
!
        call i2fspl(voisn2, zl(aplace), nbm, simple, mdpt)
!
        if (simple) then
!
            call i2gspl(mdpt, voisn1, voisn2, zl(aplace), stchm,&
                        ptchm, ast, apt)
!
        endif
!
        goto 20
!
    endif
!
    mdpt = 0
!
    if (cycle) then
        call i2fccl(zl(aplace), nbm, cycle, mdpt)
        ASSERT(.not.cycle)
    endif
!
    ptchm(apt) = ast
!
    nbchm = apt - 1
!
    call jedetr('&INTPLACE')
!
    call jedema()
end subroutine
