subroutine ef0156(nomte)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ======================================================================
    implicit none
#include "jeveux.h"
#include "asterfort/jevech.h"
#include "asterfort/tecach.h"
#include "asterfort/assert.h"
#include "asterfort/ppgan2.h"
#include "asterfort/teattr.h"
#include "asterfort/elrefe_info.h"

    character(len=16) :: nomte
!-----------------------------------------------------------------------
! REALISE  EFGE_ELNO pour barres et membranes
! ----------------------------------------------------------------------
!
    integer :: itabin(2), itabou(2),iret,k,ibid,jgano, npg, nno
    character(len=8) :: alias8
    aster_logical :: lbarre
!------------------------------------------------------------------------
!
    call teattr('S', 'ALIAS8', alias8, ibid)
    lbarre=alias8(6:8).eq.'SE2'

    call tecach('OOO', 'PCONTRR', 'L', iret=iret, nval=2, itab=itabin)
    ASSERT(iret.eq.0)
    call tecach('OOO', 'PEFFORR', 'E', iret=iret, nval=2, itab=itabou)
    ASSERT(iret.eq.0)

    if (lbarre) then
        ASSERT(itabin(2).eq.1)
        ASSERT(itabou(2).eq.2)
        zr(itabou(1)-1+1)=zr(itabin(1)-1+1)
        zr(itabou(1)-1+2)=zr(itabin(1)-1+1)

    else
        call elrefe_info(fami='RIGI', nno=nno, npg=npg,jgano=jgano)
        ASSERT(3*nno.eq.itabou(2))
        ASSERT(3*npg.eq.itabin(2))

        do k=1,itabin(2)
            zr(itabou(1)-1+k)=zr(itabin(1)-1+k)
        enddo
        call ppgan2(jgano, 1, 3, zr(itabin(1)), zr(itabou(1)))
    endif
!
end subroutine
