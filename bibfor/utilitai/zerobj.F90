function zerobj(obj)
    implicit none
    logical :: zerobj
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/jaexin.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
    character(len=*) :: obj
! ----------------------------------------------------------------------
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
! person_in_charge: jacques.pellet at edf.fr
! ----------------------------------------------------------------------
!  BUT : DETERMINER SI UN OBJET JEVEUX EST NUL (OU PAS)
!       OBJ     : NOM DE L'OBJET JEVEUX à TESTER
!
!     RESULTAT:
!       ZEROBJ : .TRUE.    SI LES VALEURS DE OBJ SONT TOUTES NULLES
!                .FALSE.   SINON
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
    character(len=1) :: typsca, xous, genr
    character(len=24) :: obj2
    integer :: i, j, n, jval, long, iret, iexi
!
! -DEB------------------------------------------------------------------
!
    call jemarq()
    zerobj=.true.
    obj2=obj
!
    call jelira(obj2, 'TYPE', cval=typsca)
    ASSERT(typsca.eq.'R' .or. typsca.eq.'C')
    call jelira(obj2, 'XOUS', cval=xous)
    call jelira(obj2, 'XOUS', cval=xous)
    call jelira(obj2, 'GENR', cval=genr)
    ASSERT(genr.eq.'V')
!
!
!     1) CAS DES OBJETS SIMPLES :
!     --------------------------------
    if (xous .eq. 'S') then
        call jeveuo(obj2, 'L', jval)
        call jelira(obj2, 'LONMAX', long)
!
        if (typsca .eq. 'R') then
            do 2, j=1,long
            if (zr(jval-1+j) .ne. 0.d0) goto 9998
 2          continue
        else
            do 3, j=1,long
            if (zc(jval-1+j) .ne. (0.d0,0.d0)) goto 9998
 3          continue
        endif
    endif
!
!
!     2) CAS DES COLLECTIONS :
!     --------------------------------
    if (xous .eq. 'X') then
        call jelira(obj2, 'NMAXOC', n)
!
        do 10,i=1,n
        call jeexin(jexnum(obj2, i), iret)
        if (iret .eq. 0) goto 10
!         -- SI UN OBJET N'A PAS D'ADRESSE DISQUE, C'EST QU'IL EST NUL :
!            (CELA PEUT ARRIVER SI PARALLELISME='GROUP_ELEM')
        call jaexin(jexnum(obj2, i), iexi)
        if (iexi .eq. 0) goto 10
        call jeveuo(jexnum(obj2, i), 'L', jval)
        call jelira(jexnum(obj2, i), 'LONMAX', long)
!
        if (typsca .eq. 'R') then
            do 20, j=1,long
            if (zr(jval-1+j) .ne. 0.d0) goto 9998
20          continue
        else
            do 30, j=1,long
            if (zc(jval-1+j) .ne. (0.d0,0.d0)) goto 9998
30          continue
        endif
10      continue
    endif
!
!
!
    goto 9999
9998  continue
    zerobj=.false.
!
!
9999  continue
    call jedema()
end function
