subroutine xfem_calc_diag(matass, nonu, neq, deeq, nbnomax, &
                           ino_xfem, is_xfem, nbnoxfem, ieq_loc,&
                           scal, deca, tab_mloc)
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
! BUT :
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   STOCKAGE DES MATRICES LOCALES DE PRE CONDITIONNMENT
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!-----------------------------------------------------------------------
!
! ARGUMENTS :
!------------
!
!-----------------------------------------------------------------------
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jexnum.h"
#include "asterfort/jeveuo.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!-----------------------------------------------------------------------
!
    character(len=19) :: matass
    character(len=14) :: nonu
    integer :: neq, nbnomax, nbnoxfem, deca
    integer :: deeq(*), ino_xfem(nbnomax)
    integer :: ieq_loc(neq)
    aster_logical :: is_xfem(nbnomax)
    real(kind=8) :: tab_mloc(deca*nbnoxfem), scal
!
!-----------------------------------------------------------------------
!
    integer :: jcoll, iligl, nunoj, nunoi, kterm, nnz, jvale, jval2, jsmhc, ipos, jpos, nvale
    integer, pointer :: smdi(:) => null()
    aster_logical :: lsym
    real(kind=8) :: valei, valej, rcoef
!
!-----------------------------------------------------------------------
!
    call jemarq()
!
    call jeveuo(nonu//'.SMOS.SMDI', 'L', vi=smdi)
    call jeveuo(nonu//'.SMOS.SMHC', 'L', jsmhc)
    call jeveuo(jexnum(matass//'.VALM', 1), 'L', jvale)
    call jelira(jexnum(matass//'.VALM', 1), 'LONMAX', nnz)
    call jelira(matass//'.VALM', 'NMAXOC', nvale)
    if (nvale .eq. 1) then
       lsym=.true.
    else if (nvale.eq.2) then
       lsym=.false.
       call jeveuo(jexnum(matass//'.VALM', 2), 'L', jval2)
    else
        ASSERT(.false.)
    endif
!
    tab_mloc(1:deca*nbnoxfem)=0.d0
!
    jcoll=1
    nunoj=deeq(2*(jcoll-1)+1)
    jpos=ieq_loc(jcoll)
    do 30 kterm=1,nnz
       if (smdi(jcoll) .lt. kterm) then
           jcoll=jcoll+1 
           nunoj=deeq(2*(jcoll-1)+1)
           jpos=ieq_loc(jcoll)
       endif
       iligl=zi4(jsmhc-1+kterm)
       nunoi=deeq(2*(iligl-1)+1)
       ipos=ieq_loc(iligl)    
       if (is_xfem(nunoi).and.ipos.gt.0) then
         tab_mloc(deca*(ino_xfem(nunoi)-1)+ipos)=max(&
                        tab_mloc(deca*(ino_xfem(nunoi)-1)+ipos),&
                        abs(zr(jvale-1+kterm)))
       endif
       if (iligl .eq. jcoll) goto 30
       if (is_xfem(nunoj).and.jpos.gt.0) then
         tab_mloc(deca*(ino_xfem(nunoj)-1)+jpos)=max(&
                        tab_mloc(deca*(ino_xfem(nunoj)-1)+jpos),&
                        abs(zr(jvale-1+kterm)))
       endif
       if (.not.lsym) then
         if (is_xfem(nunoi).and.ipos.gt.0) then
           tab_mloc(deca*(ino_xfem(nunoi)-1)+ipos)=max(&
                          tab_mloc(deca*(ino_xfem(nunoi)-1)+ipos),&
                          abs(zr(jval2-1+kterm)))
         endif
         if (is_xfem(nunoj).and.jpos.gt.0) then
           tab_mloc(deca*(ino_xfem(nunoj)-1)+jpos)=max(&
                          tab_mloc(deca*(ino_xfem(nunoj)-1)+jpos),&
                          abs(zr(jval2-1+kterm)))
         endif
       endif
!
30  enddo
!
    do 60 iligl=1,neq
      nunoi=deeq(2*(iligl-1)+1)
      if ( ieq_loc(iligl) .eq. 0 ) goto 60
      if ( .not. is_xfem(nunoi) ) goto 60
      rcoef=sqrt(tab_mloc(deca*(ino_xfem(nunoi)-1)+ieq_loc(iligl)))
      if(rcoef.le.0.d0) write(6,*)' ieq=',iligl
      ASSERT(rcoef.gt.0.d0)
      tab_mloc(deca*(ino_xfem(nunoi)-1)+ieq_loc(iligl))=scal/rcoef
60  enddo
!
    jcoll=1
    nunoj=deeq(2*(jcoll-1)+1)
    do kterm=1,nnz
       if (smdi(jcoll) .lt. kterm) then
           jcoll=jcoll+1
           nunoj=deeq(2*(jcoll-1)+1)
       endif
       if (ieq_loc(jcoll).ne.0) then
           valej=tab_mloc(deca*(ino_xfem(nunoj)-1)+ieq_loc(jcoll))
       else
           valej=1.d0
       endif
       iligl=zi4(jsmhc-1+kterm)
       nunoi=deeq(2*(iligl-1)+1)
       if (ieq_loc(iligl).ne.0) then
           valei=tab_mloc(deca*(ino_xfem(nunoi)-1)+ieq_loc(iligl))
       else
           valei=1.d0
       endif
       zr(jvale-1+kterm)=zr(jvale-1+kterm)*valei*valej
       if (.not. lsym) zr(jval2-1+kterm)=zr(jval2-1+kterm)*valei*valej
    enddo
!
    call jedema()
!
end subroutine
