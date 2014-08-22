subroutine xfem_calc_mloc(matass, nonu, neq, deeq, nbnomax, &
                           ino_xfem, is_xfem, nbnoxfem, ieq_loc, neq_mloc,&
                           nnz_mloc, deca, tab_mloc)
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
!-----------------------------------------------------------------------
!
    character(len=19) :: matass
    character(len=14) :: nonu
    integer :: neq, nbnomax, nbnoxfem, deca
    integer :: deeq(*), ino_xfem(nbnomax), nnz_mloc(nbnoxfem)
    integer :: ieq_loc(neq), neq_mloc(nbnoxfem)
    aster_logical :: is_xfem(nbnomax)
    real(kind=8) :: tab_mloc(deca*nbnoxfem)
!
!-----------------------------------------------------------------------
!
    integer :: jcoll, iligl, nunoj, nunoi, kterm, nnz, jvale, jval2, jsmhc, ipos, jpos, nm, nvale
    integer, pointer :: smdi(:) => null()
    aster_logical :: lsym
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
    jcoll=1
    nunoj=deeq(2*(jcoll-1)+1)
    do 30 kterm=1,nnz
       if (smdi(jcoll) .lt. kterm) then
           jcoll=jcoll+1 
           nunoj=deeq(2*(jcoll-1)+1)
!           if (lmd) jcoll=zi(jnlogl+jcoll-1)
       endif
       iligl=zi4(jsmhc-1+kterm)
!       if (lmd) iligl=zi(jnlogl+iligl-1)
       nunoi=deeq(2*(iligl-1)+1)
!   IMPRESSION MATRICE
!       write(33,*) iligl,jcoll,zr(jvale-1+kterm)
!       if (iligl .ne. jcoll) write(33,*) jcoll,iligl,zr(jvale-1+kterm)
!
       if ( ieq_loc(jcoll) .eq. 0 ) goto 30
       if ( nunoj .ne. nunoi ) goto 30
       if ( .not. is_xfem(nunoj) ) goto 30
       jpos=ieq_loc(jcoll)
       ipos=ieq_loc(iligl)
       if ((ipos .eq. 0) .or. (jpos .eq. 0)) goto 30
       nnz_mloc(ino_xfem(nunoj))=nnz_mloc(ino_xfem(nunoj))+1
       nm=neq_mloc(ino_xfem(nunoj))
       tab_mloc(deca*(ino_xfem(nunoj)-1)+ nm*(ipos-1)+jpos)=zr(jvale-1+kterm)
!   REMPLISSAGE DE LA PARTIE INFERIEURE (PAS FORCEMENT NECESAIRE / PAR SYMETRIE)
       if (lsym) then
           if ( ipos .ne. jpos) &
             tab_mloc(deca*(ino_xfem(nunoj)-1)+ nm*(jpos-1)+ipos)=zr(jvale-1+kterm)
       else
           if ( ipos .ne. jpos) &
             tab_mloc(deca*(ino_xfem(nunoj)-1)+ nm*(jpos-1)+ipos)=zr(jval2-1+kterm)
       endif
!
30  enddo
!
    call jedema()
!
end subroutine
