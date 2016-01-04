subroutine excart(imodat, iparg)

use calcul_module, only : ca_iachii_, ca_iachlo_, ca_iamloc_, ca_iawlo2_,&
    ca_iel_, ca_igr_, ca_iichin_, ca_ilchlo_,&
    ca_ilmloc_, ca_nbelgr_, ca_nbgr_, ca_typegd_

implicit none

! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.

! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.

! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr

#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/excar2.h"
#include "asterfort/jacopo.h"
#include "asterfort/jeexin.h"
#include "asterfort/jeveuo.h"

    integer :: imodat, iparg
!----------------------------------------------------------------------
!     entrees:
!       imodat : indice dans la collection modeloc
!----------------------------------------------------------------------
    integer :: desc, modloc, dec1, dec2, lgcata, iret
    integer :: ipt, ityplo, jparal
    integer :: nbpoin, ncmp, ngrmx, debugr
    aster_logical :: lparal
!-------------------------------------------------------------------

!   parallele or not ?
!   -------------------------
    call jeexin('&CALCUL.PARALLELE', iret)
    if (iret .ne. 0) then
        lparal=.true.
        call jeveuo('&CALCUL.PARALLELE', 'L', jparal)
    else
        lparal=.false.
    endif

!   recuperation de la carte:
!   -------------------------
    desc=zi(ca_iachii_-1+11*(ca_iichin_-1)+4)
    ngrmx=zi(desc-1+2)
    modloc=ca_iamloc_-1+zi(ca_ilmloc_-1+imodat)
    ityplo=zi(modloc-1+1)
    nbpoin=zi(modloc-1+4)
    lgcata=zi(ca_iawlo2_-1+5*(ca_nbgr_*(iparg-1)+ca_igr_-1)+2)
    debugr=zi(ca_iawlo2_-1+5*(ca_nbgr_*(iparg-1)+ca_igr_-1)+5)


!   1-  cas: cart -> elno (ou elga) : "expand"
!   -------------------------------------------
    if ((ityplo.eq.2) .or. (ityplo.eq.3)) then

        ASSERT((ityplo.ne.2) .or. (nbpoin.le.10000))
        ncmp=lgcata/nbpoin
        call excar2(ngrmx, desc, zi(modloc-1+5), ncmp, debugr)
!       on dupplique les valeurs par la fin pour ne pas
!       les ecraser :
        do ca_iel_ = ca_nbelgr_, 1, -1
            if (lparal) then
                if (.not.zl(jparal-1+ca_iel_)) cycle
            endif
            do ipt = nbpoin, 1, -1
                dec1=debugr-1+(ca_iel_-1)*ncmp
                dec2=debugr-1+(ca_iel_-1)*ncmp*nbpoin+ncmp*(ipt-1)
                call jacopo(ncmp, ca_typegd_, ca_iachlo_+dec1, ca_iachlo_+dec2)
                call jacopo(ncmp, 'L', ca_ilchlo_+dec1, ca_ilchlo_+dec2)
            enddo
        enddo

!   2-  cas: cart -> asse :
!   -----------------------
    else if (ityplo.ge.4) then
        ASSERT(.false.)


!   3-  cas: cart -> elem :
!   -----------------------
    else if (ityplo.eq.1) then
        ASSERT(nbpoin.eq.1)
        ncmp=lgcata
        call excar2(ngrmx, desc, zi(modloc-1+5), ncmp, debugr)
    endif

end subroutine
