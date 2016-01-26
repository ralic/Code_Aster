subroutine extra1(nin, lchin, lpain)

use calcul_module, only : ca_iachii_, ca_iachik_, ca_iachin_, ca_iachix_,&
     ca_iachlo_, ca_ianueq_, ca_iaoppa_, ca_iawlo2_,&
     ca_iawloc_, ca_igd_, ca_igr_, ca_iichin_, ca_ilchlo_, ca_itypgd_,&
     ca_lprno_, ca_nbgr_, ca_ncmpmx_, ca_nec_, ca_npario_, ca_typegd_,&
     ca_nute_, ca_nuop_

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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr

#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/indik8.h"
#include "asterfort/assert.h"
#include "asterfort/excart.h"
#include "asterfort/exchml.h"
#include "asterfort/exchno.h"
#include "asterfort/exresl.h"
#include "asterfort/nbpara.h"
#include "asterfort/nopara.h"
#include "asterfort/utmess.h"

    integer :: nin
    character(len=*) :: lchin(*)
    character(len=8) :: lpain(*)
!-----------------------------------------------------------------------
!     but: preparer les champs locaux "in"
!-----------------------------------------------------------------------
    integer ::    debugr, lggrel
    character(len=19) :: chin
    character(len=4) :: type
    character(len=8) :: nompar
    integer :: k, iparg, imodat
    integer :: ipar, npin, iparin
    aster_logical :: exich
!-------------------------------------------------------------------

    npin=nbpara(ca_nuop_,ca_nute_,'IN ')
    do ipar = 1, npin
        nompar=nopara(ca_nuop_,ca_nute_,'IN ',ipar)
        iparg=indik8(zk8(ca_iaoppa_),nompar,1,ca_npario_)
        iparin=indik8(lpain,nompar,1,nin)
        exich=((iparin.gt.0) .and. zl(ca_iachix_-1+iparin))
        if (.not.exich) then
            zi(ca_iawloc_-1+3*(iparg-1)+1)=-1
            zi(ca_iawlo2_-1+5*(ca_nbgr_*(iparg-1)+ca_igr_-1)+2)=0
            goto 90
        endif

        ASSERT(iparin.ne.0)
        chin=lchin(iparin)
        if (chin(1:1) .eq. ' ') then
            call utmess('E', 'CALCUL_13', sk=nompar)
        endif


        ca_iichin_=iparin
        ca_igd_=zi(ca_iachii_-1+11*(iparin-1)+1)
        ca_nec_=zi(ca_iachii_-1+11*(iparin-1)+2)
        ca_ncmpmx_=zi(ca_iachii_-1+11*(iparin-1)+3)
        ca_iachin_=zi(ca_iachii_-1+11*(iparin-1)+5)
        ca_ianueq_=zi(ca_iachii_-1+11*(iparin-1)+10)
        ca_lprno_=zi(ca_iachii_-1+11*(iparin-1)+11)
        iparg=indik8(zk8(ca_iaoppa_),nompar,1,ca_npario_)
        ca_iachlo_=zi(ca_iawloc_-1+3*(iparg-1)+1)
        ca_ilchlo_=zi(ca_iawloc_-1+3*(iparg-1)+2)
        imodat=zi(ca_iawlo2_-1+5*(ca_nbgr_*(iparg-1)+ca_igr_-1)+1)
        ASSERT((ca_iachlo_.lt.-2) .or. (ca_iachlo_.gt.0))
        ASSERT(ca_ilchlo_.ne.-1)
        type=zk8(ca_iachik_-1+2*(iparin-1)+1)(1:4)
        ca_typegd_=zk8(ca_iachik_-1+2*(iparin-1)+2)
        if (ca_typegd_ .eq. 'R') then
            ca_itypgd_=1
        else if (ca_typegd_.eq.'C') then
            ca_itypgd_=2
        else if (ca_typegd_.eq.'I') then
            ca_itypgd_=3
        else if (ca_typegd_.eq.'K8') then
            ca_itypgd_=4
        else if (ca_typegd_.eq.'K16') then
            ca_itypgd_=5
        else if (ca_typegd_.eq.'K24') then
            ca_itypgd_=6
        else
            ASSERT(.false.)
        endif


!       1- mise a .false. du champ_loc.EXIS
!       -----------------------------------------------------
        lggrel=zi(ca_iawlo2_-1+5*(ca_nbgr_*(iparg-1)+ca_igr_-1)+4)
        debugr=zi(ca_iawlo2_-1+5*(ca_nbgr_*(iparg-1)+ca_igr_-1)+5)
        do 30 k = 1, lggrel
            zl(ca_ilchlo_-1+debugr-1+k)=.false.
 30     continue


!       2- on fait l'extraction:
!       -------------------------------------------
        if (type .eq. 'CART') call excart(imodat, iparg)
        if (type .eq. 'CHML') call exchml(imodat, iparg)
        if (type .eq. 'CHNO') call exchno(imodat, iparg)
        if (type .eq. 'RESL') call exresl(imodat, iparg, chin)
 90     continue
    end do



end subroutine
