subroutine caundf(code, opt, te)
use calcul_module, only : ca_iaoppa_, ca_iawlo2_, ca_iawloc_, &
    ca_iawtyp_, ca_igr_, ca_nbgr_, ca_npario_
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
#include "asterc/indik8.h"
#include "asterc/isnnem.h"
#include "asterc/r8nnem.h"
#include "asterfort/assert.h"
#include "asterfort/jenuno.h"
#include "asterfort/jexnum.h"
#include "asterfort/nbpara.h"
#include "asterfort/nopara.h"
#include "asterfort/utmess.h"

    integer :: opt, te
    character(len=5) :: code
!-----------------------------------------------------------------------
!     entrees:
!        code :  / 'ECRIT' : on ecrit une valeur undef au bout des chloc
!                / 'VERIF' : on verifie la valeur undef au bout des chloc
!        opt  : option
!        te   : type_element
!-----------------------------------------------------------------------
    integer :: innem
    integer :: np, ipar
    integer ::  iparg, lggrel, iachlo
    character(len=3) :: typsca
    character(len=8) :: nompar
    aster_logical :: arret, ecras
    character(len=16) :: nomte, nomopt
    integer :: ich, debugr, lgcata
    real(kind=8) :: rnnem
    character(len=8) :: knnem
    character(len=24) :: valk(3)
!-------------------------------------------------------------------

    innem = isnnem()
    rnnem = r8nnem()
    knnem='????????'

    ASSERT((code.eq.'ECRIT').or.(code.eq.'VERIF'))


    if (code .eq. 'ECRIT') then
!   ---------------------------

!        -- champs "in" et "out" :
        do iparg = 1, ca_npario_
            lgcata=zi(ca_iawlo2_-1+5*(ca_nbgr_*(iparg-1)+ca_igr_-1)+2)
            if (lgcata .le. 0) cycle
            iachlo=zi(ca_iawloc_-1+3*(iparg-1)+1)
            if ((iachlo.eq.-1) .or. (iachlo.eq.-2)) cycle

            typsca = zk8(ca_iawtyp_-1+iparg)(1:3)
            lggrel=zi(ca_iawlo2_-1+5*(ca_nbgr_*(iparg-1)+ca_igr_-1)+4)
            debugr=zi(ca_iawlo2_-1+5*(ca_nbgr_*(iparg-1)+ca_igr_-1)+5)

            if (typsca .eq. 'R') then
                zr(iachlo-1+debugr-1+lggrel+1) = rnnem
            else if (typsca.eq.'C') then
                zc(iachlo-1+debugr-1+lggrel+1) = dcmplx(rnnem,rnnem)
            else if (typsca.eq.'I') then
                zi(iachlo-1+debugr-1+lggrel+1) = innem
            else if (typsca.eq.'K8') then
                zk8(iachlo-1+debugr-1+lggrel+1) = knnem
            else if (typsca.eq.'K16') then
                zk16(iachlo-1+debugr-1+lggrel+1) = knnem
            else if (typsca.eq.'K24') then
                zk24(iachlo-1+debugr-1+lggrel+1) = knnem
            else
                ASSERT(.false.)
            endif
        enddo



    else if (code.eq.'VERIF') then
!   ------------------------------

!        -- champs "out" :
        arret = .false.
        np = nbpara(opt,te,'OUT')
        do ipar = 1, np
            ecras=.false.
            nompar = nopara(opt,te,'OUT',ipar)
            iparg = indik8(zk8(ca_iaoppa_),nompar,1,ca_npario_)
            lgcata=zi(ca_iawlo2_-1+5*(ca_nbgr_*(iparg-1)+ca_igr_-1)+2)
            if (lgcata .le. 0) cycle
            ich=zi(ca_iawloc_-1+3*(iparg-1)+3)
            if (ich .eq. 0) cycle
            iachlo=zi(ca_iawloc_-1+3*(iparg-1)+1)
            if ((iachlo.eq.-1) .or. (iachlo.eq.-2)) cycle

            typsca = zk8(ca_iawtyp_-1+iparg)(1:3)
            lggrel=zi(ca_iawlo2_-1+5*(ca_nbgr_*(iparg-1)+ca_igr_-1)+4)
            debugr=zi(ca_iawlo2_-1+5*(ca_nbgr_*(iparg-1)+ca_igr_-1)+5)


            if (typsca .eq. 'R') then
                if (.not.isnan(zr(iachlo-1+debugr-1+lggrel+1))) ecras=.true.
            else if (typsca.eq.'C') then
                if (.not.isnan(dble(zc(iachlo-1+debugr-1+lggrel+1)))) ecras=.true.
                if (.not.isnan(dimag(zc(iachlo-1+debugr-1+lggrel+1)))) ecras=.true.
            else if (typsca.eq.'I') then
                if (zi(iachlo-1+debugr-1+lggrel+1) .ne. innem) ecras= .true.
            else
                ASSERT(.false.)
            endif

            if (ecras) then
                arret = .true.
                call jenuno(jexnum('&CATA.TE.NOMTE', te), nomte)
                call jenuno(jexnum('&CATA.OP.NOMOPT', opt), nomopt)
                valk(1) = nomte
                valk(2) = nomopt
                valk(3) = nompar
                call utmess('E', 'CALCUL_40', nk=3, valk=valk)
            endif

        enddo

        ASSERT(.not.arret)

    endif


end subroutine
