subroutine caundf(code, opt, te)
use module_calcul, only : ca_iaoppa_, ca_iawlo2_, ca_iawloc_, &
    ca_iawtyp_, ca_igr_, ca_nbgr_, ca_npario_
implicit none
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
! person_in_charge: jacques.pellet at edf.fr
!     ARGUMENTS:
!     ----------
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/iisnan.h"
#include "asterc/indik8.h"
#include "asterc/isnnem.h"
#include "asterc/r8nnem.h"
#include "asterfort/assert.h"
#include "asterfort/jenuno.h"
#include "asterfort/jexnum.h"
#include "asterfort/nbpara.h"
#include "asterfort/nopara.h"
#include "asterfort/utmess.h"
!
    integer :: opt, te
    character(len=5) :: code
! ----------------------------------------------------------------------
!     ENTREES:
!      CODE :  / 'ECRIT' : ON ECRIT UNE VALEUR UNDEF AU BOUT DES CHLOC
!              / 'VERIF' : ON VERIFIE LA VALEUR UNDEF AU BOUT DES CHLOC
!      OPT : OPTION
!      TE  : TYPE_ELEMENT
! ----------------------------------------------------------------------
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
!
! DEB-------------------------------------------------------------------
!
    innem = isnnem()
    rnnem = r8nnem()
    knnem='????????'
!
    ASSERT((code.eq.'ECRIT').or.(code.eq.'VERIF'))
!
!
    if (code .eq. 'ECRIT') then
!     ------------------------------------------------
!
!        -- CHAMPS "IN" ET "OUT" :
        do 10 iparg = 1, ca_npario_
            lgcata=zi(ca_iawlo2_-1+5*(ca_nbgr_*(iparg-1)+ca_igr_-1)+2)
            if (lgcata .le. 0) goto 10
            iachlo=zi(ca_iawloc_-1+3*(iparg-1)+1)
            if ((iachlo.eq.-1) .or. (iachlo.eq.-2)) goto 10
!
            typsca = zk8(ca_iawtyp_-1+iparg)
            lggrel=zi(ca_iawlo2_-1+5*(ca_nbgr_*(iparg-1)+ca_igr_-1)+4)
            debugr=zi(ca_iawlo2_-1+5*(ca_nbgr_*(iparg-1)+ca_igr_-1)+5)
!
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
 10     continue
!
!
!
    else if (code.eq.'VERIF') then
!     ------------------------------------------------
!
!        -- CHAMPS "OUT" :
        arret = .false.
        np = nbpara(opt,te,'OUT')
        do 30 ipar = 1, np
            ecras=.false.
            nompar = nopara(opt,te,'OUT',ipar)
            iparg = indik8(zk8(ca_iaoppa_),nompar,1,ca_npario_)
            lgcata=zi(ca_iawlo2_-1+5*(ca_nbgr_*(iparg-1)+ca_igr_-1)+2)
            if (lgcata .le. 0) goto 30
            ich=zi(ca_iawloc_-1+3*(iparg-1)+3)
            if (ich .eq. 0) goto 30
            iachlo=zi(ca_iawloc_-1+3*(iparg-1)+1)
            if ((iachlo.eq.-1) .or. (iachlo.eq.-2)) goto 30
!
            typsca = zk8(ca_iawtyp_-1+iparg)
            lggrel=zi(ca_iawlo2_-1+5*(ca_nbgr_*(iparg-1)+ca_igr_-1)+4)
            debugr=zi(ca_iawlo2_-1+5*(ca_nbgr_*(iparg-1)+ca_igr_-1)+5)
!
!
            if (typsca .eq. 'R') then
                if (iisnan(zr(iachlo-1+debugr-1+lggrel+1)) .eq. 0) ecras=.true.
            else if (typsca.eq.'C') then
                if (iisnan(dble(zc(iachlo-1+debugr-1+lggrel+1))) .eq. 0) ecras=.true.
                if (iisnan(dimag(zc(iachlo-1+debugr-1+lggrel+1))) .eq. 0) ecras=.true.
            else if (typsca.eq.'I') then
                if (zi(iachlo-1+debugr-1+lggrel+1) .ne. innem) ecras= .true.
            else
                ASSERT(.false.)
            endif
!
            if (ecras) then
                arret = .true.
                call jenuno(jexnum('&CATA.TE.NOMTE', te), nomte)
                call jenuno(jexnum('&CATA.OP.NOMOPT', opt), nomopt)
                valk(1) = nomte
                valk(2) = nomopt
                valk(3) = nompar
                call utmess('E', 'CALCUL_40', nk=3, valk=valk)
            endif
!
 30     continue
!
        ASSERT(.not.arret)
!
    endif
!
!
end subroutine
