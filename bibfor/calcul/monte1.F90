subroutine monte1(te2, nout, lchout, lpaout, igr2)

use calcul_module, only : ca_iachoi_, ca_iadsgd_, ca_iaoppa_, &
     ca_iawlo2_, ca_iawloc_, ca_iawtyp_, ca_nbelgr_, ca_nbgr_, ca_npario_,&
     ca_paral_, ca_lparal_, ca_nuop_, ca_iel_

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
#include "asterfort/digde2.h"
#include "asterfort/grdeur.h"
#include "asterfort/jacopo.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelibe.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/modatt.h"
#include "asterfort/nbpara.h"
#include "asterfort/nopara.h"

    integer :: nout, te2, igr2
    character(len=19) :: ch19
    character(len=*) :: lchout(*)
    character(len=8) :: lpaout(*)
!-----------------------------------------------------------------------
!     entrees:
!     igr2   : numero du grel dont on sauve les champs locaux
!
!     sorties:
!     met a jour les champs globaux de sortie de l ca_nuop_ion ca_nuop_
!-----------------------------------------------------------------------
    integer :: ipar, np, mod1, jpar, gd, iaux1, iaux2, iaux0
    integer :: iparg, iachlo, lggrel, jcelv, jresl
    integer :: descgd, jceld, code, debugr, ncmpel, debgr2
    character(len=8) :: nompar, typsca
!-----------------------------------------------------------------------

    call jemarq()


    np=nbpara(ca_nuop_,te2,'OUT')
    do ipar = 1, np
        nompar=nopara(ca_nuop_,te2,'OUT',ipar)
        iparg=indik8(zk8(ca_iaoppa_),nompar,1,ca_npario_)
        iachlo=zi(ca_iawloc_-1+3*(iparg-1)+1)
        if (iachlo .eq. -1) cycle

        gd=grdeur(nompar)
        descgd=ca_iadsgd_+7*(gd-1)
        code=zi(descgd-1+1)

        mod1=modatt(ca_nuop_,te2,'OUT',ipar)
        jpar=indik8(lpaout,nompar,1,nout)
        ch19=lchout(jpar)


        typsca=zk8(ca_iawtyp_-1+iparg)
        lggrel=zi(ca_iawlo2_-1+5*(ca_nbgr_*(iparg-1)+igr2-1)+4)
        debugr=zi(ca_iawlo2_-1+5*(ca_nbgr_*(iparg-1)+igr2-1)+5)
        if (lggrel .eq. 0) cycle


        if (code .eq. 1) then
!           -- cas : cham_elem
            jceld=zi(ca_iachoi_-1+2*(jpar-1)+1)
            debgr2=zi(jceld-1+zi(jceld-1+4+igr2)+8)
            jcelv=zi(ca_iachoi_-1+2*(jpar-1)+2)
            call jacopo(lggrel, typsca, iachlo+debugr-1, jcelv-1+debgr2)


        else
!           -- cas : resuelem
            call jeveuo(jexnum(ch19//'.RESL', igr2), 'E', jresl)

            if (ca_lparal_) then
                ncmpel=digde2(mod1)
                do 10 ca_iel_ = 1, ca_nbelgr_
                    if (ca_paral_(ca_iel_)) then
                        iaux0=(ca_iel_-1)*ncmpel
                        iaux1=iachlo+debugr-1+iaux0
                        iaux2=jresl+iaux0
                        call jacopo(ncmpel, typsca, iaux1, iaux2)
                    endif
 10             continue
            else
                call jacopo(lggrel, typsca, iachlo+debugr-1, jresl)
            endif

            call jelibe(jexnum(ch19//'.RESL', igr2))
        endif
    end do

    call jedema()
end subroutine
