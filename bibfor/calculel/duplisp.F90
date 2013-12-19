subroutine duplisp(celssp, celasp, carel, base)
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/alchml.h"
#include "asterfort/cesvar.h"
#include "asterfort/cesexi.h"
#include "asterfort/cmpcha.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisdg.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/nbelem.h"
#include "asterfort/sdmpic.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
              character(len=*), intent(in) :: celssp
              character(len=*), intent(in) :: celasp
              character(len=*), intent(in) :: carel
              character(len=*), intent(in) :: base
! ------------------------------------------------------------------
! but : transformer un cham_elem sans sous-points (celssp)
!       en 1 cham_elem avec des sous-points (celasp)
!       le nombre de sous-points est donne par la sd cara_elem (carel)
! ------------------------------------------------------------------
!     arguments:
! celssp    in/jxin  k19 : sd cham_elem sans sous-points
! celasp    in/jxou  k19 : sd cham_elem avec sous-points
! base      in       k1  : base de creation pour celasp : g/v/l
! carel     in/jxout k19 : sd cara_elem
!------------------------------------------------------------------

    character(len=1) :: bas1
    character(len=19) :: cel1,cel2,canbsp,ligrel
    character(len=16) :: option
    character(len=8) :: nompar,mailla,nomgd,tsca
    integer :: igr,ier,numa,icmp,isp,ipt,nbpt,nbsp,nbelgr
    integer ::  illiel, mxnbsp,mxvari,nbspt1,nbspt2
    integer :: ispt1,ispt2,iret,nbgr,imolo,lgcata,jmolo,iel,ncdyn
    integer :: ieq1,ieq2, adiel1, adiel2,jcelv1,jcelv2
    integer :: nbel, nbcmp
    character(len=24) :: valk(2)
    integer, pointer :: liel(:) => null()
    integer, pointer :: celd1(:) => null()
    integer, pointer :: celd2(:) => null()
    logical diff

#   define numail(igr,iel) liel(zi(illiel+igr-1)+iel-1)
!------------------------------------------------------------------

    call jemarq()
    cel1 = celssp
    cel2 = celasp
    canbsp = '&&CHRPEL.NBSP'


!   1. calcul de : mailla, ligrel, option, param, canbsp
!                  nomgd, mxnbsp, mxvari, tsca
!   ----------------------------------------------
    call dismoi('NOM_MAILLA', cel1, 'CHAM_ELEM', repk=mailla)
    call dismoi('NOM_LIGREL', cel1, 'CHAM_ELEM', repk=ligrel)
    call dismoi('NOM_OPTION', cel1, 'CHAM_ELEM', repk=option)
    call dismoi('NOM_PARAM', cel1, 'CHAM_ELEM', repk=nompar)
    call dismoi('NOM_GD', cel1, 'CHAM_ELEM', repk=nomgd)
    call dismoi('MXNBSP', cel1, 'CHAM_ELEM', repi=mxnbsp)
    call dismoi('MXVARI', cel1, 'CHAM_ELEM', repi=mxvari)
    call dismoi('TYPE_SCA', nomgd, 'GRANDEUR', repk=tsca)
    call cesvar(carel, ' ', ligrel, canbsp)
    if(mxnbsp.gt.1) then
        call utmess('F','CALCULEL2_57')
    endif
    nbspt1=1
    ispt1=1


!   2. allocation de cel2 :
!   --------------------------------
    call alchml(ligrel, option, nompar, base, cel2, iret, canbsp)
    ASSERT (iret.eq.0)


!   3. duplication des valeurs de cel1 dans cel2:
!   ---------------------------------------------
    call jeveuo(cel1//'.CELV', 'L', jcelv1)
    call jeveuo(cel1//'.CELD', 'L', vi=celd1)
    call jeveuo(cel2//'.CELV', 'E', jcelv2)
    call jeveuo(cel2//'.CELD', 'E', vi=celd2)

    call jeveuo(ligrel//'.LIEL', 'L', vi=liel)
    call jeveuo(jexatr(ligrel//'.LIEL', 'LONCUM'), 'L', illiel)

    nbgr = celd1(2)
    do igr = 1, nbgr
        imolo = celd1(celd1(4+igr)+2)
        if (imolo .eq. 0) goto 220

        lgcata = celd1(celd1(4+igr)+3)
        call jeveuo(jexnum('&CATA.TE.MODELOC', imolo), 'L', jmolo)
        diff = (zi(jmolo-1+4).gt.10000)
        ASSERT(.not.diff)
        nbpt = mod(zi(jmolo-1+4),10000)
        nbel = nbelem(ligrel,igr)
        nbcmp=lgcata/nbpt
        ASSERT(lgcata.eq.nbcmp*nbpt)

        do iel = 1, nbel
            numa = numail(igr,iel)
            if (numa .lt. 0) goto 210

            nbspt1 = celd1(celd1(4+igr)+4+4* (iel-1)+1)
            nbspt2 = celd2(celd2(4+igr)+4+4* (iel-1)+1)
            adiel1 = celd1(celd1(4+igr)+4+4* (iel-1)+4)
            adiel2 = celd2(celd2(4+igr)+4+4* (iel-1)+4)
            ncdyn = celd1(celd1(4+igr)+4+4* (iel-1)+ 2)
!           -- cel2 a ete alloue avec ncdyn=0. Il faut corriger :
            celd2(celd2(4+igr)+4+4* (iel-1)+ 2)=ncdyn
            ncdyn=max(ncdyn,1)

            do ipt = 1, nbpt
                do ispt2 = 1, nbspt2
                    do icmp = 1, nbcmp*ncdyn
                        ieq1 = adiel1 - 1 + ((ipt-1)*nbspt1+ispt1-1)* nbcmp*ncdyn + icmp
                        ieq2 = adiel2 - 1 + ((ipt-1)*nbspt2+ispt2-1)* nbcmp*ncdyn + icmp

                        if (tsca .eq. 'R') then
                            zr(jcelv2-1+ieq2)=zr(jcelv1-1+ieq1)
                        elseif (tsca .eq. 'C') then
                            zc(jcelv2-1+ieq2)=zc(jcelv1-1+ieq1)
                        elseif (tsca .eq. 'I') then
                            zi(jcelv2-1+ieq2)=zi(jcelv1-1+ieq1)
                        elseif (tsca .eq. 'K8') then
                            zk8(jcelv2-1+ieq2)=zk8(jcelv1-1+ieq1)
                        else
                            ASSERT(.false.)
                        endif
                    end do
201                 continue
                end do
            end do
210         continue
        end do
220     continue
    end do


!   Menage :
!   --------
    call detrsd('CHAM_ELEM_S',canbsp)

    call jedema()
end subroutine
