subroutine w155m2(chin, carele, ligrel, chextr, nomsym,&
                  nocmp, tymaxi)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
! ======================================================================
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/celces.h"
#include "asterfort/cescel.h"
#include "asterfort/cesexi.h"
#include "asterfort/cesred.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/liglma.h"
#include "asterfort/utmess.h"
#include "asterfort/w155m3.h"
    character(len=8) :: carele, nocmp, tymaxi
    character(len=16) :: nomsym
    character(len=19) :: chin, chextr, ligrel
!
! ----------------------------------------------------------------------
! BUT : EXTRACTION DU CHAM_ELEM CORRESPONDANT AU MIN/MAX
!       SUR LES SOUS-POINTS
!
! IN/JXIN  CHIN  : CHAM_ELEM (PLUSIEURS SOUS-POINTS) DANS LEQUEL
!                  ON DOIT EXTRAIRE CHEXTR
! IN/JXIN  CARELE  : CARA_ELEM ASSOCIE A CHIN
! IN/JXIN  LIGREL  : LIGREL SUR LEQUEL CREER CHEXTR
! IN/JXVAR CHEXTR  : CHAM_ELEM (1 SEUL SOUS-POINT) A REMPLIR
! IN       NOCMP   : NOM DE LA COMPOSANTE DONT ON CHERCHE LE MIN/MAX
! IN       TYMAXI  : /'MAXI' /'MINI' /'MAXI_ABS', /'MINI_ABS'
    character(len=24) :: linuma, linute, valk(5)
    character(len=19) :: ces1, ces2, ces3, ces4
    character(len=16) :: option
    character(len=8) :: licmp(4), ma, nomgd, tsca, nompar
    character(len=3) :: exituy
    integer :: iret, nbma, nbmat, numa, kma
    integer :: nbpt, kpt, kcmp, nncp, jlite
    integer :: iad1, iad4, jlima, ncmp
    integer :: jce2l, jce2d, jce2v, jce3k, jce3l, jce3d
    integer :: jce4l,  jce4d,  nbspmx, nucmp
    integer :: isp, nbsp, ksp, nusec, nucou, nufib, posic, posis, kcmp2
    real(kind=8) :: val, vmima
    character(len=8), pointer :: ce3c(:) => null()
    character(len=8), pointer :: ce4c(:) => null()
    real(kind=8), pointer :: ce3v(:) => null()
    real(kind=8), pointer :: ce4v(:) => null()
!
! ----------------------------------------------------------------------
    call jemarq()
    call dismoi('NOM_MAILLA', chin, 'CHAM_ELEM', repk=ma)
    call dismoi('NOM_GD', chin, 'CHAM_ELEM', repk=nomgd)
    call dismoi('TYPE_SCA', chin, 'CHAM_ELEM', repk=tsca)
    call dismoi('MXNBSP', chin, 'CHAM_ELEM', repi=nbspmx)
    call dismoi('EXI_TUYAU', ligrel, 'LIGREL', repk=exituy)
    if (nbspmx .le. 1) then
        call utmess('F', 'CALCULEL2_15')
    endif
    call dismoi('NB_MA_MAILLA', ma, 'MAILLAGE', repi=nbmat)
    ASSERT(tsca.eq.'R')
    ASSERT(exituy.eq.'OUI' .or. exituy.eq.'NON')
!
!
!     1.  LISTE DES MAILLES A TRAITER :
!     ---------------------------------
    linuma='&&W155M2.LIMA'
    linute='&&W155M2.LITE'
    call liglma(ligrel, nbma, linuma, linute)
    ASSERT(nbma.gt.0)
    call jeveuo(linuma, 'L', jlima)
    call jeveuo(linute, 'L', jlite)
!
!
!     2.  NOMBRE DE COUCHES, SECTEURS ET FIBRES  DES ELEMENTS :
!     -----------------------------------------------------------
    ces1='&&W155M2.CES1'
    ces2='&&W155M2.CES2'
    call celces(carele//'.CANBSP', 'V', ces1)
!
!     -- L'ORDRE DES CMPS EST IPORTANT (UTILISE DANS W155M3)
    licmp(1)='COQ_NCOU'
    licmp(2)='TUY_NCOU'
    licmp(3)='TUY_NSEC'
    licmp(4)='NBFIBR'
    call cesred(ces1, nbma, zi(jlima), 4, licmp,&
                'V', ces2)
    call detrsd('CHAM_ELEM_S', ces1)
    call jeveuo(ces2//'.CESD', 'L', jce2d)
    call jeveuo(ces2//'.CESV', 'L', jce2v)
    call jeveuo(ces2//'.CESL', 'L', jce2l)
!
!
!     3. CHIN -> CES3 :
!     ------------------
    ces3='&&W155M2.CES3'
    call celces(chin, 'V', ces3)
    call jeveuo(ces3//'.CESK', 'L', jce3k)
    call jeveuo(ces3//'.CESD', 'L', jce3d)
    call jeveuo(ces3//'.CESC', 'L', vk8=ce3c)
    call jeveuo(ces3//'.CESL', 'L', jce3l)
    call jeveuo(ces3//'.CESV', 'L', vr=ce3v)
    call jelira(ces3//'.CESC', 'LONMAX', ncmp)
!
!
!     4. REMPLISSAGE DU CHAM_ELEM_S RESULTAT CES4 :
!     ---------------------------------------------
    ces4='&&W155M2.CES4'
    call celces(chextr, 'V', ces4)
    call jeveuo(ces4//'.CESD', 'L', jce4d)
    call jeveuo(ces4//'.CESV', 'L', vr=ce4v)
    call jeveuo(ces4//'.CESL', 'L', jce4l)
    call jeveuo(ces4//'.CESC', 'L', vk8=ce4c)
    do kcmp = 1, ncmp
        if (ce3c(kcmp) .eq. nocmp) then
            nucmp=kcmp
            goto 20
!
        endif
    end do
    valk(1)=nocmp
    valk(2)=nomsym
    call utmess('F', 'CHAMPS_19', nk=2, valk=valk)
!
 20 continue
    do kma = 1, nbma
        numa=zi(jlima-1+kma)
        ASSERT(numa.ge.1 .and. numa.le.nbmat)
        nbpt=zi(jce3d-1+5+4*(numa-1)+1)
        nbsp=zi(jce3d-1+5+4*(numa-1)+2)
        if (nbsp .eq. 0) goto 60
!
        do kpt = 1, nbpt
!         -- 4.1 CALCUL DE VMIMA ET ISP :
!            VMIMA : VALEUR MIN/MAX ATTEINTE SUR LES SOUS-POINTS
!            ISP   : NUMERO DU SOUS-POINT REALISANT LE MIN/MAX
            do ksp = 1, nbsp
                call cesexi('C', jce3d, jce3l, numa, kpt,&
                            ksp, nucmp, iad1)
                if (iad1 .gt. 0) then
                    val=ce3v(iad1)
                    if (tymaxi(5:8) .eq. '_ABS') val=abs(val)
                    if (ksp .eq. 1) then
                        vmima=val
                        isp=ksp
                    else
                        if (tymaxi(1:4) .eq. 'MAXI') then
                            if (val .gt. vmima) then
                                vmima=val
                                isp=ksp
                            endif
                        else if (tymaxi(1:4).eq.'MINI') then
                            if (val .lt. vmima) then
                                vmima=val
                                isp=ksp
                            endif
                        else
                            ASSERT(.false.)
                        endif
                    endif
                endif
            end do
!
!         -- 4.2  CALCUL DE NUCOU, NUSEC, ... A PARTIR DE ISP :
            call w155m3(numa, jce2d, jce2l, jce2v, isp,&
                        nucou, nusec, nufib, posic, posis)
!
!         -- 4.3 STOCKAGE DE VMIMA, NUCOU, NUSEC, ...
            do kcmp2 = 1, 6
                call cesexi('C', jce4d, jce4l, numa, kpt,&
                            1, kcmp2, iad4)
                ASSERT(iad4.gt.0)
                if (kcmp2 .eq. 1) then
                    ASSERT(ce4c(kcmp2).eq.'VAL')
                    ce4v(iad4)=vmima
                else if (kcmp2.eq.2) then
                    ASSERT(ce4c(kcmp2).eq.'NUCOU')
                    ce4v(iad4)=dble(nucou)
                else if (kcmp2.eq.3) then
                    ASSERT(ce4c(kcmp2).eq.'NUSECT')
                    ce4v(iad4)=dble(nusec)
                else if (kcmp2.eq.4) then
                    ASSERT(ce4c(kcmp2).eq.'NUFIBR')
                    ce4v(iad4)=dble(nufib)
                else if (kcmp2.eq.5) then
                    ASSERT(ce4c(kcmp2).eq.'POSIC')
                    ce4v(iad4)=dble(posic)
                else if (kcmp2.eq.6) then
                    ASSERT(ce4c(kcmp2).eq.'POSIS')
                    ce4v(iad4)=dble(posis)
                else
                    ASSERT(.false.)
                endif
            end do
        end do
 60     continue
    end do
!
!
!     5 CES4 -> CHEXTR :
!     ------------------------------------
    call dismoi('NOM_OPTION', chextr, 'CHAM_ELEM', repk=option)
    call dismoi('NOM_PARAM', chextr, 'CHAM_ELEM', repk=nompar)
    call detrsd('CHAM_ELEM', chextr)
    call cescel(ces4, ligrel, option, nompar, 'OUI',&
                nncp, 'G', chextr, 'F', iret)
    ASSERT(nncp.eq.0)
!
!
!     6. MENAGE :
!     ------------
    call detrsd('CHAM_ELEM_S', ces2)
    call detrsd('CHAM_ELEM_S', ces3)
    call detrsd('CHAM_ELEM_S', ces4)
    call jedetr(linuma)
    call jedetr(linute)
!
    call jedema()
end subroutine
