subroutine pmfd01(noma, carele, vmailfib, sdgf, cesdec, ngmxel)
!
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
! --------------------------------------------------------------------------------------------------
!
!   commande AFFE_CARA_ELEM fabrication de 2 cham_elem_s/'elem' :
!          - carele//'.CANBSP'
!          - carele//'.CAFIBR'
!
!   Traitement des mots clefs AFFE_SECT et AFFE_FIBRE
!   Transformation des objets vmailfib
!   Prise en compte de cesdec
!
! person_in_charge: jean-luc.flejou at edf.fr
! --------------------------------------------------------------------------------------------------
!
    implicit none
#include "jeveux.h"
#include "asterf_types.h"
#include "asterc/getfac.h"
#include "asterfort/assert.h"
#include "asterfort/cescel.h"
#include "asterfort/cescre.h"
#include "asterfort/cesexi.h"
#include "asterfort/cesfus.h"
#include "asterfort/codent.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    integer :: ngmxel
    character(len=8) :: noma, carele, sdgf
    character(len=19) :: cesdec
    character(len=24) :: vmailfib
!
! --------------------------------------------------------------------------------------------------
!
    integer, parameter :: nbcmpmax = 20
    character(len=8) :: licmp(nbcmpmax)
!
    integer :: jmailfib, jpofig, jcafig, jces1d, jces1v, jces1l, jnbfig, jsdfig, jtyfig
    integer :: iad, icmp, ncarfi, point, ima, ibid, nbma, ncarfimax
    integer :: nb1, ispt, nncp, ifib, ig, nbgf, nbfig, nug, ipos, iret, tyfib
    integer :: nbcp, jsp
    real(kind=8) :: lcoefr(2)
    character(len=1) :: ki1
    character(len=2) :: ki2
    character(len=8) :: modele
    character(len=19) :: ces1, lichs(2), ces3, ligrmo, cel
    character(len=24) :: vpofig, vcafig, vnbfig, vsdfig, vtyfig
    aster_logical :: lcumul(2), exipmf
    complex(kind=8) :: cbid
!
! --------------------------------------------------------------------------------------------------
    call jemarq()
    cbid = dcmplx(0.d0, 0.d0)
!
    call dismoi('NB_MA_MAILLA', noma, 'MAILLAGE', repi=nbma)
    call getvid(' ', 'MODELE', scal=modele, nbret=ibid)
    ligrmo = modele//'.MODELE'
!
    call getfac('MULTIFIBRE', nb1)
!
    exipmf = (nb1.gt.0)
! --------------------------------------------------------------------------------------------------
!   il n'existe pas d'éléments PMF
    if (.not.exipmf) then
        cel = carele//'.CANBSP'
        call cescel(cesdec, ligrmo, 'TOU_INI_ELEM', ' ', 'NON',&
                    nncp, 'G', cel, 'A', iret)
        if (iret .eq. 0) goto 999
        call utmess('F', 'CALCULEL_6', sk=modele)
    endif
! --------------------------------------------------------------------------------------------------
!   il existe des éléments PMF
    call jeveuo(vmailfib, 'L', jmailfib)
!
    vnbfig = sdgf//'.NB_FIBRE_GROUPE'
    vtyfig = sdgf//'.TYPE_GROUPE'
    vpofig = sdgf//'.POINTEUR'
    vcafig = sdgf//'.CARFI'
    vsdfig = sdgf//'.CARACSD'
    call jeveuo(vnbfig, 'L', jnbfig)
    call jeveuo(vtyfig, 'L', jtyfig)
    call jeveuo(vcafig, 'L', jcafig)
    call jeveuo(vpofig, 'L', jpofig)
    call jeveuo(vsdfig, 'L', jsdfig)
!
! --------------------------------------------------------------------------------------------------
!   création du champ carele//'.CANBSP'
    nbcp=4+ngmxel
    ASSERT( nbcp .le. nbcmpmax )
!
    ces1 = '&&PMFD01.CES1'
    licmp(1)='NBFIBR'
    licmp(2)='NBGRFI'
    licmp(3)='TYGRFI'
    licmp(4)='NBCARMAX'
!
    if (ngmxel .le. 9) then
        do ig = 1, ngmxel
            call codent(ig, 'G', ki1)
            licmp(4+ig)='NUG'//ki1
        enddo
    else if (ngmxel.le.99) then
        do ig = 1, 9
            call codent(ig, 'G', ki1)
            licmp(4+ig)='NUG'//ki1
        enddo
        do ig = 10, ngmxel
            call codent(ig, 'G', ki2)
            licmp(4+ig)='NUG'//ki2
        enddo
    endif
    call cescre('V', ces1, 'ELEM', noma, 'NBSP_I', nbcp, licmp, [-1], [-1], [-nbcp])
    call jeveuo(ces1//'.CESD', 'L', jces1d)
    call jeveuo(ces1//'.CESL', 'E', jces1l)
    call jeveuo(ces1//'.CESV', 'E', jces1v)
    do ima = 1, nbma
        do icmp = 1, nbcp
            call cesexi('C', jces1d, jces1l, ima, 1, 1, icmp, iad)
            ASSERT(iad.lt.0)
            zl(jces1l-1-iad) = .true.
            zi(jces1v-1-iad) = zi(jmailfib-1+(ima-1)*nbcp+icmp)
        enddo
    enddo
! --------------------------------------------------------------------------------------------------
!   fusion de ces1 avec cesdec
    lichs(1)  = ces1
    lichs(2)  = cesdec
    lcumul(1) = .true.
    lcumul(2) = .true.
    lcoefr(1) = 1.d0
    lcoefr(2) = 1.d0
    ces3 = '&&PMFD01.CES3'
    call cesfus(2, lichs, lcumul, lcoefr, [cbid], .false._1, 'V', ces3)
    call detrsd('CHAM_ELEM_S', ces1)
!
    cel = carele//'.CANBSP'
    call cescel(ces3, ligrmo, 'TOU_INI_ELEM', ' ', 'NON', nncp, 'G', cel, 'F', ibid)
    call detrsd('CHAM_ELEM_S', ces3)
!
! --------------------------------------------------------------------------------------------------
!   Création du champ carele//'.CAFIBR'
    ncarfimax=max(zi(jsdfig+1),zi(jsdfig+2))
!   Vecteur uniquement avec les nb de fibres
    call wkvect('&&PMFD01.NBSP', 'V V I', nbma, jsp)
    do ima = 1, nbma
        zi(jsp-1+ima)=zi(jmailfib+(ima-1)*nbcp)
    enddo
!
    licmp(1)='YG'
    licmp(2)='ZG'
    licmp(3)='AIRE'
    licmp(4)='YP'
    licmp(5)='ZP'
    licmp(6)='GX'
    licmp(7)='NUMGR'
    call cescre('V', ces1, 'ELEM', noma, 'CAFI_R', ncarfimax, licmp, [-1], zi(jsp), [-ncarfimax])
    call jeveuo(ces1//'.CESD', 'L', jces1d)
    call jeveuo(ces1//'.CESL', 'E', jces1l)
    call jeveuo(ces1//'.CESV', 'E', jces1v)
!
!   zi( jmailfib+(ima-1)*nbcp : ... ) = nbfib nbgrfib typfib nbcarmax  NUG[ngmxel]
!                                       0     1       2      3        3+ig
    do ima = 1, nbma
        ipos = jmailfib+(ima-1)*nbcp
        nbgf = zi(ipos+1)
        ispt=0
        do ig = 1, nbgf
            nug    = zi(ipos+3+ig)
            nbfig  = zi(jnbfig-1+nug)
            point  = zi(jpofig-1+nug)
            tyfib  = zi(jtyfig-1+nug)
            ncarfi = zi(jsdfig+tyfib)
            do ifib = 1, nbfig
                ispt=ispt+1
                do icmp = 1, ncarfi
                    call cesexi('C', jces1d, jces1l, ima, 1, ispt, icmp, iad)
!                   ASSERT(iad.lt.0)
                    zl(jces1l-1-iad) = .true.
                    zr(jces1v-1-iad)=zr(jcafig-1+point-1+(ifib-1)*ncarfi+icmp)
                enddo
            enddo
        enddo
    enddo
!
    cel = carele//'.CAFIBR'
    call cescel(ces1, ligrmo, 'TOU_INI_ELEM', ' ', 'NAN', nncp, 'G', cel, 'F', ibid)
    call detrsd('CHAM_ELEM_S', ces1)
!
999 continue
    call jedema()
end subroutine
