subroutine pjcorr(nomo2, chbid, cns1z, ces2z, ligrel,&
                  corres, option, nompar, iret)
! person_in_charge: jacques.pellet at edf.fr
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!
    implicit none
#include "jeveux.h"
#include "asterc/indik8.h"
#include "asterfort/alchml.h"
#include "asterfort/assert.h"
#include "asterfort/celces.h"
#include "asterfort/cescre.h"
#include "asterfort/cesexi.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/u2mesk.h"
    character(len=*) :: cns1z, ces2z
    character(len=8) :: nomo2, nompar
    character(len=16) :: corres, option
    character(len=19) :: chbid, ligrel
    integer :: iret
!
! COMMANDE PROJ_CHAMP
!   RECOPIE DES VALEURS PROJETEES AUX NOEUDS DU MAILLAGE 2 PRIME
!     SUR LES POINTS DE GAUSS DU MAILLAGE 2
!   UTILISATION DE LA SD CORRES (TABLEAU AUXILIAIRE .PJEF_EL)
!
!
!
!     ------------------------------------------------------------------
!     VARIABLES LOCALES:
!     ------------------
    character(len=1) :: kbid
    character(len=8) :: nomgd, ma, licmp(2)
    character(len=19) :: cns1, ces2, cel2
    character(len=19) :: cham1s, dcel
    character(len=24) :: valk(5)
    integer :: jpo, ipo, nbma
    integer :: jce2c, jce2l, jce2v, jce2d, jce2k, icmp2
!
    integer :: jcesk, jcesd, jcesv, jcesl
!
    integer :: jcns1c, jcns1l, jcns1v, jcns1k, jcns1d
    integer :: nbno1, nbmax, ncmp1,ncmp2
    integer :: iad2, ier, nval
    integer :: icmp, iad, nbpt, nbsp, icmp1
!
    integer :: ima, ipt, isp, jcesc, jlgrf
!
!     ------------------------------------------------------------------
!
    call jemarq()
    iret = 0
    cns1 = cns1z
    ces2 = ces2z
!
    cel2 = '&&PJCORR.CEL2'
!
!     1- RECUPERATION D'INFORMATIONS DANS CNS1 :
!     ------------------------------------------
    call jeveuo(cns1//'.CNSK', 'L', jcns1k)
    call jeveuo(cns1//'.CNSD', 'L', jcns1d)
    call jeveuo(cns1//'.CNSC', 'L', jcns1c)
    call jeveuo(cns1//'.CNSV', 'L', jcns1v)
    call jeveuo(cns1//'.CNSL', 'L', jcns1l)
    call jelira(cns1//'.CNSC', 'LONMAX', ncmp1, kbid)
!
    nomgd = zk8(jcns1k-1+2)
    nbno1 = zi(jcns1d-1+1)
    nbmax = zi(jcns1d-1+2)
!
!
!------------------------------------------------------------------
!     2- ALLOCATION DE CES2 (ELGA):
!
!
!     -----------------------------
    call detrsd('CHAM_ELEM_S', ces2)
!
    cham1s = 'CHAM1S'
!
    call celces(chbid, 'V', cham1s)
!
!
    call jeveuo(cham1s//'.CESK', 'L', jcesk)
    call jeveuo(cham1s//'.CESD', 'L', jcesd)
    call jeveuo(cham1s//'.CESC', 'L', jcesc)
    call jeveuo(cham1s//'.CESV', 'L', jcesv)
    call jeveuo(cham1s//'.CESL', 'L', jcesl)
!
    call jeveuo(ligrel//'.LGRF', 'L', jlgrf)
    ma = zk8(jlgrf-1+1)
!
!
!
!
    if (nomgd .eq. 'VAR2_R') then
!
        dcel='&&PJCORR'
!
        licmp(1) = 'NPG_DYN'
        licmp(2) = 'NCMP_DYN'
        call cescre('V', dcel, 'ELEM', ma, 'DCEL_I',&
                    2, licmp, -1, -1, -2)
!
        call jeveuo(dcel//'.CESD', 'E', jcesd)
        call jeveuo(dcel//'.CESL', 'E', jcesl)
        call jeveuo(dcel//'.CESV', 'E', jcesv)
!
!
        call dismoi('F', 'NB_MA_MAILLA', nomo2, 'MODELE', nbma,&
                    kbid, ier)
!
        do 40,ima = 1,nbma
        nbpt = zi(jcesd-1+5+4* (ima-1)+1)
        ASSERT(nbpt.eq.1)
        nbsp = zi(jcesd-1+5+4* (ima-1)+2)
        ASSERT(nbsp.eq.1)
!
        do 30,ipt = 1,nbpt
        do 20,isp = 1,nbsp
        call cesexi('C', jcesd, jcesl, ima, ipt,&
                    isp, 1, iad)
        zi(jcesv-1-iad)=0
        zl(jcesl-1-iad)=.true.
        call cesexi('C', jcesd, jcesl, ima, ipt,&
                    isp, 2, iad)
        zi(jcesv-1-iad)=nbmax
        zl(jcesl-1-iad)=.true.
20      continue
30      continue
40      continue
!
        call alchml(ligrel, option, nompar, 'V', cel2,&
                    iret, dcel)
!
        if (iret .eq. 1) then
            valk(1) = nompar
            valk(2) = option
            valk(3) = ligrel
            valk(4) = cel2
            call u2mesk('F', 'CALCULEL_50', 4, valk)
        endif
!
    else
        call alchml(ligrel, option, nompar, 'V', cel2,&
                    iret, ' ')
!
        if (iret .eq. 1) then
            valk(1) = nompar
            valk(2) = option
            valk(3) = ligrel
            valk(4) = cel2
            call u2mesk('F', 'CALCULEL_50', 4, valk)
        endif
!
    endif
!
    call celces(cel2, 'V', ces2)
    call detrsd('CHAM_ELEM', cel2)
!
    call jeveuo(ces2//'.CESD', 'L', jce2d)
    call jeveuo(ces2//'.CESC', 'L', jce2c)
    call jeveuo(ces2//'.CESV', 'E', jce2v)
    call jeveuo(ces2//'.CESL', 'E', jce2l)
    call jeveuo(ces2//'.CESK', 'L', jce2k)
    call jelira(ces2//'.CESC', 'LONMAX', ncmp2, kbid)
!   -- on met les booleens a .false. :
    call jelira(ces2//'.CESL', 'LONMAX', nval, kbid)
    zl(jce2l-1+1:jce2l-1+nval)=.false.
!
!
!
!------------------------------------------------------------------
!     3- REMPLISSAGE DES VALEURS DE CES2 :
!     -------------------------------
    call jeveuo(corres//'.PJEF_EL', 'L', jpo)

    do 92 icmp1 = 1, ncmp1
        icmp2 = indik8( zk8(jce2c),zk8(jcns1c-1+icmp1), 1, ncmp2 )
        if (icmp2.eq.0) goto 92
        ASSERT(zk8(jce2c-1+icmp2).eq.zk8(jcns1c-1+icmp1))
!       -- nbno1 est le nombre de pseudo-noeuds du maillage 2
        do 98 ipo = 1, nbno1
            ima=zi(jpo-1+2*ipo-1)
            ipt= zi(jpo-1+2*ipo)
            call cesexi('C', jce2d, jce2l, ima, ipt,&
                        1, icmp2, iad2)
            ASSERT(iad2.le.0)
            iad2=-iad2
            if (iad2 .eq. 0) goto 98

            zr(jce2v-1+iad2)=zr(jcns1v+(ipo-1)*ncmp1+icmp1-1)
            zl(jce2l-1+iad2)=.true.
98      continue
92  end do


    call jedema()
end subroutine
