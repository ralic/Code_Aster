subroutine celces(celz, basez, cesz)
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
! A_UTIL
    implicit none
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/cescre.h"
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
#include "asterfort/u2mesg.h"
#include "asterfort/wkvect.h"
    character(len=*) :: celz, cesz, basez
! ------------------------------------------------------------------
! BUT : TRANSFORMER UN CHAM_ELEM (CELZ) EN CHAM_ELEM_S (CESZ)
!       LES ELEMENTS DONT LA MAILLE SUPPORT EST TARDIVE SONT
!       IGNORES.
! ------------------------------------------------------------------
!     ARGUMENTS:
! CELZ    IN/JXIN  K19 : SD CHAM_ELEM A TRANSFORMER
! BASEZ   IN       K1  : BASE DE CREATION POUR CESZ : G/V/L
! CESZ    IN/JXOUT K19 : SD CHAM_ELEM_S A CREER
!     ------------------------------------------------------------------
!     VARIABLES LOCALES:
!     ------------------
    character(len=1) :: kbid, base
    character(len=3) :: tsca
    character(len=4) :: typces, kmpic
    character(len=8) :: ma, nomgd
    character(len=19) :: cel, ces, ligrel
    logical :: diff
    integer :: nec, gd, ncmpmx, ibid, nbma, jcorr1, jcelv, jceld
    integer :: iadg, icmp, ncmp, jcesl, jcesv, jcorr2, kcmp
    integer :: ieq, icmp1, igr, iel, ialiel, illiel, numail
    integer :: jcelk, nbpt, nbgr, imolo, jmolo, k, nbgr2
    integer :: ipt, numa, iad, jnbpt, jnbspt, jnbcmp, vali(2)
    integer :: nptmx, nbel, ncmpm, nbspt, ncdyn, ncdymx, lgcata
    integer :: ico, adiel, ispt, jcesd, jlpt, jlcupt, cumu, jnocmp
    character(len=24) :: valk(2)
    real(kind=8) :: rbid
!
    numail(igr,iel) = zi(ialiel-1+zi(illiel+igr-1)+iel-1)
!     ------------------------------------------------------------------
!
    call jemarq()
    cel = celz
    ces = cesz
    base = basez
!
!     -- ON VERIFIE QUE LE CHAM_ELEM N'EST PAS TROP DYNAMIQUE :
!
!     -- SI CES EXISTE DEJA, ON LE DETRUIT :
    call detrsd('CHAM_ELEM_S', ces)
!
!
!     1- CREATION DU CHAM_ELEM_S VIERGE :
!     -------------------------------------------
!
!
!     1.1 CALCUL DE MA,NOMGD,LIGREL,GD,NEC,TSCA,NCMPMX,NBMA :
!     --------------------------------------------------------
    call dismoi('F', 'NOM_MAILLA', cel, 'CHAM_ELEM', ibid,&
                ma, ibid)
    call dismoi('F', 'NOM_GD', cel, 'CHAM_ELEM', ibid,&
                nomgd, ibid)
    call dismoi('F', 'NOM_LIGREL', cel, 'CHAM_ELEM', ibid,&
                ligrel, ibid)
!
!     -- SI CEL N'EST PAS MPI_COMPLET, ON LE COMPLETE :
    call dismoi('F', 'MPI_COMPLET', cel, 'CHAM_ELEM', ibid,&
                kmpic, ibid)
    ASSERT((kmpic.eq.'OUI').or.(kmpic.eq.'NON'))
    if (kmpic .eq. 'NON') call sdmpic('CHAM_ELEM', cel)
!
    call dismoi('F', 'NB_MA_MAILLA', ma, 'MAILLAGE', nbma,&
                kbid, ibid)
!
    call dismoi('F', 'NB_EC', nomgd, 'GRANDEUR', nec,&
                kbid, ibid)
    call dismoi('F', 'NUM_GD', nomgd, 'GRANDEUR', gd,&
                kbid, ibid)
    call dismoi('F', 'TYPE_SCA', nomgd, 'GRANDEUR', ibid,&
                tsca, ibid)
!
!
!     1.2 RECUPERATION DES OBJETS DU CHAM_ELEM ET DU LIGREL :
!     -------------------------------------------------------
    call jeveuo(cel//'.CELK', 'L', jcelk)
    call jeveuo(cel//'.CELV', 'L', jcelv)
    call jeveuo(cel//'.CELD', 'L', jceld)
    call jeveuo(ligrel//'.LIEL', 'L', ialiel)
    call jeveuo(jexatr(ligrel//'.LIEL', 'LONCUM'), 'L', illiel)
    nbgr = zi(jceld-1+2)
!
    call jelira(ligrel//'.LIEL', 'NUTIOC', nbgr2, kbid)
    if (nbgr2 .ne. nbgr) then
        valk(1)=cel
        valk(2)=ligrel
        vali(1)=nbgr
        vali(2)=nbgr2
        call u2mesg('F', 'CALCULEL_19', 2, valk, 2,&
                    vali, 0, rbid)
    endif
!
!
!     1.3 ON CHERCHE LES CMPS PRESENTES DANS LE CHAM_ELEM :
!         NCMP : NOMBRE DE CMPS PRESENTES
!         '&&CELCES.CORR1': CONTIENT LA CORRESPONDANCE ENTRE LE
!                           NUMERO D'1 CMP DU CHAM_ELEM ET LE
!                           NUMERO D'1 CMP DU CHAM_ELEM_S
!         '&&CELCES.NOM_CMP': CONTIENT LES NOMS DES CMPS DU CHAM_ELEM_S
!     -----------------------------------------------------------------
    call cmpcha(cel, '&&CELCES.NOM_CMP', '&&CELCES.CORR1', '&&CELCES.CORR2', ncmp,&
                ncmpmx)
    call jeveuo('&&CELCES.NOM_CMP', 'L', jnocmp)
    call jeveuo('&&CELCES.CORR1', 'L', jcorr1)
    call jeveuo('&&CELCES.CORR2', 'L', jcorr2)
!
!
!     1.4 CALCUL DE  NBPT(IMA), NBSPT(IMA), NBCMP(IMA)
!         CALCUL DE  NPTMX : MAXIMUM DU NOMBRE DE POINTS
!         CALCUL DE  NCDYMX : MAXIMUM DU NOMBRE DE VARI_*
!     ---------------------------------------------------------
    call wkvect('&&CELCES.NBPT', 'V V I', nbma, jnbpt)
    call wkvect('&&CELCES.NBSPT', 'V V I', nbma, jnbspt)
    do 50,numa = 1,nbma
    zi(jnbspt-1+numa) = 1
    50 end do
    call wkvect('&&CELCES.NBCMP', 'V V I', nbma, jnbcmp)
    nptmx = 0
    ncdymx = 0
!
    do 90,igr = 1,nbgr
    nbel = nbelem(ligrel,igr)
    imolo = zi(jceld-1+zi(jceld-1+4+igr)+2)
    if (imolo .eq. 0) goto 90
!
    call jeveuo(jexnum('&CATA.TE.MODELOC', imolo), 'L', jmolo)
    ASSERT(zi(jmolo-1+1).le.3)
    ASSERT(zi(jmolo-1+2).eq.gd)
    diff = (zi(jmolo-1+4).gt.10000)
    nbpt = mod(zi(jmolo-1+4),10000)
    nptmx = max(nptmx,nbpt)
!
!
!       -- CALCUL DE NCMPM : NUMERO MAX DES CMPS PORTEES
!          PAR LES ELEMENTS DU GREL
    ncmpm = 0
    do 70,ipt = 1,nbpt
    k = 1
    if (diff) k = ipt
    iadg = jmolo - 1 + 4 + (k-1)*nec + 1
    do 60,icmp = 1,ncmpmx
    if (exisdg(zi(iadg),icmp)) then
        ncmpm = max(ncmpm,icmp)
    endif
60  continue
70  continue
!
!
    do 80,iel = 1,nbel
    numa = numail(igr,iel)
    if (numa .lt. 0) goto 80
!
!         -- NOMBRE DE POINTS:
    zi(jnbpt-1+numa) = nbpt
!
!         -- NOMBRE DE SOUS-POINTS:
    nbspt = zi(jceld-1+zi(jceld-1+4+igr)+4+4* (iel-1)+1)
    zi(jnbspt-1+numa) = nbspt
!
!         -- NOMBRE DE CMPS:
    ncdyn = zi(jceld-1+zi(jceld-1+4+igr)+4+4* (iel-1)+2)
    ncdyn = max(ncdyn,1)
    ncdymx = max(ncdymx,ncdyn)
    if (nomgd(1:5) .eq. 'VARI_') then
        zi(jnbcmp-1+numa) = ncdyn
    else
        zi(jnbcmp-1+numa) = zi(jcorr1-1+ncmpm)
    endif
!
80  continue
    90 end do
    ASSERT(nptmx.ne.0)
!
!
!
!     1.6 ALLOCATION DE CES :
!     -------------------------------------------
    call dismoi('F', 'TYPE_CHAMP', cel, 'CHAM_ELEM', ibid,&
                typces, ibid)
    if (nomgd(1:5) .eq. 'VARI_') ncmp = -ncdymx
    call cescre(base, ces, typces, ma, nomgd,&
                ncmp, zk8(jnocmp), zi(jnbpt), zi(jnbspt), zi(jnbcmp))
!
!======================================================================
!
!     2- REMPLISSAGE DE CES.CESL ET CES.CESV :
!     -------------------------------------------
    call jeveuo(ces//'.CESD', 'E', jcesd)
    call jeveuo(ces//'.CESL', 'E', jcesl)
    call jeveuo(ces//'.CESV', 'E', jcesv)
!
!
    if (nomgd(1:5) .ne. 'VARI_') then
!     ----------------------------
        call wkvect('&&CELCES.LONG_PT', 'V V I', nptmx, jlpt)
        call wkvect('&&CELCES.LONG_PT_CUMU', 'V V I', nptmx, jlcupt)
        do 170,igr = 1,nbgr
        imolo = zi(jceld-1+zi(jceld-1+4+igr)+2)
        if (imolo .eq. 0) goto 170
!
!
        call jeveuo(jexnum('&CATA.TE.MODELOC', imolo), 'L', jmolo)
        diff = (zi(jmolo-1+4).gt.10000)
        nbpt = mod(zi(jmolo-1+4),10000)
        nbel = nbelem(ligrel,igr)
!
!         -- CALCUL DU NOMBRE DE CMPS POUR CHAQUE POINT
!            ET DU CUMUL SUR LES POINTS PRECEDENTS :
        do 110,ipt = 1,nbpt
        ico = 0
        k = 1
        if (diff) k = ipt
        iadg = jmolo - 1 + 4 + (k-1)*nec + 1
        do 100,kcmp = 1,ncmp
        icmp = zi(jcorr2-1+kcmp)
        if (exisdg(zi(iadg),icmp)) ico = ico + 1
100      continue
        zi(jlpt-1+ipt) = ico
110      continue
!
        cumu = 0
        do 120,ipt = 1,nbpt
        zi(jlcupt-1+ipt) = cumu
        cumu = cumu + zi(jlpt-1+ipt)
120      continue
!
!
        do 160,ipt = 1,nbpt
        k = 1
        if (diff) k = ipt
        iadg = jmolo - 1 + 4 + (k-1)*nec + 1
        ico = 0
        do 150,kcmp = 1,ncmp
        icmp = zi(jcorr2-1+kcmp)
        if (exisdg(zi(iadg),icmp)) then
            ico = ico + 1
            icmp1 = zi(jcorr1-1+icmp)
            ASSERT(icmp1.eq.kcmp)
!
            do 140,iel = 1,nbel
            numa = numail(igr,iel)
            if (numa .lt. 0) goto 140
!
            nbspt = zi(jceld-1+zi(jceld-1+4+igr)+4+4* (iel-1)+1)
            adiel = zi(jceld-1+zi(jceld-1+4+igr)+4+4* (iel-1)+4)
!
            do 130,ispt = 1,nbspt
            call cesexi('S', jcesd, jcesl, numa, ipt,&
                        ispt, icmp1, iad)
            iad = abs(iad)
            zl(jcesl-1+iad) = .true.
!
            ieq = adiel - 1 + nbspt*zi(jlcupt-1+ ipt) + (ispt-1)*zi(jlpt-1+ipt) + ico
!
            if (tsca .eq. 'R') then
                zr(jcesv-1+iad) = zr(jcelv-1+ieq)
            else if (tsca.eq.'I') then
                zi(jcesv-1+iad) = zi(jcelv-1+ieq)
            else if (tsca.eq.'C') then
                zc(jcesv-1+iad) = zc(jcelv-1+ieq)
            else if (tsca.eq.'L') then
                zl(jcesv-1+iad) = zl(jcelv-1+ieq)
            else if (tsca.eq.'K8') then
                zk8(jcesv-1+iad) = zk8(jcelv-1+ ieq)
            else if (tsca.eq.'K16') then
                zk16(jcesv-1+iad) = zk16(jcelv-1+ ieq)
            else if (tsca.eq.'K24') then
                zk24(jcesv-1+iad) = zk24(jcelv-1+ ieq)
            else
                ASSERT(.false.)
            endif
130          continue
140          continue
        endif
150      continue
160      continue
170      continue
!
!
    else
!       -- CAS DE VARI_* :
!       -------------------
        do 220,igr = 1,nbgr
        imolo = zi(jceld-1+zi(jceld-1+4+igr)+2)
        if (imolo .eq. 0) goto 220
!
!
        lgcata = zi(jceld-1+zi(jceld-1+4+igr)+3)
        call jeveuo(jexnum('&CATA.TE.MODELOC', imolo), 'L', jmolo)
        diff = (zi(jmolo-1+4).gt.10000)
        ASSERT(.not.diff)
        nbpt = mod(zi(jmolo-1+4),10000)
        ASSERT(nbpt.eq.lgcata)
        nbel = nbelem(ligrel,igr)
!
        do 210,iel = 1,nbel
        numa = numail(igr,iel)
        if (numa .lt. 0) goto 210
!
        nbspt = zi(jceld-1+zi(jceld-1+4+igr)+4+4* (iel-1)+1)
        ncdyn = max(zi(jceld-1+zi(jceld-1+4+igr)+4+4* (iel-1)+ 2),1)
        adiel = zi(jceld-1+zi(jceld-1+4+igr)+4+4* (iel-1)+4)
        do 200,ipt = 1,nbpt
        do 190,ispt = 1,nbspt
        do 180,icmp = 1,ncdyn
!
        call cesexi('S', jcesd, jcesl, numa, ipt,&
                    ispt, icmp, iad)
        iad = abs(iad)
        zl(jcesl-1+iad) = .true.
!
        ieq = adiel - 1 + ((ipt-1)*nbspt+ispt-1)* ncdyn + icmp
!
        if (tsca .eq. 'R') then
            zr(jcesv-1+iad) = zr(jcelv-1+ieq)
        else
            ASSERT(.false.)
        endif
180      continue
190      continue
200      continue
210      continue
220      continue
    endif
!
!
    call jedetr('&&CELCES.TMP_NUCMP')
    call jedetr('&&CELCES.NBPT')
    call jedetr('&&CELCES.NBSPT')
    call jedetr('&&CELCES.NBCMP')
    call jedetr('&&CELCES.LONG_PT')
    call jedetr('&&CELCES.LONG_PT_CUMU')
    call jedetr('&&CELCES.NOM_CMP')
    call jedetr('&&CELCES.CORR1')
    call jedetr('&&CELCES.CORR2')
    call jedema()
end subroutine
