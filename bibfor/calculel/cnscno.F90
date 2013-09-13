subroutine cnscno(cnsz, prchnz, prol0, basez, cnoz,&
                  kstop, iret)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
! aslint: disable=
    implicit none
#include "jeveux.h"
#include "asterc/indik8.h"
#include "asterfort/assert.h"
#include "asterfort/cmpcha.h"
#include "asterfort/codent.h"
#include "asterfort/crprn2.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/gcncon.h"
#include "asterfort/gnomsd.h"
#include "asterfort/idensd.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelibe.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/pteequ.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=*) :: cnsz, cnoz, basez, prchnz, prol0
    character(len=1) :: kstop
! ------------------------------------------------------------------
! BUT : TRANSFORMER UN CHAM_NO_S (CNSZ) EN CHAM_NO (CNOZ)
! ------------------------------------------------------------------
!     ARGUMENTS:
! CNSZ    IN/JXIN  K19 : SD CHAM_NO_S A TRANSFORMER
! PRCHNZ  IN/JXVAR K19 : SD PROF_CHNO  (OU ' ')
!          SI PRCHNZ EXISTE ON CREE CNOZ CONFORMEMENT A PRCHNZ :
!             => SI CNSZ CONTIENT DES VALEURS QUE L'ON NE SAIT PAS
!                STOCKER DANS PRCHNZ, ON LES "OUBLIE"
!             => SI PRCHNZ EXIGE DES VALEURS QUE L'ON NE TROUVE PAS
!                DANS CNSZ :
!                  - SI PROL0='OUI' : ON PRENDS LA VALEUR "ZERO"
!                  - SI PROL0='NON' : ERREUR <F>
!
!          SI PRCHNZ N'EXISTE PAS ON CREE CNOZ EN FONCTION
!             DU CONTENU DE CNSZ
!             SI PRCHNZ  = ' ' ON CREE UN PROF_CHNO "SOUS-TERRAIN"
!             SI PRCHNZ /= ' ' ON CREE UN PROF_CHNO DE NOM PRCHNZ
! PROL0   IN   K3  :  POUR PROLONGER (OU NON) LE CHAMP PAR "ZERO"
!        /OUI /NON  ( CET ARGUMENT N'EST UTILISE QUE SI PRCHNZ /= ' ')
!        "ZERO" : / 0       POUR LES CHAMPS NUMERIQUES (R/C/I)
!                 / ' '     POUR LES CHAMPS "KN"
!                 / .FALSE. POUR LES CHAMPS DE "L"
!
! BASEZ   IN       K1  : BASE DE CREATION POUR CNOZ : G/V/L
! CNOZ    IN/JXOUT K19 : SD CHAM_NO A CREER
! KSTOP   IN       K1  : COMPORTEMENT EN CAS DE PROBLEME :
!              / 'A' : ON EMET UNE ALARME ET ON REND IRET > 0
!              / 'F' : ON EMET UNE ERREUR FATALE
!              / ' ' : ON N'EMET PAS DE MESSAGE
! IRET    OUT       I  : CODE DE RETOUR :
!              / 0 : OK
!              / 1 : LE CHAM_NO N'A PAS PU ETRE CREE
!----------------------------------------------------------------------
!
    character(len=24) :: noojb
    character(len=24) :: valk(3)
!     -----------------------------------------------------------------
    integer :: icmp, nec, jcnsk, jcnsd, jcnsv, jcnsl, gd, iexi, ncmp, jcorr2
    integer :: reste, iec, code, nbno, ibid, jnucmp, jnucm1, jcnsc, jrefn
    integer :: ncmpmx, jrefe, ncmp1, neq2, jcmpgd, icmp1, k, ieq2, iexi2, nbec
    integer :: jprn2, ino, idg2, ico, jdesc, jdeeq, jvale, iret, n1
    integer :: lshift, nuprf
    character(len=1) :: base, kbid
    character(len=8) :: ma, nomgd, nomno, nomcmp
    logical :: lpchno
    character(len=3) :: tsca
    character(len=19) :: cns, cno, prchno, messag, prnoav
!     -----------------------------------------------------------------
    call jemarq()
!
!
    base=basez
    ASSERT((base.eq.'G') .or. (base.eq.'V'))
    cns=cnsz
    cno=cnoz
!     CALL UTIMSD(6,2,.TRUE.,.TRUE.,CNS,1,' ')
!
    call jeveuo(cns//'.CNSK', 'L', jcnsk)
    call jeveuo(cns//'.CNSD', 'L', jcnsd)
    call jeveuo(cns//'.CNSC', 'L', jcnsc)
    call jeveuo(cns//'.CNSV', 'L', jcnsv)
    call jeveuo(cns//'.CNSL', 'L', jcnsl)
!
    ma=zk8(jcnsk-1+1)
    nomgd=zk8(jcnsk-1+2)
    nbno=zi(jcnsd-1+1)
    ncmp1=zi(jcnsd-1+2)
!
    call dismoi('F', 'NB_EC', nomgd, 'GRANDEUR', nec,&
                kbid, ibid)
    call dismoi('F', 'TYPE_SCA', nomgd, 'GRANDEUR', ibid,&
                tsca, ibid)
    call dismoi('F', 'NB_CMP_MAX', nomgd, 'GRANDEUR', ncmpmx,&
                kbid, ibid)
    call dismoi('F', 'NB_EC', nomgd, 'GRANDEUR', nbec,&
                kbid, ibid)
    call dismoi('F', 'NUM_GD', nomgd, 'GRANDEUR', gd,&
                kbid, ibid)
!
!
!     -- SI CNO EXISTE DEJA, ON LE DETRUIT :
    call detrsd('CHAM_NO', cno)
!
!
!     1- REMPLISSAGE DE .TMP_NUCMP ET .TMP_NUCM1 :
!     --------------------------------------------
    call wkvect('&&CNSCNO.TMP_NUCMP', 'V V I', ncmpmx, jnucmp)
    call wkvect('&&CNSCNO.TMP_NUCM1', 'V V I', ncmp1, jnucm1)
!
    call jeveuo(jexnom('&CATA.GD.NOMCMP', nomgd), 'L', jcmpgd)
    do 10,icmp1=1,ncmp1
    nomcmp=zk8(jcnsc-1+icmp1)
    icmp=indik8(zk8(jcmpgd),nomcmp,1,ncmpmx)
    ASSERT(icmp.gt.0)
    zi(jnucmp-1+icmp)=icmp1
    zi(jnucm1-1+icmp1)=icmp
    10 end do
!
!
    if (prchnz .eq. ' ') then
        if (base .eq. 'G') then
            noojb='12345678.PRCHN00000.PRNO'
            call gnomsd(' ', noojb, 15, 19)
            prchno=noojb(1:19)
        else
            call gcncon('.', prchno)
        endif
    else
        prchno=prchnz
!       -- ON VEUT VERIFIER QUE PRCHNO EST COHERENT AVEC MA ET NOMGD :
        call jeexin(prchno//'.PRNO', iexi)
        if (iexi .gt. 0) then
            valk(1)=cno
            valk(2)=prchno
            call jeexin(prchno//'.REFN', iexi2)
            if (iexi2 .gt. 0) then
!         -- SI PRCHNO VIENT D'UN NUME_EQUA, ON PEUT VERIFIER :
                call jeveuo(prchno//'.REFN', 'L', jrefn)
                if ((zk24(jrefn-1+1).ne.ma) .or. (zk24(jrefn-1+2) .ne.nomgd)) then
!             -- ON ACCEPTE : DEPL_R / DEPL_C
                    if ((nomgd(1:5).eq.'DEPL_') .and. (zk24(jrefn-1+2) (1:5).eq.'DEPL_')) then
                    else
                        call utmess('F', 'CALCULEL4_6', nk=2, valk=valk)
                    endif
                endif
            else
!         -- SINON ON NE PEUT VERIFIER QUE LA LONGUEUR DE .PRNO :
                call jelira(jexnum(prchno//'.PRNO', 1), 'LONMAX', n1)
                if (n1 .ne. nbno*(nbec+2)) then
                    call utmess('F', 'CALCULEL4_6', nk=2, valk=valk)
                endif
            endif
        endif
    endif
!
!
!     2- ON CREE (SI NECESSAIRE) LE PROF_CHNO  :
!     ------------------------------------------
    call jeexin(prchno//'.PRNO', iexi)
    lpchno=(iexi.eq.0)
    if (lpchno) then
!
!       2.1 ON COMPTE LES CMPS PORTEES PAR CNS :
        neq2=0
        do 20,k=1,nbno*ncmp1
        if (zl(jcnsl-1+k)) neq2=neq2+1
20      continue
        if (neq2 .eq. 0) then
            valk(1)=cns
            valk(2)=cno
            messag='CALCULEL2_12'
            goto 70
!
        endif
!
!       2.2 ALLOCATION DES OBJETS :
        call crprn2(prchno, base, nbno, neq2, nec)
!
!       2.3 REMPLISSAGE DE .PRNO :
        call jeveuo(jexnum(prchno//'.PRNO', 1), 'E', jprn2)
        do 40,ino=1,nbno
        do 30,icmp1=1,ncmp1
        if (zl(jcnsl-1+(ino-1)*ncmp1+icmp1)) then
            icmp=zi(jnucm1-1+icmp1)
            iec=(icmp-1)/30+1
            reste=icmp-30*(iec-1)
            code=lshift(1,reste)
            idg2=jprn2-1+((2+nec)*(ino-1))+2+iec
            zi(idg2)=ior(zi(idg2),code)
            zi(jprn2-1+((2+nec)*(ino-1))+2)=zi(jprn2-1+&
                    ((2+nec)*(ino-1))+2)+1
        endif
30      continue
40      continue
!
        ico=0
        do 50,ino=1,nbno
        zi(jprn2-1+((2+nec)*(ino-1))+1)=ico+1
        ico=ico+zi(jprn2-1+((2+nec)*(ino-1))+2)
50      continue
        call jelibe(prchno//'.PRNO')
!
!       2.4 CREATION  DE .DEEQ :
!       POUR DES RAISONS DE PERFORMANCES, IL VAUT MIEUX LE
!       FAIRE PLUTARD.
    endif
!
!
!     4- ON CREE LE .REFE :
!     ------------------------
    call wkvect(cno//'.REFE', base//' V K24', 4, jrefe)
    zk24(jrefe-1+1)=ma
    zk24(jrefe-1+2)=prchno
    zk24(jrefe-1+3)=' '
    zk24(jrefe-1+4)=' '
!
!
!     5- ON CREE LE .DESC :
!     ------------------------
    call wkvect(cno//'.DESC', base//' V I', 2, jdesc)
    call jeecra(cno//'.DESC', 'DOCU', cval='CHNO')
    zi(jdesc-1+1)=gd
    zi(jdesc-1+2)=1
!
!
!     5-BIS ON CREE SI NECESSAIRE LE .DEEQ DU PROF_CHNO
!     ----------------------------------------------------
    if (lpchno) then
        call cmpcha(cno, '&&CNSCNO.NOMCMP', '&&CNSCNO.CORR1', '&&CNSCNO.CORR2', ncmp,&
                    ncmpmx)
        call jeveuo('&&CNSCNO.CORR2', 'L', jcorr2)
!       -- POUR ECONOMISER LA MEMOIRE (PENDANT PTEEQU)
!          ON LIBERE TEMPORAIREMENT .CNSV ET .CNSL :
        call jelibe(cns//'.CNSV')
        call jelibe(cns//'.CNSL')
        call pteequ(prchno, base, neq2, gd, ncmp,&
                    zi(jcorr2))
!
        call jedetr('&&CNSCNO.NOMCMP')
        call jedetr('&&CNSCNO.CORR1')
        call jedetr('&&CNSCNO.CORR2')
        call jeveuo(cns//'.CNSV', 'L', jcnsv)
        call jeveuo(cns//'.CNSL', 'L', jcnsl)
    endif
!
!
!     6- ON CREE ET ON REMPLIT LE .VALE :
!     -----------------------------------
    call jelira(prchno//'.NUEQ', 'LONMAX', neq2)
    call jeveuo(prchno//'.DEEQ', 'L', jdeeq)
    call wkvect(cno//'.VALE', base//' V '//tsca, neq2, jvale)
!
    do 60,ieq2=1,neq2
    ino=zi(jdeeq-1+2*(ieq2-1)+1)
    icmp=zi(jdeeq-1+2*(ieq2-1)+2)
    if (ino*icmp .gt. 0) then
        nomcmp=zk8(jcmpgd-1+icmp)
        icmp1=zi(jnucmp-1+icmp)
!
        if (icmp1 .eq. 0) then
            if (prol0 .eq. 'NON') then
                call jenuno(jexnum(ma//'.NOMNOE', ino), nomno)
                valk(1)=nomcmp
                valk(2)=nomno
                valk(3)=cno
                messag='CALCULEL2_13'
                goto 70
!
            else
                ASSERT(prol0.eq.'OUI')
                if (tsca .eq. 'R') then
                    zr(jvale-1+ieq2)=0.d0
!
                else if (tsca.eq.'C') then
                    zc(jvale-1+ieq2)=(0.d0,0.d0)
!
                else if (tsca.eq.'I') then
                    zi(jvale-1+ieq2)=0
!
                else if (tsca.eq.'L') then
                    zl(jvale-1+ieq2)=.false.
!
                else if (tsca.eq.'K8') then
                    zk8(jvale-1+ieq2)=' '
!
                else
                    ASSERT(.false.)
                endif
                goto 60
!
            endif
        endif
!
        if (zl(jcnsl-1+(ino-1)*ncmp1+icmp1)) then
            if (tsca .eq. 'R') then
                zr(jvale-1+ieq2)=zr(jcnsv-1+(ino-1)*ncmp1+icmp1)
!
            else if (tsca.eq.'C') then
                zc(jvale-1+ieq2)=zc(jcnsv-1+(ino-1)*ncmp1+icmp1)
!
            else if (tsca.eq.'I') then
                zi(jvale-1+ieq2)=zi(jcnsv-1+(ino-1)*ncmp1+icmp1)
!
            else if (tsca.eq.'L') then
                zl(jvale-1+ieq2)=zl(jcnsv-1+(ino-1)*ncmp1+icmp1)
!
            else if (tsca.eq.'K8') then
                zk8(jvale-1+ieq2)=zk8(jcnsv-1+(ino-1)*ncmp1+icmp1)
!
            else
                ASSERT(.false.)
            endif
!
        else
            if (prol0 .eq. 'NON') then
                call jenuno(jexnum(ma//'.NOMNOE', ino), nomno)
                valk(1)=nomcmp
                valk(2)=nomno
                valk(3)=cno
                messag='CALCULEL2_13'
                goto 70
!
            else
                ASSERT(prol0.eq.'OUI')
                if (tsca .eq. 'R') then
                    zr(jvale-1+ieq2)=0.d0
!
                else if (tsca.eq.'C') then
                    zc(jvale-1+ieq2)=(0.d0,0.d0)
!
                else if (tsca.eq.'I') then
                    zi(jvale-1+ieq2)=0
!
                else if (tsca.eq.'L') then
                    zl(jvale-1+ieq2)=.false.
!
                else if (tsca.eq.'K8') then
                    zk8(jvale-1+ieq2)=' '
!
                else
                    ASSERT(.false.)
                endif
                goto 60
!
            endif
        endif
    endif
    60 end do
!
!
!     7 - POUR ECONOMISER LES PROF_CHNO, ON REGARDE SI
!         LE PRECEDENT NE CONVIENDRAIT PAS :
!     -----------------------------------------------------
    if (prchnz .eq. ' ' .and. base .eq. 'G') then
        read (prchno(15:19),'(I5)') nuprf
        if (nuprf .gt. 0) then
            prnoav=prchno
            call codent(nuprf-1, 'D0', prnoav(15:19))
            if (idensd('PROF_CHNO',prchno,prnoav)) then
                call detrsd('PROF_CHNO', prchno)
                zk24(jrefe-1+2)=prnoav
            endif
        endif
    endif
!
!
    iret=0
    goto 80
!
!
!     -- MESSAGES D'ERREUR:
!     ---------------------
70  continue
    ASSERT(kstop.eq.'F' .or. kstop.eq.'A' .or. kstop.eq.' ')
    iret=1
    call detrsd('CHAMP', cno)
    if (kstop .eq. ' ') goto 80
!
    if (messag .eq. 'CALCULEL2_12') then
        call utmess(kstop, 'CALCULEL2_12', nk=2, valk=valk)
    else if (messag.eq.'CALCULEL2_13') then
        call utmess(kstop, 'CALCULEL2_13', nk=3, valk=valk)
    else
        ASSERT(.false.)
    endif
!
!
!
80  continue
    call jedetr('&&CNSCNO.TMP_NUCMP')
    call jedetr('&&CNSCNO.TMP_NUCM1')
    call jedema()
!     CALL UTIMSD(6,2,.TRUE.,.TRUE.,CNO,1,' ')
end subroutine
