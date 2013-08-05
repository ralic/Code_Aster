subroutine cnoaff(noma, nomgd, base, cno)
    implicit none
#include "jeveux.h"
!
#include "asterc/getfac.h"
#include "asterc/getvc8.h"
#include "asterc/getvid.h"
#include "asterc/getvis.h"
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterc/indik8.h"
#include "asterfort/assert.h"
#include "asterfort/cnscno.h"
#include "asterfort/cnscre.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/reliem.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mess.h"
#include "asterfort/vericp.h"
#include "asterfort/wkvect.h"
    character(len=1) :: base
    character(len=8) :: nomgd, noma, cno
! ----------------------------------------------------------------------
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
!
!     COMMANDE   :  CREA_CHAMP/OPERATION:'AFFE', TYPE DE CHAMP : 'NOEU'
!
!     BUT : CREER UN CHAMP AU NOEUD PAR AFFECTATION
!
!     IN     : NOMA (K8) : NOM DU MAILLAGE
!              NOMGD (K8) : NOM DE LA GRANDEUR DU CHAMP A CONSTRUIRE
!              BASE (K1) : VOLATILE ('V') OU GLOBALE ('G')
!              CNO  (K8) : NOM DU CHAMP A CONSTRUIRE
! ----------------------------------------------------------------------
!
!
    integer :: numgd, iav, ibid, ierd, nocc, jcmpt, nbcmpt
    integer :: iocc, nbcmp, nbvar, nbvai, nbvac, nbvak, nbva, vali, jcmp
    integer :: i, iret, ncmp, jtmp, ncmpmx, jcmpmx, jcnsv, jcnsl
    integer :: nbno, nbtou, nbnoe, jlno, jval, icmp, j, ino, jref, nt, nbval
    real(kind=8) :: rbid
    complex(kind=8) :: cbid
    character(len=1) :: tsca
    character(len=3) :: prol0
    character(len=8) :: k8b, kbid, typmcl(4)
    character(len=16) :: motcle(4)
    character(len=19) :: cnos
    character(len=24) :: valk(2), mesnoe, mescmp, prchno, refe
    integer :: iarg
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
!
! --- 1. RECUPERATION
!     ===============
!
!     RECUP : COMPOSANTES DE LA GRANDEUR
    call jenonu(jexnom('&CATA.GD.NOMGD', nomgd), numgd)
    if (numgd .eq. 0) then
        valk (1) = nomgd
        call u2mesg('F', 'UTILITAI6_1', 1, valk, 0,&
                    0, 0, 0.d0)
    else
        call jeveuo(jexnum('&CATA.GD.NOMCMP', numgd), 'L', jcmpmx)
        call jeveuo(jexatr('&CATA.GD.NOMCMP', 'LONCUM'), 'L', iav)
        ncmpmx = zi(iav+numgd) - zi(iav+numgd-1)
    endif
!
!
! --- 2. VERIFICATIONS
!     =================
!
!  -- DANS LE MOT-CLE FACTEUR AFFE
!     ----------------------------
    call getfac('AFFE', nocc)
    do 20 iocc = 1, nocc
        call getvtx('AFFE', 'NOM_CMP', iocc, iarg, 0,&
                    kbid, nbcmp)
        call getvr8('AFFE', 'VALE', iocc, iarg, 0,&
                    rbid, nbvar)
        call getvis('AFFE', 'VALE_I', iocc, iarg, 0,&
                    ibid, nbvai)
        call getvc8('AFFE', 'VALE_C', iocc, iarg, 0,&
                    cbid, nbvac)
        call getvid('AFFE', 'VALE_F', iocc, iarg, 0,&
                    kbid, nbvak)
!
!       => VERIF : NOMBRE DE COMPOSANTES = NOMBRE DE VALEURS
        nbva=nbvar+nbvai+nbvac+nbvak
        if (nbcmp .ne. nbva) then
            vali = iocc
            call u2mesg('F', 'UTILITAI6_3', 0, ' ', 1,&
                        vali, 0, 0.d0)
        endif
!
!       => VERIF : COMPOSANTES FOURNIES INCLUSES DANS LA LISTE DES
!       COMPOSANTES DE LA GRANDEUR
        nbcmp=-nbcmp
        ASSERT(nbcmp.gt.0)
        call wkvect('&&CNOAFF.LISTE_COMP', 'V V K8', nbcmp, jcmp)
        call getvtx('AFFE', 'NOM_CMP', iocc, iarg, nbcmp,&
                    zk8(jcmp), nbcmp)
        do 21 i = 1, nbcmp
            call vericp(zk8(jcmpmx), zk8(jcmp+i-1), ncmpmx, iret)
            if (iret .ne. 0) then
                vali = iocc
                valk (1) = nomgd
                valk (2) = zk8(jcmp+i-1)
                call u2mesg('F', 'UTILITAI6_4', 2, valk, 1,&
                            vali, 0, 0.d0)
            endif
21      continue
        call jedetr('&&CNOAFF.LISTE_COMP')
!
20  end do
!
!
! --- 3. PREPARATION AVANT LA CREATION DU CHAMP
!     =========================================
!
!  -- COMPOSANTES CONCERNEES : ZK8(JCMPT)
!     ----------------------
    mescmp = '&&CNOAFF.MES_CMP'
    call wkvect(mescmp, 'V V K8', ncmpmx, jcmpt)
    do 30 iocc = 1, nocc
        call getvtx('AFFE', 'NOM_CMP', iocc, iarg, 0,&
                    kbid, ncmp)
        ncmp=-ncmp
        call wkvect('&&CNOAFF.TMP', 'V V K8', ncmp, jtmp)
        call getvtx('AFFE', 'NOM_CMP', iocc, iarg, ncmp,&
                    zk8(jtmp), ncmp)
        if (iocc .eq. 1) then
            do 31 i = 1, ncmp
                zk8(jcmpt+i-1)=zk8(jtmp+i-1)
31          continue
            nt=ncmp
        else
            do 32 i = 1, ncmp
                j=indik8(zk8(jcmpt),zk8(jtmp+i-1),1,nt)
                if (j .eq. 0) then
                    zk8(jcmpt+nt)=zk8(jtmp+i-1)
                    nt=nt+1
                endif
32          continue
        endif
        call jedetr('&&CNOAFF.TMP')
30  end do
    nbcmpt=nt
!
!
! --- 4. CREATION DU CHAMP
!     =====================
!
    cnos='&&CNOAFF.CNOS'
    call cnscre(noma, nomgd, nbcmpt, zk8(jcmpt), 'V',&
                cnos)
!
!
! --- 5. REMPLISSAGE DU CHAMP
!     =======================
!
    mesnoe = '&&CNOAFF.MES_NOEUDS'
    motcle(1) = 'NOEUD'
    motcle(2) = 'GROUP_NO'
    motcle(3) = 'MAILLE'
    motcle(4) = 'GROUP_MA'
    typmcl(1) = 'NOEUD'
    typmcl(2) = 'GROUP_NO'
    typmcl(3) = 'MAILLE'
    typmcl(4) = 'GROUP_MA'
!
    call jeveuo(cnos//'.CNSV', 'E', jcnsv)
    call jeveuo(cnos//'.CNSL', 'E', jcnsl)
!
    call dismoi('F', 'NB_NO_MAILLA', noma, 'MAILLAGE', nbno,&
                k8b, iret)
    call dismoi('F', 'TYPE_SCA', nomgd, 'GRANDEUR', ibid,&
                tsca, iret)
!
    do 50 iocc = 1, nocc
!
!  --    NOEUDS CONCERNES
!        ----------------
        call getvtx('AFFE', 'TOUT', iocc, iarg, 1,&
                    kbid, nbtou)
        if (nbtou .ne. 0) then
            nbnoe=nbno
            call jedetr(mesnoe)
            call wkvect(mesnoe, 'V V I', nbnoe, jlno)
            do 51 i = 1, nbnoe
                zi(jlno+i-1)=i
51          continue
        else
            call reliem(' ', noma, 'NU_NOEUD', 'AFFE', iocc,&
                        4, motcle, typmcl, mesnoe, nbnoe)
            call jeveuo(mesnoe, 'L', jlno)
        endif
!
!  --    COMPOSANTES CONCERNEES
!        ----------------------
        call getvtx('AFFE', 'NOM_CMP', iocc, iarg, 0,&
                    kbid, ncmp)
        ncmp=-ncmp
        call jedetr('&&CNOAFF.CMP_IOCC')
        call wkvect('&&CNOAFF.CMP_IOCC', 'V V K8', ncmp, jcmp)
        call getvtx('AFFE', 'NOM_CMP', iocc, iarg, ncmp,&
                    zk8(jcmp), ncmp)
!
!  --    VALEURS
!        -------
        call getvr8('AFFE', 'VALE', iocc, iarg, 0,&
                    rbid, nbvar)
        call getvid('AFFE', 'VALE_F', iocc, iarg, 0,&
                    kbid, nbvak)
        call getvis('AFFE', 'VALE_I', iocc, iarg, 0,&
                    ibid, nbvai)
        call getvc8('AFFE', 'VALE_C', iocc, iarg, 0,&
                    cbid, nbvac)
!
!  --    REMPLISSAGE DES OBJETS .CNSL ET .CNSV
!        -------------------------------------
!
!   -    TYPE "R" :
        if (nbvar .ne. 0) then
            if (tsca .ne. 'R') call u2mess('F', 'UTILITAI6_2')
            nbvar=-nbvar
            call jedetr('&&CNOAFF.VAL_IOCC')
            call wkvect('&&CNOAFF.VAL_IOCC', 'V V R', nbvar, jval)
            call getvr8('AFFE', 'VALE', iocc, iarg, nbvar,&
                        zr(jval), nbvar)
            do 52 i = 1, ncmp
                icmp=indik8(zk8(jcmpt),zk8(jcmp+i-1),1,nbcmpt)
                ASSERT(icmp.gt.0)
                do 53 j = 1, nbnoe
                    ino=zi(jlno+j-1)
                    zr(jcnsv+nbcmpt*(ino-1)+icmp-1)=zr(jval+i-1)
                    zl(jcnsl+nbcmpt*(ino-1)+icmp-1)=.true.
53              continue
52          continue
            call jedetr('&&CNOAFF.VAL_IOCC')
!
!   -    TYPE "I" :
        else if (nbvai.ne.0) then
            if (tsca .ne. 'I') call u2mess('F', 'UTILITAI6_2')
            nbvai=-nbvai
            call jedetr('&&CNOAFF.VAL_IOCC')
            call wkvect('&&CNOAFF.VAL_IOCC', 'V V I', nbvai, jval)
            call getvis('AFFE', 'VALE_I', iocc, iarg, nbvai,&
                        zi(jval), nbval)
            do 54 i = 1, ncmp
                icmp=indik8(zk8(jcmpt),zk8(jcmp+i-1),1,nbcmpt)
                ASSERT(icmp.gt.0)
                do 55 j = 1, nbnoe
                    ino=zi(jlno+j-1)
                    zi(jcnsv+nbcmpt*(ino-1)+icmp-1)=zi(jval+i-1)
                    zl(jcnsl+nbcmpt*(ino-1)+icmp-1)=.true.
55              continue
54          continue
            call jedetr('&&CNOAFF.VAL_IOCC')
!
!   -    TYPE "C" :
        else if (nbvac.ne.0) then
            if (tsca .ne. 'C') call u2mess('F', 'UTILITAI6_2')
            nbvac=-nbvac
            call jedetr('&&CNOAFF.VAL_IOCC')
            call wkvect('&&CNOAFF.VAL_IOCC', 'V V C', nbvac, jval)
            call getvc8('AFFE', 'VALE_C', iocc, iarg, nbvac,&
                        zc(jval), nbvac)
            do 56 i = 1, ncmp
                icmp=indik8(zk8(jcmpt),zk8(jcmp+i-1),1,nbcmpt)
                ASSERT(icmp.gt.0)
                do 57 j = 1, nbnoe
                    ino=zi(jlno+j-1)
                    zc(jcnsv+nbcmpt*(ino-1)+icmp-1)=zc(jval+i-1)
                    zl(jcnsl+nbcmpt*(ino-1)+icmp-1)=.true.
57              continue
56          continue
            call jedetr('&&CNOAFF.VAL_IOCC')
!
!   -    TYPE "F" :
        else if (nbvak.ne.0) then
            if (tsca .ne. 'K') call u2mess('F', 'UTILITAI6_2')
            nbvak=-nbvak
            call jedetr('&&CNOAFF.VAL_IOCC')
            call wkvect('&&CNOAFF.VAL_IOCC', 'V V K8', nbvak, jval)
            call getvid('AFFE', 'VALE_F', iocc, iarg, nbvak,&
                        zk8(jval), nbvak)
            do 58 i = 1, ncmp
                icmp=indik8(zk8(jcmpt),zk8(jcmp+i-1),1,nbcmpt)
                ASSERT(icmp.gt.0)
                do 59 j = 1, nbnoe
                    ino=zi(jlno+j-1)
                    zk8(jcnsv+nbcmpt*(ino-1)+icmp-1)=zk8(jval+i-1)
                    zl(jcnsl+nbcmpt*(ino-1)+icmp-1)=.true.
59              continue
58          continue
            call jedetr('&&CNOAFF.VAL_IOCC')
        endif
!
50  end do
!
!
! --- 5. PASSAGE DU CHAM_NO_S AU CHAM_NO :
!     =============================================
    prchno=' '
    call getvtx(' ', 'PROL_ZERO', 0, iarg, 1,&
                prol0, ibid)
    call cnscno(cnos, prchno, prol0, base, cno,&
                'F', iret)
!
!
! --- 6. FIN
!     =======
    call jedetr(mescmp)
    call jedetr(cnos)
!
    call jedema()
end subroutine
