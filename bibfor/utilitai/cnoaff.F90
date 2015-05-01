subroutine cnoaff(noma, nomgd, base, cno)
    implicit none
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/indik8.h"
#include "asterfort/assert.h"
#include "asterfort/cnscno.h"
#include "asterfort/cnscre.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvc8.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/reliem.h"
#include "asterfort/utmess.h"
#include "asterfort/vericp.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    character(len=1) :: base
    character(len=8) :: nomgd, noma, cno
! ----------------------------------------------------------------------
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
    integer :: numgd, iav, ibid, nocc, jcmpt, nbcmpt
    integer :: iocc, nbcmp, nbvar, nbvai, nbvac, nbvak, nbva, vali, jcmp
    integer :: i, iret, ncmp,  ncmpmx, jcmpmx, jcnsv, jcnsl
    integer :: nbno, nbtou, nbnoe, jlno, jval, icmp, j, ino, nt, nbval
    character(len=1) :: tsca
    character(len=3) :: prol0
    character(len=8) :: kbid, typmcl(4)
    character(len=16) :: motcle(4)
    character(len=19) :: cnos
    character(len=24) :: valk(2), mesnoe, mescmp, prchno
    character(len=8), pointer :: tmp(:) => null()
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
        call utmess('F', 'UTILITAI6_1', sk=valk(1))
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
    do iocc = 1, nocc
        call getvtx('AFFE', 'NOM_CMP', iocc=iocc, nbval=0, nbret=nbcmp)
        call getvr8('AFFE', 'VALE', iocc=iocc, nbval=0, nbret=nbvar)
        call getvis('AFFE', 'VALE_I', iocc=iocc, nbval=0, nbret=nbvai)
        call getvc8('AFFE', 'VALE_C', iocc=iocc, nbval=0, nbret=nbvac)
        call getvid('AFFE', 'VALE_F', iocc=iocc, nbval=0, nbret=nbvak)
!
!       => VERIF : NOMBRE DE COMPOSANTES = NOMBRE DE VALEURS
        nbva=nbvar+nbvai+nbvac+nbvak
        if (nbcmp .ne. nbva) then
            vali = iocc
            call utmess('F', 'UTILITAI6_3', si=vali)
        endif
!
!       => VERIF : COMPOSANTES FOURNIES INCLUSES DANS LA LISTE DES
!       COMPOSANTES DE LA GRANDEUR
        nbcmp=-nbcmp
        ASSERT(nbcmp.gt.0)
        call wkvect('&&CNOAFF.LISTE_COMP', 'V V K8', nbcmp, jcmp)
        call getvtx('AFFE', 'NOM_CMP', iocc=iocc, nbval=nbcmp, vect=zk8(jcmp))
        do i = 1, nbcmp
            call vericp(zk8(jcmpmx), zk8(jcmp+i-1), ncmpmx, iret)
            if (iret .ne. 0) then
                vali = iocc
                valk (1) = nomgd
                valk (2) = zk8(jcmp+i-1)
                call utmess('F', 'UTILITAI6_4', nk=2, valk=valk, si=vali)
            endif
        end do
        call jedetr('&&CNOAFF.LISTE_COMP')
!
    end do
!
!
! --- 3. PREPARATION AVANT LA CREATION DU CHAMP
!     =========================================
!
!  -- COMPOSANTES CONCERNEES : ZK8(JCMPT)
!     ----------------------
    mescmp = '&&CNOAFF.MES_CMP'
    call wkvect(mescmp, 'V V K8', ncmpmx, jcmpt)
    do iocc = 1, nocc
        call getvtx('AFFE', 'NOM_CMP', iocc=iocc, nbval=0, nbret=ncmp)
        ncmp=-ncmp
        AS_ALLOCATE(vk8=tmp, size=ncmp)
        call getvtx('AFFE', 'NOM_CMP', iocc=iocc, nbval=ncmp, vect=tmp)
        if (iocc .eq. 1) then
            do i = 1, ncmp
                zk8(jcmpt+i-1)=tmp(i)
            end do
            nt=ncmp
        else
            do i = 1, ncmp
                j=indik8(zk8(jcmpt),tmp(i),1,nt)
                if (j .eq. 0) then
                    zk8(jcmpt+nt)=tmp(i)
                    nt=nt+1
                endif
            end do
        endif
        AS_DEALLOCATE(vk8=tmp)
    end do
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
    call dismoi('NB_NO_MAILLA', noma, 'MAILLAGE', repi=nbno)
    call dismoi('TYPE_SCA', nomgd, 'GRANDEUR', repk=tsca)
!
    do iocc = 1, nocc
!
!  --    NOEUDS CONCERNES
!        ----------------
        call getvtx('AFFE', 'TOUT', iocc=iocc, scal=kbid, nbret=nbtou)
        if (nbtou .ne. 0) then
            nbnoe=nbno
            call jedetr(mesnoe)
            call wkvect(mesnoe, 'V V I', nbnoe, jlno)
            do i = 1, nbnoe
                zi(jlno+i-1)=i
            end do
        else
            call reliem(' ', noma, 'NU_NOEUD', 'AFFE', iocc,&
                        4, motcle, typmcl, mesnoe, nbnoe)
            call jeveuo(mesnoe, 'L', jlno)
        endif
!
!  --    COMPOSANTES CONCERNEES
!        ----------------------
        call getvtx('AFFE', 'NOM_CMP', iocc=iocc, nbval=0, nbret=ncmp)
        ncmp=-ncmp
        call jedetr('&&CNOAFF.CMP_IOCC')
        call wkvect('&&CNOAFF.CMP_IOCC', 'V V K8', ncmp, jcmp)
        call getvtx('AFFE', 'NOM_CMP', iocc=iocc, nbval=ncmp, vect=zk8(jcmp))
!
!  --    VALEURS
!        -------
        call getvr8('AFFE', 'VALE', iocc=iocc, nbval=0, nbret=nbvar)
        call getvid('AFFE', 'VALE_F', iocc=iocc, nbval=0, nbret=nbvak)
        call getvis('AFFE', 'VALE_I', iocc=iocc, nbval=0, nbret=nbvai)
        call getvc8('AFFE', 'VALE_C', iocc=iocc, nbval=0, nbret=nbvac)
!
!  --    REMPLISSAGE DES OBJETS .CNSL ET .CNSV
!        -------------------------------------
!
!   -    TYPE "R" :
        if (nbvar .ne. 0) then
            if (tsca .ne. 'R') then
                call utmess('F', 'UTILITAI6_2')
            endif
            nbvar=-nbvar
            call jedetr('&&CNOAFF.VAL_IOCC')
            call wkvect('&&CNOAFF.VAL_IOCC', 'V V R', nbvar, jval)
            call getvr8('AFFE', 'VALE', iocc=iocc, nbval=nbvar, vect=zr(jval))
            do i = 1, ncmp
                icmp=indik8(zk8(jcmpt),zk8(jcmp+i-1),1,nbcmpt)
                ASSERT(icmp.gt.0)
                do j = 1, nbnoe
                    ino=zi(jlno+j-1)
                    zr(jcnsv+nbcmpt*(ino-1)+icmp-1)=zr(jval+i-1)
                    zl(jcnsl+nbcmpt*(ino-1)+icmp-1)=.true.
                end do
            end do
            call jedetr('&&CNOAFF.VAL_IOCC')
!
!   -    TYPE "I" :
        else if (nbvai.ne.0) then
            if (tsca .ne. 'I') then
                call utmess('F', 'UTILITAI6_2')
            endif
            nbvai=-nbvai
            call jedetr('&&CNOAFF.VAL_IOCC')
            call wkvect('&&CNOAFF.VAL_IOCC', 'V V I', nbvai, jval)
            call getvis('AFFE', 'VALE_I', iocc=iocc, nbval=nbvai, vect=zi(jval),&
                        nbret=nbval)
            do i = 1, ncmp
                icmp=indik8(zk8(jcmpt),zk8(jcmp+i-1),1,nbcmpt)
                ASSERT(icmp.gt.0)
                do j = 1, nbnoe
                    ino=zi(jlno+j-1)
                    zi(jcnsv+nbcmpt*(ino-1)+icmp-1)=zi(jval+i-1)
                    zl(jcnsl+nbcmpt*(ino-1)+icmp-1)=.true.
                end do
            end do
            call jedetr('&&CNOAFF.VAL_IOCC')
!
!   -    TYPE "C" :
        else if (nbvac.ne.0) then
            if (tsca .ne. 'C') then
                call utmess('F', 'UTILITAI6_2')
            endif
            nbvac=-nbvac
            call jedetr('&&CNOAFF.VAL_IOCC')
            call wkvect('&&CNOAFF.VAL_IOCC', 'V V C', nbvac, jval)
            call getvc8('AFFE', 'VALE_C', iocc=iocc, nbval=nbvac, vect=zc(jval))
            do i = 1, ncmp
                icmp=indik8(zk8(jcmpt),zk8(jcmp+i-1),1,nbcmpt)
                ASSERT(icmp.gt.0)
                do j = 1, nbnoe
                    ino=zi(jlno+j-1)
                    zc(jcnsv+nbcmpt*(ino-1)+icmp-1)=zc(jval+i-1)
                    zl(jcnsl+nbcmpt*(ino-1)+icmp-1)=.true.
                end do
            end do
            call jedetr('&&CNOAFF.VAL_IOCC')
!
!   -    TYPE "F" :
        else if (nbvak.ne.0) then
            if (tsca .ne. 'K') then
                call utmess('F', 'UTILITAI6_2')
            endif
            nbvak=-nbvak
            call jedetr('&&CNOAFF.VAL_IOCC')
            call wkvect('&&CNOAFF.VAL_IOCC', 'V V K8', nbvak, jval)
            call getvid('AFFE', 'VALE_F', iocc=iocc, nbval=nbvak, vect=zk8(jval))
            do i = 1, ncmp
                icmp=indik8(zk8(jcmpt),zk8(jcmp+i-1),1,nbcmpt)
                ASSERT(icmp.gt.0)
                do j = 1, nbnoe
                    ino=zi(jlno+j-1)
                    zk8(jcnsv+nbcmpt*(ino-1)+icmp-1)=zk8(jval+i-1)
                    zl(jcnsl+nbcmpt*(ino-1)+icmp-1)=.true.
                end do
            end do
            call jedetr('&&CNOAFF.VAL_IOCC')
        endif
!
    end do
!
!
! --- 5. PASSAGE DU CHAM_NO_S AU CHAM_NO :
!     =============================================
    prchno=' '
    call getvtx(' ', 'PROL_ZERO', scal=prol0, nbret=ibid)
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
