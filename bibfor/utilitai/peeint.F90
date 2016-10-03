subroutine peeint(resu, modele, nbocc)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/indik8.h"
#include "asterfort/alchml.h"
#include "asterfort/assert.h"
#include "asterfort/chpchd.h"
#include "asterfort/chsut1.h"
#include "asterfort/cnocns.h"
#include "asterfort/cnscno.h"
#include "asterfort/codent.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismlg.h"
#include "asterfort/dismoi.h"
#include "asterfort/exlim1.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/nopar2.h"
#include "asterfort/peecal.h"
#include "asterfort/reliem.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsorac.h"
#include "asterfort/rsutnu.h"
#include "asterfort/tbajpa.h"
#include "asterfort/tbcrsd.h"
#include "asterfort/utmess.h"
#include "asterfort/utflmd.h"
#include "asterfort/varinonu.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    integer :: nbocc
    character(len=8) :: modele
    character(len=19) :: resu
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     OPERATEUR   POST_ELEM
!     TRAITEMENT DU MOT CLE-FACTEUR "INTEGRALE"
!     ------------------------------------------------------------------
!
    integer :: iret, nbcmp, nzero, ibid, nbordr, iocc, jnuma, nbma, ncmpm
    integer :: jcmp, n1, numa, nr, np, nc, im, ni, no, jno, jin, numo, i, ivari
    integer :: nbgma, jgma, nma, jma, igm, nbpa1, nbpa2, nn, inum, nli, nlo
    integer :: nd, ib, nucmp, tord(1)
    parameter(nzero=0,nbpa1=4,nbpa2=2)
    real(kind=8) :: prec, inst
    complex(kind=8) :: cbid
    character(len=8) :: k8b, kbid, mailla, resuco, crit, nopar
    character(len=4) :: tych, ki, exirdm
    character(len=8) :: nomgd, tout, grpma, maille, typpa1(nbpa1), typpa2(nbpa2)
    parameter(tout='TOUT',grpma='GROUP_MA',maille='MAILLE')
    character(len=16) :: nompa1(nbpa1), nompa2(nbpa2), optio2
    character(len=19) :: knum, cham, kins, lisins, chamg, celmod
    character(len=19) :: ligrel, cespoi, tmpcha
    character(len=19) :: cham2, cham3, chamtm
    character(len=24) :: nomcha, valk2(5)
    logical :: exiord, toneut

    integer :: jmesma, nbmai,nbmaf,indma,iresma,vali,jvari
    character(len=8) :: typmcl(3), infoma
    character(len=16) :: motcle(3)
    character(len=24) :: mesmai, mesmaf,mesma2

    character(len=8), pointer :: cmp1(:) => null()
    character(len=8), pointer :: cmp2(:) => null()
    character(len=8), pointer :: cmp_init(:) => null()
    character(len=8), pointer :: cnsc(:) => null()

    data nompa1/'NOM_CHAM','NUME_ORDRE','INST','VOL'/
    data typpa1/'K16','I','R','R'/
    data nompa2/'CHAM_GD','VOL'/
    data typpa2/'K16','R'/
!     ------------------------------------------------------------------
!
    call jemarq()
!
!     --- RECUPERATION DU MAILLAGE ET DU NOMBRE DE MAILLES
    call dismoi('NOM_MAILLA', modele, 'MODELE', repk=mailla)
    call dismoi('NB_MA_MAILLA', mailla, 'MAILLAGE', repi=nbma)
!
    ligrel='&&PEEINT.LIGREL'
    cespoi='&&PEEINT.CESPOI'
    mesmai='&&PEEINT.MES_MAILLES'
    mesmaf='&&PEEINT.MAILLES_FILTRE'
    knum='&&PEEINT.NUME_ORDRE'
    kins='&&PEEINT.INST'
!
!     --- RECUPERATION DU RESULTAT ET DU NUMERO D'ORDRE
    call getvid(' ', 'RESULTAT', scal=resuco, nbret=nr)
    call getvr8(' ', 'PRECISION', scal=prec, nbret=np)
    call getvtx(' ', 'CRITERE', scal=crit, nbret=nc)
    call getvr8(' ', 'INST', nbval=0, nbret=ni)
    call getvis(' ', 'NUME_ORDRE', nbval=0, nbret=no)
    call getvid(' ', 'LIST_INST', nbval=0, nbret=nli)
    call getvid(' ', 'LIST_ORDRE', nbval=0, nbret=nlo)
    call getvid(' ', 'CHAM_GD', scal=chamg, nbret=nd)
!
!     --- CREATION DE LA TABLE
    call tbcrsd(resu, 'G')
    if (nr .ne. 0) then
        call tbajpa(resu, nbpa1, nompa1, typpa1)
    else
        call tbajpa(resu, nbpa2, nompa2, typpa2)
    endif
!
    exiord=.false.
    toneut=.false.
!
    if (nd .ne. 0) then
        nbordr = 1
        call wkvect(knum, 'V V I', nbordr, jno)
        zi(jno) = 1
        exiord=.true.
    else
        if (no .ne. 0) then
            exiord=.true.
            nbordr=-no
            call wkvect(knum, 'V V I', nbordr, jno)
            call getvis(' ', 'NUME_ORDRE', nbval=nbordr, vect=zi(jno), nbret=iret)
        endif
!
        if (ni .ne. 0) then
            nbordr=-ni
            call wkvect(kins, 'V V R', nbordr, jin)
            call getvr8(' ', 'INST', nbval=nbordr, vect=zr(jin), nbret=iret)
        endif
!
        if (nli .ne. 0) then
            call getvid(' ', 'LIST_INST', scal=lisins, nbret=iret)
            call jeveuo(lisins // '.VALE', 'L', jin)
            call jelira(lisins // '.VALE', 'LONMAX', nbordr)
        endif
!
        if (nlo .ne. 0) then
            exiord=.true.
            call getvid(' ', 'LIST_ORDRE', scal=lisins, nbret=iret)
            call jeveuo(lisins // '.VALE', 'L', jno)
            call jelira(lisins // '.VALE', 'LONMAX', nbordr)
        endif
!
        nn=nlo+nli+no+ni
        if (nn .eq. 0) then
            exiord=.true.
            call rsutnu(resuco, ' ', 0, knum, nbordr,&
                        prec, crit, iret)
            call jeveuo(knum, 'L', jno)
        endif
    endif
!
!
!     --- ON PARCOURT LES OCCURENCES DU MOT CLE 'INTEGRALE':
!     =====================================================
    if (nr .eq. 0) then
        tmpcha='TMP_CHAMP_GD'
        call copisd('CHAMP', 'V', chamg, tmpcha)
    endif
!
    do iocc = 1, nbocc

!       -- creation d'un ligrel reduit sur les mailles d'interet :
!       ----------------------------------------------------------
        call wkvect('&&PEEINT.IND.MAILLE', 'V V I', nbma, indma)
!
        motcle(1) = 'GROUP_MA'
        motcle(2) = 'MAILLE'
        motcle(3) = 'TOUT'
        typmcl(1) = 'GROUP_MA'
        typmcl(2) = 'MAILLE'
        typmcl(3) = 'TOUT'
!
!       -- mailles fournies par l'utilisateur:
        call reliem(' ', mailla, 'NU_MAILLE', 'INTEGRALE', iocc,&
                    3, motcle, typmcl, mesmai, nbmai)
!
!       -- mailles filtrees en fonction de la dimension pour
!          etre homogene(2d ou 3d)(mot cle type_maille)
        call getvtx('INTEGRALE', 'TYPE_MAILLE', iocc=iocc, scal=infoma, nbret=iret)
!
        if (iret .ne. 0) then
            iresma = 0
            if (infoma .eq. '1D') iresma=1
            if (infoma .eq. '2D') iresma=2
            if (infoma .eq. '3D') iresma=3
            ASSERT(iresma.ne.0)
            call utflmd(mailla, mesmai, nbmai, iresma, ' ',nbmaf, mesmaf)
            if (nbmaf .gt. 0) then
                vali= nbmai-nbmaf
                if (vali .ne.0) call utmess ('A','PREPOST2_7', si=vali)
            else
                call utmess('F', 'PREPOST2_8')
            endif
            mesma2=mesmaf
            nbmai=nbmaf
        else
            mesma2=mesmai
        endif

        call jeveuo(mesma2, 'L', jmesma)
        do i = 1, nbmai
            zi(indma+zi(jmesma+i-1)-1)=1
        end do

        call exlim1(zi(jmesma), nbmai, modele, 'V', ligrel)
!
!       -- verification si on va traiter des elements de structure
        call dismlg('EXI_RDM', ligrel, ibid, exirdm, iret)
        if (exirdm .eq. 'OUI') then
            call utmess('F', 'UTILITAI8_60')
        endif
!
!       -- COMPOSANTES DU POST-TRAITEMENT
        call getvtx('INTEGRALE', 'NOM_CMP', iocc=iocc, nbval=0, vect=k8b, nbret=nbcmp)
        nbcmp=-nbcmp
        if (nbcmp.eq.0) then
            ivari=1
            call getvtx('INTEGRALE', 'NOM_VARI', iocc=iocc, nbval=0, vect=k8b, nbret=nbcmp)
            nbcmp=-nbcmp
            ASSERT(nbcmp.gt.0)
            call wkvect('&&PEEINT.CMP', 'V V K8', nbmai*nbcmp, jcmp)
            call wkvect('&&PEEINT.NVARI', 'V V K16', nbcmp, jvari)
            call getvtx('INTEGRALE', 'NOM_VARI', iocc=iocc, nbval=nbcmp, vect=zk16(jvari),&
                    nbret=iret)
            call varinonu(modele, ' ', resuco, nbmai, zi(jmesma), nbcmp, zk16(jvari), zk8(jcmp))
        else
            ivari=0
            call wkvect('&&PEEINT.CMP', 'V V K8', nbcmp, jcmp)
            call getvtx('INTEGRALE', 'NOM_CMP', iocc=iocc, nbval=nbcmp, vect=zk8(jcmp),&
                    nbret=iret)
        endif
!
        AS_ALLOCATE(vk8=cmp_init, size=nbcmp)
        do i = 1, nbcmp
            if (ivari.eq.0) then
                cmp_init(i)=zk8(jcmp+i-1)
            else
                cmp_init(i)=zk16(jvari+i-1)(1:8)
            endif
        end do


!     --- BOUCLE SUR LES NUMEROS D'ORDRE:
!     ===================================
!
        do inum = 1, nbordr
!
!         --- SI RESULTAT ---
!         --- NUME_ORDRE, INST ---
            if (nr .ne. 0) then
                if (exiord) then
                    numo=zi(jno+inum-1)
                    call rsadpa(resuco, 'L', 1, 'INST', numo,&
                                0, sjv=jin, styp=kbid)
                    inst=zr(jin)
                else
                    inst=zr(jin+inum-1)
                    call rsorac(resuco, 'INST', 0, zr(jin+inum-1), kbid,&
                                cbid, prec, crit, tord, nbordr,&
                                iret)
                    numo=tord(1)
                endif
!
!         --- CHAMP DU POST-TRAITEMENT
                call getvtx('INTEGRALE', 'NOM_CHAM', iocc=iocc, scal=nomcha, nbret=iret)
                if (iret .eq. 0)  call utmess('F', 'POSTELEM_4')
                if (nomcha.eq.'FORC_NODA' .or. nomcha.eq.'REAC_NODA') call utmess('F', 'POSTELEM_5')
!
                call rsexch('F', resuco, nomcha, numo, cham2,&
                            iret)
!
            else
!         --- SI CHAM_GD ---
                numo = nbordr
                nomcha = chamg
                cham2 = tmpcha
!
            endif
!
            call dismoi('TYPE_CHAMP', cham2, 'CHAMP', repk=tych, arret='C',&
                        ier=iret)
            call dismoi('NOM_GD', cham2, 'CHAMP', repk=nomgd, arret='C',&
                        ier=iret)
!
            if (nomgd(6:6) .eq. 'C') goto 10
!
            if (tych(1:2) .ne. 'EL') then
!
!          --- 1. TRANSFORMATION DU CHAMP EN CHAMP NEUTRE:
!              - CHANGEMENT DE LA GRANDEUR EN NEUT_R
!              - CHAMGEMENT DES COMPOSANTES EN X1,X2,X3,...
                toneut=.true.
                chamtm='&&PEEINT.CHS1'
                call cnocns(cham2, 'V', chamtm)
                call jeveuo(chamtm//'.CNSC', 'L', vk8=cnsc)
                call jelira(chamtm//'.CNSC', 'LONMAX', ncmpm)
                AS_ALLOCATE(vk8=cmp1, size=ncmpm)
                AS_ALLOCATE(vk8=cmp2, size=ncmpm)
                do i = 1, ncmpm
                    call codent(i, 'G', ki)
                    cmp2(i)='X'//ki(1:len(ki))
                    cmp1(i)=cnsc(i)
                end do
                call chsut1(chamtm, 'NEUT_R', ncmpm, cmp1, cmp2,&
                            'V', chamtm)
                cham3='&&PEEINT.CHAM_3'
                call cnscno(chamtm, ' ', 'NON', 'V', cham3,&
                            'F', ibid)
                call detrsd('CHAM_NO_S', chamtm)
!
!           --- 2. CHANGEMENT DE DISCRETISATION : NOEU -> ELGA
                optio2 ='TOU_INI_ELGA'
                call dismoi('NOM_GD', cham3, 'CHAMP', repk=nomgd, arret='C',&
                            ier=iret)
                nopar = nopar2(optio2,nomgd,'OUT')
                celmod = '&&PEEINT.CELMOD'
                call alchml(ligrel, optio2, nopar, 'V', celmod,&
                            ib, ' ')
                if (ib .ne. 0) then
                    valk2(1)=ligrel
                    valk2(2)=nopar
                    valk2(3)=optio2
                    call utmess('F', 'UTILITAI3_23', nk=3, valk=valk2)
                endif
                cham='&&CHPCHD.CHAM'
                call chpchd(cham3, 'ELGA', celmod, 'OUI', 'V',&
                            cham)
                call detrsd('CHAMP', celmod)
                call detrsd('CHAMP', cham3)
!
            else
                cham=cham2
            endif
!
            call dismoi('TYPE_CHAMP', cham, 'CHAMP', repk=tych, arret='C',&
                        ier=iret)
!
            if (toneut) then
                do i = 1, nbcmp
                    nucmp=indik8(cmp1,cmp_init(i),1,ncmpm)
                    zk8(jcmp+i-1)=cmp2(nucmp)
                end do
            endif
!
!         --- CALCUL ET STOCKAGE DES MOYENNE : MOT-CLE 'TOUT'
            call getvtx('INTEGRALE', 'TOUT', iocc=iocc, nbval=nzero, vect=k8b,&
                        nbret=iret)
            if (iret .ne. 0) then
                call peecal(tych, resu, nomcha, tout, tout,&
                            modele, nr, cham, nbcmp, zk8(jcmp),&
                            cmp_init, numo, inst, iocc, ligrel, cespoi)
            endif
!
!         --- CALCUL ET STOCKAGE DES MOYENNES : MOT-CLE 'GROUP_MA'
            call getvtx('INTEGRALE', 'GROUP_MA', iocc=iocc, nbval=nzero, vect=k8b,&
                        nbret=n1)
            if (n1 .ne. 0) then
                nbgma=-n1
                call wkvect('&&PEEINT_GMA', 'V V K24', nbgma, jgma)
                call getvtx('INTEGRALE', 'GROUP_MA', iocc=iocc, nbval=nbgma, vect=zk24(jgma),&
                            nbret=n1)
                do igm = 1, nbgma
                    call jelira(jexnom(mailla//'.GROUPEMA', zk24(jgma+ igm-1)), 'LONMAX', nma,&
                                k8b)
                    call jeveuo(jexnom(mailla//'.GROUPEMA', zk24(jgma+ igm-1)), 'L', jnuma)
                    call peecal(tych, resu, nomcha, grpma, zk24(jgma+igm- 1),&
                                modele, nr, cham, nbcmp, zk8(jcmp),&
                                cmp_init, numo, inst, iocc, ligrel, cespoi)
                end do
                call jedetr('&&PEEINT_GMA')
            endif
!
!         --- CALCUL ET STOCKAGE DES MOYENNES : MOT-CLE 'MAILLE'
            call getvtx('INTEGRALE', 'MAILLE', iocc=iocc, nbval=nzero, vect=k8b,&
                        nbret=n1)
            if (n1 .ne. 0) then
                nma=-n1
                call wkvect('&&PEEINT_MAIL', 'V V K8', nma, jma)
                call getvtx('INTEGRALE', 'MAILLE', iocc=iocc, nbval=nma, vect=zk8(jma),&
                            nbret=n1)
                do im = 1, nma
                    call jenonu(jexnom(mailla//'.NOMMAI', zk8(jma+im-1) ), numa)
                    call peecal(tych, resu, nomcha, maille, zk8(jma+im-1),&
                                modele, nr, cham, nbcmp, zk8(jcmp),&
                                cmp_init, numo, inst, iocc, ligrel, cespoi)
                end do
                call jedetr('&&PEEINT_MAIL')
            endif
!
            AS_DEALLOCATE(vk8=cmp1)
            AS_DEALLOCATE(vk8=cmp2)
!
        end do
!
 10     continue
        call detrsd('LIGREL', ligrel)
        call detrsd('CHAM_ELEM_S', cespoi)
        call jedetr('&&PEEINT.IND.MAILLE')
        call jedetr(cespoi//'.PDSM')
        call jedetr('&&PEEINT.MES_MAILLES')
        call jedetr('&&PEEINT.MAILLES_FILTRE')
        call jedetr('&&PEEINT.CMP')
        AS_DEALLOCATE(vk8=cmp_init)
    end do
!
    if (nr .eq. 0) then
        call detrsd('CHAMP', tmpcha)
    endif
!
    call jedema()
!
end subroutine
