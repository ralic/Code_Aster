subroutine sscgma(ma, nbgmp, nbgmin)
    implicit none
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
! ----------------------------------------------------------------------
!     BUT: TRAITER LE MOT CLEF CREA_GROUP_MA
!          DE L'OPERATEUR: DEFI_GROUP
!
!     IN:
!          MA    : NOM DU MAILLAGE
!          NBGMP : NOMBRE DE GROUP_MA A CREER
!     ------------------------------------------------------------------
!
#include "jeveux.h"
!
#include "asterc/getres.h"
#include "asterc/getvis.h"
#include "asterc/getvtx.h"
#include "asterfort/assert.h"
#include "asterfort/cgmaal.h"
#include "asterfort/cgmaap.h"
#include "asterfort/cgmaba.h"
#include "asterfort/cgmacy.h"
#include "asterfort/cgmafn.h"
#include "asterfort/cgmasp.h"
#include "asterfort/cgmaxf.h"
#include "asterfort/cgmftm.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvem.h"
#include "asterfort/infniv.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/utlisi.h"
#include "asterfort/wkvect.h"
!
    character(len=8) :: ma, noma, kbid, kpos, nom1, tout
    character(len=8) :: alarm, tyma
    character(len=16) :: concep, cmd, option
    character(len=24) :: lisma, nogma, nogma2
    character(len=24) :: valk(2)
    character(len=132) :: card
    integer :: iarg
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, iagm1, iagm2, ialii1, ialii2, ialik8, ibid
    integer :: idlima, ier, ierd, ierr, ifm, igm, igm1
    integer :: igm2, ii, iii, ili1, ili2, ilmak8, im1
    integer :: ima, ind1, ind2, iocc, ireste, jgma, jjj
    integer :: jlisma, jmail, jmail2, kkk, maxcol, n, n1
    integer :: n2, n3, n4, n5, n6, n6a, n6b
    integer :: n7, n8, nalar, nb, nbcol, nbgmin, nbgmp
    integer :: nbgnaj, nbgrmn, nbid, nbis, nbk8, nbline, nbma
    integer :: nbmat, niv, ntrou, ntyp, num
!-----------------------------------------------------------------------
    call jemarq()
!
!     RECUPERATION DU NIVEAU D'IMPRESSION
!     -----------------------------------
    call infniv(ifm, niv)
!
    call getres(kbid, concep, cmd)
    lisma = '&&SSCGMA.LISTE_MAILLES'
    call jelira(ma//'.GROUPEMA', 'NMAXOC', nbgrmn)
    nbis = nbgrmn
    nbk8 = nbgrmn
    call wkvect('&&SSCGMA.LIK8', 'V V K24', nbk8, ialik8)
    call wkvect('&&SSCGMA.LII1', 'V V I', nbis, ialii1)
    call wkvect('&&SSCGMA.LII2', 'V V I', nbis, ialii2)
    call dismoi('F', 'NB_MA_MAILLA', ma, 'MAILLAGE', nbmat,&
                kbid, ierd)
!
    call getvtx(' ', 'ALARME', 1, iarg, 1,&
                alarm, nalar)
!
    nbgnaj = 0
    do 210,iocc = 1,nbgmp
!
    call getvtx('CREA_GROUP_MA', 'NOM', iocc, iarg, 1,&
                nogma, n1)
!
    call jenonu(jexnom(ma//'.GROUPEMA', nogma), ibid)
    if (ibid .gt. 0) then
        call u2mesk('F', 'ALGELINE3_7', 1, nogma)
    endif
!
    call getvem(ma, 'MAILLE', 'CREA_GROUP_MA', 'MAILLE', iocc,&
                iarg, 0, kbid, n2)
    call getvtx('CREA_GROUP_MA', 'INTERSEC', iocc, iarg, 0,&
                kbid, n3)
    call getvtx('CREA_GROUP_MA', 'UNION', iocc, iarg, 0,&
                kbid, n4)
    call getvtx('CREA_GROUP_MA', 'DIFFE   ', iocc, iarg, 0,&
                kbid, n5)
    call getvem(ma, 'GROUP_MA', 'CREA_GROUP_MA', 'GROUP_MA', iocc,&
                iarg, 0, kbid, n6)
    call getvtx('CREA_GROUP_MA', 'OPTION', iocc, iarg, 0,&
                option, n7)
    call getvtx('CREA_GROUP_MA', 'TOUT', iocc, iarg, 0,&
                tout, n8)
    n2 = -n2
    n3 = -n3
    n4 = -n4
    n5 = -n5
    n6 = -n6
    n7 = -n7
    n8 = -n8
!
!
!       -- MOT CLEF TOUT:
!       -------------------
    if (n8 .gt. 0) then
        nbma=nbmat
        call wkvect(lisma, 'V V I', nbma, jlisma)
        do 10,ima = 1,nbmat
        zi(jlisma-1+ima) = ima
10      continue
        goto 219
    endif
!
!
!       -- MOT CLEF MAILLE:
!       -------------------
    if (n2 .gt. 0) then
        call wkvect('&&SSCGMA.L_MAILLE', 'V V K8', n2, ilmak8)
        call getvem(ma, 'MAILLE', 'CREA_GROUP_MA', 'MAILLE', iocc,&
                    iarg, n2, zk8(ilmak8), n1)
        call wkvect('&&SSCGMA.MAILLE', 'V V I', n2, jmail)
        call dismoi('F', 'NB_MA_MAILLA', ma, 'MAILLAGE', nbmat,&
                    kbid, ierd)
        call wkvect('&&SSCGMA.MAILLE2', 'V V I', nbmat, jmail2)
        nbma = 0
        ier = 0
        do 20 im1 = 1, n2
            nom1 = zk8(ilmak8+im1-1)
            call jenonu(jexnom(ma//'.NOMMAI', nom1), num)
            if (num .eq. 0) then
                ier = ier + 1
                call u2mesk('E', 'SOUSTRUC_31', 1, nom1)
                goto 20
            endif
            zi(jmail2-1+num) = zi(jmail2-1+num) + 1
            if (zi(jmail2-1+num) .eq. 2) then
                valk(1) = nom1
                valk(2) = nogma
                call u2mesk('A', 'SOUSTRUC_32', 2, valk)
                goto 20
            endif
            nbma = nbma + 1
            zi(jmail+nbma-1) = num
20      continue
        if (ier .ne. 0) ASSERT(.false.)
        call wkvect(lisma, 'V V I', nbma, jlisma)
        do 30,ima = 0,nbma - 1
        zi(jlisma+ima) = zi(jmail+ima)
30      continue
        call jedetr('&&SSCGMA.MAILLE')
        call jedetr('&&SSCGMA.MAILLE2')
        call jedetr('&&SSCGMA.L_MAILLE')
        goto 219
    endif
!
!
!       -- MOT CLEF GROUP_MA:
!       ---------------------
    if (n6 .gt. 0) then
        call getvem(ma, 'GROUP_MA', 'CREA_GROUP_MA', 'GROUP_MA', iocc,&
                    iarg, 1, nogma2, nbid)
        call getvtx('CREA_GROUP_MA', 'POSITION', iocc, iarg, 0,&
                    kpos, n6b)
        call jenonu(jexnom(ma//'.GROUPEMA', nogma2), igm2)
        call jelira(jexnum(ma//'.GROUPEMA', igm2), 'LONUTI', ili2)
        call jeveuo(jexnum(ma//'.GROUPEMA', igm2), 'L', iagm2)
        ind1 = 0
        ind2 = 0
        if (n6b .eq. 0) then
            call getvis('CREA_GROUP_MA', 'NUME_INIT', iocc, iarg, 1,&
                        ind1, n6a)
            if (n6a .eq. 0) ind1 = 1
            call getvis('CREA_GROUP_MA', 'NUME_FIN', iocc, iarg, 1,&
                        ind2, n6a)
            if (n6a .eq. 0) ind2 = ili2
            if (ind2 .lt. ind1) call u2mess('F', 'SOUSTRUC_33')
            if (ili2 .lt. ind2) call u2mess('F', 'SOUSTRUC_34')
            n6a = ind2 - ind1 + 1
        else
            n6a = 1
        endif
        call wkvect(lisma, 'V V I', n6a, jlisma)
        nbma=n6a
        if (n6b .eq. 0) then
            n = ind2 - ind1 + 1
            do 40 ii = 1, n
                zi(jlisma-1+ii) = zi(iagm2-2+ind1+ii)
40          continue
            goto 219
        endif
        call getvtx('CREA_GROUP_MA', 'POSITION', iocc, iarg, 1,&
                    kpos, n6b)
        if (kpos .eq. 'INIT') then
            zi(jlisma) = zi(iagm2)
        else if (kpos.eq.'FIN') then
            ii = ili2
            zi(jlisma) = zi(iagm2+ii-1)
        else if (kpos.eq.'MILIEU') then
            ii = (ili2+1)/2
            zi(jlisma) = zi(iagm2+ii-1)
        endif
        goto 219
    endif
!
!
!       -- MOT CLEF INTER:
!       -------------------
    if (n3 .gt. 0) then
        call getvtx('CREA_GROUP_MA', 'INTERSEC', iocc, iarg, n3,&
                    zk24(ialik8), nbid)
        do 50,igm = 1,n3
        call jenonu(jexnom(ma//'.GROUPEMA', zk24(ialik8-1+igm)), igm2)
        if (igm2 .eq. 0) call u2mesk('F', 'SOUSTRUC_35', 1, zk24(ialik8-1+igm))
50      continue
!
        call jenonu(jexnom(ma//'.GROUPEMA', zk24(ialik8)), igm1)
        call jelira(jexnum(ma//'.GROUPEMA', igm1), 'LONUTI', ili1)
        call jeveuo(jexnum(ma//'.GROUPEMA', igm1), 'L', iagm1)
        if (ili1 .gt. nbis) then
            nbis = 2*ili1
            call jedetr('&&SSCGMA.LII1')
            call jedetr('&&SSCGMA.LII2')
            call wkvect('&&SSCGMA.LII1', 'V V I', nbis, ialii1)
            call wkvect('&&SSCGMA.LII2', 'V V I', nbis, ialii2)
        endif
        n = ili1
        do 60 ii = 1, n
            zi(ialii1-1+ii) = zi(iagm1-1+ii)
60      continue
!
        do 80,igm = 2,n3
        call jenonu(jexnom(ma//'.GROUPEMA', zk24(ialik8-1+igm)), igm2)
        call jelira(jexnum(ma//'.GROUPEMA', igm2), 'LONUTI', ili2)
        call jeveuo(jexnum(ma//'.GROUPEMA', igm2), 'L', iagm2)
        call utlisi('INTER', zi(ialii1), n, zi(iagm2), ili2,&
                    zi( ialii2), nbis, ntrou)
        n = ntrou
        do 70 ii = 1, n
            zi(ialii1-1+ii) = zi(ialii2-1+ii)
70      continue
80      continue
!
        if (n .eq. 0) then
            if (alarm .eq. 'OUI') then
                call u2mesk('A', 'SOUSTRUC_36', 1, nogma)
            endif
        else
            call wkvect(lisma, 'V V I', n, jlisma)
            nbma=n
            do 90 ii = 1, n
                zi(jlisma-1+ii) = zi(ialii1-1+ii)
90          continue
        endif
        goto 219
    endif
!
!
!       -- MOT CLEF UNION:
!       -------------------
    if (n4 .gt. 0) then
        call getvtx('CREA_GROUP_MA', 'UNION', iocc, iarg, n4,&
                    zk24( ialik8), nbid)
        do 100,igm = 1,n4
        call jenonu(jexnom(ma//'.GROUPEMA', zk24(ialik8-1+igm)), igm2)
        if (igm2 .eq. 0) call u2mesk('F', 'SOUSTRUC_35', 1, zk24(ialik8-1+igm))
100      continue
!
        call jenonu(jexnom(ma//'.GROUPEMA', zk24(ialik8)), igm1)
        call jelira(jexnum(ma//'.GROUPEMA', igm1), 'LONUTI', ili1)
        call jeveuo(jexnum(ma//'.GROUPEMA', igm1), 'L', iagm1)
        if (ili1 .gt. nbis) then
            nbis = 2*ili1
            call jedetr('&&SSCGMA.LII1')
            call jedetr('&&SSCGMA.LII2')
            call wkvect('&&SSCGMA.LII1', 'V V I', nbis, ialii1)
            call wkvect('&&SSCGMA.LII2', 'V V I', nbis, ialii2)
        endif
        n = ili1
        do 110 ii = 1, n
            zi(ialii1-1+ii) = zi(iagm1-1+ii)
110      continue
!
        do 130,igm = 2,n4
        call jenonu(jexnom(ma//'.GROUPEMA', zk24(ialik8-1+igm)), igm2)
        call jelira(jexnum(ma//'.GROUPEMA', igm2), 'LONUTI', ili2)
        call jeveuo(jexnum(ma//'.GROUPEMA', igm2), 'L', iagm2)
        call utlisi('UNION', zi(ialii1), n, zi(iagm2), ili2,&
                    zi( ialii2), nbis, ntrou)
!
        if (ntrou .lt. 0) then
            nbis = -2*ntrou
            call jedetr('&&SSCGMA.LII2')
            call wkvect('&&SSCGMA.LII2', 'V V I', nbis, ialii2)
            call utlisi('UNION', zi(ialii1), n, zi(iagm2), ili2,&
                        zi(ialii2), nbis, ntrou)
            call jedetr('&&SSCGMA.LII1')
            call wkvect('&&SSCGMA.LII1', 'V V I', nbis, ialii1)
        endif
        n = ntrou
        do 120 ii = 1, n
            zi(ialii1-1+ii) = zi(ialii2-1+ii)
120      continue
130      continue
!
        if (n .eq. 0) then
            if (alarm .eq. 'OUI') then
                call u2mesk('A', 'SOUSTRUC_36', 1, nogma)
            endif
        else
            call wkvect(lisma, 'V V I', n, jlisma)
            nbma=n
            do 140 ii = 1, n
                zi(jlisma-1+ii) = zi(ialii1-1+ii)
140          continue
        endif
        goto 219
    endif
!
!
!       -- MOT CLEF DIFFE:
!       -------------------
    if (n5 .gt. 0) then
        call getvtx('CREA_GROUP_MA', 'DIFFE', iocc, iarg, n5,&
                    zk24( ialik8), nbid)
        do 150,igm = 1,n5
        call jenonu(jexnom(ma//'.GROUPEMA', zk24(ialik8-1+igm)), igm2)
        if (igm2 .eq. 0) call u2mesk('F', 'SOUSTRUC_35', 1, zk24(ialik8-1+igm))
150      continue
!
        call jenonu(jexnom(ma//'.GROUPEMA', zk24(ialik8)), igm1)
        call jelira(jexnum(ma//'.GROUPEMA', igm1), 'LONUTI', ili1)
        call jeveuo(jexnum(ma//'.GROUPEMA', igm1), 'L', iagm1)
        if (ili1 .gt. nbis) then
            nbis = 2*ili1
            call jedetr('&&SSCGMA.LII1')
            call jedetr('&&SSCGMA.LII2')
            call wkvect('&&SSCGMA.LII1', 'V V I', nbis, ialii1)
            call wkvect('&&SSCGMA.LII2', 'V V I', nbis, ialii2)
        endif
        n = ili1
        do 160 ii = 1, n
            zi(ialii1-1+ii) = zi(iagm1-1+ii)
160      continue
!
        do 180,igm = 2,n5
        call jenonu(jexnom(ma//'.GROUPEMA', zk24(ialik8-1+igm)), igm2)
        call jelira(jexnum(ma//'.GROUPEMA', igm2), 'LONUTI', ili2)
        call jeveuo(jexnum(ma//'.GROUPEMA', igm2), 'L', iagm2)
        call utlisi('DIFFE', zi(ialii1), n, zi(iagm2), ili2,&
                    zi( ialii2), nbis, ntrou)
        n = ntrou
        do 170 ii = 1, n
            zi(ialii1-1+ii) = zi(ialii2-1+ii)
170      continue
180      continue
!
        if (n .eq. 0) then
            if (alarm .eq. 'OUI') then
                call u2mesk('A', 'SOUSTRUC_36', 1, nogma)
            endif
        else
            call wkvect(lisma, 'V V I', n, jlisma)
            nbma=n
            do 190 ii = 1, n
                zi(jlisma-1+ii) = zi(ialii1-1+ii)
190          continue
        endif
        goto 219
    endif
!
!
!       -- MOT CLEF OPTION:
!       -------------------
    if (n7 .gt. 0) then
!
        call getvtx('CREA_GROUP_MA', 'OPTION', iocc, iarg, 1,&
                    option, nb)
!
!            -- TRAITEMENT DE L'OPTION FACE_NORMALE :
!               -----------------------------------
        if (option(1:12) .eq. 'FACE_NORMALE') then
            call cgmafn('CREA_GROUP_MA', iocc, ma, lisma, nbma)
!
!            -- TRAITEMENT DE L'OPTION SPHERE :
!               -----------------------------
        else if (option(1:6).eq.'SPHERE') then
            call cgmasp('CREA_GROUP_MA', iocc, ma, lisma, nbma)
!
!            -- TRAITEMENT DE L'OPTION CYLINDRE :
!               -------------------------------
        else if (option(1:8).eq.'CYLINDRE') then
            call cgmacy('CREA_GROUP_MA', iocc, ma, lisma, nbma)
!
!            -- TRAITEMENT DE L'OPTION BANDE :
!               ----------------------------
        else if (option(1:5).eq.'BANDE') then
            call cgmaba('CREA_GROUP_MA', iocc, ma, lisma, nbma)
!
!            -- TRAITEMENT DE L'OPTION APPUI_LACHE :
!               ----------------------------------
        else if (option(1:11).eq.'APPUI_LACHE') then
            call u2mess('A', 'SOUSTRUC2_6')
            call cgmaal('CREA_GROUP_MA', iocc, ma, lisma, nbma)
!
!            -- TRAITEMENT DE L'OPTION APPUI_STRICT :
!               ----------------------------------
        else if (option(1:5).eq.'APPUI') then
            call cgmaap('CREA_GROUP_MA', iocc, ma, lisma, nbma)
!
!            -- TRAITEMENT DE L'OPTION FISS_XFEM :
!               ----------------------------------
        else if (option(1:9).eq.'FISS_XFEM') then
            call cgmaxf('CREA_GROUP_MA', iocc, ma, lisma, nbma)
        endif
    endif
!
!
!       -- ON FILTRE LES TYPES DE MAILLES :
!       -----------------------------------
219  continue
!
    if (nbma .gt. 0) then
        call getvtx('CREA_GROUP_MA', 'TYPE_MAILLE', iocc, iarg, 1,&
                    tyma, ntyp)
        if (tyma(1:4) .ne. 'TOUT') then
            call cgmftm(tyma, ma, lisma, nbma, ierr)
            if (ierr .ne. 0) then
                call u2mesk('F', 'SOUSTRUC2_7', 1, nogma)
            endif
        endif
    endif
!
!       -- CREATION ET AFFECTATION DU GROUP_MA :
!       ----------------------------------
    if (nbma .eq. 0) then
        if (alarm .eq. 'OUI') then
            call u2mesk('A', 'SOUSTRUC_36', 1, nogma)
        endif
    else
        call jeveuo(lisma, 'L', idlima)
!
        call jecroc(jexnom(ma//'.GROUPEMA', nogma))
        call jeecra(jexnom(ma//'.GROUPEMA', nogma), 'LONMAX', max(1,nbma))
        call jeecra(jexnom(ma//'.GROUPEMA', nogma), 'LONUTI', nbma)
        call jeveuo(jexnom(ma//'.GROUPEMA', nogma), 'E', jgma)
!
        do 200 ii = 1, nbma
            zi(jgma-1+ii) = zi(idlima-1+ii)
200      continue
!
        nbgnaj = nbgnaj + 1
!
    endif
!
    call jedetr(lisma)
!
    210 end do
!
!     IMPRESSIONS NIVEAUX 1 ET 2
!     --------------------------
    if (niv .ge. 1 .and. nbgnaj .ne. 0) then
        write (ifm,'(/,/,A,I6,/,45(''=''))')&
     &    'NOMBRE  DE GROUPES DE MAILLES CREES : ',nbgnaj
!
        write (ifm,'(/,15X,54(''-''),2(/,15X,A),/,15X,54(''-''))')&
     &    '!         NOM DU GROUPE         ! NBRE DE MAILLES DU !',&
     &    '!            MAILLES            !     GROUPE_MA      !'
!
        do 220 i = 1, nbgnaj
            ii = nbgmin + i
            call jenuno(jexnum(ma//'.GROUPEMA', ii), nogma)
            call jelira(jexnum(ma//'.GROUPEMA', ii), 'LONUTI', nbma)
            write (ifm,'(15X,A,2X,A24,5X,A,2X,I8,10X,A)') '!',nogma,'!',&
     &      nbma,'!'
220      continue
        write (ifm,'(15X,54(''-''),/)')
    endif
!
!     IMPRESSIONS NIVEAU 2
!     --------------------
    if (niv .eq. 2 .and. nbgnaj .ne. 0) then
        maxcol = 8
        do 250 i = 1, nbgnaj
            ii = nbgmin + i
            call jeveuo(jexnum(ma//'.GROUPEMA', ii), 'L', jlisma)
            call jenuno(jexnum(ma//'.GROUPEMA', ii), nogma)
            call jelira(jexnum(ma//'.GROUPEMA', ii), 'LONUTI', nbma)
            write (ifm,'(/,3A,/,28(''-''))') 'MAILLES DU GROUPE ',&
            nogma, ' :'
            nbline = nbma/maxcol
            ireste = mod(nbma,maxcol)
            if (ireste .ne. 0) nbline = nbline + 1
            nbcol = maxcol
            kkk = 0
            do 240 jjj = 1, nbline
                if (ireste .ne. 0 .and. jjj .eq. nbline) nbcol = ireste
                do 230 iii = 1, nbcol
                    kkk = kkk + 1
                    call jenuno(jexnum(ma//'.NOMMAI', zi(jlisma-1+kkk)), noma)
                    card((iii-1)*10+1:) = ' '//noma//' '
230              continue
                write (ifm,'(A)') card(:10*nbcol)
240          continue
250      continue
        write (ifm,'(/,/)')
    endif
!
!
! --- MENAGE
    call jedetr(lisma)
    call jedetr('&&SSCGMA.LIK8')
    call jedetr('&&SSCGMA.LII1')
    call jedetr('&&SSCGMA.LII2')
!
    call jedema()
!
end subroutine
