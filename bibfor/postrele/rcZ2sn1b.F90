subroutine rcZ2sn1b(lieu, numsip, numsiq, seismeb32,&
                     instsn, sn1, sp3, spmeca3)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/jemarq.h"
#include "asterc/getfac.h"
#include "asterfort/getvis.h"
#include "asterfort/getvid.h"
#include "asterfort/rcveri.h"
#include "asterfort/tbexip.h"
#include "asterfort/utmess.h"
#include "asterfort/tbexv1.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rcver1.h"
#include "asterfort/as_allocate.h"
#include "asterfort/wkvect.h"
#include "asterfort/tbliva.h"
#include "asterfort/rc32my.h"
#include "asterfort/rctres.h"
#include "asterfort/jedetr.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/jedema.h"
    character(len=4) :: lieu
    integer :: numsip, numsiq
    aster_logical :: seismeb32
    real(kind=8) :: sn1, sp3, spmeca3, instsn(2)
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_ZE200
!     CALCUL DU SN OU SN* (partie B3200) en testant tous les instants
!
! IN  : LIEU   : ORIG ou EXTR
! IN  : NUMSIP : NUMERO SITUATION P
! IN  : NUMSIQ : NUMERO SITUATION Q
! IN  : SEISMEB32: SI SEISME AVEC B3200_T
! OUT : SN1    : PARTIE B3200 de SN
! OUT : SP3    : PARTIE B3200 de SP qui dépend des instants de SN
! OUT : SPMECA3: PARTIE B3200 de SPMECA qui dépend des instants de SN
!     ------------------------------------------------------------------
!
    real(kind=8) :: trescapp, trescaqq, trescapq, prec(2), vale(2)
    integer :: nbsitu, nbther, nbpres, nbmeca, n1(2), n2(2), n3(2), iocc
    character(len=8) :: nocmp(6), crit(2), tabther(2), tabpres(2), tabmeca(2)
    character(len=16) :: valek(2)
    integer :: numesitu, n0, nume1(2), nume2(2), nume3(2), np, nq, ither
    integer :: ipres, imeca, numether, numepres,numemeca, n4, ii, nbinst(2)
    character(len=1) :: numsitu
    character(len=8) :: tableok(2), k8b
    aster_logical :: exist
    character(len=24) :: valk(4)
    integer :: jinst(2), nbabsc(2), jabsc(2), ncmp, ndim, jsigp, i, j, k, l
    real(kind=8), pointer :: contraintestot(:) => null()
    real(kind=8), pointer :: contraintesth(:) => null()
    real(kind=8), pointer :: contraintespr(:) => null()
    real(kind=8), pointer :: contraintesmec(:) => null()
    integer :: ibid, iret, jsigq, instp, instq, jsithp, jsiprp, jsimep, jseis
    complex(kind=8) :: cbid
    real(kind=8) :: momen0, momen1, diffsig(6), tresca, momen0th, momen0pr
    real(kind=8) :: momen0mec, momen1th, momen1pr, momen1mec, diffsith(6)
    integer :: jsithq, jsiprq, jsimeq, jvalin, i1, i2, i3, i4, i5, i6
    real(kind=8) :: diffsipr(6), diffsime(6), trescath, trescapr, trescame
    real(kind=8) :: tresthpp, tresprpp, tresmepp, tresthqq, tresprqq, tresmeqq
    real(kind=8) :: tresthpq, tresprpq, tresmepq, tresth, trespr, tresmec
    real(kind=8) :: k1, c1, k2, c2, k3, c3, trescapps, trescaqqs, trescapqs
    real(kind=8) :: e1(2), e2(2), e3(2), e4(2), e5(2), e6(2), sntrans(6)
    integer :: instp1, instp2, instq1, instq2, i0
    real(kind=8) :: sntran(6)
!
! DEB ------------------------------------------------------------------
    call jemarq()
!
    if (seismeb32) then
        call jeveuo('&&RC3200.SIGSEIS', 'L', jseis)
    endif
!
    do 2 i0 = 1, 2
        i1 = 2*(i0-2)+1
        e1(i0) = i1
        e2(i0) = i1
        e3(i0) = i1
        e4(i0) = i1
        e5(i0) = i1
        e6(i0) = i1
 2  continue
!
    sn1 = 0.d0
    sp3 = 0.d0
    spmeca3 = 0.d0
!
    trescapp = 0.d0
    trescaqq = 0.d0
    trescapq = 0.d0
    trescapps = 0.d0
    trescaqqs = 0.d0
    trescapqs = 0.d0
    instsn(1)=0.d0
    instsn(2)=0.d0
!
    call getfac('SITUATION', nbsitu)
    call getfac('RESU_THER', nbther)
    call getfac('RESU_PRES', nbpres)
    call getfac('RESU_MECA', nbmeca)
!
    nocmp(1) = 'SIXX'
    nocmp(2) = 'SIYY'
    nocmp(3) = 'SIZZ'
    nocmp(4) = 'SIXY'
    nocmp(5) = 'SIXZ'
    nocmp(6) = 'SIYZ'
!
    valek(1) = 'INST            '
    valek(2) = 'ABSC_CURV       '
!
    prec(1) = 1.0d-06
    prec(2) = 1.0d-06
    crit(1) = 'RELATIF'
    crit(2) = 'RELATIF'
!
!-------------------------------------------------------------------------
!       RECUPERATION DES TABLES DE TRANSITOIRES DES SITUATIONS P et Q
!------------------------------------------------------------------------
    n1(1) = 0
    n2(1) = 0
    n3(1) = 0
    n1(2) = 0
    n2(2) = 0
    n3(2) = 0
   
    do 10 iocc = 1, nbsitu, 1
!
! ------ on récupère la bonne situation
        call getvis('SITUATION', 'NUME_SITU', iocc=iocc, scal=numesitu, nbret=n0)
        if (numesitu .eq. numsip) then
!------ on récupère les numéros des tables sous le mot clé situation pour p
            call getvis('SITUATION', 'NUME_RESU_THER', iocc=iocc, scal=nume1(1), nbret=n1(1))
            call getvis('SITUATION', 'NUME_RESU_PRES', iocc=iocc, scal=nume2(1), nbret=n2(1))
            call getvis('SITUATION', 'NUME_RESU_MECA', iocc=iocc, scal=nume3(1), nbret=n3(1))
        endif
        if (numsip .ne. numsiq .and. numesitu .eq. numsiq) then
!------ on récupère les numéros des tables sous le mot clé situation pour q
            call getvis('SITUATION', 'NUME_RESU_THER', iocc=iocc, scal=nume1(2), nbret=n1(2))
            call getvis('SITUATION', 'NUME_RESU_PRES', iocc=iocc, scal=nume2(2), nbret=n2(2))
            call getvis('SITUATION', 'NUME_RESU_MECA', iocc=iocc, scal=nume3(2), nbret=n3(2))
        endif
 10 continue
!
!-- si aucune des situations n'a de transitoires
    np = n1(1)+n2(1)+n3(1)
    nq = n1(2)+n2(2)+n3(2)
    if (np+nq .eq. 0) goto 999
!-- sinon
!-- on récupère les tables associées sous RESU_THER, RESU_PRES et RESU_MECA pour p et q
    do 20 ither =1, nbther, 1
        call getvis('RESU_THER', 'NUME_RESU_THER', iocc=ither, scal=numether, nbret=n4)
        do 23 ii=1,2
            if (n1(ii) .ne. 0) then
                if (numether .eq. nume1(ii)) then
                    call getvid('RESU_THER', 'TABL_RESU_THER', iocc=ither,&
                                 scal=tabther(ii), nbret=n4)
                endif
            endif
23      continue
20  continue
    do 30 ipres =1, nbpres, 1
        call getvis('RESU_PRES', 'NUME_RESU_PRES', iocc=ipres, scal=numepres, nbret=n4)
        do 33 ii=1,2
            if (n2(ii) .ne. 0) then 
                if (numepres .eq. nume2(ii)) then
                    call getvid('RESU_PRES', 'TABL_RESU_PRES', iocc=ipres,&
                                scal=tabpres(ii), nbret=n4)
                endif
            endif
33      continue
30  continue
    do 40 imeca =1, nbmeca, 1
        call getvis('RESU_MECA', 'NUME_RESU_MECA', iocc=imeca, scal=numemeca, nbret=n4)
        do 43 ii=1,2
            if (n3(ii) .ne. 0) then 
                if (numemeca .eq. nume3(ii)) then
                    call getvid('RESU_MECA', 'TABL_RESU_MECA', iocc=imeca,&
                                scal=tabmeca(ii), nbret=n4)
                endif
            endif
43      continue
40  continue
!
!--------------------------------------------------------------
!      RECUPERATION DES INSTANTS ET DES ABSCISSES DE P et Q
!--------------------------------------------------------------
! ------ grace a la table thermique ou de pression ou mecanique
    do 53 ii=1,2
        if (ii .eq. 1) then
            numsitu='P'
        else
            numsitu='Q'
        endif
        if (n1(ii)+n2(ii)+n3(ii) .eq. 0) goto 777
!
        if (n1(ii) .ne. 0) then
            tableok(ii)  = tabther(ii) 
        else if (n2(ii) .ne. 0) then
            tableok(ii)  = tabpres(ii) 
        else
            tableok(ii)  = tabmeca(ii) 
        endif
!  
! --------- on verifie l'ordre des noeuds de la table
        call rcveri(tableok(ii))
! 
! --------- on recupere les instants de la table
        call tbexip(tableok(ii), valek(1), exist, k8b)
        if (.not. exist) then
            valk (1) = tableok(ii)
            valk (2) = valek(1)
            call utmess('F', 'POSTRCCM_1', nk=2, valk=valk)
        endif
        call tbexv1(tableok(ii), valek(1), 'RC.INSTANT.'//numsitu, 'V', nbinst(ii),&
                    k8b)
        call jeveuo('RC.INSTANT.'//numsitu, 'L', jinst(ii))
!
! --------- on recupere les abscisses curvilignes de la table
        call tbexip(tableok(ii), valek(2), exist, k8b)
        if (.not. exist) then
            valk (1) = tableok(ii)
            valk (2) = valek(2)
            call utmess('F', 'POSTRCCM_1', nk=2, valk=valk)
        endif
        call tbexv1(tableok(ii), valek(2), 'RC.ABSC.'//numsitu, 'V', nbabsc(ii),&
                   k8b)
        call jeveuo('RC.ABSC.'//numsitu, 'L', jabsc(ii))
!
! ------ ON VERIFIE LA COHERENCE DES TABLES (THERMIQUE, PRESSION ET MECA)
        if (n1(ii) .ne. 0) then
            if (n2(ii) .ne. 0) then
                call rcver1('MECANIQUE', tabpres(ii), tabther(ii))
            endif
            if (n3(ii) .ne. 0) then
                call rcver1('MECANIQUE', tabmeca(ii), tabther(ii))
            endif
        endif
        if (n2(ii) .ne. 0) then
            if (n3(ii) .ne. 0) then
                call rcver1('MECANIQUE', tabmeca(ii), tabpres(ii))
            endif
        endif
!
777     continue
53  continue
!
    if (np*nq .ne. 0) then
        if (nbabsc(1) .ne. nbabsc(2)) then
            call utmess('F', 'POSTRCCM_45')
        endif
    endif
!
    ncmp=6
!
    AS_ALLOCATE(vr=contraintestot,  size=nbabsc(1))
    AS_ALLOCATE(vr=contraintesth,  size=nbabsc(1))
    AS_ALLOCATE(vr=contraintespr,  size=nbabsc(1))
    AS_ALLOCATE(vr=contraintesmec, size=nbabsc(1))
! 
!--------------------------------------------------------------
!      STOCKAGE DES CONTRAINTES DE LA SITUATION P
!                ET CALCUL DE SN(P,P)
!--------------------------------------------------------------
    do 114 j = 1, 6
        sntran(j) = 0.d0
 114 continue
!
    if (np .eq. 0) goto 888
!
    ndim = ncmp*nbinst(1)
    call wkvect('&&RC3200.SIGMP', 'V V R', ndim, jsigp)
    call wkvect('&&RC3200.SITHP', 'V V R', ndim, jsithp)
    call wkvect('&&RC3200.SIPRP', 'V V R', ndim, jsiprp)
    call wkvect('&&RC3200.SIMEP', 'V V R', ndim, jsimep)
!
    do 16 i = 1, nbinst(1)

        vale(1) = zr(jinst(1)+i-1)

        do 14 j = 1, ncmp
!
            do 18 k = 1, nbabsc(1)
!
                vale(2) = zr(jabsc(1)+k-1)
!
                if (n1(1) .ne. 0) then
                    call tbliva(tabther(1), 2, valek, [ibid], vale,&
                               [cbid], k8b, crit, prec, nocmp(j),&
                               k8b, ibid, contraintesth(k), cbid, k8b,&
                               iret)
                    if (iret .ne. 0) then
                        valk (1) = tabther(1)
                        valk (2) = nocmp(j)
                        valk (3) = valek(1)
                        valk (4) = valek(2)
                        call utmess('F', 'POSTRCCM_2', nk=4, valk=valk, nr=2,&
                                    valr=vale)
                    endif
                endif
                if (n2(1) .ne. 0) then
                    call tbliva(tabpres(1), 2, valek, [ibid], vale,&
                                [cbid], k8b, crit, prec, nocmp(j),&
                                k8b, ibid, contraintespr(k), cbid, k8b,&
                                iret)
                    if (iret .ne. 0) then
                        valk (1) = tabpres(1)
                        valk (2) = nocmp(j)
                        valk (3) = valek(1)
                        valk (4) = valek(2)
                        call utmess('F', 'POSTRCCM_2', nk=4, valk=valk, nr=2,&
                                    valr=vale)
                    endif
                endif
                if (n3(1) .ne. 0) then
                    call tbliva(tabmeca(1), 2, valek, [ibid], vale,&
                               [cbid], k8b, crit, prec, nocmp(j),&
                               k8b, ibid, contraintesmec(k), cbid, k8b,&
                               iret)
                    if (iret .ne. 0) then
                        valk (1) = tabmeca(1)
                        valk (2) = nocmp(j)
                        valk (3) = valek(1)
                        valk (4) = valek(2)
                        call utmess('F', 'POSTRCCM_2', nk=4, valk=valk, nr=2,&
                                    valr=vale)
                    endif
                endif
                contraintestot(k) = contraintesmec(k)+contraintespr(k)+ contraintesth(k)
 18         continue
            call rc32my(nbabsc(1), zr(jabsc(1)), contraintestot, momen0, momen1)
            call rc32my(nbabsc(1), zr(jabsc(1)), contraintesth, momen0th, momen1th)
            call rc32my(nbabsc(1), zr(jabsc(1)), contraintespr, momen0pr, momen1pr)
            call rc32my(nbabsc(1), zr(jabsc(1)), contraintesmec, momen0mec, momen1mec)
            if (lieu .eq. 'ORIG') then
                zr(jsigp+(i-1)*ncmp+j-1) = momen0 - 0.5d0*momen1
                zr(jsithp+(i-1)*ncmp+j-1) = momen0th - 0.5d0*momen1th
                zr(jsiprp+(i-1)*ncmp+j-1) = momen0pr - 0.5d0*momen1pr
                zr(jsimep+(i-1)*ncmp+j-1) = momen0mec - 0.5d0*momen1mec
            else
                zr(jsigp+(i-1)*ncmp+j-1) = momen0 + 0.5d0*momen1
                zr(jsithp+(i-1)*ncmp+j-1) = momen0th + 0.5d0*momen1th
                zr(jsiprp+(i-1)*ncmp+j-1) = momen0pr + 0.5d0*momen1pr
                zr(jsimep+(i-1)*ncmp+j-1) = momen0mec + 0.5d0*momen1mec
            endif
 14     continue
 16 continue

    do 22 i = 1, nbinst(1)
        do 32 l = 1, nbinst(1)
            do 42 j = 1, ncmp
                diffsig(j)=zr(jsigp+(i-1)*ncmp+j-1)-zr(jsigp+(l-1)*ncmp+j-1)
                diffsith(j)=zr(jsithp+(i-1)*ncmp+j-1)-zr(jsithp+(l-1)*ncmp+j-1)
                diffsipr(j)=zr(jsiprp+(i-1)*ncmp+j-1)-zr(jsiprp+(l-1)*ncmp+j-1)
                diffsime(j)=zr(jsimep+(i-1)*ncmp+j-1)-zr(jsimep+(l-1)*ncmp+j-1)
                if (j .eq. ncmp) then
                    call rctres(diffsig, tresca)
                    call rctres(diffsith, trescath)
                    call rctres(diffsipr, trescapr)
                    call rctres(diffsime, trescame)
                    if (tresca .gt. trescapp) then
                        trescapp=tresca
                        tresthpp=trescath
                        tresprpp=trescapr
                        tresmepp=trescame
                        instp1 = i
                        instp2 = l
                    endif
                endif
         42 continue
     32 continue
 22 continue
    do 115 j = 1, 6
        sntran(j) = zr(jsigp+(instp1-1)*ncmp+j-1)-zr(jsigp+(instp2-1)*ncmp+j-1)
 115 continue
!
888 continue 
!
    if (seismeb32) then
        do 69 i1 = 1, 2
            do 59 i2 = 1, 2
                do 49 i3 = 1, 2
                    do 39 i4 = 1, 2
                        do 29 i5 = 1, 2
                            do 19 i6 = 1, 2
                                do 79 j = 1, 6
                                        sntrans(j) = sntran(j)+e1(i1)*zr(jseis+j-1)+&
                                                e2(i2)*zr(jseis+1*6+j-1)+&
                                                e3(i3)*zr(jseis+2*6+j-1)+e4(i4)*zr(jseis+3*6+j-1)+&
                                                e5(i5)*zr(jseis+4*6+j-1)+e6(i6)*zr(jseis+5*6+j-1)
79                              continue
                                call rctres(sntrans,tresca)
                                trescapps = max(trescapps,tresca)
19                          continue
29                      continue
39                  continue
49              continue
59          continue
69      continue
    endif
!
!--------------------------------------------------------------
!      STOCKAGE DES CONTRAINTES DE LA SITUATION Q
!                ET CALCUL DE SN(Q,Q)
!--------------------------------------------------------------
    if (nq .eq. 0) goto 555
!
    ndim = ncmp*nbinst(2)
    call wkvect('&&RC3200.SIGMQ', 'V V R', ndim, jsigq)
    call wkvect('&&RC3200.SITHQ', 'V V R', ndim, jsithq)
    call wkvect('&&RC3200.SIPRQ', 'V V R', ndim, jsiprq)
    call wkvect('&&RC3200.SIMEQ', 'V V R', ndim, jsimeq)
!
    do 17 i = 1, nbinst(2)

        vale(1) = zr(jinst(2)+i-1)

        do 15 j = 1, ncmp
!
            do 11 k = 1, nbabsc(2)
!
                vale(2) = zr(jabsc(2)+k-1)
!
                if (n1(2) .ne. 0) then
                    call tbliva(tabther(2), 2, valek, [ibid], vale,&
                                [cbid], k8b, crit, prec, nocmp(j),&
                                k8b, ibid, contraintesth(k), cbid, k8b,&
                                iret)
                    if (iret .ne. 0) then
                        valk (1) = tabther(2)
                        valk (2) = nocmp(j)
                        valk (3) = valek(1)
                        valk (4) = valek(2)
                        call utmess('F', 'POSTRCCM_2', nk=4, valk=valk, nr=2,&
                                    valr=vale)
                    endif
                endif
                if (n2(2) .ne. 0) then
                    call tbliva(tabpres(2), 2, valek, [ibid], vale,&
                               [cbid], k8b, crit, prec, nocmp(j),&
                               k8b, ibid, contraintespr(k), cbid, k8b,&
                               iret)
                    if (iret .ne. 0) then
                        valk (1) = tabpres(2)
                        valk (2) = nocmp(j)
                        valk (3) = valek(1)
                        valk (4) = valek(2)
                        call utmess('F', 'POSTRCCM_2', nk=4, valk=valk, nr=2,&
                                    valr=vale)
                    endif
                endif
                if (n3(2) .ne. 0) then
                    call tbliva(tabmeca(2), 2, valek, [ibid], vale,&
                               [cbid], k8b, crit, prec, nocmp(j),&
                               k8b, ibid, contraintesmec(k), cbid, k8b,&
                               iret)
                    if (iret .ne. 0) then
                        valk (1) = tabmeca(2)
                        valk (2) = nocmp(j)
                        valk (3) = valek(1)
                        valk (4) = valek(2)
                        call utmess('F', 'POSTRCCM_2', nk=4, valk=valk, nr=2,&
                                    valr=vale)
                    endif
                endif
                contraintestot(k) = contraintesmec(k)+contraintespr(k)+ contraintesth(k)
 11         continue
            call rc32my(nbabsc(2), zr(jabsc(2)), contraintestot, momen0, momen1)
            call rc32my(nbabsc(2), zr(jabsc(2)), contraintesth, momen0th, momen1th)
            call rc32my(nbabsc(2), zr(jabsc(2)), contraintespr, momen0pr, momen1pr)
            call rc32my(nbabsc(2), zr(jabsc(2)), contraintesmec, momen0mec, momen1mec)
            if (lieu .eq. 'ORIG') then
                zr(jsigq+(i-1)*ncmp+j-1) = momen0 - 0.5d0*momen1
                zr(jsithq+(i-1)*ncmp+j-1) = momen0th - 0.5d0*momen1th
                zr(jsiprq+(i-1)*ncmp+j-1) = momen0pr - 0.5d0*momen1pr
                zr(jsimeq+(i-1)*ncmp+j-1) = momen0mec - 0.5d0*momen1mec
            else
                zr(jsigq+(i-1)*ncmp+j-1) = momen0 + 0.5d0*momen1
                zr(jsithq+(i-1)*ncmp+j-1) = momen0th + 0.5d0*momen1th
                zr(jsiprq+(i-1)*ncmp+j-1) = momen0pr + 0.5d0*momen1pr
                zr(jsimeq+(i-1)*ncmp+j-1) = momen0mec + 0.5d0*momen1mec
            endif
 15     continue
 17 continue

    do 24 i = 1, nbinst(2)
        do 34 l = 1, nbinst(2)
            do 44 j = 1, ncmp
                diffsig(j)=zr(jsigq+(i-1)*ncmp+j-1)-zr(jsigq+(l-1)*ncmp+j-1)
                diffsith(j)=zr(jsithq+(i-1)*ncmp+j-1)-zr(jsithq+(l-1)*ncmp+j-1)
                diffsipr(j)=zr(jsiprq+(i-1)*ncmp+j-1)-zr(jsiprq+(l-1)*ncmp+j-1)
                diffsime(j)=zr(jsimeq+(i-1)*ncmp+j-1)-zr(jsimeq+(l-1)*ncmp+j-1)
                if (j .eq. ncmp) then
                    call rctres(diffsig, tresca)
                    call rctres(diffsith, trescath)
                    call rctres(diffsipr, trescapr)
                    call rctres(diffsime, trescame)
                    if (tresca .gt. trescaqq) then
                        trescaqq=tresca
                        tresthqq=trescath
                        tresprqq=trescapr
                        tresmeqq=trescame
                        instq1 = i
                        instq2 = l
                    endif
                endif
         44 continue
     34 continue
 24 continue
!
    if (seismeb32) then
        do 163 i1 = 1, 2
            do 153 i2 = 1, 2
                do 143 i3 = 1, 2
                    do 133 i4 = 1, 2
                        do 123 i5 = 1, 2
                            do 113 i6 = 1, 2
                                do 173 j = 1, 6
                                    diffsig(j)=zr(jsigq+(instq1-1)*ncmp+j-1)-&
                                               zr(jsigq+(instq2-1)*ncmp+j-1)
                                    sntrans(j) = diffsig(j)+e1(i1)*zr(jseis+j-1)+&
                                                e2(i2)*zr(jseis+1*6+j-1)+&
                                                e3(i3)*zr(jseis+2*6+j-1)+e4(i4)*zr(jseis+3*6+j-1)+&
                                                e5(i5)*zr(jseis+4*6+j-1)+e6(i6)*zr(jseis+5*6+j-1)
173                             continue
                                call rctres(sntrans,tresca)
                                trescaqqs = max(trescaqqs,tresca)
113                          continue
123                      continue
133                  continue
143              continue
153          continue
163      continue
    endif
!
555 continue 
!
!--------------------------------------------------------------
!                CALCUL DE SN(P,Q)
!--------------------------------------------------------------
    if (np*nq .eq. 0) goto 444
!
    do 26 i = 1, nbinst(1)
        do 36 l = 1, nbinst(2)
            do 46 j = 1, ncmp
                diffsig(j)=zr(jsigp+(i-1)*ncmp+j-1)-zr(jsigq+(l-1)*ncmp+j-1)
                diffsith(j)=zr(jsithp+(i-1)*ncmp+j-1)-zr(jsithq+(l-1)*ncmp+j-1)
                diffsipr(j)=zr(jsiprp+(i-1)*ncmp+j-1)-zr(jsiprq+(l-1)*ncmp+j-1)
                diffsime(j)=zr(jsimep+(i-1)*ncmp+j-1)-zr(jsimeq+(l-1)*ncmp+j-1)
                if (j .eq. ncmp) then
                    call rctres(diffsig, tresca)
                    call rctres(diffsith, trescath)
                    call rctres(diffsipr, trescapr)
                    call rctres(diffsime, trescame)
                    if (tresca .gt. trescapq) then
                        trescapq=tresca
                        tresthpq=trescath
                        tresprpq=trescapr
                        tresmepq=trescame
                        instp = i
                        instq = l
                    endif
                endif
         46 continue
     36 continue
 26 continue
!
    if (seismeb32) then
        do 162 i1 = 1, 2
            do 152 i2 = 1, 2
                do 142 i3 = 1, 2
                    do 132 i4 = 1, 2
                        do 122 i5 = 1, 2
                            do 112 i6 = 1, 2
                                do 172 j = 1, 6
                                    diffsig(j)=zr(jsigp+(instp-1)*ncmp+j-1)-&
                                               zr(jsigq+(instq-1)*ncmp+j-1)
                                    sntrans(j) = diffsig(j)+e1(i1)*zr(jseis+j-1)+&
                                                e2(i2)*zr(jseis+1*6+j-1)+&
                                                e3(i3)*zr(jseis+2*6+j-1)+e4(i4)*zr(jseis+3*6+j-1)+&
                                                e5(i5)*zr(jseis+4*6+j-1)+e6(i6)*zr(jseis+5*6+j-1)
172                             continue
                                call rctres(sntrans,tresca)
                                trescapqs = max(trescapqs,tresca)
112                          continue
122                      continue
132                  continue
142              continue
152          continue
162      continue
    endif
!
444 continue 
!
!--------------------------------------------------------------
!        ON PREND LE MAX DE SN(P,P), SN(Q,Q) et SN(P,Q)
!--------------------------------------------------------------
    sn1=max(trescapp, trescaqq, trescapq)
    if (seismeb32) then
        sn1=max(trescapps, trescaqqs, trescapqs)
    endif
    if (sn1 .eq. trescapp) then
        tresth=tresthpp
        trespr=tresprpp
        tresmec=tresmepp
        if (np .ne. 0) then
            instsn(1) = zr(jinst(1)+instp1-1)
            instsn(2) = zr(jinst(1)+instp2-1)
        endif
    else if (sn1 .eq. trescaqq) then
        tresth=tresthqq
        trespr=tresprqq
        tresmec=tresmeqq
        if (nq .ne. 0) then
            instsn(1) = zr(jinst(2)+instq1-1)
            instsn(2) = zr(jinst(2)+instq2-1)
        endif
    else
        tresth=tresthpq
        trespr=tresprpq
        tresmec=tresmepq
        if (np*nq .ne. 0) then
            instsn(1) = zr(jinst(1)+instp-1)
            instsn(2) = zr(jinst(2)+instq-1)
        endif
    endif
!
!-----------------------------------------------------------
!   CALCUL DE LA PARTIE SP3 (QUI DEPEND DES INSTANTS DE SN)
!-----------------------------------------------------------
!
    call jeveuo('&&RC3200.INDI', 'L', jvalin)
    k1 = zr(jvalin)
    c1 = zr(jvalin+1)
    k2 = zr(jvalin+2)
    c2 = zr(jvalin+3) 
    k3 = zr(jvalin+4)
    c3 = zr(jvalin+5)
!
    sp3 = (k3*c3-1)*tresth+(k1*c1-1)*trespr+(k2*c2-1)*tresmec
    spmeca3 = (k1*c1-1)*trespr+(k2*c2-1)*tresmec
!
    if (np .ne. 0) then
        call jedetr('RC.INSTANT.P')
        call jedetr('RC.ABSC.P')
        call jedetr('&&RC3200.SIGMP')
        call jedetr('&&RC3200.SITHP')
        call jedetr('&&RC3200.SIPRP')
        call jedetr('&&RC3200.SIMEP')
    endif
    if (nq .ne. 0) then
        call jedetr('RC.INSTANT.Q')
        call jedetr('RC.ABSC.Q')
        call jedetr('&&RC3200.SIGMQ')
        call jedetr('&&RC3200.SITHQ')
        call jedetr('&&RC3200.SIPRQ')
        call jedetr('&&RC3200.SIMEQ')
    endif
!
    AS_DEALLOCATE(vr=contraintestot)
    AS_DEALLOCATE(vr=contraintesmec)
    AS_DEALLOCATE(vr=contraintesth)
    AS_DEALLOCATE(vr=contraintespr)
!
999 continue
!
    call jedema()
end subroutine
