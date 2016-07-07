subroutine rcZ2sp1b(lieu, numsip, numsiq,&
                    seismeb32, instsp, sp1, spmeca1)
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
#include "asterfort/tbliva.h"
#include "asterfort/rctres.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/codent.h"
#include "asterfort/jelira.h"
#include "asterfort/jexnom.h"
#include "asterfort/jedetr.h"
#include "asterfort/wkvect.h"
#include "asterfort/jedema.h"
    character(len=4) :: lieu
    integer :: numsip, numsiq
    aster_logical :: seismeb32
    real(kind=8) :: sp1(2), spmeca1(2), instsp(4)
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
!     CALCUL DU SP en testant tous les instants
!
!     ------------------------------------------------------------------
! IN  : LIEU   : ='ORIG' : ORIGINE DU SEGEMNT, ='EXTR' : EXTREMITE
! IN  : NUMSIP : NUMERO SITUATION DE L'ETAT STABILISE P
! IN  : NUMSIQ : NUMERO SITUATION DE L'ETAT STABILISE Q
! OUT : SP1    : PARTIE B3200 du SP
!
    real(kind=8) :: trescapp, prec(2)
    integer :: nbsitu, nbther, nbpres, nbmeca, iocc, numesitu, n0
    character(len=8) :: nocmp(6), crit(2), tabther(2), tabpres(2), tabmeca(2)
    character(len=16) :: valek(2)
    integer :: nume1(2), nume2(2), nume3(2), n1(2), n2(2), n3(2), np, ither, numether
    integer :: n4, ipres, numepres, imeca, numemeca, nbinst(2), jinst(2)
    character(len=8) :: tableok(2), k8b
    aster_logical :: exist
    character(len=24) :: valk(4)
    character(len=1) :: numsitu
    integer :: nbabsc(2), jabsc(2), i, j, ibid, iret, l
    real(kind=8), pointer :: contraintesth(:) => null()
    real(kind=8), pointer :: contraintespr(:) => null()
    real(kind=8), pointer :: contraintesmec(:) => null()
    real(kind=8) :: vale(2), diffsig(6), tresca
    complex(kind=8) :: cbid
    integer :: i1, ncmp
    real(kind=8) :: trescaqq, trescapq, trescamax
    integer :: nq, ii, ndim, jsigp, jsigq, instp1, instp2, instq1, instq2
    real(kind=8) :: trescaqp, tresca1, tresca2, diffsime(6), tresme, tresmepp
    real(kind=8) :: tresmeqq, tresmepq, tresmeqp
    integer :: jmecap, jmecaq, instp1c, instp2c, instq1c, instq2c
    integer :: jseis, i2, i3, i4, i5, i6, i0
    real(kind=8) :: sptran(6), sptrans(6), e1(2), e2(2), e3(2), e4(2), e5(2), e6(2)
    real(kind=8) :: spmec(6), spmecs(6), diffsig2(6), diffsime2(6), spmec1(6)
    real(kind=8) :: spmec2(6), sptran1(6), sptran2(6), tresme1, tresme2
    real(kind=8) :: tresmepps, tresmeqqs, tresmepqs, tresmeqps
    real(kind=8) :: trescapps, trescaqqs, trescapqs, trescaqps
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
    sp1(1) = 0.d0
    sp1(2) = 0.d0
    spmeca1(1) = 0.d0
    spmeca1(2) = 0.d0
    trescapp = 0.d0
    trescaqq = 0.d0
    trescapq = 0.d0
    trescaqp = 0.d0
    tresmepp = 0.d0
    tresmeqq = 0.d0
    tresmepq = 0.d0
    tresmeqp = 0.d0
    trescapps = 0.d0
    trescaqqs = 0.d0
    trescapqs = 0.d0
    trescaqps = 0.d0
    tresmepps = 0.d0
    tresmeqqs = 0.d0
    tresmepqs = 0.d0
    tresmeqps = 0.d0
    instsp(1) = 0.d0
    instsp(2) = 0.d0
    instsp(3) = 0.d0
    instsp(4) = 0.d0
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
    ncmp=6
!
    AS_ALLOCATE(vr=contraintesth,  size=1)
    AS_ALLOCATE(vr=contraintespr,  size=1)
    AS_ALLOCATE(vr=contraintesmec, size=1)
! 
!--------------------------------------------------------------
!      STOCKAGE DES CONTRAINTES DE LA SITUATION P
!                ET CALCUL DE SP(P,P)
!--------------------------------------------------------------
    do 114 j = 1, 6
        sptran(j) = 0.d0
        spmec(j) = 0.d0
 114 continue
!
    if (np .eq. 0) goto 888
!
    ndim = ncmp*nbinst(1)
    call wkvect('&&RC3200.SIGMP', 'V V R', ndim, jsigp)
    call wkvect('&&RC3200.SIGMP.MECA', 'V V R', ndim, jmecap)
    if (lieu .eq. 'ORIG') then
        vale(2)=zr(jabsc(1))
    else
        vale(2)=zr(jabsc(1)+nbabsc(1)-1)
    endif
!
    do 16 i = 1, nbinst(1)

        vale(1) = zr(jinst(1)+i-1)

        do 14 j = 1, ncmp
!
            if (n1(1) .ne. 0) then
                call tbliva(tabther(1), 2, valek, [ibid], vale,&
                            [cbid], k8b, crit, prec, nocmp(j),&
                            k8b, ibid, contraintesth(1), cbid, k8b,&
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
                           k8b, ibid, contraintespr(1), cbid, k8b,&
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
                           k8b, ibid, contraintesmec(1), cbid, k8b,&
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
            zr(jsigp+(i-1)*ncmp+j-1) = contraintesmec(1)+contraintespr(1)+ contraintesth(1)
            zr(jmecap+(i-1)*ncmp+j-1) = contraintesmec(1)+contraintespr(1)
 14     continue
 16 continue
!
    do 22 i = 1, nbinst(1)
        do 32 l = 1, nbinst(1)
            do 42 j = 1, ncmp
                diffsig(j)=zr(jsigp+(i-1)*ncmp+j-1)-zr(jsigp+(l-1)*ncmp+j-1)
                diffsime(j)=zr(jmecap+(i-1)*ncmp+j-1)-zr(jmecap+(l-1)*ncmp+j-1)
                if (j .eq. ncmp) then
                    call rctres(diffsig, tresca)
                    call rctres(diffsime, tresme)
                    if (tresca .gt. trescapp) then
                        trescapp=tresca
                        tresmepp=tresme
                        instp1 = i
                        instp2 = l
                    endif
                endif
         42 continue
     32 continue
 22 continue
!
    do 115 j = 1, 6
        sptran(j) = zr(jsigp+(instp1-1)*ncmp+j-1)-zr(jsigp+(instp2-1)*ncmp+j-1)
        spmec(j) = zr(jmecap+(instp1-1)*ncmp+j-1)-zr(jmecap+(instp2-1)*ncmp+j-1)
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
                                        sptrans(j) = sptran(j)+e1(i1)*zr(jseis+j-1)+&
                                                e2(i2)*zr(jseis+1*6+j-1)+&
                                                e3(i3)*zr(jseis+2*6+j-1)+e4(i4)*zr(jseis+3*6+j-1)+&
                                                e5(i5)*zr(jseis+4*6+j-1)+e6(i6)*zr(jseis+5*6+j-1)
                                        spmecs(j) = spmec(j)+e1(i1)*zr(jseis+j-1)+&
                                                e2(i2)*zr(jseis+1*6+j-1)+&
                                                e3(i3)*zr(jseis+2*6+j-1)+e4(i4)*zr(jseis+3*6+j-1)+&
                                                e5(i5)*zr(jseis+4*6+j-1)+e6(i6)*zr(jseis+5*6+j-1)
79                              continue
                                call rctres(sptrans,tresca)
                                call rctres(spmecs,tresme)
                                trescapps = max(trescapps,tresca)
                                tresmepps = max(tresmepps,tresme)
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
!                ET CALCUL DE SP(Q,Q)
!--------------------------------------------------------------
    if (nq .eq. 0) goto 555
!
    ndim = ncmp*nbinst(2)
    call wkvect('&&RC3200.SIGMQ', 'V V R', ndim, jsigq)
    call wkvect('&&RC3200.SIGMQ.MECA', 'V V R', ndim, jmecaq)
    if (lieu .eq. 'ORIG') then
        vale(2)=zr(jabsc(2))
    else
        vale(2)=zr(jabsc(2)+nbabsc(2)-1)
    endif
!
    do 17 i = 1, nbinst(2)

        vale(1) = zr(jinst(2)+i-1)

        do 15 j = 1, ncmp
!
            if (n1(2) .ne. 0) then
                call tbliva(tabther(2), 2, valek, [ibid], vale,&
                            [cbid], k8b, crit, prec, nocmp(j),&
                            k8b, ibid, contraintesth(1), cbid, k8b,&
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
                           k8b, ibid, contraintespr(1), cbid, k8b,&
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
                           k8b, ibid, contraintesmec(1), cbid, k8b,&
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
            zr(jsigq+(i-1)*ncmp+j-1) = contraintesmec(1)+contraintespr(1)+ contraintesth(1)
            zr(jmecaq+(i-1)*ncmp+j-1) = contraintesmec(1)+contraintespr(1)
 15     continue
 17 continue
!
    do 24 i = 1, nbinst(2)
        do 34 l = 1, nbinst(2)
            do 44 j = 1, ncmp
                diffsig(j)=zr(jsigq+(i-1)*ncmp+j-1)-zr(jsigq+(l-1)*ncmp+j-1)
                diffsime(j)=zr(jmecaq+(i-1)*ncmp+j-1)-zr(jmecaq+(l-1)*ncmp+j-1)
                if (j .eq. ncmp) then
                    call rctres(diffsig, tresca)
                    call rctres(diffsime, tresme)
                    if (tresca .gt. trescaqq) then
                        trescaqq=tresca
                        tresmeqq=tresme
                        instq1 = i
                        instq2 = l
                    endif
                endif
         44 continue
     34 continue
 24 continue
!
555 continue 
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
                                        diffsime(j)=zr(jmecaq+(instq1-1)*ncmp+j-1)-&
                                                    zr(jmecaq+(instq2-1)*ncmp+j-1)
                                        sptrans(j) = diffsig(j)+e1(i1)*zr(jseis+j-1)+&
                                                e2(i2)*zr(jseis+1*6+j-1)+&
                                                e3(i3)*zr(jseis+2*6+j-1)+e4(i4)*zr(jseis+3*6+j-1)+&
                                                e5(i5)*zr(jseis+4*6+j-1)+e6(i6)*zr(jseis+5*6+j-1)
                                        spmecs(j) = diffsime(j)+e1(i1)*zr(jseis+j-1)+&
                                                e2(i2)*zr(jseis+1*6+j-1)+&
                                                e3(i3)*zr(jseis+2*6+j-1)+e4(i4)*zr(jseis+3*6+j-1)+&
                                                e5(i5)*zr(jseis+4*6+j-1)+e6(i6)*zr(jseis+5*6+j-1)
173                             continue
                                call rctres(sptrans,tresca)
                                call rctres(spmecs,tresme)
                                trescaqqs = max(trescaqqs,tresca)
                                tresmeqqs = max(tresmeqqs,tresme)
113                          continue
123                      continue
133                  continue
143              continue
153          continue
163      continue
    endif
!
!--------------------------------------------------------------
!                CALCUL DE SP(P,Q)
!--------------------------------------------------------------
    if (np*nq .eq. 0) goto 444
!
    do 26 i = 1, nbinst(1)
        do 36 l = 1, nbinst(2)
            do 46 j = 1, ncmp
                diffsig(j)=zr(jsigp+(i-1)*ncmp+j-1)-zr(jsigq+(l-1)*ncmp+j-1)
                diffsime(j)=zr(jmecap+(i-1)*ncmp+j-1)-zr(jmecaq+(l-1)*ncmp+j-1)
                if (j .eq. ncmp) then
                    call rctres(diffsig, tresca)
                    call rctres(diffsime, tresme)
                    if (tresca .gt. trescapq) then
                        trescapq=tresca
                        tresmepq=tresme
                        instp1c = i
                        instq1c = l
                    endif
                endif
         46 continue
     36 continue
 26 continue
!
    tresca1 = 0.d0
    tresca2 = 0.d0
    do 27 i = 1, nbinst(1)
        do 47 j = 1, ncmp
            diffsig(j)=zr(jsigp+(i-1)*ncmp+j-1)-zr(jsigp+(instp1c-1)*ncmp+j-1)
            if (j .eq. ncmp) then
                call rctres(diffsig, tresca)
                if (tresca .gt. tresca1) then
                    tresca1=tresca
                    instp2c = i
                endif
             endif
     47 continue
 27 continue
    do 28 l = 1, nbinst(2)
        do 48 j = 1, ncmp
            diffsig(j)=zr(jsigq+(l-1)*ncmp+j-1)-zr(jsigq+(instq1c-1)*ncmp+j-1)
            if (j .eq. ncmp) then
                call rctres(diffsig, tresca)
                if (tresca .gt. tresca2) then
                    tresca2=tresca
                    instq2c = l
                endif
            endif
     48 continue
 28 continue
!
    do 149 j = 1, ncmp
        diffsig(j)=zr(jsigq+(instq2c-1)*ncmp+j-1)-zr(jsigp+(instp2c-1)*ncmp+j-1)
        diffsime(j)=zr(jmecaq+(instq2c-1)*ncmp+j-1)-zr(jmecap+(instp2c-1)*ncmp+j-1)
        if (j .eq. ncmp) then
            call rctres(diffsig, trescaqp)
            call rctres(diffsime, tresmeqp)
        endif
 149 continue
!
    if (seismeb32) then
        do 162 i1 = 1, 2
            do 152 i2 = 1, 2
                do 142 i3 = 1, 2
                    do 132 i4 = 1, 2
                        do 122 i5 = 1, 2
                            do 112 i6 = 1, 2
                                do 172 j = 1, 6
                                        diffsig(j)=zr(jsigp+(instp1c-1)*ncmp+j-1)-&
                                                   zr(jsigq+(instq1c-1)*ncmp+j-1)
                                        diffsig2(j)=zr(jsigp+(instp2c-1)*ncmp+j-1)-&
                                                    zr(jsigq+(instq2c-1)*ncmp+j-1)
                                        diffsime(j)=zr(jmecap+(instp1c-1)*ncmp+j-1)-&
                                                    zr(jmecaq+(instq1c-1)*ncmp+j-1)
                                        diffsime2(j)=zr(jmecap+(instp2c-1)*ncmp+j-1)-&
                                                     zr(jmecaq+(instq2c-1)*ncmp+j-1)
                                        sptran1(j) = diffsig(j)+e1(i1)*zr(jseis+j-1)+&
                                                e2(i2)*zr(jseis+1*6+j-1)+&
                                                e3(i3)*zr(jseis+2*6+j-1)+e4(i4)*zr(jseis+3*6+j-1)+&
                                                e5(i5)*zr(jseis+4*6+j-1)+e6(i6)*zr(jseis+5*6+j-1)
                                        sptran2(j) = diffsig2(j)+e1(i1)*zr(jseis+j-1)+&
                                                e2(i2)*zr(jseis+1*6+j-1)+&
                                                e3(i3)*zr(jseis+2*6+j-1)+e4(i4)*zr(jseis+3*6+j-1)+&
                                                e5(i5)*zr(jseis+4*6+j-1)+e6(i6)*zr(jseis+5*6+j-1)
                                        spmec1(j) = diffsime(j)+e1(i1)*zr(jseis+j-1)+&
                                                e2(i2)*zr(jseis+1*6+j-1)+&
                                                e3(i3)*zr(jseis+2*6+j-1)+e4(i4)*zr(jseis+3*6+j-1)+&
                                                e5(i5)*zr(jseis+4*6+j-1)+e6(i6)*zr(jseis+5*6+j-1)
                                        spmec2(j) = diffsime2(j)+e1(i1)*zr(jseis+j-1)+&
                                                e2(i2)*zr(jseis+1*6+j-1)+&
                                                e3(i3)*zr(jseis+2*6+j-1)+e4(i4)*zr(jseis+3*6+j-1)+&
                                                e5(i5)*zr(jseis+4*6+j-1)+e6(i6)*zr(jseis+5*6+j-1)
172                             continue
                                call rctres(sptran1,tresca1)
                                call rctres(sptran2,tresca2)
                                call rctres(spmec1,tresme1)
                                call rctres(spmec2,tresme2)
                                tresca=max(tresca1,tresca2)
                                tresme=max(tresme1,tresme2)
                                if (tresca .gt. trescapqs) then 
                                    trescapqs = max(tresca1,tresca2)
                                    trescaqps = min(tresca1,tresca2)
                                endif
                                if (tresme .gt. tresmepqs) then 
                                    tresmepqs = max(tresme1,tresme2)
                                    tresmeqps = min(tresme1,tresme2)
                                endif
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
!        ON PREND LE MAX DE SP(P,P), SP(Q,Q) et SP(P,Q)
!--------------------------------------------------------------
    trescamax=max(trescapp, trescaqq, trescapq)
!
    if(trescamax .eq. trescapp) then
        sp1(1) = trescapp
        sp1(2) = trescaqq
        spmeca1(1) = tresmepp
        spmeca1(2) = tresmeqq
        if (np .ne. 0) then
            instsp(1) = zr(jinst(1)+instp1-1)
            instsp(2) = zr(jinst(1)+instp2-1)
        endif
        if (nq .ne. 0) then
            instsp(3) = zr(jinst(2)+instq1-1)
            instsp(4) = zr(jinst(2)+instq2-1)
        endif
    else if (trescamax .eq. trescaqq) then
        sp1(1) = trescaqq
        sp1(2) = trescapp
        spmeca1(1) = tresmeqq
        spmeca1(2) = tresmepp
        if (np .ne. 0) then
            instsp(3) = zr(jinst(1)+instp1-1)
            instsp(4) = zr(jinst(1)+instp2-1)
        endif
        if (nq .ne. 0) then
            instsp(1) = zr(jinst(2)+instq1-1)
            instsp(2) = zr(jinst(2)+instq2-1)
        endif
    else if (trescamax .eq. trescapq) then
        sp1(1) = trescapq
        sp1(2) = trescaqp
        spmeca1(1) = tresmepq
        spmeca1(2) = tresmeqp
        if (np*nq .ne. 0) then
            instsp(1) = zr(jinst(1)+instp1c-1)
            instsp(2) = zr(jinst(2)+instq1c-1)
            instsp(3) = zr(jinst(1)+instp2-1)
            instsp(4) = zr(jinst(2)+instq2-1)
        endif
    endif
    if (seismeb32) then
        trescamax=max(trescapps, trescaqqs, trescapqs)
        if(trescamax .eq. trescapps) then
            sp1(1) = trescapps
            sp1(2) = trescaqqs
            spmeca1(1) = tresmepps
            spmeca1(2) = tresmeqqs
            if (np .ne. 0) then
                instsp(1) = zr(jinst(1)+instp1-1)
                instsp(2) = zr(jinst(1)+instp2-1)
            endif
            if (nq .ne. 0) then
                instsp(3) = zr(jinst(2)+instq1-1)
                instsp(4) = zr(jinst(2)+instq2-1)
            endif
        else if (trescamax .eq. trescaqqs) then
            sp1(1) = trescaqqs
            sp1(2) = trescapps
            spmeca1(1) = tresmeqqs
            spmeca1(2) = tresmepps
            if (np .ne. 0) then
                instsp(3) = zr(jinst(1)+instp1-1)
                instsp(4) = zr(jinst(1)+instp2-1)
            endif
            if (nq .ne. 0) then
                instsp(1) = zr(jinst(2)+instq1-1)
                instsp(2) = zr(jinst(2)+instq2-1)
            endif
        else if (trescamax .eq. trescapqs) then
            if (np*nq .ne. 0) then
                instsp(1) = zr(jinst(1)+instp1c-1)
                instsp(2) = zr(jinst(2)+instq1c-1)
                instsp(3) = zr(jinst(1)+instp2-1)
                instsp(4) = zr(jinst(2)+instq2-1)
            endif
            sp1(1) = trescapqs
            sp1(2) = trescaqps
            spmeca1(1) = tresmepqs
            spmeca1(2) = tresmeqps
        endif
    endif
!
!--------------------------------------------------------------
    if (np .ne. 0) then
        call jedetr('RC.INSTANT.P')
        call jedetr('RC.ABSC.P')
        call jedetr('&&RC3200.SIGMP')
        call jedetr('&&RC3200.SIGMP.MECA')
    endif
    if (nq .ne. 0) then
        call jedetr('RC.INSTANT.Q')
        call jedetr('RC.ABSC.Q')
        call jedetr('&&RC3200.SIGMQ')
        call jedetr('&&RC3200.SIGMQ.MECA')
    endif
!
    AS_DEALLOCATE(vr=contraintesmec)
    AS_DEALLOCATE(vr=contraintesth)
    AS_DEALLOCATE(vr=contraintespr)
!
999 continue
!
    call jedema()
end subroutine
