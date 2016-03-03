subroutine rc32t()
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/jemarq.h"
#include "asterc/getfac.h"
#include "asterfort/jecrec.h"
#include "asterfort/getvis.h"
#include "asterfort/codent.h"
#include "asterfort/getvid.h"
#include "asterfort/rcveri.h"
#include "asterfort/tbexip.h"
#include "asterfort/utmess.h"
#include "asterfort/tbexv1.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rcver1.h"
#include "asterfort/as_allocate.h"
#include "asterfort/jecroc.h"
#include "asterfort/jeecra.h"
#include "asterfort/jexnom.h"
#include "asterfort/tbliva.h"
#include "asterfort/rc32my.h"
#include "asterfort/rctres.h"
#include "asterfort/trace.h"
#include "asterfort/jedetr.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/jedema.h"
!     ------------------------------------------------------------------
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ------------------------------------------------------------------
!     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_ZE200 et B3200
!     STOCKAGE DES CONTRAINTES TOTALES ET LINEARISEES :
!                 - THERMIQUES SOUS "RESU_THER"
!                 - DE PRESSION SOUS "RESU_PRES"
!                 - DUS AUX EFFORTS ET MOMENTS SOUS "RESU_MECA"
!     ------------------------------------------------------------------
!
    integer :: nbsitu, nbther, nbpres, nbmeca, iocc, numesitu, n0
    character(len=24) :: jvorig, jvextr, valk(4)
    character(len=8) :: nocmp(6), crit(2), knume, tabther, tabpres
    character(len=16) :: valek(2)
    integer :: nume1, n1, nume2, n2, nume3, n3, nn, ither, numether
    integer :: n4, ipres, numepres, imeca, numemeca, nbinst, jinst
    character(len=8) :: tabmeca, tableok, k8b
    aster_logical :: exist
    integer :: nbabsc, jabsc, ncmp, ndim, jorig, jextr, k, i, j
    real(kind=8) :: prec(2)
    real(kind=8) :: sigtot(1000*6), tminsn(2), tmaxsn(2), tminsp(2)
    real(kind=8) :: tmaxsp(2), tresc(2), trescb(2), r3(2), r3b(2)
    real(kind=8) :: tremin(2), treminb(2), tremax(2), tremaxb(2)
    real(kind=8) :: vale(2), momen0, momen1, siglin(1000*6)
    integer :: ibid, iret, kk, l
    complex(kind=8) :: cbid
    real(kind=8), pointer :: contraintesth(:) => null()
    real(kind=8), pointer :: contraintespr(:) => null()
    real(kind=8), pointer :: contraintestot(:) => null()
    real(kind=8), pointer :: contraintesmec(:) => null()
    real(kind=8), pointer :: contraintesm(:) => null()
! DEB ------------------------------------------------------------------
    call jemarq()
!
    call getfac('SITUATION', nbsitu)
!
    jvorig = '&&RC3200.TRANSIT.ORIG'
    jvextr = '&&RC3200.TRANSIT.EXTR'
    call jecrec(jvorig, 'V V R', 'NO', 'DISPERSE', 'VARIABLE',&
                nbsitu)
    call jecrec(jvextr, 'V V R', 'NO', 'DISPERSE', 'VARIABLE',&
                nbsitu)
!
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
    do 10 iocc = 1, nbsitu, 1
!
! ------ on récupère le numéro de la situation
        call getvis('SITUATION', 'NUME_SITU', iocc=iocc, scal=numesitu, nbret=n0)
        knume = 'S       '
        call codent(numesitu, 'D0', knume(2:8))
!
!------ on récupère les numéros des tables sous le mot clé situation 
!------ puis les tables associées sous RESU_THER, RESU_PRES et RESU_MECA 
        call getvis('SITUATION', 'NUME_RESU_THER', iocc=iocc, scal=nume1, nbret=n1)
        call getvis('SITUATION', 'NUME_RESU_PRES', iocc=iocc, scal=nume2, nbret=n2)
        call getvis('SITUATION', 'NUME_RESU_MECA', iocc=iocc, scal=nume3, nbret=n3)
        nn= n1+n2+n3
        if (nn .eq. 0) goto 888
!
        if (n1 .ne. 0) then 
            do 20 ither =1, nbther, 1
                call getvis('RESU_THER', 'NUME_RESU_THER', iocc=ither, scal=numether, nbret=n4)
                if (numether .eq. nume1) then
                    call getvid('RESU_THER', 'TABL_RESU_THER', iocc=ither, scal=tabther, nbret=n4)
                endif
20          continue
        endif
!
        if (n2 .ne. 0) then 
            do 30 ipres =1, nbpres, 1
                call getvis('RESU_PRES', 'NUME_RESU_PRES', iocc=ipres, scal=numepres, nbret=n4)
                if (numepres .eq. nume2) then
                    call getvid('RESU_PRES', 'TABL_RESU_PRES', iocc=ipres, scal=tabpres, nbret=n4)
                endif
30          continue
        endif
!
        if (n3 .ne. 0) then 
            do 40 imeca =1, nbmeca, 1
                call getvis('RESU_MECA', 'NUME_RESU_MECA', iocc=imeca, scal=numemeca, nbret=n4)
                if (numemeca .eq. nume3) then
                    call getvid('RESU_MECA', 'TABL_RESU_MECA', iocc=imeca, scal=tabmeca, nbret=n4)
                endif
40          continue
        endif
!----------------------------------------------------
! ------ RECUPERATION DES INSTANTS ET DES ABSCISSES
!----------------------------------------------------
! ------ grace a la table thermique ou de pression ou mecanique
        if (n1 .ne. 0) then
            tableok = tabther
        elseif (n2 .ne. 0) then
            tableok = tabpres
        else
            tableok = tabmeca
        endif
!  
! --------- on verifie l'ordre des noeuds de la table
        call rcveri(tableok)
! 
! --------- on recupere les instants de la table
        call tbexip(tableok, valek(1), exist, k8b)
        if (.not. exist) then
            valk (1) = tableok
            valk (2) = valek(1)
            call utmess('F', 'POSTRCCM_1', nk=2, valk=valk)
        endif
        call tbexv1(tableok, valek(1), 'RC.INSTANT', 'V', nbinst,&
                    k8b)
        call jeveuo('RC.INSTANT', 'L', jinst)
!
! --------- on recupere les abscisses curvilignes de la table
        call tbexip(tableok, valek(2), exist, k8b)
        if (.not. exist) then
            valk (1) = tableok
            valk (2) = valek(2)
            call utmess('F', 'POSTRCCM_1', nk=2, valk=valk)
        endif
        call tbexv1(tableok, valek(2), 'RC.ABSC', 'V', nbabsc,&
                    k8b)
        call jeveuo('RC.ABSC', 'L', jabsc)
!
! ------ ON VERIFIE LA COHERENCE DES TABLES (THERMIQUE, PRESSION ET MECA)
        if (n1 .ne. 0) then
            if (n2 .ne. 0) then
                call rcver1('MECANIQUE', tabpres, tabther)
            endif
            if (n3 .ne. 0) then
                call rcver1('MECANIQUE', tabmeca, tabther)
            endif
        endif
        if (n2 .ne. 0) then
            if (n3 .ne. 0) then
                call rcver1('MECANIQUE', tabmeca, tabpres)
            endif
        endif
!
!---------------------------------------------------------------
! ------ CREATION DES 2*5 TABLES A 6 composantes de contraintes
! ------ (totales+ mécaniques en tminsp et tmaxsp)
! ------ (linéarisées + M_0 + M_1 en tminsn et tmaxsn)
!---------------------------------------------------------------
!
        AS_ALLOCATE(vr=contraintesth,  size=nbabsc)
        AS_ALLOCATE(vr=contraintespr,  size=nbabsc)
        AS_ALLOCATE(vr=contraintesmec, size=nbabsc)
        AS_ALLOCATE(vr=contraintestot, size=nbabsc)
        AS_ALLOCATE(vr=contraintesm,   size=nbabsc)
!
        ncmp=6
        ndim = 2 * 5 * ncmp
        call jecroc(jexnom(jvorig, knume))
        call jeecra(jexnom(jvorig, knume), 'LONMAX', ndim)
        call jeecra(jexnom(jvorig, knume), 'LONUTI', ndim)
        call jeveuo(jexnom(jvorig, knume), 'E', jorig)
!
        call jecroc(jexnom(jvextr, knume))
        call jeecra(jexnom(jvextr, knume), 'LONMAX', ndim)
        call jeecra(jexnom(jvextr, knume), 'LONUTI', ndim)
        call jeveuo(jexnom(jvextr, knume), 'E', jextr)
!
        do 116 k = 1, nbabsc*ncmp
            sigtot(k) = 0.d0
116     continue
!
        do 1116 k = 1, 2
            tminsn(k) = 0.d0
            tmaxsn(k) = 0.d0
            tminsp(k) = 0.d0
            tmaxsp(k) = 0.d0
            tresc(k) = 0.d0
            trescb(k) = 0.d0
            r3(k) = 0.d0
            r3b(k) = 0.d0
            tremin(k) = 0.d0
            treminb(k) = 0.d0
            tremax(k) = 0.d0
            tremaxb(k) = 0.d0
1116    continue
!
! --------- ETAPE 1 : Déterminer tminsp, tmaxsp, tminsn, tmasxn
!
        do 12 i = 1, nbinst
!
            vale(1) = zr(jinst+i-1)
!
            do 14 j = 1, ncmp
!
                do 16 k = 1, nbabsc
!
                    vale(2) = zr(jabsc+k-1)
!
                    if (n1 .ne. 0) then
                        call tbliva(tabther, 2, valek, [ibid], vale,&
                                    [cbid], k8b, crit, prec, nocmp(j),&
                                    k8b, ibid, contraintesth(k), cbid, k8b,&
                                    iret)
                        if (iret .ne. 0) then
                            valk (1) = tabther
                            valk (2) = nocmp(j)
                            valk (3) = valek(1)
                            valk (4) = valek(2)
                            call utmess('F', 'POSTRCCM_2', nk=4, valk=valk, nr=2,&
                                        valr=vale)
                        endif
                    endif
                    if (n2 .ne. 0) then
                        call tbliva(tabpres, 2, valek, [ibid], vale,&
                                    [cbid], k8b, crit, prec, nocmp(j),&
                                    k8b, ibid, contraintespr(k), cbid, k8b,&
                                    iret)
                        if (iret .ne. 0) then
                            valk (1) = tabpres
                            valk (2) = nocmp(j)
                            valk (3) = valek(1)
                            valk (4) = valek(2)
                            call utmess('F', 'POSTRCCM_2', nk=4, valk=valk, nr=2,&
                                        valr=vale)
                        endif
                    endif
                    if (n3 .ne. 0) then
                        call tbliva(tabmeca, 2, valek, [ibid], vale,&
                                    [cbid], k8b, crit, prec, nocmp(j),&
                                    k8b, ibid, contraintesmec(k), cbid, k8b,&
                                    iret)
                        if (iret .ne. 0) then
                            valk (1) = tabmeca
                            valk (2) = nocmp(j)
                            valk (3) = valek(1)
                            valk (4) = valek(2)
                            call utmess('F', 'POSTRCCM_2', nk=4, valk=valk, nr=2,&
                                        valr=vale)
                        endif
                    endif
                    contraintestot(k)= contraintesmec(k)+contraintespr(k)+ contraintesth(k)
                    sigtot((k-1)*ncmp+j) = contraintesmec(k)+contraintespr(k)+ contraintesth(k)
 16             continue
                call rc32my(nbabsc, zr(jabsc), contraintestot, momen0, momen1)
                siglin(1*j)= momen0 - 0.5d0*momen1
                siglin((nbabsc-1)*ncmp+j)= momen0 + 0.5d0*momen1
 14         continue
!
! --------- tmaxsp et tminsp
!
            kk=0
            do 18 k = 1, nbabsc, nbabsc-1
                kk=kk+1
                call rctres(sigtot((k-1)*ncmp+1), tresc(kk))
!
                if (trace(3,sigtot((k-1)*ncmp+1)) .lt. 0.d0) then
                    r3(kk) = tresc(kk)*(-1.0d0)
                else
                    r3(kk) = tresc(kk)
                endif
                if (i .eq. 1) then
                    tremin(kk) = r3(kk)
                    tremax(kk) = r3(kk)
                    tminsp(kk) = vale(1)
                    tmaxsp(kk) = vale(1)
                else
                    if (r3(kk) .lt. tremin(kk)) then
                        tremin(kk) = r3(kk)
                        tminsp(kk) = vale(1)
                    endif
                    if (r3(kk) .gt. tremax(kk)) then
                        tremax(kk) = r3(kk)
                        tmaxsp(kk) = vale(1)
                    endif
                endif
 18         continue
!
! --------- tmaxsn et tminsn
!
            kk=0
!
            do 19 k = 1, nbabsc, nbabsc-1
                kk=kk+1
                call rctres(siglin((k-1)*ncmp+1), trescb(kk))
!
                if (trace(3,siglin((k-1)*ncmp+1)) .lt. 0.d0) then
                    r3b(kk) = trescb(kk)*(-1.0d0)
                else
                    r3b(kk) = trescb(kk)
                endif
                if (i .eq. 1) then
                    treminb(kk) = r3b(kk)
                    tremaxb(kk) = r3b(kk)
                    tminsn(kk) = vale(1)
                    tmaxsn(kk) = vale(1)
                else
                    if (r3b(kk) .lt. treminb(kk)) then
                        treminb(kk) = r3b(kk)
                        tminsn(kk) = vale(1)
                    endif
                    if (r3b(kk) .gt. tremaxb(kk)) then
                        tremaxb(kk) = r3b(kk)
                        tmaxsn(kk) = vale(1)
                    endif
                endif
 19         continue
!
 12     continue
!
!
! --------- ETAPE 2 : Remplir les vecteurs 
!
! --------- pour tminsp a l'origine    
        do 66 j = 1, ncmp
            vale(1) = tminsp(1)
            vale(2) = zr(jabsc)
            if (n1 .ne. 0) then
                call tbliva(tabther, 2, valek, [ibid], vale,&
                            [cbid], k8b, crit, prec, nocmp(j),&
                            k8b, ibid, contraintesth(1), cbid, k8b,&
                            iret)
            endif
            if (n2 .ne. 0) then
                call tbliva(tabpres, 2, valek, [ibid], vale,&
                            [cbid], k8b, crit, prec, nocmp(j),&
                            k8b, ibid, contraintespr(1), cbid, k8b,&
                            iret)
            endif
            if (n3 .ne. 0) then
                call tbliva(tabmeca, 2, valek, [ibid], vale,&
                            [cbid], k8b, crit, prec, nocmp(j),&
                            k8b, ibid, contraintesmec(1), cbid, k8b,&
                            iret)
            endif
            contraintestot(1)=contraintesth(1)+contraintespr(1)+contraintesmec(1)
            contraintesm(1)=contraintespr(1)+contraintesmec(1)
!
            zr(jorig-1+j) = contraintestot(1)
!
            l = ncmp*8 + j
            zr(jorig-1+l) = contraintesm(1)
!
! --------- pour tminsn a l'origine
!
            vale(1) = tminsn(1)
            do 167 k = 1, nbabsc
                vale(2) = zr(jabsc+k-1)
                if (n1 .ne. 0) then
                    call tbliva(tabther, 2, valek, [ibid], vale,&
                                [cbid], k8b, crit, prec, nocmp(j),&
                                k8b, ibid, contraintesth(k), cbid, k8b,&
                                iret)
                endif
                if (n2 .ne. 0) then
                    call tbliva(tabpres, 2, valek, [ibid], vale,&
                                [cbid], k8b, crit, prec, nocmp(j),&
                                k8b, ibid, contraintespr(k), cbid, k8b,&
                                iret)
                endif
                if (n3 .ne. 0) then
                    call tbliva(tabmeca, 2, valek, [ibid], vale,&
                                [cbid], k8b, crit, prec, nocmp(j),&
                                k8b, ibid, contraintesmec(k), cbid, k8b,&
                                iret)
                endif
            contraintestot(k)=contraintesth(k)+contraintespr(k)+contraintesmec(k)
 167        continue
!
            call rc32my(nbabsc, zr(jabsc), contraintestot, momen0, momen1)
!
            l = ncmp*2 + j
            zr(jorig-1+l) = momen0 - 0.5d0*momen1
!
            l = ncmp*4 + j
            zr(jorig-1+l) = momen0
!
            l = ncmp*6 + j
            zr(jorig-1+l) = 0.5d0*momen1
!
! --------- pour tminsp a l'extremite 
            vale(1) = tminsp(2)
            vale(2) = zr(jabsc+nbabsc-1)
            if (n1 .ne. 0) then
                call tbliva(tabther, 2, valek, [ibid], vale,&
                            [cbid], k8b, crit, prec, nocmp(j),&
                            k8b, ibid, contraintesth(nbabsc), cbid, k8b,&
                            iret)
            endif
            if (n2 .ne. 0) then
                call tbliva(tabpres, 2, valek, [ibid], vale,&
                            [cbid], k8b, crit, prec, nocmp(j),&
                            k8b, ibid, contraintespr(nbabsc), cbid, k8b,&
                            iret)
            endif
            if (n3 .ne. 0) then
                call tbliva(tabmeca, 2, valek, [ibid], vale,&
                            [cbid], k8b, crit, prec, nocmp(j),&
                            k8b, ibid, contraintesmec(nbabsc), cbid, k8b,&
                            iret)
            endif
            contraintestot(nbabsc)=contraintesth(nbabsc)+contraintespr(nbabsc)+contraintesmec(nbabsc)
            contraintesm(nbabsc)=contraintespr(nbabsc)+contraintesmec(nbabsc)
!
            zr(jextr-1+j) = contraintestot(nbabsc)
!
            l = ncmp*8 + j
            zr(jextr-1+l) = contraintesm(nbabsc)
!
! --------- pour tminsn a l'extremite 
            vale(1) = tminsn(2)
            do 267 k = 1, nbabsc
                vale(2) = zr(jabsc+k-1)
!
                if (n1 .ne. 0) then
                    call tbliva(tabther, 2, valek, [ibid], vale,&
                                [cbid], k8b, crit, prec, nocmp(j),&
                                k8b, ibid, contraintesth(k), cbid, k8b,&
                                iret)
                endif
                if (n2 .ne. 0) then
                    call tbliva(tabpres, 2, valek, [ibid], vale,&
                                [cbid], k8b, crit, prec, nocmp(j),&
                                k8b, ibid, contraintespr(k), cbid, k8b,&
                                iret)
                endif
                if (n3 .ne. 0) then
                    call tbliva(tabmeca, 2, valek, [ibid], vale,&
                                [cbid], k8b, crit, prec, nocmp(j),&
                                k8b, ibid, contraintesmec(k), cbid, k8b,&
                                iret)
                endif
            contraintestot(k)=contraintesth(k)+contraintespr(k)+contraintesmec(k)
 267        continue
!
            call rc32my(nbabsc, zr(jabsc), contraintestot, momen0, momen1)
!
            l = ncmp*2 + j
            zr(jextr-1+l) = momen0 + 0.5d0*momen1
!
            l = ncmp*4 + j
            zr(jextr-1+l) = momen0
!
            l = ncmp*6 + j
            zr(jextr-1+l) = 0.5d0*momen1
!
!
! --------- pour tmaxsp a l'origine 
            vale(1) = tmaxsp(1)
            vale(2) = zr(jabsc)
            if (n1 .ne. 0) then
                call tbliva(tabther, 2, valek, [ibid], vale,&
                            [cbid], k8b, crit, prec, nocmp(j),&
                            k8b, ibid, contraintesth(1), cbid, k8b,&
                            iret)
            endif
            if (n2 .ne. 0) then
                call tbliva(tabpres, 2, valek, [ibid], vale,&
                            [cbid], k8b, crit, prec, nocmp(j),&
                            k8b, ibid, contraintespr(1), cbid, k8b,&
                            iret)
            endif
            if (n3 .ne. 0) then
                call tbliva(tabmeca, 2, valek, [ibid], vale,&
                            [cbid], k8b, crit, prec, nocmp(j),&
                            k8b, ibid, contraintesmec(1), cbid, k8b,&
                            iret)
            endif
            contraintestot(1)=contraintesth(1)+contraintespr(1)+contraintesmec(1)
            contraintesm(1)=contraintespr(1)+contraintesmec(1)
!
            l = ncmp + j
            zr(jorig-1+l) = contraintestot(1)
!
            l = ncmp*9 + j
            zr(jorig-1+l) = contraintesm(1)
!
! --------- pour tmaxsn a l'origine 
            vale(1) = tmaxsn(1)
            do 367 k = 1, nbabsc
                vale(2) = zr(jabsc+k-1)
!
                if (n1 .ne. 0) then
                    call tbliva(tabther, 2, valek, [ibid], vale,&
                                [cbid], k8b, crit, prec, nocmp(j),&
                                k8b, ibid, contraintesth(k), cbid, k8b,&
                                iret)
                endif
                if (n2 .ne. 0) then
                    call tbliva(tabpres, 2, valek, [ibid], vale,&
                                [cbid], k8b, crit, prec, nocmp(j),&
                                k8b, ibid, contraintespr(k), cbid, k8b,&
                                iret)
                endif
                if (n3 .ne. 0) then
                    call tbliva(tabmeca, 2, valek, [ibid], vale,&
                                [cbid], k8b, crit, prec, nocmp(j),&
                                k8b, ibid, contraintesmec(k), cbid, k8b,&
                                iret)
                endif
            contraintestot(k)=contraintesth(k)+contraintespr(k)+contraintesmec(k)
 367        continue
!
            call rc32my(nbabsc, zr(jabsc), contraintestot, momen0, momen1)
!
            l = ncmp*3 + j
            zr(jorig-1+l) = momen0 - 0.5d0*momen1
!
            l = ncmp*5 + j
            zr(jorig-1+l) = momen0
!
            l = ncmp*7 + j
            zr(jorig-1+l) = 0.5d0*momen1
!
! --------- pour tmaxsp a l'extremite
!
            vale(1) = tmaxsp(2)
            vale(2) = zr(jabsc+nbabsc-1)
            if (n1 .ne. 0) then
                call tbliva(tabther, 2, valek, [ibid], vale,&
                            [cbid], k8b, crit, prec, nocmp(j),&
                            k8b, ibid, contraintesth(nbabsc), cbid, k8b,&
                            iret)
            endif 
            if (n2 .ne. 0) then
                call tbliva(tabpres, 2, valek, [ibid], vale,&
                            [cbid], k8b, crit, prec, nocmp(j),&
                            k8b, ibid, contraintespr(nbabsc), cbid, k8b,&
                            iret)
            endif
            if (n3 .ne. 0) then
                call tbliva(tabmeca, 2, valek, [ibid], vale,&
                            [cbid], k8b, crit, prec, nocmp(j),&
                            k8b, ibid, contraintesmec(nbabsc), cbid, k8b,&
                            iret)
            endif
            contraintestot(nbabsc)=contraintesth(nbabsc)+contraintespr(nbabsc)+contraintesmec(nbabsc)
            contraintesm(nbabsc)=contraintespr(nbabsc)+contraintesmec(nbabsc)
!
            l = ncmp + j
            zr(jextr-1+l) = contraintestot(nbabsc)
!
            l = ncmp*9 + j
            zr(jextr-1+l) = contraintesm(nbabsc)
!
! --------- pour tmaxsn a l'extremite
!
            vale(1) = tmaxsn(2)
            do 467 k = 1, nbabsc
                vale(2) = zr(jabsc+k-1)
!
                if (n1 .ne. 0) then
                    call tbliva(tabther, 2, valek, [ibid], vale,&
                                [cbid], k8b, crit, prec, nocmp(j),&
                                k8b, ibid, contraintesth(k), cbid, k8b,&
                                iret)
                endif 
                if (n2 .ne. 0) then
                    call tbliva(tabpres, 2, valek, [ibid], vale,&
                                [cbid], k8b, crit, prec, nocmp(j),&
                                k8b, ibid, contraintespr(k), cbid, k8b,&
                                iret)
                endif
                if (n3 .ne. 0) then
                    call tbliva(tabmeca, 2, valek, [ibid], vale,&
                                [cbid], k8b, crit, prec, nocmp(j),&
                                k8b, ibid, contraintesmec(k), cbid, k8b,&
                                iret)
                endif
            contraintestot(k)=contraintesth(k)+contraintespr(k)+contraintesmec(k)
 467        continue
!
            call rc32my(nbabsc, zr(jabsc), contraintestot, momen0, momen1)
!
            l = ncmp*3 + j
            zr(jextr-1+l) = momen0 + 0.5d0*momen1
!
            l = ncmp*5 + j
            zr(jextr-1+l) = momen0
!
            l = ncmp*7 + j
            zr(jextr-1+l) = 0.5d0*momen1
!
 66     continue
        call jedetr('RC.INSTANT')
        call jedetr('RC.ABSC')
        AS_DEALLOCATE(vr=contraintesmec)
        AS_DEALLOCATE(vr=contraintesth)
        AS_DEALLOCATE(vr=contraintespr)
        AS_DEALLOCATE(vr=contraintestot)
        AS_DEALLOCATE(vr=contraintesm)
!
 888    continue
!
 10 end do
!  
    call jedema()
end subroutine
