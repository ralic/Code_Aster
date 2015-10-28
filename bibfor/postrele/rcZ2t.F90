subroutine rcZ2t()
    implicit none
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
!
!     ------------------------------------------------------------------
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/rc32my.h"
#include "asterfort/rctres.h"
#include "asterfort/rcver1.h"
#include "asterfort/rcveri.h"
#include "asterfort/tbexip.h"
#include "asterfort/tbexv1.h"
#include "asterfort/tbliva.h"
#include "asterfort/trace.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    integer :: ibid, n1, iocc, nbther, nume, nbinst, jinst, i, j, k, l, ndim, n4, nn
    integer :: nbabsc, jabsc, jorig, jextr, ncmp, iret, kk, nbpres, nb, n2, n3
    integer :: nume1, nume2, nbmeca, numether, numepres, numemeca, ither, ipres, imeca
    integer :: numes, n0
    parameter  ( ncmp = 6 )
    real(kind=8) :: prec(2), momen0, momen1, vale(2),sigmec(1000*ncmp),siglin(1000*ncmp)
    real(kind=8) :: sigth(1000*ncmp), tresc(2), r3(2), sigpr(1000*ncmp),sigtot(1000*ncmp)
    real(kind=8) :: tremin(2), tremax(2), tminsp(2), tmaxsp(2), tminsn(2), tmaxsn(2)
    real(kind=8) :: treminb(2), tremaxb(2), trescb(2), r3b(2)
    complex(kind=8) :: cbid
    aster_logical :: exist
    character(len=8) :: k8b, crit(2), nocmp(ncmp), table, knume, table2, table3
    character(len=8) :: tableok
    character(len=16) :: motclf, motclf2, valek(2), motclf3, motclf4
    character(len=24) :: instan, abscur, jvorig, jvextr
    character(len=24) :: valk(7)
    real(kind=8), pointer :: contraintesth(:) => null()
    real(kind=8), pointer :: contraintespr(:) => null()
    real(kind=8), pointer :: contraintestot(:) => null()
    real(kind=8), pointer :: contraintesmec(:) => null()
    real(kind=8), pointer :: contraintesm(:) => null()
! DEB ------------------------------------------------------------------
    call jemarq()
!
    motclf = 'RESU_THER'
    motclf2 = 'RESU_PRES'
    motclf3 = 'RESU_MECA'
    motclf4 = 'SITUATION'
    call getfac(motclf4, nb)
    call getfac(motclf, nbther)
    call getfac(motclf2, nbpres)
    call getfac(motclf3, nbmeca)
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
    jvorig = '&&RC3200.T .ORIG'
    jvextr = '&&RC3200.T .EXTR'
    call jecrec(jvorig, 'V V R', 'NO', 'DISPERSE', 'VARIABLE',&
                nb)
    call jecrec(jvextr, 'V V R', 'NO', 'DISPERSE', 'VARIABLE',&
                nb)
!
    do 10 iocc = 1, nb, 1
!
! ------ STOCKAGE DU TRANSITOIRE (Ti, Tj, ...) ASSOCIE A LA SITUATION (Si, Sj, ...)
!
        call getvis(motclf4, 'NUME_SITU', iocc=iocc, scal=numes, nbret=n0)
        knume = 'T       '
        call codent(numes, 'D0', knume(2:8))
!
        call getvis(motclf4, 'NUME_RESU_THER', iocc=iocc, scal=nume, nbret=n1)
        call getvis(motclf4, 'NUME_RESU_PRES', iocc=iocc, scal=nume1, nbret=n2)
        call getvis(motclf4, 'NUME_RESU_MECA', iocc=iocc, scal=nume2, nbret=n3)
        nn= n1+n2+n3
        if (nn .eq. 0) goto 888
!
        if (n1 .ne. 0) then 
            do 20 ither =1, nbther, 1
                call getvis(motclf, 'NUME_RESU_THER', iocc=ither, scal=numether, nbret=n4)
                if (numether .eq. nume) then
                    call getvid(motclf, 'TABL_RESU_THER', iocc=ither, scal=table, nbret=n4)
                endif
20          continue
        endif
!
        if (n2 .ne. 0) then 
            do 30 ipres =1, nbpres, 1
                call getvis(motclf2, 'NUME_RESU_PRES', iocc=ipres, scal=numepres, nbret=n4)
                if (numepres .eq. nume1) then
                    call getvid(motclf2, 'TABL_RESU_PRES', iocc=ipres, scal=table2, nbret=n4)
                endif
30          continue
        endif
!
        if (n3 .ne. 0) then 
            do 40 imeca =1, nbmeca, 1
                call getvis(motclf3, 'NUME_RESU_MECA', iocc=imeca, scal=numemeca, nbret=n4)
                if (numemeca .eq. nume2) then
                    call getvid(motclf3, 'TABL_RESU_MECA', iocc=imeca, scal=table3, nbret=n4)
                endif
40          continue
        endif
!
! ------ RECUPERATION DES INSTANTS ET DES ABSCISSES
!
! --------- grace a la table thermique ou de pression ou mecanique
!
        if (n1 .ne. 0) then
            tableok = table
        elseif (n2 .ne. 0) then
            tableok = table2
        else
            tableok = table3
        endif
!  
! --------- on verifie l'ordre des noeuds de la table
!
        call rcveri(tableok)
! 
! --------- on recupere les instants de la table
!
        call tbexip(tableok, valek(1), exist, k8b)
        if (.not. exist) then
            valk (1) = tableok
            valk (2) = valek(1)
            call utmess('F', 'POSTRCCM_1', nk=2, valk=valk)
        endif
        instan = '&&RCZ2.INSTANT'
        call tbexv1(tableok, valek(1), instan, 'V', nbinst,&
                    k8b)
        call jeveuo(instan, 'L', jinst)
!
! --------- on recupere les abscisses curvilignes de la table
!
        call tbexip(tableok, valek(2), exist, k8b)
        if (.not. exist) then
            valk (1) = tableok
            valk (2) = valek(2)
            call utmess('F', 'POSTRCCM_1', nk=2, valk=valk)
        endif
        abscur = '&&RCZ2.ABSC_CURV'
        call tbexv1(tableok, valek(2), abscur, 'V', nbabsc,&
                    k8b)
        call jeveuo(abscur, 'L', jabsc)
!
! ------ ON VERIFIE LA COHERENCE DES TABLES (THERMIQUE, PRESSION ET MECA)
        if (n1 .ne. 0) then
            if (n2 .ne. 0) then
                call rcver1('MECANIQUE', table2, table)
            endif
            if (n3 .ne. 0) then
                call rcver1('MECANIQUE', table3, table)
            endif
        endif
        if (n2 .ne. 0) then
            if (n3 .ne. 0) then
                call rcver1('MECANIQUE', table3, table2)
            endif
        endif
!
! ------ CREATION ET REMPLISSAGE DE 2*5 TABLES A 6 composantes 
! ------ (CONT. TOT.+ CONT. MECA. en tminsp et tmaxsp)
! ------ (CONT. TOT. LIN. + M_0 + M_1 en tminsn et tmaxsn)
!
        AS_ALLOCATE(vr=contraintesth, size=nbabsc)
        AS_ALLOCATE(vr=contraintespr, size=nbabsc)
        AS_ALLOCATE(vr=contraintesmec, size=nbabsc)
        AS_ALLOCATE(vr=contraintestot, size=nbabsc)
        AS_ALLOCATE(vr=contraintesm, size=nbabsc)
!
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
            sigth(k) = 0.d0
            sigpr(k) = 0.d0
            sigtot(k) = 0.d0
            sigmec(k) = 0.d0
116     continue
        do 1116 k = 1, 2
            tresc(k) = 0.d0
            trescb(k) = 0.d0
            r3(k) = 0.d0
            r3b(k) = 0.d0
            tminsp(k) = 0.d0
            tminsn(k) = 0.d0
            tremin(k) = 0.d0
            treminb(k) = 0.d0
            tmaxsp(k) = 0.d0
            tmaxsn(k) = 0.d0
            tremax(k) = 0.d0
            tremaxb(k) = 0.d0
1116    continue
!
        do 12 i = 1, nbinst
            vale(1) = zr(jinst+i-1)
!
            do 14 j = 1, ncmp
!
                do 16 k = 1, nbabsc
                    if (k .eq. 1 .or. k .eq. nbabsc) then 
                        kk=k
                    else
                        kk = 0
                    endif
!
                    vale(2) = zr(jabsc+k-1)
!
                    if (n1 .ne. 0) then
                        call tbliva(table, 2, valek, [ibid], vale,&
                                    [cbid], k8b, crit, prec, nocmp(j),&
                                    k8b, ibid, contraintesth(k), cbid, k8b,&
                                    iret)
                        if (iret .ne. 0) then
                            valk (1) = table
                            valk (2) = nocmp(j)
                            valk (3) = valek(1)
                            valk (4) = valek(2)
                            call utmess('F', 'POSTRCCM_2', nk=4, valk=valk, nr=2,&
                                        valr=vale)
                        endif
                        sigth((k-1)*ncmp+j) = contraintesth(k)
                    endif
                    if (n2 .ne. 0) then
                        call tbliva(table2, 2, valek, [ibid], vale,&
                                    [cbid], k8b, crit, prec, nocmp(j),&
                                    k8b, ibid, contraintespr(k), cbid, k8b,&
                                    iret)
                        if (iret .ne. 0) then
                            valk (1) = table2
                            valk (2) = nocmp(j)
                            valk (3) = valek(1)
                            valk (4) = valek(2)
                            call utmess('F', 'POSTRCCM_2', nk=4, valk=valk, nr=2,&
                                        valr=vale)
                        endif
                        sigpr((k-1)*ncmp+j) = contraintespr(k)
                    endif
                    if (n3 .ne. 0) then
                        call tbliva(table3, 2, valek, [ibid], vale,&
                                    [cbid], k8b, crit, prec, nocmp(j),&
                                    k8b, ibid, contraintesmec(k), cbid, k8b,&
                                    iret)
                        if (iret .ne. 0) then
                            valk (1) = table3
                            valk (2) = nocmp(j)
                            valk (3) = valek(1)
                            valk (4) = valek(2)
                            call utmess('F', 'POSTRCCM_2', nk=4, valk=valk, nr=2,&
                                        valr=vale)
                        endif
                        sigmec((k-1)*ncmp+j) = contraintesmec(k)
                    endif
                    contraintestot(k)= contraintesmec(k)+contraintespr(k)+ contraintesth(k)
                    sigtot((k-1)*ncmp+j) = sigth((k-1)*ncmp+j)+ sigpr((k-1)*ncmp+j)+&
                    & sigmec((k-1)*ncmp+j)
 16             continue
                call rc32my(nbabsc, zr(jabsc), contraintestot, momen0, momen1)
                siglin(1*j)= momen0 - 0.5d0*momen1
                siglin((nbabsc-1)*ncmp+j)= momen0 + 0.5d0*momen1
 14         continue
!
! --------- on calcule tmaxsp et tminsp
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
! --------- on calcule tmaxsn et tminsn
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
        write(*,*)'TOTO',tmaxsn
        write(*,*)'TITI',tminsn
!
! --------- on remplit les vecteurs
! --------- pour tminsp a l'origine    
        do 66 j = 1, ncmp
            vale(1) = tminsp(1)
            do 166 k = 1, nbabsc
                vale(2) = zr(jabsc+k-1)
!
                if (n1 .ne. 0) then
                    call tbliva(table, 2, valek, [ibid], vale,&
                                [cbid], k8b, crit, prec, nocmp(j),&
                                k8b, ibid, contraintesth(k), cbid, k8b,&
                                iret)
                endif
                if (n2 .ne. 0) then
                    call tbliva(table2, 2, valek, [ibid], vale,&
                                [cbid], k8b, crit, prec, nocmp(j),&
                                k8b, ibid, contraintespr(k), cbid, k8b,&
                                iret)
                endif
                if (n3 .ne. 0) then
                    call tbliva(table3, 2, valek, [ibid], vale,&
                                [cbid], k8b, crit, prec, nocmp(j),&
                                k8b, ibid, contraintesmec(k), cbid, k8b,&
                                iret)
                endif
            contraintestot(k)=contraintesth(k)+contraintespr(k)+contraintesmec(k)
            contraintesm(k)=contraintespr(k)+contraintesmec(k)
 166        continue
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
!
                if (n1 .ne. 0) then
                    call tbliva(table, 2, valek, [ibid], vale,&
                                [cbid], k8b, crit, prec, nocmp(j),&
                                k8b, ibid, contraintesth(k), cbid, k8b,&
                                iret)
                endif
                if (n2 .ne. 0) then
                    call tbliva(table2, 2, valek, [ibid], vale,&
                                [cbid], k8b, crit, prec, nocmp(j),&
                                k8b, ibid, contraintespr(k), cbid, k8b,&
                                iret)
                endif
                if (n3 .ne. 0) then
                    call tbliva(table3, 2, valek, [ibid], vale,&
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
            do 266 k = 1, nbabsc
                vale(2) = zr(jabsc+k-1)
!
                if (n1 .ne. 0) then
                    call tbliva(table, 2, valek, [ibid], vale,&
                                [cbid], k8b, crit, prec, nocmp(j),&
                                k8b, ibid, contraintesth(k), cbid, k8b,&
                                iret)
                endif
                if (n2 .ne. 0) then
                    call tbliva(table2, 2, valek, [ibid], vale,&
                                [cbid], k8b, crit, prec, nocmp(j),&
                                k8b, ibid, contraintespr(k), cbid, k8b,&
                                iret)
                endif
                if (n3 .ne. 0) then
                    call tbliva(table3, 2, valek, [ibid], vale,&
                                [cbid], k8b, crit, prec, nocmp(j),&
                                k8b, ibid, contraintesmec(k), cbid, k8b,&
                                iret)
                endif
            contraintestot(k)=contraintesth(k)+contraintespr(k)+contraintesmec(k)
            contraintesm(k)=contraintespr(k)+contraintesmec(k)
 266        continue
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
                    call tbliva(table, 2, valek, [ibid], vale,&
                                [cbid], k8b, crit, prec, nocmp(j),&
                                k8b, ibid, contraintesth(k), cbid, k8b,&
                                iret)
                endif
                if (n2 .ne. 0) then
                    call tbliva(table2, 2, valek, [ibid], vale,&
                                [cbid], k8b, crit, prec, nocmp(j),&
                                k8b, ibid, contraintespr(k), cbid, k8b,&
                                iret)
                endif
                if (n3 .ne. 0) then
                    call tbliva(table3, 2, valek, [ibid], vale,&
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
            do 366 k = 1, nbabsc
                vale(2) = zr(jabsc+k-1)
!
                if (n1 .ne. 0) then
                    call tbliva(table, 2, valek, [ibid], vale,&
                                [cbid], k8b, crit, prec, nocmp(j),&
                                k8b, ibid, contraintesth(k), cbid, k8b,&
                                iret)
                endif
                if (n2 .ne. 0) then
                    call tbliva(table2, 2, valek, [ibid], vale,&
                                [cbid], k8b, crit, prec, nocmp(j),&
                                k8b, ibid, contraintespr(k), cbid, k8b,&
                                iret)
                endif
                if (n3 .ne. 0) then
                    call tbliva(table3, 2, valek, [ibid], vale,&
                                [cbid], k8b, crit, prec, nocmp(j),&
                                k8b, ibid, contraintesmec(k), cbid, k8b,&
                                iret)
                endif
            contraintestot(k)=contraintesth(k)+contraintespr(k)+contraintesmec(k)
            contraintesm(k)=contraintespr(k)+contraintesmec(k)
 366        continue
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
                    call tbliva(table, 2, valek, [ibid], vale,&
                                [cbid], k8b, crit, prec, nocmp(j),&
                                k8b, ibid, contraintesth(k), cbid, k8b,&
                                iret)
                endif
                if (n2 .ne. 0) then
                    call tbliva(table2, 2, valek, [ibid], vale,&
                                [cbid], k8b, crit, prec, nocmp(j),&
                                k8b, ibid, contraintespr(k), cbid, k8b,&
                                iret)
                endif
                if (n3 .ne. 0) then
                    call tbliva(table3, 2, valek, [ibid], vale,&
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
            do 466 k = 1, nbabsc
                vale(2) = zr(jabsc+k-1)
!
                if (n1 .ne. 0) then
                    call tbliva(table, 2, valek, [ibid], vale,&
                                [cbid], k8b, crit, prec, nocmp(j),&
                                k8b, ibid, contraintesth(k), cbid, k8b,&
                                iret)
                endif 
                if (n2 .ne. 0) then
                    call tbliva(table2, 2, valek, [ibid], vale,&
                                [cbid], k8b, crit, prec, nocmp(j),&
                                k8b, ibid, contraintespr(k), cbid, k8b,&
                                iret)
                endif
                if (n3 .ne. 0) then
                    call tbliva(table3, 2, valek, [ibid], vale,&
                                [cbid], k8b, crit, prec, nocmp(j),&
                                k8b, ibid, contraintesmec(k), cbid, k8b,&
                                iret)
                endif
            contraintestot(k)=contraintesth(k)+contraintespr(k)+contraintesmec(k)
            contraintesm(k)=contraintespr(k)+contraintesmec(k)
 466        continue
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
                    call tbliva(table, 2, valek, [ibid], vale,&
                                [cbid], k8b, crit, prec, nocmp(j),&
                                k8b, ibid, contraintesth(k), cbid, k8b,&
                                iret)
                endif 
                if (n2 .ne. 0) then
                    call tbliva(table2, 2, valek, [ibid], vale,&
                                [cbid], k8b, crit, prec, nocmp(j),&
                                k8b, ibid, contraintespr(k), cbid, k8b,&
                                iret)
                endif
                if (n3 .ne. 0) then
                    call tbliva(table3, 2, valek, [ibid], vale,&
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
        call jedetr(instan)
        call jedetr(abscur)
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
