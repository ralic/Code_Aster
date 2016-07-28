subroutine rc32sp1b(ze200, lieu, numsip, numsiq, seismeb32,&
                    seismeunit, mse, pi, mi, pj, mj,&
                    instsp, sp1, spmeca1, noth)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterc/getfac.h"
#include "asterfort/codent.h"
#include "asterfort/jeexin.h"
#include "asterfort/jexnom.h"
#include "asterfort/getvis.h"
#include "asterfort/getvid.h"
#include "asterfort/rcveri.h"
#include "asterfort/tbexip.h"
#include "asterfort/utmess.h"
#include "asterfort/tbexv1.h"
#include "asterfort/rcver1.h"
#include "asterfort/as_allocate.h"
#include "asterfort/wkvect.h"
#include "asterfort/tbliva.h"
#include "asterfort/rctres.h"
#include "asterfort/jedetr.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/jedema.h"
    aster_logical :: ze200, seismeb32, seismeunit, noth
    character(len=4) :: lieu
    integer :: numsip, numsiq
    real(kind=8) :: mse(12), pi, mi(12), pj, mj(12)
    real(kind=8) :: instsp(4), sp1(2), spmeca1(2)
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
!     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE
!     CALCUL DU SP en testant tous les instants
!
!     ------------------------------------------------------------------
! IN  : LIEU   : ='ORIG' : ORIGINE DU SEGEMNT, ='EXTR' : EXTREMITE
! IN  : NUMSIP : NUMERO SITUATION DE L'ETAT STABILISE P
! IN  : NUMSIQ : NUMERO SITUATION DE L'ETAT STABILISE Q
! OUT : SP1    : PARTIE B3200 du SP
!
    character(len=2) :: typ
    integer :: jseis, j, i0, i1, nbsitu, nbther, nbpres, nbmeca, iret, jsigu
    real(kind=8) :: seisfx(6), seisfy(6), seisfz(6), seismx(6), seismy(6)
    real(kind=8) :: seismz(6), seisfx2(6), seisfy2(6), seisfz2(6), seismx2(6)
    real(kind=8) :: seismy2(6), seismz2(6), e0(2), e1(2), e2(2), e3(2), e4(2)
    real(kind=8) :: e5(2), e6(2), trescapp, tresmepp, prec(2), sij(6), mij(12)
    character(len=8) :: nocmp(6), crit(2), knumes
    character(len=16) :: valek(2)
    integer :: icmps, icmp, n1(2), n2(2), n3(2), iocc, numesitu, n0, nume1(2)
    real(kind=8) :: sigu, pij, vale(2), dsig(6), dsime(6), sptran(6), spme(6)
    integer :: nume2(2), nume3(2), np, nq, ither, numether, n4, ii, ipres
    character(len=8) :: tabther(2), tabpres(2), tabmeca(2), tableok(2), k8b
    integer :: numepres, imeca, numemeca, nbinst(2), jinst(2), nbabsc(2), ncmp
    character(len=1) :: numsitu
    aster_logical :: exist
    character(len=24) :: valk(4)
    integer :: jabsc(2), ndim, jsigp, jmecap, i, ibid, l, i2, i3, i4, i5, i6
    real(kind=8), pointer :: contraintesth(:) => null()
    real(kind=8), pointer :: contraintespr(:) => null()
    real(kind=8), pointer :: contraintesmec(:) => null()
    complex(kind=8) :: cbid
    integer :: i7, i8, i9, i10, i11, i12, jsigq, jmecaq, ind1, ind2, indme1
    real(kind=8) :: tresca, tresme, trescapq(2), tresmepq(2), tresca1, tresca2
    integer :: indme2, inst1, inst2, instp1c, instq1c, instp2c, instq2c
!
! DEB ------------------------------------------------------------------
    call jemarq()
!
    typ ='00'
    if (seismeb32) then
        call jeveuo('&&RC3200.SIGSEIS', 'L', jseis)
        do 10 j = 1, 6
            seisfx(j) = zr(jseis+j-1)
            seisfy(j) = zr(jseis+1*6+j-1)
            seisfz(j) = zr(jseis+2*6+j-1)
            seismx(j) = zr(jseis+3*6+j-1)
            seismy(j) = zr(jseis+4*6+j-1)
            seismz(j) = zr(jseis+5*6+j-1)
            seisfx2(j)= 0.d0
            seisfy2(j)= 0.d0
            seisfz2(j)= 0.d0
            seismx2(j)= 0.d0
            seismy2(j)= 0.d0
            seismz2(j)= 0.d0
 10     continue
    endif
!
    do 20 i0 = 1, 2
        i1 = 2*(i0-2)+1
        e0(i0) = i1
        e1(i0) = i1
        e2(i0) = i1
        e3(i0) = i1
        e4(i0) = i1
        e5(i0) = i1
        e6(i0) = i1
 20  continue
!
    sp1(1) = 0.d0
    sp1(2) = 0.d0
    spmeca1(1) = 0.d0
    spmeca1(2) = 0.d0
    noth=.false.
    trescapp = 0.d0
    tresmepp = 0.d0
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
!--------------------------------------
!     SI B3200 de type unitaire
!--------------------------------------
    knumes = 'S       '
    call codent(numsip, 'D0', knumes(2:8))
    do 30 j = 1, 6
            sij(j) = 0.d0
30  continue
    if (.not. ze200) then
        call jeexin(jexnom('&&RC3200.SITU_ETAT_A', knumes), iret)
        if (iret .ne. 0) then
            call jeveuo('&&RC3200.MECA_UNIT .'//lieu, 'L', jsigu)
        do 40 icmps = 1, 6
            do 50 icmp = 1, 12
                mij(icmp)=mi(icmp)-mj(icmp)
                sigu = zr(jsigu-1+6*(icmp-1)+icmps)
                sij(icmps) = sij(icmps) + mij(icmp)*sigu
50          continue
            pij=pi-pj
            sigu = zr(jsigu-1+72+icmps)
            sij(icmps) = sij(icmps) + pij*sigu
40      continue
!
        endif
    endif
!
    if(seismeunit) then
        do 60 j = 1, 6
            seisfx(j) = 2*mse(1)*zr(jsigu-1+j)
            seisfy(j) = 2*mse(2)*zr(jsigu-1+1*6+j)
            seisfz(j) = 2*mse(3)*zr(jsigu-1+2*6+j)
            seismx(j) = 2*mse(4)*zr(jsigu-1+3*6+j)
            seismy(j) = 2*mse(5)*zr(jsigu-1+4*6+j)
            seismz(j) = 2*mse(6)*zr(jsigu-1+5*6+j)
            seisfx2(j)= 2*mse(7)*zr(jsigu-1+6*6+j)
            seisfy2(j)= 2*mse(8)*zr(jsigu-1+7*6+j)
            seisfz2(j)= 2*mse(9)*zr(jsigu-1+8*6+j)
            seismx2(j)= 2*mse(10)*zr(jsigu-1+9*6+j)
            seismy2(j)= 2*mse(11)*zr(jsigu-1+10*6+j)
            seismz2(j)= 2*mse(12)*zr(jsigu-1+11*6+j)
60      continue
    endif
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
!
    do 70 iocc = 1, nbsitu, 1
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
70  continue
!
!-- si aucune des situations n'a de transitoires
    np = n1(1)+n2(1)+n3(1)
    nq = n1(2)+n2(2)+n3(2)
    if (np+nq .eq. 0) goto 999
!-- sinon on récupère les tables associées
!-- sous RESU_THER, RESU_PRES et RESU_MECA pour p et q
    do 80 ither =1, nbther, 1
        call getvis('RESU_THER', 'NUME_RESU_THER', iocc=ither, scal=numether, nbret=n4)
        do 90 ii=1,2
            if (n1(ii) .ne. 0) then
                if (numether .eq. nume1(ii)) then
                    call getvid('RESU_THER', 'TABL_RESU_THER', iocc=ither,&
                                 scal=tabther(ii), nbret=n4)
                endif
            endif
90      continue
80  continue
    do 100 ipres =1, nbpres, 1
        call getvis('RESU_PRES', 'NUME_RESU_PRES', iocc=ipres, scal=numepres, nbret=n4)
        do 110 ii=1,2
            if (n2(ii) .ne. 0) then 
                if (numepres .eq. nume2(ii)) then
                    call getvid('RESU_PRES', 'TABL_RESU_PRES', iocc=ipres,&
                                 scal=tabpres(ii), nbret=n4)
                endif
            endif
110     continue
100 continue
    do 120 imeca =1, nbmeca, 1
        call getvis('RESU_MECA', 'NUME_RESU_MECA', iocc=imeca, scal=numemeca, nbret=n4)
        do 130 ii=1,2
            if (n3(ii) .ne. 0) then 
                if (numemeca .eq. nume3(ii)) then
                    call getvid('RESU_MECA', 'TABL_RESU_MECA', iocc=imeca,&
                                scal=tabmeca(ii), nbret=n4)
                endif
            endif
130     continue
120 continue
!
!--------------------------------------------------------------
!      RECUPERATION DES INSTANTS ET DES ABSCISSES DE P et Q
!--------------------------------------------------------------
! ------ grace a la table thermique ou de pression ou mecanique
    do 140 ii=1,2
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
140  continue
!
    ncmp=6
!
    AS_ALLOCATE(vr=contraintesth,  size=1)
    AS_ALLOCATE(vr=contraintespr,  size=1)
    AS_ALLOCATE(vr=contraintesmec, size=1)
!
999 continue
! 
!--------------------------------------
!   DANS LE CAS D'UNE SITUATION SEULE
!--------------------------------------
    if (np .eq. 0) then
        nbinst(1)=1
        goto 888
    endif
!
    ndim = ncmp*nbinst(1)
    call wkvect('&&RC3200.SIGMP', 'V V R', ndim, jsigp)
    call wkvect('&&RC3200.SIGMP.MECA', 'V V R', ndim, jmecap)
!
    if (lieu .eq. 'ORIG') then
        vale(2)=zr(jabsc(1))
    else
        vale(2)=zr(jabsc(1)+nbabsc(1)-1)
    endif
!
    do 150 i = 1, nbinst(1)
        vale(1) = zr(jinst(1)+i-1)
!
        do 160 j = 1, ncmp
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
160     continue
150 continue
!
888 continue
!
    if (seismeb32 .or. seismeunit) then
      do 12 i = 1, nbinst(1)
        do 22 l = 1, nbinst(1)
          do 32 i0 = 1, 2
            do 42 i1 = 1, 2
              do 52 i2 = 1, 2
                do 62 i3 = 1, 2
                  do 72 i4 = 1, 2
                    do 82 i5 = 1, 2
                      do 92 i6 = 1, 2
                        do 102 i7 = 1, 2
                          do 112 i8 = 1, 2
                            do 122 i9 = 1, 2
                              do 132 i10 = 1, 2
                                do 142 i11 = 1, 2
                                  do 152 i12 = 1, 2
                                    do 162 j = 1, 6
                                      if (np .ne. 0) then
                                        dsig(j)=zr(jsigp+(i-1)*ncmp+j-1)-zr(jsigp+(l-1)*ncmp+j-1)
                                        dsime(j)=zr(jmecap+(i-1)*ncmp+j-1)-zr(jmecap+(l-1)*ncmp+j-1)
                                      else
                                        dsig(j)=0.d0
                                        dsime(j)=0.d0
                                        instsp(1) = -1.0
                                        instsp(2) = -1.0
                                      endif 
                                      sptran(j) = dsig(j)+e0(i0)*sij(j)+&
                                                e1(i1)*seisfx(j)+ e2(i2)*seisfy(j)+&
                                                e3(i3)*seisfz(j)+e4(i4)*seismx(j)+&
                                                e5(i5)*seismy(j)+e6(i6)*seismz(j)+&
                                                e1(i7)*seisfx2(j)+ e2(i8)*seisfy2(j)+&
                                                e3(i9)*seisfz2(j)+e4(i10)*seismx2(j)+&
                                                e5(i11)*seismy2(j)+e6(i12)*seismz2(j)
                                      spme(j) = dsime(j)+e0(i0)*sij(j)+&
                                                e1(i1)*seisfx(j)+ e2(i2)*seisfy(j)+&
                                                e3(i3)*seisfz(j)+e4(i4)*seismx(j)+&
                                                e5(i5)*seismy(j)+e6(i6)*seismz(j)+&
                                                e1(i7)*seisfx2(j)+ e2(i8)*seisfy2(j)+&
                                                e3(i9)*seisfz2(j)+e4(i10)*seismx2(j)+&
                                                e5(i11)*seismy2(j)+e6(i12)*seismz2(j)
162                                 continue
                                    call rctres(sptran,tresca)
                                    call rctres(spme,tresme)
                                    if (tresca .gt. trescapp) then
                                      trescapp = tresca
                                      if (np .ne. 0) then
                                        instsp(1)=zr(jinst(1)+i-1)
                                        instsp(2)=zr(jinst(1)+l-1)
                                      endif
                                    endif
                                    tresmepp =max(tresmepp, tresme)
152                               continue
142                             continue
132                           continue
122                         continue
112                       continue
102                     continue
92                    continue
82                  continue
72                continue
62              continue
52            continue
42          continue
32        continue
22      continue
12    continue
    else
      do 13 i = 1, nbinst(1)
        do 23 l = 1, nbinst(1)
          do 33 i0 = 1, 2
            do 43 j = 1, 6
              if (np .ne. 0) then
                dsig(j)=zr(jsigp+(i-1)*ncmp+j-1)-zr(jsigp+(l-1)*ncmp+j-1)
                dsime(j)=zr(jmecap+(i-1)*ncmp+j-1)-zr(jmecap+(l-1)*ncmp+j-1)
              else
                dsig(j)=0.d0
                dsime(j)=0.d0
                instsp(1) = -1.0
                instsp(2) = -1.0
              endif 
              sptran(j) = dsig(j)+e0(i0)*sij(j)
              spme(j) = dsime(j)+e0(i0)*sij(j)
43          continue
            call rctres(sptran,tresca)
            call rctres(spme,tresme)
            if (tresca .gt. trescapp) then
              trescapp = tresca
              if (np .ne. 0) then
                instsp(1)=zr(jinst(1)+i-1)
                instsp(2)=zr(jinst(1)+l-1)
              endif
            endif
            tresmepp=max(tresmepp,tresme)
33        continue
23      continue
13    continue
    endif
!
    sp1(1)=trescapp
    spmeca1(1)=tresmepp
!
    if (numsip .eq. numsiq) goto 666
!
!--------------------------------------
!     DANS LE CAS D'UNE COMBINAISON
!--------------------------------------
    trescapq(1)=0.d0
    trescapq(2)=0.d0
    tresmepq(1)=0.d0
    tresmepq(2)=0.d0
    if (nq .eq. 0) then
        nbinst(2)=1
        goto 555
    endif
!
!-- STOCKAGE DES CONTRAINTES TRANSITOIRES DE Q
    ndim = ncmp*nbinst(2)
    call wkvect('&&RC3200.SIGMQ', 'V V R', ndim, jsigq)
    call wkvect('&&RC3200.SIGMQ.MECA', 'V V R', ndim, jmecaq)
!
    if (lieu .eq. 'ORIG') then
        vale(2)=zr(jabsc(2))
    else
        vale(2)=zr(jabsc(2)+nbabsc(2)-1)
    endif
!
    do 170 i = 1, nbinst(2)
        vale(1) = zr(jinst(2)+i-1)
!
        do 180 j = 1, ncmp
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
180     continue
170 continue
!
555 continue 
!
!--------------------------------------------------------------
!                CALCUL DE SP(P,Q)
!--------------------------------------------------------------
    if (np .ne. 0) then
        if (nq .ne. 0) then
            ind1 = jsigp
            ind2 = jsigq
            indme1 = jmecap
            indme2 = jmecaq
            inst1 = nbinst(1)
            inst2 = nbinst(2)
        else
            ind1 = jsigp
            ind2 = jsigp
            indme1 = jmecap
            indme2 = jmecap
            inst1 = nbinst(1)
            inst2 = nbinst(1)
        endif
    else
        if (nq .ne. 0) then
            ind1 = jsigq
            ind2 = jsigq
            indme1 = jmecaq
            indme2 = jmecaq
            inst1 = nbinst(2)
            inst2 = nbinst(2)
        else
            inst1 = 1
            inst2 = 1
        endif
    endif
!On détermine SP1
    if (seismeb32 .or. seismeunit) then
      do 14 i = 1, inst1
        do 24 l = 1, inst2
          do 34 i0 = 1, 2
            do 44 i1 = 1, 2
              do 54 i2 = 1, 2
                do 64 i3 = 1, 2
                  do 74 i4 = 1, 2
                    do 84 i5 = 1, 2
                      do 94 i6 = 1, 2
                        do 104 i7 = 1, 2
                          do 114 i8 = 1, 2
                            do 124 i9 = 1, 2
                              do 134 i10 = 1, 2
                                do 144 i11 = 1, 2
                                  do 154 i12 = 1, 2
                                    do 164 j = 1, 6
                                      if (np+nq .ne. 0) then
                                        dsig(j)=zr(ind1+(i-1)*ncmp+j-1)-zr(ind2+(l-1)*ncmp+j-1)
                                        dsime(j)=zr(indme1+(i-1)*ncmp+j-1)-zr(indme2+(l-1)*ncmp+j-1)
                                      else
                                        noth = .true.
                                        dsig(j)=0.d0
                                        dsime(j)=0.d0
                                        instsp(1) = -1.0
                                        instsp(2) = -1.0
                                      endif 
                                      sptran(j) = dsig(j)+e0(i0)*sij(j)+&
                                                e1(i1)*seisfx(j)+ e2(i2)*seisfy(j)+&
                                                e3(i3)*seisfz(j)+e4(i4)*seismx(j)+&
                                                e5(i5)*seismy(j)+e6(i6)*seismz(j)+&
                                                e1(i7)*seisfx2(j)+ e2(i8)*seisfy2(j)+&
                                                e3(i9)*seisfz2(j)+e4(i10)*seismx2(j)+&
                                                e5(i11)*seismy2(j)+e6(i12)*seismz2(j)
                                      spme(j) = dsime(j)+e0(i0)*sij(j)+&
                                                e1(i1)*seisfx(j)+ e2(i2)*seisfy(j)+&
                                                e3(i3)*seisfz(j)+e4(i4)*seismx(j)+&
                                                e5(i5)*seismy(j)+e6(i6)*seismz(j)+&
                                                e1(i7)*seisfx2(j)+ e2(i8)*seisfy2(j)+&
                                                e3(i9)*seisfz2(j)+e4(i10)*seismx2(j)+&
                                                e5(i11)*seismy2(j)+e6(i12)*seismz2(j)
164                                 continue
                                    call rctres(sptran,tresca)
                                    if (tresca .gt. trescapq(1)) then
                                      trescapq(1) = tresca
                                      call rctres(spme,tresme)
                                      tresmepq(1) = tresme
                                      if (np .ne. 0) then
                                        if (nq .ne. 0) then
                                          instsp(1)=zr(jinst(1)+i-1)
                                          instp1c = i
                                          instsp(2)=zr(jinst(2)+l-1)
                                          instq1c=l
                                        else
                                          instsp(1)=zr(jinst(1)+i-1)
                                          instsp(2)=zr(jinst(1)+l-1)
                                        endif
                                      else
                                        if (nq .ne. 0) then
                                          instsp(1)=zr(jinst(2)+i-1)
                                          instsp(2)=zr(jinst(2)+l-1)
                                        endif
                                      endif
                                    endif
154                               continue
144                             continue
134                           continue
124                         continue
114                       continue
104                     continue
94                    continue
84                  continue
74                continue
64              continue
54            continue
44          continue
34        continue
24      continue
14    continue
    else
      do 15 i = 1, inst1
        do 25 l = 1, inst2
          do 35 i0 = 1, 2
            do 45 j = 1, 6
              if (nq+np .ne. 0) then
                dsig(j)=zr(ind1+(i-1)*ncmp+j-1)-zr(ind2+(l-1)*ncmp+j-1)
                dsime(j)=zr(indme1+(i-1)*ncmp+j-1)-zr(indme2+(l-1)*ncmp+j-1)
              else
                dsig(j)=0.d0
                dsime(j)=0.d0
                instsp(1) = -1.0
                instsp(2) = -1.0
                noth = .true.
              endif 
              sptran(j) = dsig(j)+e0(i0)*sij(j)
              spme(j) = dsime(j)+e0(i0)*sij(j)
45          continue
            call rctres(sptran,tresca)
            if (tresca .gt. trescapq(1)) then
              trescapq(1) = tresca
              call rctres(spme,tresme)
              tresmepq(1) = tresme
              if (np .ne. 0) then
                if (nq .ne. 0) then
                  instsp(1)=zr(jinst(1)+i-1)
                  instp1c = i
                  instsp(2)=zr(jinst(2)+l-1)
                  instq1c = l
                else
                  instsp(1)=zr(jinst(1)+i-1)
                  instsp(2)=zr(jinst(1)+l-1)
                endif
              else
                if (nq .ne. 0) then
                  instsp(1)=zr(jinst(2)+i-1)
                  instsp(2)=zr(jinst(2)+l-1)
                endif
              endif
            endif
35        continue
25      continue
15    continue
    endif 
!
!On détermine SP2
!
!-- on trouve d'abord les instants de SP2
    if (np*nq .eq. 0) goto 444
 !
    tresca1 = 0.d0
    tresca2 = 0.d0
    do 190 i = 1, nbinst(1)
        do 200 j = 1, ncmp
            dsig(j)=zr(jsigp+(i-1)*ncmp+j-1)-zr(jsigp+(instp1c-1)*ncmp+j-1)
            if (j .eq. ncmp) then
                call rctres(dsig, tresca)
                if (tresca .gt. tresca1) then
                    tresca1=tresca
                    instp2c = i
                endif
             endif
200     continue
190 continue
    do 210 l = 1, nbinst(2)
        do 220 j = 1, ncmp
            dsig(j)=zr(jsigq+(l-1)*ncmp+j-1)-zr(jsigq+(instq1c-1)*ncmp+j-1)
            if (j .eq. ncmp) then
                call rctres(dsig, tresca)
                if (tresca .gt. tresca2) then
                    tresca2=tresca
                    instq2c = l
                endif
            endif
     220 continue
210 continue
!
444 continue
!
    do 230 j = 1, ncmp
        dsig(j)=0.d0
        dsime(j)=0.d0
230 continue
    instsp(3)=-1.d0
    instsp(4)=-1.d0
!
    if (np .ne. 0) then
        if (nq .ne. 0) then
            instsp(3)=zr(jinst(1)+instp2c-1)
            instsp(4)=zr(jinst(2)+instq2c-1)
            do 240 j = 1, ncmp
                dsig(j)=zr(jsigp+(instp2c-1)*ncmp+j-1)-zr(jsigq+(instq2c-1)*ncmp+j-1)
                dsime(j)=zr(jmecap+(instp2c-1)*ncmp+j-1)-zr(jmecaq+(instq2c-1)*ncmp+j-1)
240         continue
        endif
    endif
!
    if (seismeb32 .or. seismeunit) then
      do 16 i0 = 1, 2
        do 26 i1 = 1, 2
          do 36 i2 = 1, 2
            do 46 i3 = 1, 2
              do 56 i4 = 1, 2
                do 66 i5 = 1, 2
                  do 76 i6 = 1, 2
                    do 86 i7 = 1, 2
                      do 96 i8 = 1, 2
                        do 106 i9 = 1, 2
                          do 116 i10 = 1, 2
                            do 126 i11 = 1, 2
                              do 136 i12 = 1, 2
                                do 146 j = 1, 6
                                    sptran(j) = dsig(j)+e0(i0)*sij(j)+&
                                                e1(i1)*seisfx(j)+ e2(i2)*seisfy(j)+&
                                                e3(i3)*seisfz(j)+e4(i4)*seismx(j)+&
                                                e5(i5)*seismy(j)+e6(i6)*seismz(j)+&
                                                e1(i7)*seisfx2(j)+ e2(i8)*seisfy2(j)+&
                                                e3(i9)*seisfz2(j)+e4(i10)*seismx2(j)+&
                                                e5(i11)*seismy2(j)+e6(i12)*seismz2(j)
                                    spme(j) = dsime(j)+e0(i0)*sij(j)+&
                                                e1(i1)*seisfx(j)+ e2(i2)*seisfy(j)+&
                                                e3(i3)*seisfz(j)+e4(i4)*seismx(j)+&
                                                e5(i5)*seismy(j)+e6(i6)*seismz(j)+&
                                                e1(i7)*seisfx2(j)+ e2(i8)*seisfy2(j)+&
                                                e3(i9)*seisfz2(j)+e4(i10)*seismx2(j)+&
                                                e5(i11)*seismy2(j)+e6(i12)*seismz2(j)
146                             continue
                                call rctres(sptran,tresca)
                                call rctres(spme,tresme)
                                trescapq(2)=max(trescapq(2), tresca)
                                tresmepq(2)=max(tresmepq(2), tresme)
136                           continue
126                         continue
116                       continue
106                     continue
96                    continue
86                  continue
76                continue
66              continue
56            continue
46          continue
36        continue
26      continue
16    continue
    else
      do 17 i0 = 1, 2
        do 27 j = 1, 6
            sptran(j) = dsig(j)+e0(i0)*sij(j)
            spme(j) = dsime(j)+e0(i0)*sij(j)
27      continue
        call rctres(sptran,tresca)
        call rctres(spme,tresme)
        trescapq(2)=max(trescapq(2),tresca)
        tresmepq(2)=max(tresmepq(2),tresme)
17    continue
    endif
!
    sp1(1) = trescapq(1)
    sp1(2) = trescapq(2)
    spmeca1(1) = tresmepq(1)
    spmeca1(2) = tresmepq(2)
!
666 continue
!
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
    if(np+nq .ne. 0) then
        AS_DEALLOCATE(vr=contraintesmec)
        AS_DEALLOCATE(vr=contraintesth)
        AS_DEALLOCATE(vr=contraintespr)
    endif
!
    call jedema()
end subroutine
