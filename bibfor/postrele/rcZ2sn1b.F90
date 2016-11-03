subroutine rcZ2sn1b(ze200, lieu, numsip, numsiq, seismeb32,&
                    seismeunit, seismeze200, mse, propi, propj, proqi, proqj,&
                    instsn, sn, sp3, spmeca3)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/codent.h"
#include "asterfort/jeexin.h"
#include "asterfort/jexnom.h"
#include "asterc/getfac.h"
#include "asterfort/getvis.h"
#include "asterfort/getvid.h"
#include "asterfort/rcveri.h"
#include "asterfort/tbexip.h"
#include "asterfort/utmess.h"
#include "asterfort/tbexv1.h"
#include "asterfort/rcver1.h"
#include "asterfort/rcZ2s2.h"
#include "asterfort/as_allocate.h"
#include "asterfort/wkvect.h"
#include "asterfort/tbliva.h"
#include "asterfort/rc32my.h"
#include "asterfort/rctres.h"
#include "asterfort/jedetr.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/jedema.h"
    aster_logical :: ze200, seismeb32, seismeunit, seismeze200
    character(len=4) :: lieu
    integer :: numsip, numsiq
    real(kind=8) :: mse(12), propi(20), propj(20), proqi(20)
    real(kind=8) :: proqj(20), instsn(2), sn, sp3, spmeca3
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
!     CALCUL DU SN avec la méthode TRESCA SIGNE
!
!     ------------------------------------------------------------------
! IN  : LIEU   : ='ORIG' : ORIGINE DU SEGEMNT, ='EXTR' : EXTREMITE
! IN  : NUMSIP : NUMERO SITUATION DE L'ETAT STABILISE P
! IN  : NUMSIQ : NUMERO SITUATION DE L'ETAT STABILISE Q
! OUT : SN    
!
    integer :: jseis, j, iret2, jsigu, nbsitu, nbther, nbpres, nbmeca
    real(kind=8) :: seisfx(6), seisfy(6), seisfz(6), seismx(6), seismy(6)
    real(kind=8) :: seismz(6), seisfx2(6), seisfy2(6), seisfz2(6), seismx2(6)
    real(kind=8) :: seismy2(6), seismz2(6), prec(2), snpp, sn2pp, trescapp
    character(len=8) :: knumes, nocmp(6), crit(2)
    character(len=16) :: valek(2)
    integer :: n1(2), n2(2), n3(2), iocc, numesitu, n0, nume1(2), nume2(2)
    integer :: nume3(2), np, nq, ither, numether, n4, ii, ipres, numepres
    character(len=8) ::  tabther(2), tabpres(2), tabmeca(2), tableok(2)
    integer :: imeca, numemeca, nbinst(2),jinst(2), nbabsc(2), jabsc(2)
    character(len=1) :: numsitu
    aster_logical :: exist
    character(len=8) :: k8b
    character(len=24) :: valk(4)
    real(kind=8) :: sij(6), mij(12), sigu, pij, vale(2), momen0, momen1
    integer :: icmp, ncmp, ndim, jsigp, jsithp, jsiprp, jsimep, i, kk, ibid
    real(kind=8), pointer :: contraintesth(:) => null()
    real(kind=8), pointer :: contraintespr(:) => null()
    real(kind=8), pointer :: contraintesmec(:) => null()
    real(kind=8), pointer :: contraintestot(:) => null()
    complex(kind=8) :: cbid
    integer :: iret, i0, i1, l, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12
    real(kind=8) :: momen0th, momen0pr, momen0mec, momen1th, momen1pr, momen1mec
    real(kind=8) :: e0(2), e1(2), dsig(6), dsith(6), dsipr(6), dsime(6)
    real(kind=8) :: instpmin, instpmax, snt(6), tresca, tresth, tresmec, trespr
    real(kind=8) :: snqq, sn2qq, trescaqq, instqmin, instqmax, sqt(6)
    integer :: jsigq, jsithq, jsiprq, jsimeq, ind1, ind2, indth1, indth2
    real(kind=8) :: tresthqq, tresmecqq, tresprqq, sn2pq, snpq, trescapq, sn2
    real(kind=8) :: sij1(6), sij2(6), sij3(6), sij4(6), mij1(12), mij2(12)
    real(kind=8) :: mij3(12), mij4(12), pij1, pij2, pij3, pij4, seis(6)
    integer :: indpr1, indpr2, indme1, indme2, inst1, inst2, jinst1, jinst2
    real(kind=8) :: st11(6), st12(6), st13(6), st14(6), tresthpq, tresprpq
    real(kind=8) :: tresmecpq, k1, c1, k2, c2, k3, c3
    integer :: instp1, instq1, jvalin
!
! DEB ------------------------------------------------------------------
    call jemarq()
!
    sn  = 0.d0
    sp3 = 0.d0
    spmeca3 = 0.d0
!
!--------------------------------------------
!     SI SEISME ET B3200 de type instantanée
!--------------------------------------------
    if (seismeb32) then
        call jeveuo('&&RC3200.SIGSEIS', 'L', jseis)
        do 20 j = 1, 6
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
20      continue
    endif
!
!--------------------------------------
!     SI SEISME B3200 de type unitaire
!--------------------------------------
    iret2  = 0
    knumes = 'S       '
    call codent(numsip, 'D0', knumes(2:8))
    if (.not. ze200) then
        call jeexin(jexnom('&&RC3200.SITU_ETAT_A', knumes), iret2)
        if (iret2 .ne. 0) then
            call jeveuo('&&RC3200.MECA_UNIT .'//lieu, 'L', jsigu)
        endif
    endif
!
    if(seismeunit) then
        do 30 j = 1, 6
            seisfx(j) = 2*mse(1)*zr(jsigu-1+78+j)
            seisfy(j) = 2*mse(2)*zr(jsigu-1+78+1*6+j)
            seisfz(j) = 2*mse(3)*zr(jsigu-1+78+2*6+j)
            seismx(j) = 2*mse(4)*zr(jsigu-1+78+3*6+j)
            seismy(j) = 2*mse(5)*zr(jsigu-1+78+4*6+j)
            seismz(j) = 2*mse(6)*zr(jsigu-1+78+5*6+j)
            seisfx2(j)= 2*mse(7)*zr(jsigu-1+78+6*6+j)
            seisfy2(j)= 2*mse(8)*zr(jsigu-1+78+7*6+j)
            seisfz2(j)= 2*mse(9)*zr(jsigu-1+78+8*6+j)
            seismx2(j)= 2*mse(10)*zr(jsigu-1+78+9*6+j)
            seismy2(j)= 2*mse(11)*zr(jsigu-1+78+10*6+j)
            seismz2(j)= 2*mse(12)*zr(jsigu-1+78+11*6+j)
30      continue
    endif
!
!---------------- RECUPERATION DES TABLES DE  -----------------------------
!------------- TRANSITOIRES DES SITUATIONS P et Q--------------------------
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
    valek(1) = 'INST            '
    valek(2) = 'ABSC_CURV       '
    prec(1) = 1.0d-06
    prec(2) = 1.0d-06
    crit(1) = 'RELATIF'
    crit(2) = 'RELATIF'
!
    n1(1) = 0
    n2(1) = 0
    n3(1) = 0
    n1(2) = 0
    n2(2) = 0
    n3(2) = 0
!
    do 40 iocc = 1, nbsitu, 1
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
40  continue
!
!-- si aucune des situations n'a de transitoires
    np = n1(1)+n2(1)+n3(1)
    nq = n1(2)+n2(2)+n3(2)
    if (np+nq .eq. 0) goto 999
!-- sinon on récupère les tables associées
!-- sous RESU_THER, RESU_PRES et RESU_MECA pour p et q
    do 41 ither =1, nbther, 1
        call getvis('RESU_THER', 'NUME_RESU_THER', iocc=ither, scal=numether, nbret=n4)
        do 42 ii=1,2
            if (n1(ii) .ne. 0) then
                if (numether .eq. nume1(ii)) then
                    call getvid('RESU_THER', 'TABL_RESU_THER', iocc=ither,&
                                 scal=tabther(ii), nbret=n4)
                endif
            endif
42      continue
41  continue
    do 43 ipres =1, nbpres, 1
        call getvis('RESU_PRES', 'NUME_RESU_PRES', iocc=ipres, scal=numepres, nbret=n4)
        do 44 ii=1,2
            if (n2(ii) .ne. 0) then 
                if (numepres .eq. nume2(ii)) then
                    call getvid('RESU_PRES', 'TABL_RESU_PRES', iocc=ipres,&
                                 scal=tabpres(ii), nbret=n4)
                endif
            endif
44      continue
43  continue
    do 45 imeca =1, nbmeca, 1
        call getvis('RESU_MECA', 'NUME_RESU_MECA', iocc=imeca, scal=numemeca, nbret=n4)
        do 46 ii=1,2
            if (n3(ii) .ne. 0) then 
                if (numemeca .eq. nume3(ii)) then
                    call getvid('RESU_MECA', 'TABL_RESU_MECA', iocc=imeca,&
                                scal=tabmeca(ii), nbret=n4)
                endif
            endif
46      continue
45  continue
!
!---------------- RECUPERATION DES INSTANTS ET  ------------------------
!------------- ABSCISSES DES SITUATIONS P et Q--------------------------
!
! ------ grace a la table thermique ou de pression ou mecanique
    do 47 ii=1,2
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
777 continue
47  continue
999 continue
!
    if(np*nq .ne. 0) then
        if (nbabsc(1) .ne. nbabsc(2)) then
            call utmess('F', 'POSTRCCM_45')
        endif
    endif
!
!--------------------------------------------------------------------
!                  DANS LE CAS D'UNE SITUATION SEULE
!                          CALCUL DE SN(P,P)
!--------------------------------------------------------------------
    snpp  = 0.d0
    sn2pp = 0.d0
    trescapp = 0.d0
!
!-- on calcule la partie ze200
    if (ze200) call rcZ2s2('SN', propi, propj, seismeze200, mse, sn2pp)
!
!-- on calcule la partie b3200_unitaire
    do 48 j = 1, 6
        sij(j) = 0.d0
48  continue
    if (iret2 .ne. 0) then
        do 50 j = 1, 6
            do 60 icmp = 1, 12
                mij(icmp) = propi(1+icmp)-propj(1+icmp)
                sigu = zr(jsigu-1+78+6*(icmp-1)+j)
                sij(j) = sij(j)+mij(icmp)*sigu
60          continue
            pij = propi(1)-propj(1)
            sigu = zr(jsigu-1+78+72+j)
            sij(j) = sij(j)+pij*sigu
50      continue
    endif
!
!-- on calcule la partie b3200_transitoire
    ncmp=6
    if(np+nq .ne. 0) then
        AS_ALLOCATE(vr=contraintestot,  size=nbabsc(1))
        AS_ALLOCATE(vr=contraintesth,  size=nbabsc(1))
        AS_ALLOCATE(vr=contraintespr,  size=nbabsc(1))
        AS_ALLOCATE(vr=contraintesmec, size=nbabsc(1))
    endif
! 
    if (np .eq. 0) then
        nbinst(1)=1
        goto 888
    endif
!
    ndim = ncmp*nbinst(1)
    call wkvect('&&RC3200.SIGMP', 'V V R', ndim, jsigp)
    call wkvect('&&RC3200.SITHP', 'V V R', ndim, jsithp)
    call wkvect('&&RC3200.SIPRP', 'V V R', ndim, jsiprp)
    call wkvect('&&RC3200.SIMEP', 'V V R', ndim, jsimep)
!
    do 70 i = 1, nbinst(1)
        vale(1) = zr(jinst(1)+i-1)
!
        do 80 j = 1, ncmp
!
          do 85 kk = 1, nbabsc(1)
            vale(2) = zr(jabsc(1)+kk-1)

            if (n1(1) .ne. 0) then
                call tbliva(tabther(1), 2, valek, [ibid], vale,&
                            [cbid], k8b, crit, prec, nocmp(j),&
                            k8b, ibid, contraintesth(kk), cbid, k8b,&
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
                           k8b, ibid, contraintespr(kk), cbid, k8b,&
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
                           k8b, ibid, contraintesmec(kk), cbid, k8b,&
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
            contraintestot(kk) = contraintesmec(kk)+contraintespr(kk)+ contraintesth(kk)
85        continue
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
80      continue
70 continue
!
888 continue
!
!-- on maximise sur les différentes possibilités de signes
    do 90 i0 = 1, 2
        i1 = 2*(i0-2)+1
        e0(i0) = i1
        e1(i0) = i1
90  continue
!
    if (seismeb32 .or. seismeunit) then
      do 91 i = 1, nbinst(1)
        do 92 l = 1, nbinst(1)
          do 100 i1 = 1, 2
            do 101 i2 = 1, 2
              do 102 i3 = 1, 2
                do 103 i4 = 1, 2
                  do 104 i5 = 1, 2
                    do 105 i6 = 1, 2
                      do 106 i7 = 1, 2
                        do 107 i8 = 1, 2
                          do 108 i9 = 1, 2
                            do 109 i10 = 1, 2
                              do 110 i11 = 1, 2
                                do 111 i12 = 1, 2
                                  do 112 i0 = 1, 2
                                    do 113 j = 1, 6
                                      if (np .ne. 0) then
                                        dsig(j)=zr(jsigp+(i-1)*ncmp+j-1)-zr(jsigp+(l-1)*ncmp+j-1)
                                        dsith(j)=zr(jsithp+(i-1)*ncmp+j-1)-zr(jsithp+(l-1)*ncmp+j-1)
                                        dsipr(j)=zr(jsiprp+(i-1)*ncmp+j-1)-zr(jsiprp+(l-1)*ncmp+j-1)
                                        dsime(j)=zr(jsimep+(i-1)*ncmp+j-1)-zr(jsimep+(l-1)*ncmp+j-1)
                                      else
                                        dsig(j)=0.d0
                                        dsith(j)=0.d0
                                        dsipr(j)=0.d0
                                        dsime(j)=0.d0
                                        instpmin = -1.0
                                        instpmax = -1.0
                                      endif 
                                      snt(j) =  dsig(j)         +e0(i0)*sij(j)     +&
                                                e1(i1)*seisfx(j)  +e1(i2)*seisfy(j)  +&
                                                e1(i3)*seisfz(j)  +e1(i4)*seismx(j)  +&
                                                e1(i5)*seismy(j)  +e1(i6)*seismz(j)  +&
                                                e1(i7)*seisfx2(j) +e1(i8)*seisfy2(j) +&
                                                e1(i9)*seisfz2(j) +e1(i10)*seismx2(j)+&
                                                e1(i11)*seismy2(j)+e1(i12)*seismz2(j)
113                                 continue
                                    call rctres(snt,tresca)
                                    if (tresca .gt. trescapp) then
                                      trescapp = tresca
                                      call rctres(dsime,tresmec)
                                      call rctres(dsith,tresth)
                                      call rctres(dsipr,trespr)
                                      if (np .ne. 0) then
                                        instpmin=zr(jinst(1)+i-1)
                                        instpmax=zr(jinst(1)+l-1)
                                      endif
                                    endif
112                               continue
111                             continue
110                           continue
109                         continue
108                       continue
107                     continue
106                   continue
105                 continue
104               continue
103             continue
102           continue
101         continue
100       continue
92      continue
91    continue
    else
      do 114 i = 1, nbinst(1)
        do 115 l = 1, nbinst(1)
          do 116 i0 = 1, 2
            do 117 j=1,6
              if (np .ne. 0) then
                dsig(j)=zr(jsigp+(i-1)*ncmp+j-1)-zr(jsigp+(l-1)*ncmp+j-1)
                dsith(j)=zr(jsithp+(i-1)*ncmp+j-1)-zr(jsithp+(l-1)*ncmp+j-1)
                dsipr(j)=zr(jsiprp+(i-1)*ncmp+j-1)-zr(jsiprp+(l-1)*ncmp+j-1)
                dsime(j)=zr(jsimep+(i-1)*ncmp+j-1)-zr(jsimep+(l-1)*ncmp+j-1)
              else
                dsig(j)=0.d0
                dsith(j)=0.d0
                dsipr(j)=0.d0
                dsime(j)=0.d0
                instpmin = -1.0
                instpmax = -1.0
              endif 
              snt(j)  = dsig(j)+e0(i0)*sij(j)
        117 continue
            call rctres(snt,tresca)
            if (tresca .gt. trescapp) then
              trescapp = tresca
              call rctres(dsime,tresmec)
              call rctres(dsith,tresth)
              call rctres(dsipr,trespr)
              if (np .ne. 0) then
                instpmin=zr(jinst(1)+i-1)
                instpmax=zr(jinst(1)+l-1)
              endif
            endif
116       continue
115     continue
114   continue
    endif
!
    snpp    = trescapp+sn2pp
    sn  = snpp
    instsn(1) = instpmin
    instsn(2) = instpmax
!
    if (numsip .eq. numsiq) goto 666
!
!--------------------------------------------------------------------
!                  DANS LE CAS D'UNE COMBINAISON DE
!                          SITUATIONS P ET Q
!--------------------------------------------------------------------
!--------------------- CALCUL DE sn(Q,Q)-----------------------------
    snqq  = 0.d0
    sn2qq = 0.d0
    trescaqq = 0.d0
!
!-- on calcule la partie ze200
    if (ze200) call rcZ2s2('SN', proqi, proqj, seismeze200, mse, sn2qq)
!
!-- on calcule la partie b3200_unitaire
    do 120 j = 1, 6
        sij(j) = 0.d0
120 continue
    if (iret2 .ne. 0) then
        do 130 j = 1, 6
            do 140 icmp = 1, 12
                mij(icmp) = proqi(1+icmp)-proqj(1+icmp)
                sigu = zr(jsigu-1+78+6*(icmp-1)+j)
                sij(j) = sij(j)+mij(icmp)*sigu
140         continue
            pij = proqi(1)-proqj(1)
            sigu = zr(jsigu-1+78+72+j)
            sij(j) = sij(j)+pij*sigu
130     continue
    endif
!
!-- on calcule la partie b3200_transitoire
    if (nq .eq. 0) then
        nbinst(2)=1
        goto 555
    endif
!
    ndim = ncmp*nbinst(2)
    call wkvect('&&RC3200.SIGMQ', 'V V R', ndim, jsigq)
    call wkvect('&&RC3200.SITHQ', 'V V R', ndim, jsithq)
    call wkvect('&&RC3200.SIPRQ', 'V V R', ndim, jsiprq)
    call wkvect('&&RC3200.SIMEQ', 'V V R', ndim, jsimeq)
!
    do 150 i = 1, nbinst(2)
        vale(1) = zr(jinst(2)+i-1)
!
        do 155 j = 1, ncmp
!
          do 160 kk = 1, nbabsc(2)
            vale(2) = zr(jabsc(2)+kk-1)
            if (n1(2) .ne. 0) then
                call tbliva(tabther(2), 2, valek, [ibid], vale,&
                            [cbid], k8b, crit, prec, nocmp(j),&
                            k8b, ibid, contraintesth(kk), cbid, k8b,&
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
                           k8b, ibid, contraintespr(kk), cbid, k8b,&
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
                           k8b, ibid, contraintesmec(kk), cbid, k8b,&
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
            contraintestot(kk) = contraintesmec(kk)+contraintespr(kk)+ contraintesth(kk)
160       continue
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
155     continue
150 continue
!
555 continue
!
!-- on maximise sur les différentes possibilités de signes
    if (seismeb32 .or. seismeunit) then
      do 161 i = 1, nbinst(2)
        do 162 l = 1, nbinst(2)
          do 170 i1 = 1, 2
            do 171 i2 = 1, 2
              do 172 i3 = 1, 2
                do 173 i4 = 1, 2
                  do 174 i5 = 1, 2
                    do 175 i6 = 1, 2
                      do 176 i7 = 1, 2
                        do 177 i8 = 1, 2
                          do 178 i9 = 1, 2
                            do 179 i10 = 1, 2
                              do 180 i11 = 1, 2
                                do 181 i12 = 1, 2
                                  do 182 i0 = 1, 2
                                    do 183 j = 1, 6
                                      if (nq .ne. 0) then
                                        dsig(j)=zr(jsigq+(i-1)*ncmp+j-1)-zr(jsigq+(l-1)*ncmp+j-1)
                                        dsith(j)=zr(jsithq+(i-1)*ncmp+j-1)-zr(jsithq+(l-1)*ncmp+j-1)
                                        dsipr(j)=zr(jsiprq+(i-1)*ncmp+j-1)-zr(jsiprq+(l-1)*ncmp+j-1)
                                        dsime(j)=zr(jsimeq+(i-1)*ncmp+j-1)-zr(jsimeq+(l-1)*ncmp+j-1)
                                      else
                                        dsig(j)=0.d0
                                        dsith(j)=0.d0
                                        dsipr(j)=0.d0
                                        dsime(j)=0.d0
                                        instqmin = -1.0
                                        instqmax = -1.0
                                      endif 
                                      sqt(j) =  dsig(j)         +e0(i0)*sij(j)     +&
                                                e1(i1)*seisfx(j)  +e1(i2)*seisfy(j)  +&
                                                e1(i3)*seisfz(j)  +e1(i4)*seismx(j)  +&
                                                e1(i5)*seismy(j)  +e1(i6)*seismz(j)  +&
                                                e1(i7)*seisfx2(j) +e1(i8)*seisfy2(j) +&
                                                e1(i9)*seisfz2(j) +e1(i10)*seismx2(j)+&
                                                e1(i11)*seismy2(j)+e1(i12)*seismz2(j)
183                                 continue
                                    call rctres(sqt,tresca)
                                    if (tresca .gt. trescaqq) then
                                      trescaqq = tresca
                                      call rctres(dsime,tresmecqq)
                                      call rctres(dsith,tresthqq)
                                      call rctres(dsipr,tresprqq)
                                      if (nq .ne. 0) then
                                        instqmin=zr(jinst(2)+i-1)
                                        instqmax=zr(jinst(2)+l-1)
                                      endif
                                    endif
182                               continue
181                             continue
180                           continue
179                         continue
178                       continue
177                     continue
176                   continue
175                 continue
174               continue
173             continue
172           continue
171         continue
170       continue
162     continue
161   continue
    else
      do 184 i = 1, nbinst(1)
        do 185 l = 1, nbinst(1)
          do 186 i0 = 1, 2
            do 187 j=1,6
              if (nq .ne. 0) then
                dsig(j)=zr(jsigq+(i-1)*ncmp+j-1)-zr(jsigq+(l-1)*ncmp+j-1)
                dsith(j)=zr(jsithq+(i-1)*ncmp+j-1)-zr(jsithq+(l-1)*ncmp+j-1)
                dsipr(j)=zr(jsiprq+(i-1)*ncmp+j-1)-zr(jsiprq+(l-1)*ncmp+j-1)
                dsime(j)=zr(jsimeq+(i-1)*ncmp+j-1)-zr(jsimeq+(l-1)*ncmp+j-1)
              else
                dsig(j)=0.d0
                dsith(j)=0.d0
                dsipr(j)=0.d0
                dsime(j)=0.d0
                instqmin = -1.0
                instqmax = -1.0
              endif
              sqt(j)  = dsig(j)+e0(i0)*sij(j)
        187 continue
            call rctres(sqt,tresca)
            if (tresca .gt. trescaqq) then
                trescaqq = tresca
                call rctres(dsime,tresmecqq)
                call rctres(dsith,tresthqq)
                call rctres(dsipr,tresprqq)
                if (nq .ne. 0) then
                  instqmin=zr(jinst(2)+i-1)
                  instqmax=zr(jinst(2)+l-1)
              endif
            endif
186       continue
185     continue
184   continue
    endif
!
    snqq = trescaqq+sn2qq
!
! -- Comparaison de sn(P,P) et sn(Q,Q)
    if (snqq .gt. snpp) then
        sn  = snqq
        tresth=tresthqq
        tresmec = tresmecqq
        trespr = tresprqq
        instsn(1)= instqmin
        instsn(2)= instqmax
    endif
!
!--------------------- CALCUL DE SN(P,Q)-----------------------------
    sn2pq = 0.d0
    snpq = 0.d0
    trescapq = 0.d0
!
!-- on calcule la partie ze200
    if (ze200) then
        call rcZ2s2('SN', propi, proqi, seismeze200, mse, sn2)
        sn2pq = max (sn2pq, sn2)
        call rcZ2s2('SN', propj, proqj, seismeze200, mse, sn2)
        sn2pq = max (sn2pq, sn2)
        call rcZ2s2('SN', propi, proqj, seismeze200, mse, sn2)
        sn2pq = max (sn2pq, sn2)
        call rcZ2s2('SN', propj, proqi, seismeze200, mse, sn2)
        sn2pq = max (sn2pq, sn2)
    endif
!
!-- on calcule la partie b3200_unitaire
    do 200 j = 1, 6
        sij1(j) = 0.d0
        sij2(j) = 0.d0
        sij3(j) = 0.d0
        sij4(j) = 0.d0
200 continue
    if (iret2 .ne. 0) then
        do 205 j = 1, 6
            do 210 icmp = 1, 12
                mij1(icmp) = propi(1+icmp)-proqi(1+icmp)
                mij2(icmp) = propj(1+icmp)-proqj(1+icmp)
                mij3(icmp) = propi(1+icmp)-proqj(1+icmp)
                mij4(icmp) = propj(1+icmp)-proqi(1+icmp)
                sigu = zr(jsigu-1+6*(icmp-1)+j)
                sij1(j) = sij1(j) + mij1(icmp)*sigu
                sij2(j) = sij2(j) + mij2(icmp)*sigu
                sij3(j) = sij3(j) + mij3(icmp)*sigu
                sij4(j) = sij4(j) + mij4(icmp)*sigu
210         continue
            pij1 = propi(1)-proqi(1)
            pij2 = propj(1)-proqj(1)
            pij3 = propi(1)-proqj(1)
            pij4 = propj(1)-proqi(1)
            sigu = zr(jsigu-1+72+j)
            sij1(j) = sij1(j) + pij1*sigu
            sij2(j) = sij2(j) + pij2*sigu
            sij3(j) = sij3(j) + pij3*sigu
            sij4(j) = sij4(j) + pij4*sigu
205     continue
    endif
!
!-- on calcule la partie b3200_transitoire
    if (np .ne. 0) then
        if (nq .ne. 0) then
            ind1 = jsigp
            ind2 = jsigq
            indth1 = jsithp
            indth2 = jsithq
            indpr1 = jsiprp
            indpr2 = jsiprq
            indme1 = jsimep
            indme2 = jsimeq
            inst1 = nbinst(1)
            inst2 = nbinst(2)
            jinst1=jinst(1)
            jinst2=jinst(2)
        else
            ind1 = jsigp
            ind2 = jsigp
            indth1 = jsithp
            indth2 = jsithp
            indpr1 = jsiprp
            indpr2 = jsiprp
            indme1 = jsimep
            indme2 = jsimep
            inst1 = nbinst(1)
            inst2 = nbinst(1)
            jinst1=jinst(1)
            jinst2=jinst(1)
        endif
    else
        if (nq .ne. 0) then
            ind1 = jsigq
            ind2 = jsigq
            indth1 = jsithq
            indth2 = jsithq
            indpr1 = jsiprq
            indpr2 = jsiprq
            indme1 = jsimeq
            indme2 = jsimeq
            inst1 = nbinst(2)
            inst2 = nbinst(2)
            jinst1=jinst(2)
            jinst2=jinst(2)
        else
            inst1 = 1
            inst2 = 1
        endif
    endif
!
!-- on maximise sur les possibilités de signes
    if (seismeb32 .or. seismeunit) then
      do 215 i = 1, inst1
        do 216 l = 1, inst2
          do 220 i1 = 1, 2
            do 221 i2 = 1, 2
              do 222 i3 = 1, 2
                do 223 i4 = 1, 2
                  do 224 i5 = 1, 2
                    do 225 i6 = 1, 2
                      do 226 i7 = 1, 2
                        do 227 i8 = 1, 2
                          do 228 i9 = 1, 2
                            do 229 i10 = 1, 2
                              do 230 i11 = 1, 2
                                do 231 i12 = 1, 2
                                  do 232 i0 = 1, 2
                                    do 233 j = 1, 6
                                      if (np+nq .ne. 0) then
                                        dsig(j)=zr(ind1+(i-1)*ncmp+j-1)-zr(ind2+(l-1)*ncmp+j-1)
                                        dsith(j)=zr(indth1+(i-1)*ncmp+j-1)-zr(indth2+(l-1)*ncmp+j-1)
                                        dsipr(j)=zr(indpr1+(i-1)*ncmp+j-1)-zr(indpr2+(l-1)*ncmp+j-1)
                                        dsime(j)=zr(indme1+(i-1)*ncmp+j-1)-zr(indme2+(l-1)*ncmp+j-1)
                                      else
                                        dsig(j)=0.d0
                                        dsith(j)=0.d0
                                        dsipr(j)=0.d0
                                        dsime(j)=0.d0
                                      endif
                                      seis(j)= e1(i1)*seisfx(j)+e1(i2)*seisfy(j)+&
                                               e1(i3)*seisfz(j)+e1(i4)*seismx(j)+&
                                               e1(i5)*seismy(j)+e1(i6)*seismz(j)+&
                                               e1(i7)*seisfx2(j)+e1(i8)*seisfy2(j)+&
                                               e1(i9)*seisfz2(j)+e1(i10)*seismx2(j)+&
                                               e1(i11)*seismy2(j)+e1(i12)*seismz2(j)
                                      st11(j) = dsig(j)+e0(i0)*sij1(j)+seis(j)
                                      st12(j) = dsig(j)+e0(i0)*sij2(j)+seis(j)
                                      st13(j) = dsig(j)+e0(i0)*sij3(j)+seis(j)
                                      st14(j) = dsig(j)+e0(i0)*sij4(j)+seis(j)
233                                 continue
!
                                    call rctres(st11,tresca)
                                    if (tresca .gt. trescapq) then
                                      trescapq = tresca
                                      call rctres(dsith,tresthpq)
                                      call rctres(dsipr,tresprpq)
                                      call rctres(dsime,tresmecpq)
                                      if (np+nq .ne. 0) then
                                        instp1=i
                                        instq1=l
                                      endif
                                    endif
                                    call rctres(st12,tresca)
                                    if (tresca .gt. trescapq) then
                                      trescapq = tresca
                                      call rctres(dsith,tresthpq)
                                      call rctres(dsipr,tresprpq)
                                      call rctres(dsime,tresmecpq)
                                      if (np+nq .ne. 0) then
                                        instp1=i
                                        instq1=l
                                      endif
                                    endif
                                    call rctres(st13,tresca)
                                    if (tresca .gt. trescapq) then
                                      trescapq = tresca
                                      call rctres(dsith,tresthpq)
                                      call rctres(dsipr,tresprpq)
                                      call rctres(dsime,tresmecpq)
                                      if (np+nq .ne. 0) then
                                        instp1=i
                                        instq1=l
                                      endif
                                    endif
                                    call rctres(st14,tresca)
                                    if (tresca .gt. trescapq) then
                                      trescapq = tresca
                                      call rctres(dsith,tresthpq)
                                      call rctres(dsipr,tresprpq)
                                      call rctres(dsime,tresmecpq)
                                      if (np+nq .ne. 0) then
                                        instp1=i
                                        instq1=l
                                      endif
                                    endif
!
232                               continue
231                             continue
230                           continue
229                         continue
228                       continue
227                     continue
226                   continue
225                 continue
224               continue
223             continue
222           continue
221         continue
220       continue
216     continue
215   continue
    else
      do 234 i = 1, inst1
        do 235 l = 1, inst2
          do 236 i0 = 1, 2
            do 237 j=1,6
                if (nq+np .ne. 0) then
                  dsig(j)=zr(ind1+(i-1)*ncmp+j-1)-zr(ind2+(l-1)*ncmp+j-1)
                  dsith(j)=zr(indth1+(i-1)*ncmp+j-1)-zr(indth2+(l-1)*ncmp+j-1)
                  dsipr(j)=zr(indpr1+(i-1)*ncmp+j-1)-zr(indpr2+(l-1)*ncmp+j-1)
                  dsime(j)=zr(indme1+(i-1)*ncmp+j-1)-zr(indme2+(l-1)*ncmp+j-1)
                else
                  dsig(j)=0.d0
                  dsith(j)=0.d0
                  dsipr(j)=0.d0
                  dsime(j)=0.d0
                endif
                st11(j)=dsig(j)+e0(i0)*sij1(j)
                st12(j)=dsig(j)+e0(i0)*sij2(j)
                st13(j)=dsig(j)+e0(i0)*sij3(j)
                st14(j)=dsig(j)+e0(i0)*sij4(j)
237         continue
            call rctres(st11,tresca)
            if (tresca .gt. trescapq) then
                trescapq = tresca
                call rctres(dsith,tresthpq)
                call rctres(dsipr,tresprpq)
                call rctres(dsime,tresmecpq)
                if (np+nq .ne. 0) then
                    instp1=i
                    instq1=l
                endif
            endif
            call rctres(st12,tresca)
            if (tresca .gt. trescapq) then
                trescapq = tresca
                call rctres(dsith,tresthpq)
                call rctres(dsipr,tresprpq)
                call rctres(dsime,tresmecpq)
                if (np+nq .ne. 0) then
                    instp1=i
                    instq1=l
                endif
            endif
            call rctres(st13,tresca)
            if (tresca .gt. trescapq) then
                trescapq = tresca
                call rctres(dsith,tresthpq)
                call rctres(dsipr,tresprpq)
                call rctres(dsime,tresmecpq)
                if (np+nq .ne. 0) then
                    instp1=i
                    instq1=l
                endif
            endif
            call rctres(st14,tresca)
            if (tresca .gt. trescapq) then
                trescapq = tresca
                call rctres(dsith,tresthpq)
                call rctres(dsipr,tresprpq)
                call rctres(dsime,tresmecpq)
                if (np+nq .ne. 0) then
                    instp1=i
                    instq1=l
                endif
            endif
!
236       continue
235     continue
234   continue
    endif
!
    snpq=trescapq+sn2pq
!
    if(snpq .gt. sn) then
        sn  = snpq
        tresth=tresthpq
        tresmec = tresmecpq
        trespr = tresprpq
        if(np+nq .ne. 0) then
            instsn(1) = zr(jinst1+instp1-1)
            instsn(2) = zr(jinst2+instq1-1)
        else
            instsn(1)=-1.d0
            instsn(2)=-1.d0
        endif
    endif
! 
666 continue
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
    if(np+nq .ne. 0) then
        AS_DEALLOCATE(vr=contraintestot)
        AS_DEALLOCATE(vr=contraintesmec)
        AS_DEALLOCATE(vr=contraintesth)
        AS_DEALLOCATE(vr=contraintespr)
    endif
!
    call jedema()
end subroutine
