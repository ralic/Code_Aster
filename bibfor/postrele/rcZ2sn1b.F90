subroutine rcZ2sn1b(ze200, lieu, numsip, numsiq, seismeb32,&
                    seismeunit, seismeze200, mse, propi, propj, proqi, proqj,&
                    instsn, sn, sp3, spmeca3, snet, trescapr, tresth)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/codent.h"
#include "asterfort/jeexin.h"
#include "asterfort/jexnom.h"
#include "asterfort/rcZ2s2.h"
#include "asterfort/jelira.h"
#include "asterfort/rctres.h"
#include "asterfort/jedema.h"
#include "asterc/getfac.h"
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
#include "asterfort/jedetr.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/rc32my.h"
    aster_logical :: ze200, seismeb32, seismeunit, seismeze200
    character(len=4) :: lieu
    integer :: numsip, numsiq
    real(kind=8) :: mse(12), propi(20), propj(20), proqi(20), trescapr
    real(kind=8) :: proqj(20), instsn(2), sn, sp3, spmeca3, snet, tresth
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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
! IN  : ZE200       : EST-ON en ZE200a ou ZE200b ?
! IN  : LIEU        : ORIGINE DU SEGMENT OU EXTREMITE
! IN  : NUMSIP      : NUMERO SITUATION P
! IN  : NUMSIQ      : NUMERO SITUATION Q
! IN  : SEISMEB32   : EST-ON en SEISME+B3200_T
! IN  : SEISMEUNIT  : EST-ON en SEISME+B3200_UNIT
! IN  : SEISMEZE200 : EST-ON en SEISME+ZE200a (ou ZE200b)
! IN  : MSE         : MOMENTS DUS AU SEISME
! IN  : PROPI       : PROPRIETES DE L'ETAT A DE LA SITUATION P 
!                        (PRESSION, MOMENTS et MATERIAU)
! IN  : PROPJ       : PROPRIETES DE L'ETAT B DE LA SITUATION P 
!                        (PRESSION, MOMENTS et MATERIAU)
! IN  : PROQI       : PROPRIETES DE L'ETAT A DE LA SITUATION Q 
!                        (PRESSION, MOMENTS et MATERIAU)
! IN  : PROQJ       : PROPRIETES DE L'ETAT B DE LA SITUATION Q 
!                        (PRESSION, MOMENTS et MATERIAU)
! OUT : INSTSN      : INSTANTS DE CALCUL DE SN
! OUT : SN 
! OUT : SP3 
! OUT : SPMECA3   
!
    integer :: jseis, j, iretp, jsigu, nbsitu, nbther, nbpres, nbmeca
    real(kind=8) :: seisfx(6), seisfy(6), seisfz(6), seismx(6), seismy(6)
    real(kind=8) :: seismz(6), seisfx2(6), seisfy2(6), seisfz2(6), seismx2(6)
    real(kind=8) :: seismy2(6), seismz2(6), prec(2), spp, s2pp, trescapp, trescappet
    character(len=8) :: knumes, nocmp(6), crit(2)
    character(len=16) :: valek(2)
    integer :: n1(2), n2(2), n3(2), iocc, numesitu, n0, nume1(2), nume2(2)
    integer :: nume3(2), np, nq, ither, numether, n5, ii, ipres, numepres
    character(len=8) ::  tabther(2), tabpres(2), tabmeca(2), tableok(2)
    integer :: imeca, numemeca, nbinst(2),jinst(2), nbabsc(2), jabsc(2)
    character(len=1) :: numsitu
    aster_logical :: exist
    character(len=8) :: k8b
    character(len=24) :: valk(4)
    real(kind=8) :: sa(6), mij(12), sigu, pij, vale(2), momen0, momen1
    integer :: icmp, ncmp, ndim, jsigp, jsithp, jsiprp, jsimep, i, kk, ibid
    real(kind=8), pointer :: contraintesth(:) => null()
    real(kind=8), pointer :: contraintespr(:) => null()
    real(kind=8), pointer :: contraintesmec(:) => null()
    real(kind=8), pointer :: contraintestot(:) => null()
    real(kind=8), pointer :: temp(:) => null()
    complex(kind=8) :: cbid
    integer :: iret3, i0, i1, l, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12
    real(kind=8) :: momen0th, momen0pr, momen0mec, momen1th, momen1pr, momen1mec
    real(kind=8) :: e0(2), sb(6), sbth(6), sbpr(6), sbme(6)
    real(kind=8) :: instpmin, instpmax, st(6), tresca, tresmec, trespr
    real(kind=8) :: sqq, s2qq, trescaqq, instqmin, instqmax
    integer :: jsigq, jsithq, jsiprq, jsimeq, ind1, ind2, indth1, indth2
    real(kind=8) :: tresthqq, tresmecqq, tresprqq, s2pq, spq, trescapq, s2
    real(kind=8) :: mij1(12), mij2(12)
    real(kind=8) :: mij3(12), mij4(12), pij1, pij2, pij3, pij4, seis(6)
    integer :: indpr1, indpr2, indme0, indme2, inst1, inst2, jinst1, jinst2
    real(kind=8) :: st11(6), st12(6), st13(6), st14(6), tresthpq, tresprpq
    real(kind=8) :: tresmecpq, k1, c1, k2, c2, k3, c3, tempa, tempb, A1(12)
    integer :: instp1, instq1, jvalin, jtempa, jtempb, jtempp, iret2p, long
    real(kind=8) :: B1(12), momp(12), sc(6), sbid(6), sc1(6), sc2(6)
    character(len=8) :: tabtemp(2)
    integer :: n4(2), ndim2, jtempq, iretq, iret2q, jflethp, jpremoyp, jpremoyq
    real(kind=8) :: momq(12), siprmoy(6), siprmoyt(6), siprmoy11(6), siprmoy21(6)
    real(kind=8) :: smom2(6), spres1(6), spres2(6), spres3(6), spres4(6)
    real(kind=8) :: smom11(6), smom12(6), smom13(6), smom14(6), sbet(6), stet(6)
    real(kind=8) :: siprmoy31(6), siprmoy41(6)

!
! DEB ------------------------------------------------------------------
    call jemarq()
!
    sn  = 0.d0
    sp3 = 0.d0
    spmeca3 = 0.d0
    snet = 0.d0
    trescapr = 0.d0
    tresth = 0.d0
!
! TROIS CONTRIBUTIONS POSSIBLES POUR SN
! SA  : UNITAIRE
!       SOUS SITUATION : CHAR_ETAT_A, CHAR_ETAT_B, PRES_A, PRES_B 
!       (RESU_MECA_UNIT et CHAR_MECA doivent être renseignés)
! SB  : TRANSITOIRE
!       SOUS SITUATION : NUME_RESU_THER NUME_RESU_MECA, NUME_RESU_PRES 
!       (RESU_MECA et RESU_PRES doivent être renseignés)
! SC  : INTERPOLATION DES MOMENTS
!       SOUS SITUATION : TEMP_A, TEMP_B, CHAR_ETAT_A, CHAR_ETAT_B, TABL_TEMP
!       (RESU_MECA_UNIT et CHAR_MECA doivent être renseignés) 
!
!------------------------
!     SI SEISME B3200
!------------------------
!-- type instantané
    if (seismeb32) then
        call jeveuo('&&RC3200.SIGSEIS', 'L', jseis)
        do 20 j = 1, 6
            seisfx(j) = 2*zr(jseis+j-1)
            seisfy(j) = 2*zr(jseis+1*6+j-1)
            seisfz(j) = 2*zr(jseis+2*6+j-1)
            seismx(j) = 2*zr(jseis+3*6+j-1)
            seismy(j) = 2*zr(jseis+4*6+j-1)
            seismz(j) = 2*zr(jseis+5*6+j-1)
            seisfx2(j)= 0.d0
            seisfy2(j)= 0.d0
            seisfz2(j)= 0.d0
            seismx2(j)= 0.d0
            seismy2(j)= 0.d0
            seismz2(j)= 0.d0
20      continue
    endif
!
!-- type unitaire
    iretp  = 0
    iret2p  = 0
    knumes = 'S       '
    call codent(numsip, 'D0', knumes(2:8))
    if (.not. ze200) then
        call jeexin(jexnom('&&RC3200.SITU_ETAT_A', knumes), iretp)
        if (iretp .ne. 0) then
            call jeveuo('&&RC3200.MECA_UNIT .'//lieu, 'L', jsigu)
            call jeexin(jexnom('&&RC3200.TEMP_ETAT_A', knumes), iret2p)
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
    n4(1) = 0
    n1(2) = 0
    n2(2) = 0
    n3(2) = 0
    n4(2) = 0
!
    do 31 iocc = 1, nbsitu, 1
! ------ on récupère la bonne situation
        call getvis('SITUATION', 'NUME_SITU', iocc=iocc, scal=numesitu, nbret=n0)
        if (numesitu .eq. numsip) then
!------ on récupère les numéros des tables sous le mot clé situation pour p
            call getvis('SITUATION', 'NUME_RESU_THER', iocc=iocc, scal=nume1(1), nbret=n1(1))
            call getvis('SITUATION', 'NUME_RESU_PRES', iocc=iocc, scal=nume2(1), nbret=n2(1))
            call getvis('SITUATION', 'NUME_RESU_MECA', iocc=iocc, scal=nume3(1), nbret=n3(1))
            call getvid('SITUATION', 'TABL_TEMP', iocc=iocc, scal=tabtemp(1), nbret=n4(1))
        endif
        if (numsip .ne. numsiq .and. numesitu .eq. numsiq) then
!------ on récupère les numéros des tables sous le mot clé situation pour q
            call getvis('SITUATION', 'NUME_RESU_THER', iocc=iocc, scal=nume1(2), nbret=n1(2))
            call getvis('SITUATION', 'NUME_RESU_PRES', iocc=iocc, scal=nume2(2), nbret=n2(2))
            call getvis('SITUATION', 'NUME_RESU_MECA', iocc=iocc, scal=nume3(2), nbret=n3(2))
            call getvid('SITUATION', 'TABL_TEMP', iocc=iocc, scal=tabtemp(2), nbret=n4(2))
        endif
31  continue
!
!-- si aucune des situations n'a de transitoires
    np = n1(1)+n2(1)+n3(1)
    nq = n1(2)+n2(2)+n3(2)
    if ((np+nq+n4(1)+n4(2)) .eq. 0) goto 999
!-- sinon on récupère les tables associées
!-- sous RESU_THER, RESU_PRES et RESU_MECA pour p et q
    do 32 ither =1, nbther, 1
        call getvis('RESU_THER', 'NUME_RESU_THER', iocc=ither, scal=numether, nbret=n5)
        do 33 ii=1,2
            if (n1(ii) .ne. 0) then
                if (numether .eq. nume1(ii)) then
                    call getvid('RESU_THER', 'TABL_RESU_THER', iocc=ither,&
                                 scal=tabther(ii), nbret=n5)
                endif
            endif
33      continue
32  continue
    do 34 ipres =1, nbpres, 1
        call getvis('RESU_PRES', 'NUME_RESU_PRES', iocc=ipres, scal=numepres, nbret=n5)
        do 35 ii=1,2
            if (n2(ii) .ne. 0) then 
                if (numepres .eq. nume2(ii)) then
                    call getvid('RESU_PRES', 'TABL_RESU_PRES', iocc=ipres,&
                                 scal=tabpres(ii), nbret=n5)
                endif
            endif
35      continue
34  continue
    do 36 imeca =1, nbmeca, 1
        call getvis('RESU_MECA', 'NUME_RESU_MECA', iocc=imeca, scal=numemeca, nbret=n5)
        do 37 ii=1,2
            if (n3(ii) .ne. 0) then 
                if (numemeca .eq. nume3(ii)) then
                    call getvid('RESU_MECA', 'TABL_RESU_MECA', iocc=imeca,&
                                scal=tabmeca(ii), nbret=n5)
                endif
            endif
37      continue
36  continue
!
!---------------- RECUPERATION DES INSTANTS ET  ------------------------
!------------- ABSCISSES DES SITUATIONS P et Q--------------------------
!
! ------ grace a la table thermique ou de pression ou mecanique
    do 38 ii=1,2
        if (ii .eq. 1) then
            numsitu='P'
        else
            numsitu='Q'
        endif
        if ((n1(ii)+n2(ii)+n3(ii)+n4(ii)) .eq. 0) goto 777
!
        if (n1(ii) .ne. 0) then
            tableok(ii)  = tabther(ii) 
        else if (n2(ii) .ne. 0) then
            tableok(ii)  = tabpres(ii) 
        else if (n3(ii) .ne. 0) then
            tableok(ii)  = tabmeca(ii)
        else
            tableok(ii)  = tabtemp(ii)  
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
38  continue
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
    spp  = 0.d0
    s2pp = 0.d0
    trescapp = -1.d0
    trescappet = -1.d0
!
!-- partie ze200
    if (ze200) call rcZ2s2('SN', propi, propj, seismeze200, mse, s2pp)
!
!-- SA partie b3200_unitaire
    do 40 j = 1, 6
        sa(j) = 0.d0
40  continue
    if (iretp .ne. 0 .and. iret2p .eq. 0) then
        do 50 j = 1, 6
            do 60 icmp = 1, 12
                mij(icmp) = propi(1+icmp)-propj(1+icmp)
                sigu = zr(jsigu-1+78+6*(icmp-1)+j)
                sa(j) = sa(j)+mij(icmp)*sigu
60          continue
            pij = propi(1)-propj(1)
            sigu = zr(jsigu-1+78+72+j)
            sa(j) = sa(j)+pij*sigu
50      continue
    endif
!
!-- SB partie transitoire
    do 65 j = 1, 6
        sb(j) = 0.d0
65  continue
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
    call wkvect('&&RC3200.FLETHP', 'V V R', ndim, jflethp)
    call wkvect('&&RC3200.PREMOYP', 'V V R', ndim, jpremoyp)
!
    do 70 i = 1, nbinst(1)
        vale(1) = zr(jinst(1)+i-1)
!
        do 71 j = 1, ncmp
!
          do 72 kk = 1, nbabsc(1)
            vale(2) = zr(jabsc(1)+kk-1)

            if (n1(1) .ne. 0) then
                call tbliva(tabther(1), 2, valek, [ibid], vale,&
                            [cbid], k8b, crit, prec, nocmp(j),&
                            k8b, ibid, contraintesth(kk), cbid, k8b,&
                            iret3)
                if (iret3 .ne. 0) then
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
                           iret3)
                if (iret3 .ne. 0) then
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
                           iret3)
                if (iret3 .ne. 0) then
                    valk (1) = tabmeca(1)
                    valk (2) = nocmp(j)
                    valk (3) = valek(1)
                    valk (4) = valek(2)
                    call utmess('F', 'POSTRCCM_2', nk=4, valk=valk, nr=2,&
                                valr=vale)
                endif
            endif
            contraintestot(kk) = contraintesmec(kk)+contraintespr(kk)+ contraintesth(kk)
72        continue
          call rc32my(nbabsc(1), zr(jabsc(1)), contraintestot, momen0, momen1)
          call rc32my(nbabsc(1), zr(jabsc(1)), contraintesth, momen0th, momen1th)
          call rc32my(nbabsc(1), zr(jabsc(1)), contraintespr, momen0pr, momen1pr)
          call rc32my(nbabsc(1), zr(jabsc(1)), contraintesmec, momen0mec, momen1mec)
          zr(jpremoyp+(i-1)*ncmp+j-1) = momen0pr
          if (lieu .eq. 'ORIG') then
              zr(jsigp+(i-1)*ncmp+j-1) = momen0 - 0.5d0*momen1
              zr(jsithp+(i-1)*ncmp+j-1) = momen0th - 0.5d0*momen1th
              zr(jsiprp+(i-1)*ncmp+j-1) = momen0pr - 0.5d0*momen1pr
              zr(jsimep+(i-1)*ncmp+j-1) = momen0mec - 0.5d0*momen1mec
              zr(jflethp+(i-1)*ncmp+j-1) = - 0.5d0*momen1th
          else
              zr(jsigp+(i-1)*ncmp+j-1) = momen0 + 0.5d0*momen1
              zr(jsithp+(i-1)*ncmp+j-1) = momen0th + 0.5d0*momen1th
              zr(jsiprp+(i-1)*ncmp+j-1) = momen0pr + 0.5d0*momen1pr
              zr(jsimep+(i-1)*ncmp+j-1) = momen0mec + 0.5d0*momen1mec
              zr(jflethp+(i-1)*ncmp+j-1) = + 0.5d0*momen1th
          endif
71      continue
70 continue
!
888 continue
!
!-- SC partie unitaire avec interpolation des moments
    AS_ALLOCATE(vr=temp,  size=1)
    ndim2 = ncmp*nbinst(1)
    call wkvect('&&RC3200.TEMPP', 'V V R', ndim2, jtempp)
    do 75 j = 1, ndim2
        zr(jtempp+j-1)=0.d0
75  continue
    do 80 j = 1, 6
        sc(j) = 0.d0
        sc1(j) = 0.d0
        sc2(j) = 0.d0
        if (iret2p .ne.0) then
            pij=propi(1)-propj(1)
            sigu = zr(jsigu-1+78+72+j)
            sc1(j) = pij*sigu
        endif 
80  continue
!
    if (iret2p .ne. 0) then
        call jelira(jexnom('&&RC3200.TEMP_ETAT_A', knumes), 'LONUTI', long)
        call jeveuo(jexnom('&&RC3200.TEMP_ETAT_A', knumes), 'L', jtempa)
        tempa = zr(jtempa)
        call jelira(jexnom('&&RC3200.TEMP_ETAT_B', knumes), 'LONUTI', long)
        call jeveuo(jexnom('&&RC3200.TEMP_ETAT_B', knumes), 'L', jtempb)
        tempb = zr(jtempb)
        if (tempa .lt. tempb) then
            do 81 icmp = 1, 12
                    A1(icmp) = (propi(icmp+1)-propj(icmp+1))/(tempa-tempb)
                    B1(icmp) = (propj(icmp+1)*tempa-propi(icmp+1)*tempb)/(tempa-tempb)
81          continue
        else
            do 82 icmp = 1, 12
                    A1(icmp) = (propj(icmp+1)-propi(icmp+1))/(tempb-tempa)
                    B1(icmp) = (propi(icmp+1)*tempb-propj(icmp+1)*tempa)/(tempb-tempa)
82          continue
        endif
!
        do 83 i = 1, nbinst(1)
            vale(1) = zr(jinst(1)+i-1)
            if (lieu .eq. 'ORIG') then
                vale(2) = zr(jabsc(1))
            else
                vale(2) = zr(jabsc(1)+nbabsc(1)-1)
            endif
!
            call tbliva(tabtemp(1), 2, valek, [ibid], vale,&
                        [cbid], k8b, crit, prec, 'TEMP',&
                        k8b, ibid, temp(1), cbid, k8b,&
                        iret3)
            if (iret3 .ne. 0) then
                valk (1) = tabtemp(1)
                valk (2) = 'TEMP'
                valk (3) = valek(1)
                valk (4) = valek(2)
                call utmess('F', 'POSTRCCM_2', nk=4, valk=valk, nr=2,&
                            valr=vale)
            endif
            do 84 j = 1, 6
              sbid(j) = 0.d0
              do 85 icmp = 1, 12
                momp(icmp) = A1(icmp)*temp(1)+B1(icmp)
                sigu = zr(jsigu-1+78+6*(icmp-1)+j)
                sbid(j) = sbid(j) + momp(icmp)*sigu
85            continue
              zr(jtempp+(i-1)*ncmp+j-1) = sbid(j)
84          continue
83      continue
    endif
!
!-- on maximise sur les différentes possibilités de signes
    do 90 i0 = 1, 2
        i1 = 2*(i0-2)+1
        e0(i0) = i1
90  continue
!
    if (seismeb32 .or. seismeunit) then
      do 91 i = 1, nbinst(1)
        do 92 l = 1, nbinst(1)
          do 100 i0 = 1, 2
            do 101 i1 = 1, 2
              do 102 i2 = 1, 2
                do 103 i3 = 1, 2
                  do 104 i4 = 1, 2
                    do 105 i5 = 1, 2
                      do 106 i6 = 1, 2
                        do 107 i7 = 1, 2
                          do 108 i8 = 1, 2
                            do 109 i9 = 1, 2
                              do 110 i10 = 1, 2
                                do 111 i11 = 1, 2
                                  do 112 i12 = 1, 2
                                    do 113 j = 1, 6
!
                                      if (np .ne. 0) then
                                        sb(j)=zr(jsigp+(i-1)*ncmp+j-1)-zr(jsigp+(l-1)*ncmp+j-1)
                                        sc2(j)=zr(jtempp+(i-1)*ncmp+j-1)-zr(jtempp+(l-1)*ncmp+j-1)
                                        sbth(j)=zr(jsithp+(i-1)*ncmp+j-1)-zr(jsithp+(l-1)*ncmp+j-1)
                                        sbpr(j)=zr(jsiprp+(i-1)*ncmp+j-1)-zr(jsiprp+(l-1)*ncmp+j-1)
                                        sbme(j)=zr(jsimep+(i-1)*ncmp+j-1)-zr(jsimep+(l-1)*ncmp+j-1)
                                        sbet(j)=sb(j)-&
                                            (zr(jflethp+(i-1)*ncmp+j-1)-zr(jflethp+(l-1)*ncmp+j-1))
                                      else
                                        sc2(j)=zr(jtempp+(i-1)*ncmp+j-1)
                                        sb(j)=0.d0
                                        sbth(j)=0.d0
                                        sbpr(j)=0.d0
                                        sbme(j)=0.d0
                                        sbet(j) = 0.d0
                                        instpmin = -1.0
                                        instpmax = -1.0
                                      endif 
!
                                      sc(j) =  sc1(j) + sc2(j)
                                      st(j) =  sb(j) + e0(i0)*sa(j) + e0(i0)*sc(j)  +&
                                               e0(i1)*seisfx(j)  +e0(i2)*seisfy(j)  +&
                                               e0(i3)*seisfz(j)  +e0(i4)*seismx(j)  +&
                                               e0(i5)*seismy(j)  +e0(i6)*seismz(j)  +&
                                               e0(i7)*seisfx2(j) +e0(i8)*seisfy2(j) +&
                                               e0(i9)*seisfz2(j) +e0(i10)*seismx2(j)+&
                                               e0(i11)*seismy2(j)+e0(i12)*seismz2(j)
                                      stet(j) =  sbet(j) + e0(i0)*sa(j) + e0(i0)*sc(j)  +&
                                               e0(i1)*seisfx(j)  +e0(i2)*seisfy(j)  +&
                                               e0(i3)*seisfz(j)  +e0(i4)*seismx(j)  +&
                                               e0(i5)*seismy(j)  +e0(i6)*seismz(j)  +&
                                               e0(i7)*seisfx2(j) +e0(i8)*seisfy2(j) +&
                                               e0(i9)*seisfz2(j) +e0(i10)*seismx2(j)+&
                                               e0(i11)*seismy2(j)+e0(i12)*seismz2(j)
113                                 continue
                                    call rctres(st,tresca)
                                    if (tresca .gt. trescapp) then
                                      trescapp = tresca
                                      call rctres(sbme,tresmec)
                                      call rctres(sbth,tresth)
                                      call rctres(sbpr,trespr)
                                      if (np .ne. 0) then
                                        instpmin=zr(jinst(1)+i-1)
                                        instpmax=zr(jinst(1)+l-1)
                                      endif
                                    endif
                                    call rctres(stet,tresca)
                                    trescappet = max(tresca, trescappet)
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
!
                if (np .ne. 0) then
                  sb(j)=zr(jsigp+(i-1)*ncmp+j-1)-zr(jsigp+(l-1)*ncmp+j-1)
                  sc2(j)=zr(jtempp+(i-1)*ncmp+j-1)-zr(jtempp+(l-1)*ncmp+j-1)
                  sbth(j)=zr(jsithp+(i-1)*ncmp+j-1)-zr(jsithp+(l-1)*ncmp+j-1)
                  sbpr(j)=zr(jsiprp+(i-1)*ncmp+j-1)-zr(jsiprp+(l-1)*ncmp+j-1)
                  sbme(j)=zr(jsimep+(i-1)*ncmp+j-1)-zr(jsimep+(l-1)*ncmp+j-1)
                  sbet(j)=sb(j)- (zr(jflethp+(i-1)*ncmp+j-1)-zr(jflethp+(l-1)*ncmp+j-1))
                  siprmoyt(j) = zr(jpremoyp+(i-1)*ncmp+j-1)-zr(jpremoyp+(l-1)*ncmp+j-1)
                else
                  sb(j)=0.d0
                  sc2(j)=zr(jtempp+(i-1)*ncmp+j-1)
                  sbth(j)=0.d0
                  sbpr(j)=0.d0
                  sbme(j)=0.d0
                  sbet(j)=0.d0
                  siprmoyt(j)=0.d0
                  instpmin = -1.0
                  instpmax = -1.0
                endif 
!
                sc(j) =  sc1(j)+sc2(j)
                st(j)  = sb(j)+e0(i0)*sa(j)+e0(i0)*sc(j)
                stet(j)  = sbet(j)+e0(i0)*sa(j)+e0(i0)*sc(j)
117         continue
            call rctres(st,tresca)
            if (tresca .gt. trescapp) then
              trescapp = tresca
              call rctres(sbme,tresmec)
              call rctres(sbth,tresth)
              call rctres(sbpr,trespr)
              if (np .ne. 0) then
                instpmin=zr(jinst(1)+i-1)
                instpmax=zr(jinst(1)+l-1)
              endif
            endif
            call rctres(stet,tresca)
            trescappet = max(tresca, trescappet)
            call rctres(siprmoyt,tresca)
            trescapr = max(tresca, trescapr)
116       continue
115     continue
114   continue
    endif
!
    spp    = trescapp+s2pp
    snet = trescappet+s2pp
    sn  = spp
    instsn(1) = instpmin
    instsn(2) = instpmax
!
!-- Pour le calcul du rochet thermique
    do 41 j = 1, 6
        siprmoy(j) = 0.d0
41  continue
    if (iretp .ne. 0) then
        do 51 j = 1, 6
            pij = propi(1)-propj(1)
            sigu = zr(jsigu-1+156+72+j)
            siprmoy(j) = pij*sigu
51      continue
    endif
    call rctres(siprmoy,tresca)
    trescapr = max(tresca, trescapr)
!
    if (numsip .eq. numsiq) goto 666
!
!--------------------------------------------------------------------
!                  DANS LE CAS D'UNE COMBINAISON DE
!                          SITUATIONS P ET Q
!--------------------------------------------------------------------
!--------------------- CALCUL DE SN(Q,Q)-----------------------------
    sqq  = 0.d0
    s2qq = 0.d0
    trescaqq = -1.d0
!
!-- partie ze200
    if (ze200) call rcZ2s2('SN', proqi, proqj, seismeze200, mse, s2qq)
!
!-- SA partie b3200_unitaire
    iretq  = 0
    iret2q  = 0
    knumes = 'S       '
    call codent(numsiq, 'D0', knumes(2:8))
    if (.not. ze200) then
        call jeexin(jexnom('&&RC3200.SITU_ETAT_A', knumes), iretq)
        if (iretq .ne. 0) then
            if (iretp .eq. 0) call jeveuo('&&RC3200.MECA_UNIT .'//lieu, 'L', jsigu)
            call jeexin(jexnom('&&RC3200.TEMP_ETAT_A', knumes), iret2q)
        endif
    endif
!
    do 120 j = 1, 6
        sa(j) = 0.d0
120 continue
    if (iretq .ne. 0 .and. iret2q .eq. 0) then
        do 130 j = 1, 6
            do 140 icmp = 1, 12
                mij(icmp) = proqi(1+icmp)-proqj(1+icmp)
                sigu = zr(jsigu-1+78+6*(icmp-1)+j)
                sa(j) = sa(j)+mij(icmp)*sigu
140         continue
            pij = proqi(1)-proqj(1)
            sigu = zr(jsigu-1+78+72+j)
            sa(j) = sa(j)+pij*sigu
130     continue
    endif
!
!-- SB partie transitoire
    do 150 j = 1, 6
        sb(j) = 0.d0
150 continue
!
    if (nq .eq. 0) then
        nbinst(2)=1
        goto 555
    endif
    ndim = ncmp*nbinst(2)
    call wkvect('&&RC3200.SIGMQ', 'V V R', ndim, jsigq)
    call wkvect('&&RC3200.SITHQ', 'V V R', ndim, jsithq)
    call wkvect('&&RC3200.SIPRQ', 'V V R', ndim, jsiprq)
    call wkvect('&&RC3200.SIMEQ', 'V V R', ndim, jsimeq)
    call wkvect('&&RC3200.PREMOYQ', 'V V R', ndim, jpremoyq)
!
    do 151 i = 1, nbinst(2)
        vale(1) = zr(jinst(2)+i-1)
!
        do 152 j = 1, ncmp
!
          do 153 kk = 1, nbabsc(2)
            vale(2) = zr(jabsc(2)+kk-1)
            if (n1(2) .ne. 0) then
                call tbliva(tabther(2), 2, valek, [ibid], vale,&
                            [cbid], k8b, crit, prec, nocmp(j),&
                            k8b, ibid, contraintesth(kk), cbid, k8b,&
                            iret3)
                if (iret3 .ne. 0) then
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
                           iret3)
                if (iret3 .ne. 0) then
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
                           iret3)
                if (iret3 .ne. 0) then
                    valk (1) = tabmeca(2)
                    valk (2) = nocmp(j)
                    valk (3) = valek(1)
                    valk (4) = valek(2)
                    call utmess('F', 'POSTRCCM_2', nk=4, valk=valk, nr=2,&
                                valr=vale)
                endif
            endif
            contraintestot(kk) = contraintesmec(kk)+contraintespr(kk)+ contraintesth(kk)
153       continue
          call rc32my(nbabsc(2), zr(jabsc(2)), contraintestot, momen0, momen1)
          call rc32my(nbabsc(2), zr(jabsc(2)), contraintesth, momen0th, momen1th)
          call rc32my(nbabsc(2), zr(jabsc(2)), contraintespr, momen0pr, momen1pr)
          call rc32my(nbabsc(2), zr(jabsc(2)), contraintesmec, momen0mec, momen1mec)
          zr(jpremoyq+(i-1)*ncmp+j-1) = momen0pr
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
152     continue
151 continue
!
555 continue
!
!-- SC partie unitaire avec interpolation des moments
    ndim2 = ncmp*nbinst(2)
    call wkvect('&&RC3200.TEMPQ', 'V V R', ndim2, jtempq)
    do 154 j = 1, ndim2
        zr(jtempq+j-1)=0.d0
154 continue
!
    do 170 j = 1, 6
        sc(j) = 0.d0
        sc1(j) = 0.d0
        sc2(j) = 0.d0
        if (iret2q .ne.0) then
            pij=proqi(1)-proqj(1)
            sigu = zr(jsigu-1+78+72+j)
            sc1(j) = pij*sigu
        endif 
170 continue
!
    if (iret2q .ne. 0) then
        call jelira(jexnom('&&RC3200.TEMP_ETAT_A', knumes), 'LONUTI', long)
        call jeveuo(jexnom('&&RC3200.TEMP_ETAT_A', knumes), 'L', jtempa)
        tempa = zr(jtempa)
        call jelira(jexnom('&&RC3200.TEMP_ETAT_B', knumes), 'LONUTI', long)
        call jeveuo(jexnom('&&RC3200.TEMP_ETAT_B', knumes), 'L', jtempb)
        tempb = zr(jtempb)
        if (tempa .lt. tempb) then
            do 171 icmp = 1, 12
                    A1(icmp) = (proqi(icmp+1)-proqj(icmp+1))/(tempa-tempb)
                    B1(icmp) = (proqj(icmp+1)*tempa-proqi(icmp+1)*tempb)/(tempa-tempb)
171         continue
        else
            do 172 icmp = 1, 12
                    A1(icmp) = (proqj(icmp+1)-proqi(icmp+1))/(tempb-tempa)
                    B1(icmp) = (proqi(icmp+1)*tempb-proqj(icmp+1)*tempa)/(tempb-tempa)
172         continue
        endif
!
        do 173 i = 1, nbinst(2)
            vale(1) = zr(jinst(2)+i-1)
            if (lieu .eq. 'ORIG') then
                vale(2) = zr(jabsc(2))
            else
                vale(2) = zr(jabsc(2)+nbabsc(2)-1)
            endif
!
            call tbliva(tabtemp(2), 2, valek, [ibid], vale,&
                        [cbid], k8b, crit, prec, 'TEMP',&
                        k8b, ibid, temp(1), cbid, k8b,&
                        iret3)
            if (iret3 .ne. 0) then
                valk (1) = tabtemp(2)
                valk (2) = 'TEMP'
                valk (3) = valek(1)
                valk (4) = valek(2)
                call utmess('F', 'POSTRCCM_2', nk=4, valk=valk, nr=2,&
                            valr=vale)
            endif
            do 174 j = 1, 6
              sbid(j) = 0.d0
              do 175 icmp = 1, 12
                momq(icmp) = A1(icmp)*temp(1)+B1(icmp)
                sigu = zr(jsigu-1+78+6*(icmp-1)+j)
                sbid(j) = sbid(j) + momq(icmp)*sigu
175           continue
              zr(jtempq+(i-1)*ncmp+j-1) = sbid(j)
174         continue
173     continue
    endif
!
!-- on maximise sur les différentes possibilités de signes
    if (seismeb32 .or. seismeunit) then
      do 176 i = 1, nbinst(2)
        do 177 l = 1, nbinst(2)
          do 180 i0 = 1, 2
            do 181 i1 = 1, 2
              do 182 i2 = 1, 2
                do 183 i3 = 1, 2
                  do 184 i4 = 1, 2
                    do 185 i5 = 1, 2
                      do 186 i6 = 1, 2
                        do 187 i7 = 1, 2
                          do 188 i8 = 1, 2
                            do 189 i9 = 1, 2
                              do 190 i10 = 1, 2
                                do 191 i11 = 1, 2
                                  do 192 i12 = 1, 2
                                    do 193 j = 1, 6
!
                                      if (nq .ne. 0) then
                                        sb(j)=zr(jsigq+(i-1)*ncmp+j-1)-zr(jsigq+(l-1)*ncmp+j-1)
                                        sc2(j)=zr(jtempq+(i-1)*ncmp+j-1)-zr(jtempq+(l-1)*ncmp+j-1)
                                        sbth(j)=zr(jsithq+(i-1)*ncmp+j-1)-zr(jsithq+(l-1)*ncmp+j-1)
                                        sbpr(j)=zr(jsiprq+(i-1)*ncmp+j-1)-zr(jsiprq+(l-1)*ncmp+j-1)
                                        sbme(j)=zr(jsimeq+(i-1)*ncmp+j-1)-zr(jsimeq+(l-1)*ncmp+j-1)
                                      else
                                        sb(j)=0.d0
                                        sc2(j)=zr(jtempq+(i-1)*ncmp+j-1)
                                        sbth(j)=0.d0
                                        sbpr(j)=0.d0
                                        sbme(j)=0.d0
                                        instqmin = -1.0
                                        instqmax = -1.0
                                      endif 
!
                                      sc(j) =  sc1(j) + sc2(j)
                                      st(j) =  sb(j) + e0(i0)*sa(j) + e0(i0)*sc(j)  +&
                                               e0(i1)*seisfx(j)  +e0(i2)*seisfy(j)  +&
                                               e0(i3)*seisfz(j)  +e0(i4)*seismx(j)  +&
                                               e0(i5)*seismy(j)  +e0(i6)*seismz(j)  +&
                                               e0(i7)*seisfx2(j) +e0(i8)*seisfy2(j) +&
                                               e0(i9)*seisfz2(j) +e0(i10)*seismx2(j)+&
                                               e0(i11)*seismy2(j)+e0(i12)*seismz2(j)
193                                 continue
                                    call rctres(st,tresca)
                                    if (tresca .gt. trescaqq) then
                                      trescaqq = tresca
                                      call rctres(sbme,tresmecqq)
                                      call rctres(sbth,tresthqq)
                                      call rctres(sbpr,tresprqq)
                                      if (nq .ne. 0) then
                                        instqmin=zr(jinst(2)+i-1)
                                        instqmax=zr(jinst(2)+l-1)
                                      endif
                                    endif
192                               continue
191                             continue
190                           continue
189                         continue
188                       continue
187                     continue
186                   continue
185                 continue
184               continue
183             continue
182           continue
181         continue
180       continue
177     continue
176   continue
    else
      do 178 i = 1, nbinst(2)
        do 179 l = 1, nbinst(2)
          do 194 i0 = 1, 2
            do 195 j=1,6
!
              if (nq .ne. 0) then
                sb(j)=zr(jsigq+(i-1)*ncmp+j-1)-zr(jsigq+(l-1)*ncmp+j-1)
                sc2(j)=zr(jtempq+(i-1)*ncmp+j-1)-zr(jtempq+(l-1)*ncmp+j-1)
                sbth(j)=zr(jsithq+(i-1)*ncmp+j-1)-zr(jsithq+(l-1)*ncmp+j-1)
                sbpr(j)=zr(jsiprq+(i-1)*ncmp+j-1)-zr(jsiprq+(l-1)*ncmp+j-1)
                sbme(j)=zr(jsimeq+(i-1)*ncmp+j-1)-zr(jsimeq+(l-1)*ncmp+j-1)
                siprmoyt(j)=zr(jpremoyq+(i-1)*ncmp+j-1)-zr(jpremoyq+(l-1)*ncmp+j-1)
              else
                sb(j)=0.d0
                sc2(j)=zr(jtempq+(i-1)*ncmp+j-1)
                sbth(j)=0.d0
                sbpr(j)=0.d0
                sbme(j)=0.d0
                siprmoyt(j)=0.d0
                instqmin = -1.0
                instqmax = -1.0
              endif
!
              sc(j) =  sc1(j) + sc2(j)
              st(j)  = sb(j)+e0(i0)*sa(j)+e0(i0)*sc(j)
195         continue
            call rctres(st,tresca)
            if (tresca .gt. trescaqq) then
                trescaqq = tresca
                call rctres(sbme,tresmecqq)
                call rctres(sbth,tresthqq)
                call rctres(sbpr,tresprqq)
                if (nq .ne. 0) then
                  instqmin=zr(jinst(2)+i-1)
                  instqmax=zr(jinst(2)+l-1)
              endif
            endif
            call rctres(siprmoyt,tresca)
            trescapr = max(tresca, trescapr)
194       continue
179     continue
178   continue
    endif
!
    sqq = trescaqq+s2qq
!
! -- Comparaison de SN(P,P) et SN(Q,Q)
    if (sqq .gt. spp) then
        sn  = sqq
        tresth=tresthqq
        tresmec = tresmecqq
        trespr = tresprqq
        instsn(1)= instqmin
        instsn(2)= instqmax
    endif
!
!-- Pour le calcul du rochet thermique
    do 42 j = 1, 6
        siprmoy(j) = 0.d0
42  continue
    if (iretq .ne. 0) then
        do 52 j = 1, 6
            pij = proqi(1)-proqj(1)
            sigu = zr(jsigu-1+156+72+j)
            siprmoy(j) = pij*sigu
52      continue
    endif
    call rctres(siprmoy,tresca)
    trescapr = max(tresca, trescapr)
!
!--------------------- CALCUL DE SN(P,Q)-----------------------------
    s2pq = 0.d0
    spq = 0.d0
    trescapq = 0.d0
!
!-- partie ze200
    if (ze200) then
        call rcZ2s2('SN', propi, proqi, seismeze200, mse, s2)
        s2pq=max(s2,s2pq)
        call rcZ2s2('SN', propj, proqj, seismeze200, mse, s2)
        s2pq=max(s2,s2pq)
        call rcZ2s2('SN', propi, proqj, seismeze200, mse, s2)
        s2pq=max(s2,s2pq)
        call rcZ2s2('SN', propj, proqi, seismeze200, mse, s2)
        s2pq=max(s2,s2pq)
    endif
!
!-- partie due à la pression
    do 197 j = 1, 6
        spres1(j) = 0.d0
        spres2(j) = 0.d0
        spres3(j) = 0.d0
        spres4(j) = 0.d0
197 continue
    if (iretp .ne. 0 .or. iretq .ne. 0) then
        do 198 j = 1, 6
            pij1 = propi(1)-proqi(1)
            pij2 = propj(1)-proqj(1)
            pij3 = propi(1)-proqj(1)
            pij4 = propj(1)-proqi(1)
            sigu = zr(jsigu-1+72+78+j)
            spres1(j) = pij1*sigu
            spres2(j) = pij2*sigu
            spres3(j) = pij3*sigu
            spres4(j) = pij4*sigu
198     continue
    endif
!
!-- partie due aux moments unitaires
    do 199 j = 1, 6
        smom11(j) = 0.d0
        smom12(j) = 0.d0
        smom13(j) = 0.d0
        smom14(j) = 0.d0
199 continue
!
    if(iretp .eq. 0 .and. iretq .eq. 0) goto 444 
!
    do 200 j = 1, 6
        do 201 icmp = 1, 12
            mij1(icmp) = 0.d0
            mij2(icmp) = 0.d0
            mij3(icmp) = 0.d0
            mij4(icmp) = 0.d0
            if (iretp .ne. 0 .and. iret2p .eq. 0) then
                mij1(icmp) = mij1(icmp)+propi(1+icmp)
                mij2(icmp) = mij2(icmp)+propj(1+icmp)
                mij3(icmp) = mij3(icmp)+propi(1+icmp)
                mij4(icmp) = mij4(icmp)+propj(1+icmp)
            endif
            if (iretq .ne. 0 .and. iret2q .eq. 0) then
                mij1(icmp) = mij1(icmp)-proqi(1+icmp)
                mij2(icmp) = mij2(icmp)-proqj(1+icmp)
                mij3(icmp) = mij3(icmp)-proqj(1+icmp)
                mij4(icmp) = mij4(icmp)-proqi(1+icmp)
            endif
            sigu = zr(jsigu-1+78+6*(icmp-1)+j)
            smom11(j) = smom11(j) + mij1(icmp)*sigu
            smom12(j) = smom12(j) + mij2(icmp)*sigu
            smom13(j) = smom13(j) + mij3(icmp)*sigu
            smom14(j) = smom14(j) + mij4(icmp)*sigu
201     continue
200 continue
!
444 continue
!
!-- Pour le calcul du rochet thermique
    do 43 j = 1, 6
        siprmoy11(j) = 0.d0
        siprmoy21(j) = 0.d0
        siprmoy31(j) = 0.d0
        siprmoy41(j) = 0.d0
        siprmoy(j) = 0.d0
43  continue
    if (iretp .ne. 0 .or. iretq .ne. 0) then
        do 53 j = 1, 6
            sigu = zr(jsigu-1+156+72+j)
            siprmoy11(j) = pij1*sigu
            siprmoy21(j) = pij2*sigu
            siprmoy31(j) = pij3*sigu
            siprmoy41(j) = pij4*sigu
53      continue
    endif
!
!-- SB partie transitoire
    do 211 j = 1, 6
        sb(j) = 0.d0
211 continue
    if (np .ne. 0) then
        if (nq .ne. 0) then
            ind1 = jsigp
            ind2 = jsigq
            indth1 = jsithp
            indth2 = jsithq
            indpr1 = jsiprp
            indpr2 = jsiprq
            indme0 = jsimep
            indme2 = jsimeq
            inst1 = nbinst(1)
            inst2 = nbinst(2)
            jinst1=jinst(1)
            jinst2=jinst(2)
        else
            ind1 = jsigp
            ind2 = 1
            indth1 = jsithp
            indth2 = 1
            indpr1 = jsiprp
            indpr2 = 1
            indme0 = jsimep
            indme2 = 1
            inst1 = nbinst(1)
            inst2 = 1
            jinst1=jinst(1)
            jinst2=1
        endif
    else
        if (nq .ne. 0) then
            ind1 = 1
            ind2 = jsigq
            indth1 = 1
            indth2 = jsithq
            indpr1 = 1
            indpr2 = jsiprq
            indme0 = 1
            indme2 = jsimeq
            inst1 = 1
            inst2 = nbinst(2)
            jinst1=1
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
          do 220 i0 = 1, 2
            do 221 i1 = 1, 2
              do 222 i2 = 1, 2
                do 223 i3 = 1, 2
                  do 224 i4 = 1, 2
                    do 225 i5 = 1, 2
                      do 226 i6 = 1, 2
                        do 227 i7 = 1, 2
                          do 228 i8 = 1, 2
                            do 229 i9 = 1, 2
                              do 230 i10 = 1, 2
                                do 231 i11 = 1, 2
                                  do 232 i12 = 1, 2
                                    do 233 j = 1, 6
                                      if (np+nq .ne. 0) then
                                        sb(j)=zr(ind1+(i-1)*ncmp+j-1)-zr(ind2+(l-1)*ncmp+j-1)
                                        smom2(j)=zr(jtempp+(i-1)*ncmp+j-1)-zr(jtempq+(l-1)*ncmp+j-1)
                                        sbth(j)=zr(indth1+(i-1)*ncmp+j-1)-zr(indth2+(l-1)*ncmp+j-1)
                                        sbpr(j)=zr(indpr1+(i-1)*ncmp+j-1)-zr(indpr2+(l-1)*ncmp+j-1)
                                        sbme(j)=zr(indme0+(i-1)*ncmp+j-1)-zr(indme2+(l-1)*ncmp+j-1)
                                      else
                                        smom2(j)=zr(jtempp+(i-1)*ncmp+j-1)-zr(jtempq+(l-1)*ncmp+j-1)
                                        sb(j)=0.d0
                                        sbth(j)=0.d0
                                        sbpr(j)=0.d0
                                        sbme(j)=0.d0
                                      endif
                                      seis(j)= e0(i1)*seisfx(j)+e0(i2)*seisfy(j)+&
                                               e0(i3)*seisfz(j)+e0(i4)*seismx(j)+&
                                               e0(i5)*seismy(j)+e0(i6)*seismz(j)+&
                                               e0(i7)*seisfx2(j)+e0(i8)*seisfy2(j)+&
                                               e0(i9)*seisfz2(j)+e0(i10)*seismx2(j)+&
                                               e0(i11)*seismy2(j)+e0(i12)*seismz2(j)
                                      st11(j) = sb(j)+e0(i0)*(smom11(j)+smom2(j)+spres1(j))+seis(j)
                                      st12(j) = sb(j)+e0(i0)*(smom12(j)+smom2(j)+spres2(j))+seis(j)
                                      st13(j) = sb(j)+e0(i0)*(smom13(j)+smom2(j)+spres3(j))+seis(j)
                                      st14(j) = sb(j)+e0(i0)*(smom14(j)+smom2(j)+spres4(j))+seis(j)
233                                 continue
!
                                    call rctres(st11,tresca)
                                    if (tresca .gt. trescapq) then
                                      trescapq = tresca
                                      call rctres(sbth,tresthpq)
                                      call rctres(sbpr,tresprpq)
                                      call rctres(sbme,tresmecpq)
                                      if (np+nq .ne. 0) then
                                        instp1=i
                                        instq1=l
                                      endif
                                    endif
                                    call rctres(st12,tresca)
                                    if (tresca .gt. trescapq) then
                                      trescapq = tresca
                                      call rctres(sbth,tresthpq)
                                      call rctres(sbpr,tresprpq)
                                      call rctres(sbme,tresmecpq)
                                      if (np+nq .ne. 0) then
                                        instp1=i
                                        instq1=l
                                      endif
                                    endif
                                    call rctres(st13,tresca)
                                    if (tresca .gt. trescapq) then
                                      trescapq = tresca
                                      call rctres(sbth,tresthpq)
                                      call rctres(sbpr,tresprpq)
                                      call rctres(sbme,tresmecpq)
                                      if (np+nq .ne. 0) then
                                        instp1=i
                                        instq1=l
                                      endif
                                    endif
                                    call rctres(st14,tresca)
                                    if (tresca .gt. trescapq) then
                                      trescapq = tresca
                                      call rctres(sbth,tresthpq)
                                      call rctres(sbpr,tresprpq)
                                      call rctres(sbme,tresmecpq)
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
                  sb(j)=zr(ind1+(i-1)*ncmp+j-1)-zr(ind2+(l-1)*ncmp+j-1)
                  smom2(j)=zr(jtempp+(i-1)*ncmp+j-1)-zr(jtempq+(l-1)*ncmp+j-1)
                  sbth(j)=zr(indth1+(i-1)*ncmp+j-1)-zr(indth2+(l-1)*ncmp+j-1)
                  sbpr(j)=zr(indpr1+(i-1)*ncmp+j-1)-zr(indpr2+(l-1)*ncmp+j-1)
                  sbme(j)=zr(indme0+(i-1)*ncmp+j-1)-zr(indme2+(l-1)*ncmp+j-1)
                else
                  smom2(j)=zr(jtempp+(i-1)*ncmp+j-1)-zr(jtempq+(l-1)*ncmp+j-1)
                  sb(j)=0.d0
                  sbth(j)=0.d0
                  sbpr(j)=0.d0
                  sbme(j)=0.d0
                endif
                st11(j) = sb(j)+e0(i0)*(smom11(j)+smom2(j)+spres1(j))
                st12(j) = sb(j)+e0(i0)*(smom12(j)+smom2(j)+spres2(j))
                st13(j) = sb(j)+e0(i0)*(smom13(j)+smom2(j)+spres3(j))
                st14(j) = sb(j)+e0(i0)*(smom14(j)+smom2(j)+spres4(j))
                if (np .ne. 0) then
                    if (nq .ne. 0) then
                        siprmoyt(j) = zr(jpremoyp+(i-1)*ncmp+j-1)-zr(jpremoyq+(l-1)*ncmp+j-1)
                    else
                        siprmoyt(j) = zr(jpremoyp+(i-1)*ncmp+j-1)
                    endif
                else
                    if (nq .ne. 0) then
                        siprmoyt(j) = zr(jpremoyq+(l-1)*ncmp+j-1)
                    else
                        siprmoyt(j) = 0.d0
                    endif 
                endif   
                siprmoy(j) =  siprmoy11(j)-siprmoyt(j)  
                call rctres (siprmoy, tresca)
                trescapr = max (trescapr, tresca) 
                siprmoy(j) =  siprmoy21(j)-siprmoyt(j)  
                call rctres (siprmoy, tresca)
                trescapr = max (trescapr, tresca)
                siprmoy(j) =  siprmoy31(j)-siprmoyt(j)  
                call rctres (siprmoy, tresca)
                trescapr = max (trescapr, tresca)
                siprmoy(j) =  siprmoy41(j)-siprmoyt(j)  
                call rctres (siprmoy, tresca)
                trescapr = max (trescapr, tresca)
237         continue
            call rctres(st11,tresca)
            if (tresca .gt. trescapq) then
                trescapq = tresca
                call rctres(sbth,tresthpq)
                call rctres(sbpr,tresprpq)
                call rctres(sbme,tresmecpq)
                if (np+nq .ne. 0) then
                    instp1=i
                    instq1=l
                endif
            endif
            call rctres(st12,tresca)
            if (tresca .gt. trescapq) then
                trescapq = tresca
                call rctres(sbth,tresthpq)
                call rctres(sbpr,tresprpq)
                call rctres(sbme,tresmecpq)
                if (np+nq .ne. 0) then
                    instp1=i
                    instq1=l
                endif
            endif
            call rctres(st13,tresca)
            if (tresca .gt. trescapq) then
                trescapq = tresca
                call rctres(sbth,tresthpq)
                call rctres(sbpr,tresprpq)
                call rctres(sbme,tresmecpq)
                if (np+nq .ne. 0) then
                    instp1=i
                    instq1=l
                endif
            endif
            call rctres(st14,tresca)
            if (tresca .gt. trescapq) then
                trescapq = tresca
                call rctres(sbth,tresthpq)
                call rctres(sbpr,tresprpq)
                call rctres(sbme,tresmecpq)
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
    spq=trescapq+s2pq
!
    if(spq .gt. sn) then
        sn  = spq
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
        call jedetr('&&RC3200.SIGMP')
        call jedetr('&&RC3200.SITHP')
        call jedetr('&&RC3200.SIPRP')
        call jedetr('&&RC3200.SIMEP')
        call jedetr('&&RC3200.FLETHP')
        call jedetr('&&RC3200.PREMOYP')
    endif
    if ((np+n4(1)) .ne. 0) then
        call jedetr('RC.INSTANT.P')
        call jedetr('RC.ABSC.P')
    endif
    if ((nq+n4(2)) .ne. 0) then
        call jedetr('RC.INSTANT.Q')
        call jedetr('RC.ABSC.Q')
    endif
    if (nq .ne. 0) then
        call jedetr('&&RC3200.SIGMQ')
        call jedetr('&&RC3200.SITHQ')
        call jedetr('&&RC3200.SIPRQ')
        call jedetr('&&RC3200.SIMEQ')
        call jedetr('&&RC3200.PREMOYQ')
    endif
!
    call jedetr('&&RC3200.TEMPP')
    if (numsip .ne. numsiq) call jedetr('&&RC3200.TEMPQ')
    AS_DEALLOCATE(vr=temp)
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
