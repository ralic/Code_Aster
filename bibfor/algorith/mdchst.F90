subroutine mdchst(numddl, typnum, imode, iamor, pulsat,&
                  masgen, amogen, lflu, nbnli, nbpal,&
                  noecho, nbrfis, logcho, parcho, intitu,&
                  ddlcho, ier)
    implicit none
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/angvx.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvem.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/mdchan.h"
#include "asterfort/mdchdl.h"
#include "asterfort/mdchre.h"
#include "asterfort/posddl.h"
#include "asterfort/tbliva.h"
#include "asterfort/utmess.h"
#include "asterfort/utnono.h"
#include "asterfort/wkvect.h"
!
    integer :: nbnli, iamor, imode, ier, logcho(nbnli, *), ddlcho(*), nbrfis
    real(kind=8) :: parcho(nbnli, *), pulsat(*), masgen(*), amogen(*)
    logical :: lflu
    character(len=8) :: noecho(nbnli, *), intitu(*)
    character(len=14) :: numddl
    character(len=16) :: typnum, typfro
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     ROUTINE APPELEE PAR MDCHOC
!     TRAITEMENT DU CAS OU NUME_DDL = 'NUME_DDL_SDASTER'
!
! IN  : NUMDDL : NOM DE LA NUMEROTATION
! IN  : TYPNUM : TYPE DE LA NUMEROTATION
! IN  : IMODE  : NUMERO DU MODE DE MASSE LA PLUS ELEVEE
! IN  : IAMOR  : NUMERO DE L'AMORTISSEMENT ASSOCIE
! IN  : PULSAT : PULSATIONS DES MODES
! IN  : MASGEN : MASSES GENERALISEES DES MODES
! IN  : AMOGEN : MATRICE DES AMORTISSEMENTS GENERALISES
! IN  : LFLU   : LOGIQUE INDIQUANT LA PRESENCE DE LAME FLUIDE
! IN  : NBNLI  : DIMENSION DES TABLEAUX (NBCHOC+NBSISM+NBFLAM)
! OUT : NOECHO : NOEUD DE CHOC (VOIR MDCHOC)
! OUT : LOGCHO : LOGIQUE CHOC (VOIR MDCHOC)
! OUT : PARCHO : PARAMETRE DE CHOC (VOIR MDCHOC)
! OUT : INTITU : INTITULE DE CHOC
! OUT : DDLCHO : TABLEAU DES NUMEROTATIONS DES NOEUDS DE CHOC
! OUT : IER    : NIVEAU D'ERREUR
!     ------------------------------------------------------------------
!
    integer :: nbchoc, nbsism, nbflam, nbocc, i, j, ioc, ibid, il, jcoor, jmama
    integer :: nbnma, kma, nn1, nn2, ino1, ino2, ig, n1, namtan, iret, nmliai
    integer :: jmail, im, iliai, nmgr, ngrm, numai, irett, compt1, compt2
    integer :: nbmail, nbno, j1, j2, bono1, bono2
    real(kind=8) :: ktang, ctang, k, rap, xjeu, r8bid
    real(kind=8) :: alpha, beta, axe(3)
    complex(kind=8) :: cbid
    logical :: lnoue2, memail
    character(len=8) :: kbid, repere, mailla, nomno1, nomno2, k8typ
    character(len=10) :: motfac
    character(len=24) :: mdgene, refo, nomgr1, nomgr2, mamai
    character(len=24) :: valk(2)
!     ------------------------------------------------------------------
!  COUPLAGE EDYOS
!  =>
    integer :: ipat, ipal, nno, nddl, nddl1, nddl2
!     ANCIENS INCLUDE (CALCIUM.H)
!     ===========================
    character(len=3) :: comp(6)
!     =================================
    integer :: nbpal
!
    integer :: palmax
    parameter (palmax=20)
!
    integer :: dimnas
    parameter     (dimnas=8)
!
    integer :: iadrk
    character(len=24) :: cpal, cnpal(palmax)
    integer :: iarg
!
    call jemarq()
    call getfac('CHOC', nbchoc)
    call getfac('ANTI_SISM', nbsism)
    call getfac('FLAMBAGE', nbflam)
    nbocc = nbchoc + nbsism + nbflam
    mdgene = ' '
    call dismoi('F', 'NOM_MAILLA', numddl, 'NUME_DDL', ibid,&
                mailla, iret)
!
    call jeveuo(mailla//'.COORDO    .VALE', 'L', jcoor)
!
    do 10 il = 1, nbnli
        noecho(il,3) = numddl(1:8)
        noecho(il,4) = mailla
        noecho(il,7) = numddl(1:8)
        noecho(il,8) = mailla
10  end do
!
    iliai = 0
    motfac = 'CHOC'
    do 100 i = 1, nbocc
        ioc = i
        if (i .gt. nbchoc+nbsism) then
            motfac = 'FLAMBAGE'
            ioc = i-(nbchoc+nbsism)
        else if (i.gt.nbchoc) then
            motfac = 'ANTI_SISM'
            ioc = i-nbchoc
        endif
        lnoue2 = .false.
        nmliai = 0
!
        if (motfac .eq. 'CHOC') then
            call getvtx(motfac, 'MAILLE', iocc=ioc, nbval=0, nbret=ibid)
            if (ibid .ne. 0) then
                lnoue2 = .true.
                nmliai = -ibid
                call wkvect('&&MDCHST.MAILLE', 'V V K8', nmliai, jmail)
                call getvem(mailla, 'MAILLE', motfac, 'MAILLE', ioc,&
                            iarg, nmliai, zk8(jmail), ibid)
                do 110 im = 1, nmliai
                    mamai = zk8(jmail-1+im)
                    call jenonu(jexnom(mailla//'.NOMMAI', mamai), numai)
                    call jeveuo(jexnum(mailla//'.CONNEX', numai), 'L', jmama)
                    call jelira(jexnum(mailla//'.CONNEX', numai), 'LONMAX', nbnma)
                    if (nbnma .ne. 2) then
                        valk (1) = mamai
                        valk (2) = 'SEG2'
                        call utmess('F', 'ALGORITH13_39', nk=2, valk=valk)
                    endif
                    iliai = iliai + 1
                    call jenuno(jexnum(mailla//'.NOMNOE', zi(jmama)), noecho(iliai, 1))
                    call jenuno(jexnum(mailla//'.NOMNOE', zi(jmama+1)), noecho(iliai, 5))
                    call mdchdl(nbnli, noecho, lnoue2, iliai, ddlcho,&
                                ier)
110              continue
                call jedetr('&&MDCHST.MAILLE')
                goto 102
            endif
!
            call getvtx(motfac, 'GROUP_MA', iocc=ioc, nbval=0, nbret=ibid)
            if (ibid .ne. 0) then
                lnoue2 = .true.
                nmliai = 0
                ngrm = -ibid
                call wkvect('&&MDCHST.GROUP_MA', 'V V K24', ngrm, jmail)
                call getvem(mailla, 'GROUP_MA', motfac, 'GROUP_MA', ioc,&
                            iarg, ngrm, zk24(jmail), ibid)
                do 120 ig = 1, ngrm
                    mamai = zk24(jmail-1+ig)
                    call jelira(jexnom(mailla//'.GROUPEMA', mamai), 'LONMAX', nmgr)
                    call jeveuo(jexnom(mailla//'.GROUPEMA', mamai), 'L', kma)
                    nmliai = nmliai + nmgr
                    do 122 im = 1, nmgr
                        numai = zi(kma-1+im)
                        call jeveuo(jexnum(mailla//'.CONNEX', numai), 'L', jmama)
                        call jelira(jexnum(mailla//'.CONNEX', numai), 'LONMAX', nbnma)
                        if (nbnma .ne. 2) then
                            call jenuno(jexnum(mailla//'.NOMMAI', numai), kbid)
                            valk (1) = kbid
                            valk (2) = 'SEG2'
                            call utmess('F', 'ALGORITH13_39', nk=2, valk=valk)
                        endif
                        iliai = iliai + 1
                        call jenuno(jexnum(mailla//'.NOMNOE', zi(jmama) ), noecho(iliai, 1))
                        call jenuno(jexnum(mailla//'.NOMNOE', zi(jmama+ 1)), noecho(iliai, 5))
                        call mdchdl(nbnli, noecho, lnoue2, iliai, ddlcho,&
                                    ier)
122                  continue
120              continue
                call jedetr('&&MDCHST.GROUP_MA')
                goto 102
            endif
        endif
!
        call getvem(mailla, 'NOEUD', motfac, 'NOEUD_1', ioc,&
                    iarg, 1, nomno1, ibid)
        if (ibid .ne. 0) then
            iliai = iliai + 1
            noecho(iliai,1) = nomno1
            call getvem(mailla, 'NOEUD', motfac, 'NOEUD_2', ioc,&
                        iarg, 1, nomno2, nn1)
            if (nn1 .ne. 0) then
                noecho(iliai,5) = nomno2
                lnoue2 = .true.
            else
                call getvtx(motfac, 'GROUP_NO_2', iocc=ioc, scal=nomgr2, nbret=nn2)
                if (nn2 .ne. 0) then
                    call utnono(' ', mailla, 'NOEUD', nomgr2, nomno2,&
                                iret)
                    if (iret .eq. 10) then
                        call utmess('F', 'ELEMENTS_67', sk=nomgr2)
                    else if (iret.eq.1) then
                        valk (1) = nomgr2
                        valk (2) = nomno2
                        call utmess('A', 'ALGORITH13_41', nk=2, valk=valk)
                    endif
                    noecho(iliai,5) = nomno2
                    lnoue2 = .true.
                else
                    noecho(iliai,5) = nomno1
                endif
            endif
            call mdchdl(nbnli, noecho, lnoue2, iliai, ddlcho,&
                        ier)
            goto 102
        endif
!
        call getvem(mailla, 'GROUP_NO', motfac, 'GROUP_NO_1', ioc,&
                    iarg, 1, nomgr1, ibid)
        call utnono(' ', mailla, 'NOEUD', nomgr1, nomno1,&
                    iret)
        if (iret .eq. 10) then
            call utmess('F', 'ELEMENTS_67', sk=nomgr1)
        else if (iret.eq.1) then
            valk (1) = nomgr1
            valk (2) = nomno1
            call utmess('A', 'ALGORITH13_41', nk=2, valk=valk)
        endif
        iliai = iliai + 1
        noecho(iliai,1) = nomno1
        call getvem(mailla, 'NOEUD', motfac, 'NOEUD_2', ioc,&
                    iarg, 1, nomno2, nn1)
        if (nn1 .ne. 0) then
            noecho(iliai,5) = nomno2
            lnoue2 = .true.
        else
            call getvtx(motfac, 'GROUP_NO_2', iocc=ioc, scal=nomgr2, nbret=nn2)
            if (nn2 .ne. 0) then
                call utnono(' ', mailla, 'NOEUD', nomgr2, nomno2,&
                            iret)
                if (iret .eq. 10) then
                    call utmess('F', 'ELEMENTS_67', sk=nomgr2)
                else if (iret.eq.1) then
                    valk (1) = nomgr2
                    valk (2) = nomno2
                    call utmess('A', 'ALGORITH13_41', nk=2, valk=valk)
                endif
                noecho(iliai,5) = nomno2
                lnoue2 = .true.
            else
                noecho(iliai,5) = nomno1
            endif
        endif
        call mdchdl(nbnli, noecho, lnoue2, iliai, ddlcho,&
                    ier)
102      continue
!
        iliai = iliai - max(1,nmliai)
        do 130 il = 1, max(1, nmliai)
            iliai = iliai + 1
            call jenonu(jexnom(mailla//'.NOMNOE', noecho(iliai, 1)), ino1)
            call jenonu(jexnom(mailla//'.NOMNOE', noecho(iliai, 5)), ino2)
            do 132 j = 1, 3
                parcho(iliai,7+j) = zr(jcoor+3*(ino1-1)+j-1)
                parcho(iliai,10+j) = zr(jcoor+3*(ino2-1)+j-1)
132          continue
!
            ktang = 0.d0
            ctang = 0.d0
            namtan = 0
            if (motfac .eq. 'CHOC') then
                call getvtx(motfac, 'INTITULE', iocc=ioc, scal=intitu( iliai), nbret=n1)
                call getvr8(motfac, 'JEU', iocc=ioc, scal=parcho(iliai, 1), nbret=n1)
                call getvr8(motfac, 'DIST_1', iocc=ioc, scal=parcho(iliai, 30), nbret=n1)
                call getvr8(motfac, 'DIST_2', iocc=ioc, scal=parcho(iliai, 31), nbret=n1)
                call getvr8(motfac, 'RIGI_NOR', iocc=ioc, scal=parcho( iliai, 2), nbret=n1)
                call getvr8(motfac, 'AMOR_NOR', iocc=ioc, scal=parcho( iliai, 3), nbret=n1)
                call getvr8(motfac, 'RIGI_TAN', iocc=ioc, scal=ktang, nbret=n1)
                call getvtx(motfac, 'FROTTEMENT', iocc=ioc, scal=typfro, nbret=n1)
                if (typfro .eq. 'COULOMB         ') then
                    call getvr8(motfac, 'COULOMB', iocc=ioc, scal=parcho( iliai, 6), nbret=n1)
                    call getvr8(motfac, 'COULOMB', iocc=ioc, scal=parcho( iliai, 7), nbret=n1)
                else if (typfro .eq. 'COULOMB_STAT_DYN') then
                    call getvr8(motfac, 'COULOMB_DYNA', iocc=ioc, scal=parcho(iliai, 6),&
                                nbret=n1)
                    call getvr8(motfac, 'COULOMB_STAT', iocc=ioc, scal=parcho(iliai, 7),&
                                nbret=n1)
                endif
                call getvr8(motfac, 'AMOR_TAN', iocc=ioc, scal=ctang, nbret=namtan)
                call getvtx(motfac, 'LAME_FLUIDE', iocc=ioc, scal=kbid, nbret=n1)
                if (kbid(1:3) .eq. 'OUI') then
                    lflu=.true.
                    logcho(iliai,2)=1
                    call getvr8('CHOC', 'ALPHA   ', iocc=ioc, scal=parcho(iliai, 32), nbret=n1)
                    call getvr8('CHOC', 'BETA    ', iocc=ioc, scal=parcho(iliai, 33), nbret=n1)
                    call getvr8('CHOC', 'CHI     ', iocc=ioc, scal=parcho(iliai, 34), nbret=n1)
                    call getvr8('CHOC', 'DELTA   ', iocc=ioc, scal=parcho(iliai, 35), nbret=n1)
                endif
                call getvid(motfac, 'OBSTACLE', iocc=ioc, scal=noecho( iliai, 9), nbret=n1)
                call tbliva(noecho(iliai, 9), 1, 'LIEU', ibid, r8bid,&
                            cbid, 'DEFIOBST', kbid, r8bid, 'TYPE',&
                            k8typ, ibid, r8bid, cbid, refo,&
                            irett)
                ASSERT(irett.eq.0)
                if (refo(1:9) .eq. 'BI_PLAN_Y') then
                    noecho(iliai,9) = 'BI_PLANY'
                else if (refo(1:9).eq.'BI_PLAN_Z') then
                    noecho(iliai,9) = 'BI_PLANZ'
                else if (refo(1:11).eq.'BI_CERC_INT') then
                    noecho(iliai,9) = 'BI_CERCI'
                else if (refo(1:7).ne.'DISCRET') then
                    noecho(iliai,9) = refo(1:8)
                endif
                if (noecho(iliai,9) .eq. 'BI_CERCI' .and. parcho(iliai, 31) .lt.&
                    parcho(iliai,30)) then
                    call utmess('F', 'ALGORITH5_35')
                endif
            else if (motfac.eq.'FLAMBAGE') then
                intitu(i) = noecho(iliai,1)
                call getvr8(motfac, 'JEU', iocc=ioc, scal=parcho(iliai, 1), nbret=n1)
                call getvr8(motfac, 'DIST_1', iocc=ioc, scal=parcho(iliai, 30), nbret=n1)
                call getvr8(motfac, 'DIST_2', iocc=ioc, scal=parcho(iliai, 31), nbret=n1)
                call getvr8(motfac, 'RIGI_NOR', iocc=ioc, scal=parcho( iliai, 2), nbret=n1)
                call getvr8(motfac, 'FNOR_CRIT', iocc=ioc, scal=parcho( iliai, 50), nbret=n1)
                call getvr8(motfac, 'FNOR_POST_FL', iocc=ioc, scal=parcho( iliai, 51), nbret=n1)
                call getvr8(motfac, 'RIGI_NOR_POST_FL', iocc=ioc, scal=parcho(iliai, 52),&
                            nbret=n1)
                logcho(iliai,5) = 1
                if (parcho(iliai,2 ) .le. 0.d0 .or. parcho(iliai,52) .le. 0.d0) then
                    call utmess('F', 'ALGORITH5_40')
                else
                    rap=parcho(iliai,50)/parcho(iliai,2)-parcho(iliai,&
                    51)/ parcho(iliai,52)
                    if (rap .lt. 0.d0) then
                        call utmess('F', 'ALGORITH5_41')
                    endif
                endif
                call getvid(motfac, 'OBSTACLE', iocc=ioc, scal=noecho( iliai, 9), nbret=n1)
                call tbliva(noecho(iliai, 9), 1, 'LIEU', ibid, r8bid,&
                            cbid, 'DEFIOBST', kbid, r8bid, 'TYPE',&
                            k8typ, ibid, r8bid, cbid, refo,&
                            irett)
                ASSERT(irett.eq.0)
                if (refo(1:9) .eq. 'BI_PLAN_Y') then
                    noecho(iliai,9) = 'BI_PLANY'
                else if (refo(1:9).eq.'BI_PLAN_Z') then
                    noecho(iliai,9) = 'BI_PLANZ'
                else if (refo(1:11).eq.'BI_CERC_INT') then
                    noecho(iliai,9) = 'BI_CERCI'
                else if (refo(1:7).ne.'DISCRET') then
                    noecho(iliai,9) = refo(1:8)
                endif
                if (noecho(iliai,9) .eq. 'BI_CERCI' .and. parcho(iliai, 31) .lt.&
                    parcho(iliai,30)) then
                    call utmess('F', 'ALGORITH5_35')
                endif
!
            else if (motfac.eq.'ANTI_SISM') then
                intitu(iliai) = noecho(iliai,1)
                call getvr8(motfac, 'RIGI_K1   ', iocc=ioc, scal=parcho(iliai, 39), nbret=n1)
                call getvr8(motfac, 'RIGI_K2   ', iocc=ioc, scal=parcho(iliai, 40), nbret=n1)
                call getvr8(motfac, 'SEUIL_FX  ', iocc=ioc, scal=parcho(iliai, 41), nbret=n1)
                call getvr8(motfac, 'C         ', iocc=ioc, scal=parcho(iliai, 42), nbret=n1)
                call getvr8(motfac, 'PUIS_ALPHA', iocc=ioc, scal=parcho( iliai, 43), nbret=n1)
                call getvr8(motfac, 'DX_MAX    ', iocc=ioc, scal=parcho(iliai, 44), nbret=n1)
                logcho(iliai,4)=1
                noecho(iliai,9) = 'BI_PLANY'
            endif
! --------- SI AMOR_TAN NON RENSEIGNE ON LUI AFFECTE UNE VAL OPTIMISEE
            if (namtan .eq. 0 .and. ktang .ne. 0.d0) then
                k = pulsat(imode)**2 * masgen(imode)
                ctang = 2.d0*sqrt(&
                        masgen(imode)*(k+ktang) ) - 2.d0* amogen(iamor)*sqrt( k*masgen(imode))
                call utmess('I', 'ALGORITH16_10', si=i, sr=ctang)
            endif
            parcho(iliai,4) = ktang
            parcho(iliai,5) = ctang
!
            if (noecho(iliai,9)(1:2) .eq. 'BI') then
                xjeu = (&
                       parcho(iliai,11)-parcho(iliai,8))**2 + (parcho(iliai,12)-parcho(iliai,9))*&
                       &*2 + (parcho(iliai, 13)-parcho(iliai,10)&
                       )**2
            endif
!
            call mdchre(motfac, ioc, iliai, mdgene, typnum,&
                        repere, nbnli, parcho, lnoue2)
!
            call mdchan(motfac, ioc, iliai, mdgene, typnum,&
                        repere, xjeu, nbnli, noecho, parcho)
!
130      continue
!
100  end do
!    COUPLAGE EDYOS
    if (nbpal .gt. 0) then
        cpal = 'C_PAL'
        comp(1)='DX'
        comp(2)='DY'
        comp(3)='DZ'
        comp(4)='DRX'
        comp(5)='DRY'
        comp(6)='DRZ'
        call jeveuo(cpal, 'L', iadrk)
        do 21 ipal = 1, nbpal
            noecho(ipal,1)=zk8(iadrk+(ipal-1)+2*palmax)(1:dimnas)
            noecho(ipal,5)=noecho(ipal,1)
            cnpal(ipal)=zk8(iadrk+(ipal-1)+2*palmax)(1:dimnas)
21      continue
        do 22 ipal = 1, nbpal
            call utnono(' ', mailla, 'NOEUD', cnpal(ipal), nomno1,&
                        iret)
            if (iret .eq. 10) then
!            CALL U2MESK('F','ELEMENTS_67',1,NOMGR2)
                nomno1 = cnpal(ipal)(1:8)
            else if (iret.eq.1) then
                valk (1) = cnpal(ipal)
                valk (2) = nomno1
                call utmess('A', 'ALGORITH13_41', nk=2, valk=valk)
            endif
            do 23 ipat = 1, 6
                call posddl('NUME_DDL', numddl, nomno1, comp(ipat), nno,&
                            nddl)
                ddlcho(6*(ipal-1)+ipat) = nddl
23          continue
22      continue
    endif
! FIN PALIERS EDYOS
!
!    ROTOR FISSURE
    motfac='ROTOR_FISS'
    comp(1)='DRX'
    comp(2)='DRY'
    comp(3)='DRZ'
    if (nbrfis .gt. 0) then
        do 61 i = 1, nbrfis
            iliai = iliai + 1
            call getvem(mailla, 'NOEUD', motfac, 'NOEUD_D', i,&
                        iarg, 1, nomno1, ibid)
            call getvem(mailla, 'NOEUD', motfac, 'NOEUD_G', i,&
                        iarg, 1, nomno2, ibid)
!
!
            call getvem(mailla, 'GROUP_NO', motfac, 'GROUP_NO_D', i,&
                        iarg, 1, nomgr1, ibid)
            if (ibid .ne. 0) then
                call utnono(' ', mailla, 'NOEUD', nomgr1, nomno1,&
                            iret)
                if (iret .eq. 10) then
                    call utmess('F', 'ELEMENTS_67', sk=nomgr1)
                else if (iret.eq.1) then
                    valk (1) = nomgr1
                    valk (2) = nomno1
                    call utmess('A', 'ALGORITH13_41', nk=2, valk=valk)
                endif
            endif
!
            call getvem(mailla, 'GROUP_NO', motfac, 'GROUP_NO_G', i,&
                        iarg, 1, nomgr2, ibid)
            if (ibid .ne. 0) then
                call utnono(' ', mailla, 'NOEUD', nomgr2, nomno2,&
                            iret)
                if (iret .eq. 10) then
                    call utmess('F', 'ELEMENTS_67', sk=nomgr2)
                else if (iret.eq.1) then
                    valk (1) = nomgr2
                    valk (2) = nomno2
                    call utmess('A', 'ALGORITH13_41', nk=2, valk=valk)
                endif
            endif
!
            do 63 ipat = 1, 3
                call posddl('NUME_DDL', numddl, nomno1, comp(ipat), nn1,&
                            nddl1)
                call posddl('NUME_DDL', numddl, nomno2, comp(ipat), nn2,&
                            nddl2)
                ddlcho(iliai-1+6*(i-1)+ipat) = nddl1
                ddlcho(iliai-1+6*(i-1)+ipat+3) = nddl2
63          continue
!
!
! DETERMINATION DES DIRECTION ET ORIENTATION DU ROTOR
            compt1=0
            compt2=0
            call jelira(mailla//'.CONNEX', 'NMAXOC', nbmail)
            do 66 numai = 1, nbmail
                call jelira(jexnum(mailla//'.CONNEX', numai), 'LONMAX', nbno)
                if ((nbno.gt.1) .and. (nbno.lt.4)) then
                    call jeveuo(jexnum(mailla//'.CONNEX', numai), 'L', ibid)
                    do 77 j1 = 1, nbno
                        if (zi(ibid+j1-1) .eq. nn1) then
                            memail=.false.
                            do 78 j2 = 1, nbno
                                if (zi(ibid+j2-1) .eq. nn2) memail= .true.
78                          continue
                            if (.not.memail) then
                                compt1=compt1+1
                                if (j1 .eq. 1) bono1=zi(ibid+1)
                                if (j1 .eq. 2) bono1=zi(ibid)
                            endif
                        endif
                        if (zi(ibid+j1-1) .eq. nn2) then
                            memail=.false.
                            do 79 j2 = 1, nbno
                                if (zi(ibid+j2-1) .eq. nn1) memail= .true.
79                          continue
                            if (.not.memail) then
                                compt2=compt2+1
                                if (j1 .eq. 1) bono2=zi(ibid+1)
                                if (j1 .eq. 2) bono2=zi(ibid)
                            endif
                        endif
77                  continue
                endif
66          continue
            ASSERT(compt1 .ge. 1)
            ASSERT(compt2 .ge. 1)
!
            do 89 j = 1, 3
                axe(j)=zr(jcoor+3*(bono1-1)+j-1) - zr(jcoor+3*(bono2-&
                1)+j-1)
89          continue
!
! ORIENTATION DU ROTOR
            call angvx(axe, alpha, beta)
            parcho(iliai,17) = sin(alpha)
            parcho(iliai,18) = cos(alpha)
            parcho(iliai,19) = sin(beta)
            parcho(iliai,20) = cos(beta)
!
61      end do
    endif
!
! FIN ROTOR FISSURE
!
    call jedema()
end subroutine
