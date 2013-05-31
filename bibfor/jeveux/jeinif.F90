subroutine jeinif(sti, sto, nomf, clas, nrep,&
                  nbloc, lbloc)
! person_in_charge: j-pierre.lefebvre at edf.fr
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
! aslint: disable=W1501
    implicit none
    include 'jeveux_private.h'
    include 'asterc/gtopti.h'
    include 'asterc/gtoptk.h'
    include 'asterfort/assert.h'
    include 'asterfort/codent.h'
    include 'asterfort/jjalls.h'
    include 'asterfort/jjcren.h'
    include 'asterfort/jjecrs.h'
    include 'asterfort/jjprem.h'
    include 'asterfort/jxecro.h'
    include 'asterfort/jxlibd.h'
    include 'asterfort/jxlir1.h'
    include 'asterfort/jxliro.h'
    include 'asterfort/jxouvr.h'
    include 'asterfort/lxmins.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mesk.h'
    integer :: nrep, nbloc, lbloc
    character(len=*) :: sti, sto, nomf, clas
! ----------------------------------------------------------------------
! ROUTINE UTILISATEUR D'OUVERTURE D'UNE CLASSE
!
! IN  STI    : STATUT EN DEBUT DE TRAVAIL ('DUMMY','DEBUT','POURSUIT')
! IN  STO    : STATUT EN FIN DE TRAVAIL ('SAUVE','DETRUIT')
! IN  NOMF   : NOM LOCALE DE LA BASE
! IN  CLAS   : NOM LOCALE DE LA CLASSE
! IN  NREP   : LONGUEUR DU REPERTOIRE
! IN  NBLOC  : NOMBRE D'ENREGISTREMMENTS DU FICHIER D'ACCES DIRECT
!              SI NBLOC = 0,  ON LE DETERMINE A PARTIR DE MFIC
! IN  LBLOC  : LONGUEUR DES ENREGISTREMMENTS DU FICHIER D'ACCES DIRECT
!              CETTE LONGUEUR EST DONNEE EN KILO (1024) MOT (ENTIER)
!
! ----------------------------------------------------------------------
    integer :: iclas, iclaos, iclaco, idatos, idatco, idatoc
    common /iatcje/  iclas ,iclaos , iclaco , idatos , idatco , idatoc
!
    character(len=24) :: nomco
    character(len=32) :: nomuti, nomos, nomoc, bl32
    common /nomcje/  nomuti , nomos , nomco , nomoc , bl32
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, iadrs, ic, icre, ipgca
    integer :: iret, jcara, jdate, jdocu, jgenr, jhcod
    integer :: jiacce, jiadd, jiadm, jindir, jlong, jlono, jltyp
    integer :: jluti, jmarq, jorig, jrnom, jtype, jusadi, k
    integer :: l, lcarao, ldynol, lloc, lmarq, lon, lon1
    integer :: lon2, n, nbacce, nbext, nbgros, nblim, nblma1
    integer :: nblma2, nbloco, nbpeti, nrepo
!-----------------------------------------------------------------------
    parameter  ( n = 5 )
    common /jiatje/  jltyp(n), jlong(n), jdate(n), jiadd(n), jiadm(n),&
     &                 jlono(n), jhcod(n), jcara(n), jluti(n), jmarq(n)
!
    common /jkatje/  jgenr(n), jtype(n), jdocu(n), jorig(n), jrnom(n)
    common /jiacce/  jiacce(n),nbacce(2*n)
    common /jusadi/  jusadi(n)
    common /jindir/  jindir(n)
    common /inbdet/  nblim(n),nbgros(n),nbpeti(n)
! ----------------------------------------------------------------------
    integer :: nblmax, nbluti, longbl, kitlec, kitecr, kiadm, iitlec, iitecr
    integer :: nitecr, kmarq
    common /ificje/  nblmax(n) , nbluti(n) , longbl(n) ,&
     &                 kitlec(n) , kitecr(n) ,             kiadm(n) ,&
     &                 iitlec(n) , iitecr(n) , nitecr(n) , kmarq(n)
!
    integer :: nrhcod, nremax, nreuti
    common /icodje/  nrhcod(n) , nremax(n) , nreuti(n)
!
    character(len=2) :: dn2
    character(len=5) :: classe
    character(len=8) :: nomfic, kstout, kstini
    common /kficje/  classe    , nomfic(n) , kstout(n) , kstini(n) ,&
     &                 dn2(n)
    character(len=8) :: nombas
    common /kbasje/  nombas(n)
    integer :: idn, iext, nbenrg
    common /iextje/  idn(n) , iext(n) , nbenrg(n)
    integer :: nbcla
    common /nficje/  nbcla
! ----------------------------------------------------------------------
    integer :: igenr(1), itype(1), idocu(1), iorig(1), irnom(4)
    equivalence      (igenr,genr),(itype,type),&
     &                 (idocu,docu),(iorig,orig),(irnom,rnom)
    integer :: lbis, lois, lols, lor8, loc8
    common /ienvje/  lbis , lois , lols , lor8 , loc8
    integer :: lfic, mfic
    common /fenvje/  lfic,mfic
!
    integer :: ipgc, kdesma(2), lgd, lgduti, kposma(2), lgp, lgputi
    common /iadmje/  ipgc,kdesma,   lgd,lgduti,kposma,   lgp,lgputi
    integer :: ldyn, lgdyn, nbdyn, nbfree
    common /idynje/  ldyn , lgdyn , nbdyn , nbfree
! ----------------------------------------------------------------------
    character(len=1) :: kclas
    character(len=4) :: z
    parameter      ( z = 'INIT' )
    character(len=8) :: knom, knomf, kstin, kstou, cversb, cversu
    character(len=24) :: valk(3)
    integer :: ncar, itlec(1), itecr(1), iadadd(2), lgbl
    integer :: vali(7), irt, ind
    real(kind=8) :: valr(1)
    parameter      ( ncar = 11 )
! ----------------------------------------------------------------------
    logical :: lenrg
    integer :: lidbas, lideff
    parameter      ( lidbas = 20 , lideff = 15 )
    character(len=8) :: cidbas(lidbas)
    integer :: kat(lidbas), lso(lidbas), kdy(lidbas)
    data cidbas  / '$$CARA  ' , '$$IADD  ' , '$$GENR  ' , '$$TYPE  ' ,&
     &               '$$DOCU  ' , '$$ORIG  ' , '$$RNOM  ' , '$$LTYP  ' ,&
     &               '$$LONG  ' , '$$LONO  ' , '$$DATE  ' , '$$LUTI  ' ,&
     &               '$$HCOD  ' , '$$USADI ' , '$$ACCE  ' , '$$MARQ  ' ,&
     &               '$$INDI  ' , '$$TLEC  ' , '$$TECR  ' , '$$IADM  ' /
! DEB ------------------------------------------------------------------
    ipgca = ipgc
    ipgc = -2
    irt = 0
!
    kclas = clas
    kstin = sti
    kstou = sto
    knom = nomf
    call lxmins(nomf)
    knomf = nomf
!
    call assert(knomf .ne. '        ' .and. len(nomf) .le. 8)
    call assert(kclas .ne. ' ')
    call assert(index (classe, kclas) .eq. 0)
!
    call assert(kstin .eq. 'DEBUT   ' .or. kstin .eq. 'POURSUIT' .or. kstin .eq. 'DUMMY   ')
    call assert(kstou .eq. 'SAUVE   ' .or. kstou .eq. 'DETRUIT ')
    call assert(kstin .ne. 'DUMMY   ' .or. kstou .ne. 'SAUVE   ')
    call assert(nrep .gt. 0)
    call assert(lbloc .gt. 0)
!
    ic = index (classe , ' ')
    call assert(ic .gt. 0)
    nomfic(ic) = knomf
    nombas(ic) = knom
    kstini(ic) = kstin
    kstout(ic) = kstou
    classe(ic:ic) = kclas
    nbcla = index( classe , '$' ) - 1
    if (nbcla .eq. -1) nbcla = n
!
    iclas = ic
    nbgros(ic) = 0
    nbpeti(ic) = 0
    nomuti = ' '
    nomos = '$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'
    nomco = '$$$$$$$$$$$$$$$$$$$$$$$$'
    nomoc = '$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'
!
! --- ON INTERDIT L'APPEL A JJLDYN LORS DE L'ALLOCATION
! --- DYNAMIQUE  (ET LES APPELS RECURSIFS)
!
    ldynol = ldyn
    if (ldyn .eq. 1) then
        ldyn = 2
    endif
!
    if (kstin .eq. 'DEBUT   ' .or. kstin .eq. 'DUMMY   ') then
        nremax(ic) = nrep
        nreuti(ic) = 0
        nrhcod(ic) = jjprem(nremax(ic),irt)
        if (irt .eq. 1) then
            if (ic .eq. 1) then
                call u2mesg('A', 'JEVEUX_64', 1, nombas(ic), 1,&
                            nremax(ic), 0, valr)
            else
                call u2mesg('A', 'JEVEUX_65', 1, nombas(ic), 1,&
                            nremax(ic), 0, valr)
            endif
        endif
        nbluti(ic) = 0
        longbl(ic) = lbloc
        if (nbloc .eq. 0) then
            nblmax(ic) = mfic/(longbl(ic)*lois)
        else
            nblmax(ic) = min ( nbloc , mfic/(longbl(ic)*lois) )
        endif
        nblim(ic) = 500
!
        lmarq = 2 * nrep * lois
        call jjalls(lmarq, ic, 'V', 'I', lois,&
                    z, imarq, iadrs, kmarq(ic), kdy(16))
        kat(16) = kmarq(ic)
        jmarq(ic) = iadrs - 1
        call jjecrs(kat(16), ic, 16, 0, 'E',&
                    imarq(jmarq(ic)+2*16-1))
!
        lcarao = ncar * lois
        call jjalls(lcarao, ic, 'V', 'I', lois,&
                    z, cara, iadrs, kat(1), kdy( 1))
        jcara(ic) = iadrs
        call jjecrs(kat(1), ic, 1, 0, 'E',&
                    imarq(jmarq(ic)+1))
!
        nbenrg(ic) = min(lfic/(longbl(ic)*lois),nblmax(ic))
!
        nbloco = nblmax(ic) * lois
        call jjalls(nbloco, ic, 'V', 'I', lois,&
                    z, iacce, iadrs, kat(15), kdy(15))
        jiacce(ic) = iadrs - 1
        call jjecrs(kat(15), ic, 15, 0, 'E',&
                    imarq(jmarq(ic)+2*15-1))
!
        lgbl = nremax(ic)*lois
        call jjalls(lgbl, ic, 'V', 'I', lois,&
                    z, indir, iadrs, kat(17), kdy( 17))
        jindir(ic) = iadrs - 1
        call jjecrs(kat(17), ic, 17, 0, 'E',&
                    imarq(jmarq(ic)+2*17-1))
        do 345 ind = 1, nremax(ic)
            indir(jindir(ic)+ind) = ind
345      continue
!
        lgbl = 1024*longbl(ic)*lois
        call jjalls(lgbl, ic, 'V', 'I', lois,&
                    z, itlec, iadrs, kitlec(ic), kdy(18))
        kat(18) = kitlec(ic)
        kitlec(ic) = ( kitlec(ic) - 1 ) * lois
        call jjecrs(kat(18), ic, 18, 0, 'E',&
                    imarq(jmarq(ic)+2*18-1))
!
        call jjalls(lgbl, ic, 'V', 'I', lois,&
                    z, itecr, iadrs, kitecr(ic), kdy(19))
        kat(19) = kitecr(ic)
        kitecr(ic) = ( kitecr(ic) - 1 ) * lois
        call jjecrs(kat(19), ic, 19, 0, 'E',&
                    imarq(jmarq(ic)+2*19-1))
!
        nrepo = nrep * lois
        call jjalls(2*nrepo, ic, 'V', 'I', lois,&
                    z, iadm, iadrs, kiadm(ic), kdy(20))
        kat(20) = kiadm(ic)
        jiadm(ic) = iadrs - 1
        call jjecrs(kat(20), ic, 20, 0, 'E',&
                    imarq(jmarq(ic)+2*20-1))
!
! ----- OPEN DU FICHIER
!
        if (kstin .ne. 'DUMMY   ') then
            call jxouvr(ic, 1)
            iext(ic) = 1
        endif
!
! ----- ECRITURE DANS L'OBJET CARA
! ----- NREMAX NREUTI NRHCOD NBLMAX NBLUTI
!
        cara(jcara(ic) ) = nremax(ic)
        cara(jcara(ic) +1 ) = nreuti(ic)
        cara(jcara(ic) +2 ) = nrhcod(ic)
        cara(jcara(ic) +3 ) = nblmax(ic)
        cara(jcara(ic) +4 ) = nbluti(ic)
        cara(jcara(ic) +5 ) = longbl(ic)
        call gtopti('versMAJ', cara(jcara(ic) +8 ), iret)
        call gtopti('versMIN', cara(jcara(ic) +9 ), iret)
        call gtopti('versSUB', cara(jcara(ic) +10), iret)
        lon = 2 * nremax(ic) * lois
        call jjalls(lon, ic, 'V', 'I', lois,&
                    z, iadd, iadrs, kat( 2), kdy(2))
        jiadd(ic) = iadrs - 1
        call jjecrs(kat(2), ic, 2, 0, 'E',&
                    imarq(jmarq(ic)+2*2-1))
        lon = nremax(ic) * len(genr(1))
        call jjalls(lon, ic, 'V', 'K', len(genr(1)),&
                    z, igenr, iadrs, kat(3), kdy(3))
        jgenr(ic) = iadrs - 1
        call jjecrs(kat(3), ic, 3, 0, 'E',&
                    imarq(jmarq(ic)+2*3-1))
        lon = nremax(ic) * len(type(1))
        call jjalls(lon, ic, 'V', 'K', len(type(1)),&
                    z, itype, iadrs, kat(4), kdy(4))
        jtype(ic) = iadrs - 1
        call jjecrs(kat(4), ic, 4, 0, 'E',&
                    imarq(jmarq(ic)+2*4-1))
        lon = nremax(ic) * len(docu(1))
        call jjalls(lon, ic, 'V', 'K', len(docu(1)),&
                    z, idocu, iadrs, kat(5), kdy(5))
        jdocu(ic) = iadrs - 1
        call jjecrs(kat(5), ic, 5, 0, 'E',&
                    imarq(jmarq(ic)+2*5-1))
        lon = nremax(ic) * len(orig(1))
        call jjalls(lon, ic, 'V', 'K', len(orig(1)),&
                    z, iorig, iadrs, kat(6), kdy(6))
        jorig(ic) = iadrs - 1
        call jjecrs(kat(6), ic, 6, 0, 'E',&
                    imarq(jmarq(ic)+2*6-1))
        lon = nremax(ic) * len(rnom(1))
        call jjalls(lon, ic, 'V', 'K', len(rnom(1)),&
                    z, irnom, iadrs, kat(7), kdy(7))
        jrnom(ic) = iadrs - 1
        call jjecrs(kat(7), ic, 7, 0, 'E',&
                    imarq(jmarq(ic)+2*7-1))
        do 30 ind = 1, nremax(ic)
            rnom(jrnom(ic) + ind ) = '?'
30      continue
        lon = nremax(ic) * lois
        call jjalls(lon, ic, 'V', 'I', lois,&
                    z, ltyp, iadrs, kat(8), kdy(8))
        jltyp(ic) = iadrs - 1
        call jjecrs(kat(8), ic, 8, 0, 'E',&
                    imarq(jmarq(ic)+2*8-1))
        call jjalls(lon, ic, 'V', 'I', lois,&
                    z, long, iadrs, kat(9), kdy(9))
        jlong(ic) = iadrs - 1
        call jjecrs(kat(9), ic, 9, 0, 'E',&
                    imarq(jmarq(ic)+2*9-1))
        call jjalls(lon, ic, 'V', 'I', lois,&
                    z, lono, iadrs, kat(10), kdy( 10))
        jlono(ic) = iadrs - 1
        call jjecrs(kat(10), ic, 10, 0, 'E',&
                    imarq(jmarq(ic)+2*10-1))
        call jjalls(lon, ic, 'V', 'I', lois,&
                    z, date, iadrs, kat(11), kdy( 11))
        jdate(ic) = iadrs - 1
        call jjecrs(kat(11), ic, 11, 0, 'E',&
                    imarq(jmarq(ic)+2*11-1))
        call jjalls(lon, ic, 'V', 'I', lois,&
                    z, luti, iadrs, kat(12), kdy( 12))
        jluti(ic) = iadrs - 1
        call jjecrs(kat(12), ic, 12, 0, 'E',&
                    imarq(jmarq(ic)+2*12-1))
        lon = nrhcod(ic) * lois
        call jjalls(lon, ic, 'V', 'I', lois,&
                    z, hcod, iadrs, kat(13), kdy( 13))
        jhcod(ic) = iadrs - 1
        call jjecrs(kat(13), ic, 13, 0, 'E',&
                    imarq(jmarq(ic)+2*13-1))
        lon = 3*nblmax(ic)*lois
        call jjalls(lon, ic, 'V', 'I', lois,&
                    z, iusadi, iadrs, kat(14), kdy( 14))
        do 123 l = 1, nblmax(ic)
            iusadi( iadrs + (3*l-2) - 1 ) = -1
            iusadi( iadrs + (3*l-1) - 1 ) = -1
            iusadi( iadrs + (3*l ) - 1 ) = 0
123      continue
        jusadi(ic) = iadrs - 1
        call jjecrs(kat(14), ic, 14, 0, 'E',&
                    imarq(jmarq(ic)+2*14-1))
        icre = 1
        ltyp(jltyp(ic)+3 ) = len(genr(1))
        ltyp(jltyp(ic)+4 ) = len(type(1))
        ltyp(jltyp(ic)+5 ) = len(docu(1))
        ltyp(jltyp(ic)+6 ) = len(orig(1))
        ltyp(jltyp(ic)+7 ) = len(rnom(1))
        do 5 i = 1, lidbas
            nomuti = '________'//nombas(ic)//'________'//cidbas(i)
            call jjcren(nomuti, icre, iret)
            genr(jgenr(ic)+i) = 'V'
            if ((i.ge.3 .and. i.le.7)) then
                type(jtype(ic)+i) = 'K'
            else
                type(jtype(ic)+i) = 'I'
                ltyp(jltyp(ic)+i) = lois
            endif
            if (i .eq. 1) then
                long(jlong(ic)+i) = ncar
                lono(jlono(ic)+i) = ncar
                iadd(jiadd(ic)+2*i-1) = 0
                iadd(jiadd(ic)+2*i ) = 0
                call jxecro(ic, kat(1), iadd(jiadd(ic)+2*i-1), lono( jlono(ic)+i)*lois, 0,&
                            1)
            else if (i.eq.2 .or. i.eq.16 .or. i.eq.20) then
                long(jlong(ic)+i) = 2*nremax(ic)
                lono(jlono(ic)+i) = 2*nremax(ic)
                lso(i) = 2*nremax(ic) * lois
            else if (i.eq.13) then
                long(jlong(ic)+i) = nrhcod(ic)
                lono(jlono(ic)+i) = nrhcod(ic)
                lso(i) = nrhcod(ic) * ltyp(jltyp(ic)+i)
            else if (i.eq.14) then
                long(jlong(ic)+i) = 3*nblmax(ic)
                lono(jlono(ic)+i) = 3*nblmax(ic)
                lso(i) = 3*nblmax(ic) * ltyp(jltyp(ic)+i)
            else if (i.eq.15) then
                long(jlong(ic)+i) = nblmax(ic)
                lono(jlono(ic)+i) = nblmax(ic)
                lso(i) = nblmax(ic) * ltyp(jltyp(ic)+i)
            else if (i.eq.18 .or. i.eq.19) then
                long(jlong(ic)+i) = 1024 * longbl(ic)
                lono(jlono(ic)+i) = 1024 * longbl(ic)
                lso(i) = 1024 * longbl(ic) * ltyp(jltyp(ic)+i)
            else
                long(jlong(ic)+i) = nremax(ic)
                lono(jlono(ic)+i) = nremax(ic)
                lloc = lono(jlono(ic)+i) * ltyp(jltyp(ic)+i)
                if (mod(lloc,lois) .ne. 0) then
                    lono(jlono(ic)+i) = ((1+lloc/lois)*lois)/ltyp( jltyp(ic)+i)
                endif
                lso(i) = lono(jlono(ic)+i) * ltyp(jltyp(ic)+i)
            endif
            iadm(jiadm(ic)+2*i-1) = kat(i)
            iadm(jiadm(ic)+2*i ) = kdy(i)
 5      continue
!
        do 10 i = 2, lideff
            iadd (jiadd(ic)+2*i-1) = 0
            iadd (jiadd(ic)+2*i ) = 0
            call jxecro(ic, kat(i), iadd(jiadd(ic)+2*i-1), lso(i), 0,&
                        i)
10      continue
        cara(jcara(ic)+6) = iadd(jiadd(ic) + 2*2-1 )
        cara(jcara(ic)+7) = iadd(jiadd(ic) + 2*2 )
    else
!
! ----- OPEN FICHIER
! ----- LECTURE DANS LE PREMIER BLOC DU FICHIER ET FERMETURE
!
        lcarao = ncar * lois
        call jjalls(lcarao, ic, 'V', 'I', lois,&
                    z, cara, iadrs, kat(1), kdy(1))
        jcara(ic) = iadrs
!
!  ---- L'ECRITURE DU STATUT ET DE L'ETAT SERA DE NOUVEAU EFFECTUEE
! ----- LORSQUE LES DIMENSIONS AURONT ETE RELUES
!
        call jjecrs(kat(1), ic, 1, 0, 'E',&
                    vali)
!
        call jxlir1(ic, cara(jcara(ic)))
        cversb = '  .  .  '
        call codent(cara(jcara(ic) + 8 ), 'D ', cversb(1:2))
        call codent(cara(jcara(ic) + 9 ), 'D0', cversb(4:5))
        call codent(cara(jcara(ic) + 10), 'D0', cversb(7:8))
        call gtoptk('versionD0', cversu, iret)
        nremax(ic) = cara(jcara(ic) )
        nreuti(ic) = cara(jcara(ic) + 1 )
        nrhcod(ic) = cara(jcara(ic) + 2 )
        nblmax(ic) = cara(jcara(ic) + 3 )
        nbluti(ic) = cara(jcara(ic) + 4 )
        longbl(ic) = cara(jcara(ic) + 5 )
        iadadd(1) = cara(jcara(ic) + 6 )
        iadadd(2) = cara(jcara(ic) + 7 )
        if (cversu .ne. cversb) then
            valk(1) = nombas(ic)
            valk(2) = cversb
            valk(3) = cversu
            call u2mesk('A', 'JEVEUX_08', 3, valk)
        endif
!
        if (nbloc .eq. 0) then
            nblma2 = mfic/(longbl(ic)*lois)
        else
            nblma2 = min ( nbloc , mfic/(longbl(ic)*lois) )
        endif
!
! ---- LORSQUE LE NOMBRE D'ENREGISTREMENTS MAXIMUM EST MODIFIE
!
        nblma1 = nblmax(ic)
        if (nblmax(ic) .ge. nblma2) then
            lenrg = .false.
            nblma2 = nblmax(ic)
        else
            vali(1) = nblmax(ic)
            vali(2) = nblma2
            valk(1) = nombas(ic)
            call u2mesg('I', 'JEVEUX_36', 1, valk, 2,&
                        vali, 0, valr)
            lenrg = .true.
        endif
!
        valk(1)= nombas(ic)
        valk(2)= cversb
        vali(1)= nbluti(ic)
        vali(2)= nblmax(ic)
        vali(3)= 1024*longbl(ic)*lois
        vali(4)= nreuti(ic)
        vali(5)= nremax(ic)
        vali(6)= (nreuti(ic)*100)/nremax(ic)
!
        call u2mesg('I', 'JEVEUX_21', 2, valk, 6,&
                    vali, 0, valr)
!
        nblmax(ic)= nblma2
!
        lmarq = 2 * nremax(ic) * lois
        call jjalls(lmarq, ic, 'V', 'I', lois,&
                    z, imarq, iadrs, kmarq(ic), kdy(16))
        kat(16) = kmarq(ic)
        jmarq(ic) = iadrs - 1
        call jjecrs(kat(16), ic, 16, 0, 'E',&
                    imarq(jmarq(ic)+2*16-1))
!
        call jjecrs(kat(1), ic, 1, 0, 'E',&
                    imarq(jmarq(ic)+2*1-1))
!
        nbenrg(ic) = min ( lfic/(longbl(ic)*lois) , nblma2 )
!
! ----- NOUVEL OPEN DE LA BASE
        nbext = (nbluti(ic)/nbenrg(ic))+1
        do 100 k = 0, nbext-1
            call jxouvr(ic, k+1)
100      continue
        iext(ic) = nbext
!
        lgbl = nremax(ic)*lois
        call jjalls(lgbl, ic, 'V', 'I', lois,&
                    z, indir, iadrs, kat(17), kdy( 17))
        jindir(ic) = iadrs - 1
        call jjecrs(kat(17), ic, 17, 0, 'E',&
                    imarq(jmarq(ic)+2*17-1))
        do 567 ind = 1, nremax(ic)
            indir(jindir(ic)+ind) = ind
567      continue
!
        lgbl = 1024*longbl(ic)*lois
        call jjalls(lgbl, ic, 'V', 'I', lois,&
                    z, itlec, iadrs, kitlec(ic), kdy(18))
        kat(18) = kitlec(ic)
        kitlec(ic) = ( kitlec(ic) - 1 ) * lois
        call jjecrs(kat(18), ic, 18, 0, 'E',&
                    imarq(jmarq(ic)+2*18-1))
        call jjalls(lgbl, ic, 'V', 'I', lois,&
                    z, itecr, iadrs, kitecr(ic), kdy(19))
        kat(19) = kitecr(ic)
        kitecr(ic) = ( kitecr(ic) - 1 ) * lois
        call jjecrs(kat(19), ic, 19, 0, 'E',&
                    imarq(jmarq(ic)+2*19-1))
        lon = nremax(ic) * lois
        call jjalls(2*lon, ic, 'V', 'I', lois,&
                    z, iadm, iadrs, kiadm(ic), kdy(20))
        kat(20) = kiadm(ic)
        jiadm(ic) = iadrs - 1
        call jjecrs(kat(20), ic, 20, 0, 'E',&
                    imarq(jmarq(ic)+2*20-1))
!
        lon2 = nblma2 * lois
        call jjalls(lon2, ic, 'V', 'I', lois,&
                    z, iacce, iadrs, kat(15), kdy( 15))
        jiacce(ic) = iadrs - 1
        call jjecrs(kat(15), ic, 15, 0, 'E',&
                    imarq(jmarq(ic)+2*15-1))
!
        call jjalls(2*lon, ic, 'V', 'I', lois,&
                    z, iadd, iadrs, kat(2), kdy( 2))
        jiadd(ic) = iadrs - 1
        call jjecrs(kat(2), ic, 2, 0, 'E',&
                    imarq(jmarq(ic)+2*2-1))
!
        call jxliro(ic, kat( 2), iadadd, 2*lon)
!
        lon2 = 3*nblma2 * lois
        call jjalls(lon2, ic, 'V', 'I', lois,&
                    z, iusadi, iadrs, kat(14), kdy( 14))
        do 231 l = 1, nblma2
            iusadi( iadrs + (3*l-2) - 1 ) = -1
            iusadi( iadrs + (3*l-1) - 1 ) = -1
            iusadi( iadrs + (3*l ) - 1 ) = 0
231      continue
        jusadi(ic) = iadrs - 1
        call jjecrs(kat(14), ic, 14, 0, 'E',&
                    imarq(jmarq(ic)+2*14-1))
        lon1 = 3*nblma1 * lois
        call jxliro(ic, kat(14), iadd(jiadd(ic)+2*14-1), lon1)
        if (lenrg) then
            call jxlibd(0, 14, ic, iadd(jiadd(ic)+2*14-1), lon1)
            iadd(jiadd(ic)+2*14-1) = 0
            iadd(jiadd(ic)+2*14 ) = 0
            call jxecro(ic, kat(14), iadd(jiadd(ic)+2*14-1), lon2, 0,&
                        14)
        endif
!
        lon1 = nblma1 * lois
        call jxliro(ic, kat(15), iadd(jiadd(ic)+2*15-1), lon1)
        if (lenrg) then
            call jxlibd(0, 15, ic, iadd(jiadd(ic)+2*15-1), lon1)
            iadd(jiadd(ic)+2*15-1) = 0
            iadd(jiadd(ic)+2*15 ) = 0
            call jxecro(ic, kat(15), iadd(jiadd(ic)+2*15-1), lon2, 0,&
                        15)
        endif
!
        lon = nremax(ic) * len(genr(1))
        call jjalls(lon, ic, 'V', 'K', len(genr(1)),&
                    z, igenr, iadrs, kat(3), kdy(3))
        jgenr(ic) = iadrs - 1
        call jjecrs(kat(3), ic, 3, 0, 'E',&
                    imarq(jmarq(ic)+2*3-1))
        call jxliro(ic, kat(3), iadd(jiadd(ic)+2*3-1), lon)
!
        lon = nremax(ic) * len(type(1))
        call jjalls(lon, ic, 'V', 'K', len(type(1)),&
                    z, itype, iadrs, kat(4), kdy(4))
        jtype(ic) = iadrs - 1
        call jjecrs(kat(4), ic, 4, 0, 'E',&
                    imarq(jmarq(ic)+2*4-1))
        call jxliro(ic, kat(4), iadd(jiadd(ic)+2*4-1), lon)
        lon = nremax(ic) * len(docu(1))
        call jjalls(lon, ic, 'V', 'K', len(docu(1)),&
                    z, idocu, iadrs, kat(5), kdy(5))
        jdocu(ic) = iadrs - 1
        call jjecrs(kat(5), ic, 5, 0, 'E',&
                    imarq(jmarq(ic)+2*5-1))
        call jxliro(ic, kat(5), iadd(jiadd(ic)+2*5-1), lon)
        lon = nremax(ic) * len(orig(1))
        call jjalls(lon, ic, 'V', 'K', len(orig(1)),&
                    z, iorig, iadrs, kat(6), kdy(6))
        jorig(ic) = iadrs - 1
        call jjecrs(kat(6), ic, 6, 0, 'E',&
                    imarq(jmarq(ic)+2*6-1))
        call jxliro(ic, kat(6), iadd(jiadd(ic)+2*6-1), lon)
        lon = nremax(ic) * len(rnom(1))
        call jjalls(lon, ic, 'V', 'K', len(rnom(1)),&
                    z, irnom, iadrs, kat(7), kdy(7))
        jrnom(ic) = iadrs - 1
        call jjecrs(kat(7), ic, 7, 0, 'E',&
                    imarq(jmarq(ic)+2*7-1))
        call jxliro(ic, kat(7), iadd(jiadd(ic)+2*7-1), lon)
        lon = nremax(ic) * lois
        call jjalls(lon, ic, 'V', 'I', lois,&
                    z, ltyp, iadrs, kat(8), kdy(8))
        jltyp(ic) = iadrs - 1
        call jjecrs(kat(8), ic, 8, 0, 'E',&
                    imarq(jmarq(ic)+2*8-1))
        call jxliro(ic, kat(8), iadd(jiadd(ic)+2*8-1), lon)
        call jjalls(lon, ic, 'V', 'I', lois,&
                    z, long, iadrs, kat(9), kdy(9))
        jlong(ic) = iadrs - 1
        call jjecrs(kat(9), ic, 9, 0, 'E',&
                    imarq(jmarq(ic)+2*9-1))
        call jxliro(ic, kat(9), iadd(jiadd(ic)+2*9-1), lon)
        call jjalls(lon, ic, 'V', 'I', lois,&
                    z, lono, iadrs, kat(10), kdy(10))
        jlono(ic) = iadrs - 1
        call jjecrs(kat(10), ic, 10, 0, 'E',&
                    imarq(jmarq(ic)+2*10-1))
        call jxliro(ic, kat(10), iadd(jiadd(ic)+2*10-1), lon)
        call jjalls(lon, ic, 'V', 'I', lois,&
                    z, date, iadrs, kat(11), kdy(11))
        jdate(ic) = iadrs - 1
        call jjecrs(kat(11), ic, 11, 0, 'E',&
                    imarq(jmarq(ic)+2*11-1))
        call jxliro(ic, kat(11), iadd(jiadd(ic)+2*11-1), lon)
        call jjalls(lon, ic, 'V', 'I', lois,&
                    z, luti, iadrs, kat(12), kdy(12))
        jluti(ic) = iadrs - 1
        call jjecrs(kat(12), ic, 12, 0, 'E',&
                    imarq(jmarq(ic)+2*12-1))
        call jxliro(ic, kat(12), iadd(jiadd(ic)+2*12-1), lon)
        lon = nrhcod(ic) * lois
        call jjalls(lon, ic, 'V', 'I', lois,&
                    z, hcod, iadrs, kat(13), kdy(13))
        jhcod(ic) = iadrs - 1
        call jjecrs(kat(13), ic, 13, 0, 'E',&
                    imarq(jmarq(ic)+2*13-1))
        call jxliro(ic, kat(13), iadd(jiadd(ic)+2*13-1), lon)
        do 20 i = 1, lidbas
            iadm(jiadm(ic) + 2*i-1 ) = kat(i)
            iadm(jiadm(ic) + 2*i ) = kdy(i)
20      continue
        if (lenrg) then
            long(jlong(ic)+15) = nblma2
            lono(jlono(ic)+15) = nblma2
            long(jlong(ic)+14) = 3*nblma2
            lono(jlono(ic)+14) = 3*nblma2
            lon2 = lono(jlono(ic)+9) * ltyp(jltyp(ic)+9)
            call jxecro(ic, kat(9), iadd(jiadd(ic)+2*9-1), lon2, 0,&
                        9)
            lon2 = lono(jlono(ic)+10) * ltyp(jltyp(ic)+10)
            call jxecro(ic, kat(10), iadd(jiadd(ic)+2*10-1), lon2, 0,&
                        10)
            call jxecro(ic, kat(1), iadd(jiadd(ic)+2*1-1), lono(jlono( ic)+1)*lois, 0,&
                        1)
        endif
    endif
!
    ldyn = ldynol
    ipgc = ipgca
! FIN ------------------------------------------------------------------
end subroutine
