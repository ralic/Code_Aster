subroutine jelihd(nomf, fichdf, clas)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! aslint: disable=W1501
    implicit none
    include 'jeveux.h'
    include 'jeveux_private.h'
    include 'asterc/gtoptk.h'
    include 'asterc/hdfcld.h'
    include 'asterc/hdfclf.h'
    include 'asterc/hdfclg.h'
    include 'asterc/hdfnbo.h'
    include 'asterc/hdfnom.h'
    include 'asterc/hdfopd.h'
    include 'asterc/hdfopf.h'
    include 'asterc/hdfopg.h'
    include 'asterc/hdfrat.h'
    include 'asterc/hdfrsv.h'
    include 'asterc/hdftsd.h'
    include 'asterc/hdftyp.h'
    include 'asterfort/assert.h'
    include 'asterfort/codent.h'
    include 'asterfort/iunifi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jjalls.h'
    include 'asterfort/jjecrs.h'
    include 'asterfort/jjhrsv.h'
    include 'asterfort/jjlchd.h'
    include 'asterfort/jjlide.h'
    include 'asterfort/jjlidy.h'
    include 'asterfort/jjlihd.h'
    include 'asterfort/jjprem.h'
    include 'asterfort/jxecro.h'
    include 'asterfort/jxouvr.h'
    include 'asterfort/lxmins.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mesk.h'
    character(len=*) :: nomf, fichdf, clas
! ----------------------------------------------------------------------
! ROUTINE UTILISATEUR D'OUVERTURE D'UNE BASE AVEC LECTURE SUR
! FICHIER HDF
!
! IN  NOMF   : NOM LOCALE DE LA BASE
! IN  FICHDF : NOM LOCAL DU FICHIER HDF UTILISE POUR L'IMPRESSION
! IN  CLAS   : NOM DE LA CLASSE ASSOCIEE
!
! ----------------------------------------------------------------------
    character(len=24) :: nomco
    character(len=32) :: nomuti, nomos, nomoc, bl32
    common /nomcje/  nomuti , nomos , nomco , nomoc , bl32
    integer :: iclas, iclaos, iclaco, idatos, idatco, idatoc
    common /iatcje/  iclas ,iclaos , iclaco , idatos , idatco , idatoc
! ----------------------------------------------------------------------
    integer :: lk1zon, jk1zon, liszon, jiszon
    common /izonje/  lk1zon , jk1zon , liszon , jiszon
    integer :: n
    parameter      ( n = 5 )
    integer :: jltyp, jlong, jdate, jiadd, jiadm, jlono, jhcod, jcara, jluti
    integer :: jmarq
    common /jiatje/  jltyp(n), jlong(n), jdate(n), jiadd(n), jiadm(n),&
     &                 jlono(n), jhcod(n), jcara(n), jluti(n), jmarq(n)
!
    integer :: jgenr, jtype, jdocu, jorig, jrnom
    common /jkatje/  jgenr(n), jtype(n), jdocu(n), jorig(n), jrnom(n)
    integer :: jiacce, jusadi, jindir, nbacce
    common /jiacce/  jiacce(n),nbacce(2*n)
    common /jusadi/  jusadi(n)
    common /jindir/  jindir(n)
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
    integer :: lbis, lois, lols, lor8, loc8
    common /ienvje/  lbis , lois , lols , lor8 , loc8
    integer :: lfic, mfic
    common /fenvje/  lfic,mfic
!
    integer :: ipgc, kdesma(2), lgd, lgduti, kposma(2), lgp, lgputi
    common /iadmje/  ipgc,kdesma,   lgd,lgduti,kposma,   lgp,lgputi
    integer :: istat
    common /istaje/  istat(4)
    real(kind=8) :: svuse, smxuse
    common /statje/  svuse,smxuse
    character(len=4) :: z
    parameter      ( z = 'INIT' )
    character(len=8) :: knom, knomf, cversb, cversu
    integer :: ncar, itlec(1), itecr(1)
    parameter      ( ncar = 11 )
! ----------------------------------------------------------------------
    character(len=1) :: kclas, typei, genri, typeb
    character(len=80) :: nhdf
    character(len=32) :: crnom, ngrp, k32(1), nomo
    character(len=24) :: kattr(5), kattrg(5), nomatr, nomat2
    character(len=8) :: k8(1), nrep(2)
    parameter      ( nomatr = 'ATTRIBUTS JEVEUX',&
     &                 nomat2 = 'BASE GLOBALE JEVEUX' )
    integer :: ipgca, ltypi, lonoi, nbobj, ik32(1), jk32, ik8(1), jk8
    integer :: idfic, idts, ltypb, nbval, iadmi, vali(2)
    integer :: idos, kitab, idgr, idt1, idt2, idg, julist
    integer :: iret1, iret2, iret3, idco, lmarq
    equivalence     (ik32,k32),(ik8,k8)
    integer :: igenr(1), itype(1), idocu(1), iorig(1), irnom(4)
    integer :: i, k, ktemp1, ktemp2, lon, lon2, ic, lcarao, iadrs, lonok
    integer :: longj, itab, jtab, iconv, nblma2
    equivalence     (igenr,genr),(itype,type),&
     &                (idocu,docu),(iorig,orig),(irnom,rnom)
!     ------------------------------------------------------------------
    integer :: ilorep, ideno, idehc
    parameter      ( ilorep=1,ideno=2,idehc=6)
! ----------------------------------------------------------------------
    integer :: lidbas, lideff, idyn32, idyn8
    parameter      ( lidbas = 20 , lideff = 15 )
    character(len=8) :: cidbas(lidbas)
    character(len=24) :: valk(3)
    character(len=32) :: nomsys, d32
    integer :: kat(lidbas), lso(lidbas), kdy(lidbas), lgbl, iadyn
    integer :: irt, ind
    real(kind=8) :: valr
    data cidbas  / '$$CARA  ' , '$$IADD  ' , '$$GENR  ' , '$$TYPE  ' ,&
     &               '$$DOCU  ' , '$$ORIG  ' , '$$RNOM  ' , '$$LTYP  ' ,&
     &               '$$LONG  ' , '$$LONO  ' , '$$DATE  ' , '$$LUTI  ' ,&
     &               '$$HCOD  ' , '$$USADI ' , '$$ACCE  ' , '$$MARQ  ' ,&
     &               '$$INDI  ' , '$$TLEC  ' , '$$TECR  ' , '$$IADM  ' /
!     ------------------------------------------------------------------
    data nomsys  / '________XXXXXXXX________$$CARA' /
    data             nrep / 'T_HCOD' , 'T_NOM' /
    data             d32 /'$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'/
! DEB ------------------------------------------------------------------
    ipgca = ipgc
    ipgc = -2
!
    iconv = 0
    kclas = clas
    knom = nomf
    knomf = nomf
    call lxmins(knomf)
!
    call assert(knomf .ne. '        ' .and. len(nomf) .le. 8)
    call assert(kclas .ne. ' ')
    call assert(index (classe, kclas) .eq. 0)
!
    ic = index (classe , ' ')
    call assert(ic .ne. 0)
    nomfic(ic) = knomf
    nombas(ic) = knom
    kstini(ic) = 'POURSUIT'
    kstout(ic) = 'SAUVE   '
    classe(ic:ic) = kclas
    nbcla = index( classe , '$' ) - 1
    if (nbcla .eq. -1) nbcla = n
    iclas = ic
    nomuti = ' '
    nomos = d32
    nomco = d32(1:24)
    nomoc = d32
    ltypb = 0
    typeb = ' '
    do 1001 i = 1, 5
        kattr(i) = ' '
        kattrg(i) = ' '
1001  end do
!
! ----- OPEN FICHIER
! ----- LECTURE DE L'OBJET ________GLOBALE ________$$CARA
! ----- PERMET DE DIMENSIONNER LES OBJETS SYSTEMES ASSOCIES A LA BASE
!
    nhdf = fichdf
    idfic = hdfopf (nhdf)
    if (idfic .lt. 0) then
        call u2mesk('F', 'JEVEUX_54', 1, nhdf)
    endif
    ngrp ='/'
    idg = hdfopg (idfic,ngrp)
    iret1 = hdfrat (idg,nomat2,5,kattr)
    iret1 = hdfclg (idg)
    julist = iunifi ('MESSAGE')
    write(julist,*) '<I> RELECTURE DE LA BASE GLOBALE AU FORMAT'//&
     &                ' HDF SUR LE FICHIER :'
    write(julist,*) '    ',nhdf
    write(julist,*) '    '
    write(julist,*) '    CONSTRUITE AVEC LA VERSION ',kattr(1)
    write(julist,*) '    SUR LA PLATE-FORME ',kattr(2)
    write(julist,*) '    SOUS SYSTEME ',kattr(3)
    write(julist,*) '    '
!
    nbobj = hdfnbo(idfic,ngrp)
    crnom = nomsys(1:8)//knom//nomsys(17:24)//cidbas(1)
    idts = hdfopd(idfic,ngrp,crnom)
    lcarao = ncar * lois
    call jjalls(lcarao, ic, 'V', 'I', lois,&
                z, cara, iadrs, kat(1), iadyn)
    jcara(ic) = iadrs
    call jjhrsv(idts, ncar, kat(1))
!
    cversb = '  .  .  '
    call codent(cara(jcara(ic) + 8 ), 'D ', cversb(1:2))
    call codent(cara(jcara(ic) + 9 ), 'D0', cversb(4:5))
    call codent(cara(jcara(ic) + 10), 'D0', cversb(7:8))
    call gtoptk('versionD0', cversu, iret1)
    nremax(ic) = cara(jcara(ic) )
    nreuti(ic) = cara(jcara(ic) + 1 )
    nrhcod(ic) = cara(jcara(ic) + 2 )
    nblmax(ic) = cara(jcara(ic) + 3 )
    nbluti(ic) = cara(jcara(ic) + 4 )
    longbl(ic) = cara(jcara(ic) + 5 )
    if (cversu .ne. cversb) then
        valk(1) = nombas(ic)
        valk(2) = cversb
        valk(3) = cversu
        call u2mesk('A', 'JEVEUX_08', 3, valk)
    endif
!
! --- LORSQUE LE NOMBRE D'ENREGISTREMENTS MAXIMUM EST MODIFIE
!
    nblma2 = mfic/(longbl(ic)*lois)
    if (nblmax(ic) .ge. nblma2) then
        nblma2 = nblmax(ic)
    else
        vali(1) = nblmax(ic)
        vali(2) = nblma2
        valk(1) = nombas(ic)
        call u2mesg('I', 'JEVEUX_36', 1, valk, 2,&
                    vali, 0, valr)
    endif
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
    idts = hdfopd(idfic,ngrp,crnom)
    lcarao = ncar * lois
    call jjalls(lcarao, ic, 'V', 'I', lois,&
                z, cara, iadrs, kat(1), kdy(1))
    jcara(ic) = iadrs
    call jjecrs(kat(1), ic, 1, 0, 'E',&
                imarq(jmarq(ic)+2*1-1))
    call jjhrsv(idts, ncar, kat(1))
!
    nbenrg(ic) = min ( lfic/(longbl(ic)*lois) , nblmax(ic) )
!
! ----OPEN DU FICHIER BINAIRE ASSOCIE A LA BASE JEVEUX
!
    call jxouvr(ic, 1)
    iext(ic) = 1
!
! ----ALLOCATION DES TAMPONS DE LECTURE/ECRITURE, DES ADRESSES MEMOIRE
!     ET DES MARQUES
!
    lgbl=1024*longbl(ic)*lois
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
    lon = nremax(ic) * lois
    call jjecrs(kat(19), ic, 19, 0, 'E',&
                imarq(jmarq(ic)+2*19-1))
    call jjalls(2*lon, ic, 'V', 'I', lois,&
                z, iadm, iadrs, kiadm(ic), kdy(20))
    kat(20) = kiadm(ic)
    jiadm(ic) = iadrs - 1
    call jjecrs(kat(20), ic, 20, 0, 'E',&
                imarq(jmarq(ic)+2*20-1))
!
    call jjalls(lon, ic, 'V', 'I', lois,&
                z, indir, iadrs, kat(17), kdy(17))
    jindir(ic) = iadrs - 1
    call jjecrs(kat(17), ic, 17, 0, 'E',&
                imarq(jmarq(ic)+2*17-1))
    do 345 ind = 1, nremax(ic)
        indir(jindir(ic)+ind) = ind
345  end do
!
!
!     CES DEUX OBJETS SYSTEME NE DOIVENT PAS ETRE RELUS SUR FICHIER HDF
!
    call jjalls(2*lon, ic, 'V', 'I', lois,&
                z, iadd, iadrs, kat(2), kdy(2))
    jiadd(ic) = iadrs - 1
    call jjecrs(kat(2), ic, 2, 0, 'E',&
                imarq(jmarq(ic)+2*2-1))
!
    lon2 = nblma2 * lois
    call jjalls(lon2, ic, 'V', 'I', lois,&
                z, iacce, iadrs, kat(15), kdy(15))
    jiacce(ic) = iadrs - 1
    call jjecrs(kat(15), ic, 15, 0, 'E',&
                imarq(jmarq(ic)+2*15-1))
!
    lon = nremax(ic) * len(genr(1))
    call jjalls(lon, ic, 'V', 'K', len(genr(1)),&
                z, igenr, iadrs, kat(3), kdy(3))
    jgenr(ic) = iadrs - 1
    call jjecrs(kat(3), ic, 3, 0, 'E',&
                imarq(jmarq(ic)+2*3-1))
    crnom = nomsys(1:8)//knom//nomsys(17:24)//cidbas(3)
    idts = hdfopd(idfic,ngrp,crnom)
    iret1 = hdftsd(idts,typei,ltypi,lonoi)
    call jjhrsv(idts, lonoi, kat(3))
!
    lon = nremax(ic) * len(type(1))
    call jjalls(lon, ic, 'V', 'K', len(type(1)),&
                z, itype, iadrs, kat(4), kdy(4))
    jtype(ic) = iadrs - 1
    call jjecrs(kat(4), ic, 4, 0, 'E',&
                imarq(jmarq(ic)+2*4-1))
    crnom = nomsys(1:8)//knom//nomsys(17:24)//cidbas(4)
    idts = hdfopd(idfic,ngrp,crnom)
    iret1 = hdftsd(idts,typei,ltypi,lonoi)
    call jjhrsv(idts, lonoi, kat(4))
!
    lon = nremax(ic) * len(docu(1))
    call jjalls(lon, ic, 'V', 'K', len(docu(1)),&
                z, idocu, iadrs, kat(5), kdy(5))
    jdocu(ic) = iadrs - 1
    call jjecrs(kat(5), ic, 5, 0, 'E',&
                imarq(jmarq(ic)+2*5-1))
    crnom = nomsys(1:8)//knom//nomsys(17:24)//cidbas(5)
    idts = hdfopd(idfic,ngrp,crnom)
    iret1 = hdftsd(idts,typei,ltypi,lonoi)
    call jjhrsv(idts, lonoi, kat(5))
!
    lon = nremax(ic) * len(orig(1))
    call jjalls(lon, ic, 'V', 'K', len(orig(1)),&
                z, iorig, iadrs, kat(6), kdy(6))
    jorig(ic) = iadrs - 1
    call jjecrs(kat(6), ic, 6, 0, 'E',&
                imarq(jmarq(ic)+2*6-1))
    crnom = nomsys(1:8)//knom//nomsys(17:24)//cidbas(6)
    idts = hdfopd(idfic,ngrp,crnom)
    iret1 = hdftsd(idts,typei,ltypi,lonoi)
    call jjhrsv(idts, lonoi, kat(6))
!
    lon = nremax(ic) * len(rnom(1))
    call jjalls(lon, ic, 'V', 'K', len(rnom(1)),&
                z, irnom, iadrs, kat(7), kdy(7))
    jrnom(ic) = iadrs - 1
    call jjecrs(kat(7), ic, 7, 0, 'E',&
                imarq(jmarq(ic)+2*7-1))
    crnom = nomsys(1:8)//knom//nomsys(17:24)//cidbas(7)
    idts = hdfopd(idfic,ngrp,crnom)
    iret1 = hdftsd(idts,typei,ltypi,lonoi)
    call jjhrsv(idts, lonoi, kat(7))
!
    lon = nremax(ic) * lois
    call jjalls(lon, ic, 'V', 'I', lois,&
                z, ltyp, iadrs, kat(8), kdy(8))
    jltyp(ic) = iadrs - 1
    call jjecrs(kat(8), ic, 8, 0, 'E',&
                imarq(jmarq(ic)+2*8-1))
    crnom = nomsys(1:8)//knom//nomsys(17:24)//cidbas(8)
    idts = hdfopd(idfic,ngrp,crnom)
    iret1 = hdftsd(idts,typei,ltypi,lonoi)
    call jjhrsv(idts, lonoi, kat(8))
!
    call jjalls(lon, ic, 'V', 'I', lois,&
                z, long, iadrs, kat(9), kdy(9))
    jlong(ic) = iadrs - 1
    call jjecrs(kat(9), ic, 9, 0, 'E',&
                imarq(jmarq(ic)+2*9-1))
    crnom = nomsys(1:8)//knom//nomsys(17:24)//cidbas(9)
    idts = hdfopd(idfic,ngrp,crnom)
    iret1 = hdftsd(idts,typei,ltypi,lonoi)
    call jjhrsv(idts, lonoi, kat(9))
!
    call jjalls(lon, ic, 'V', 'I', lois,&
                z, lono, iadrs, kat(10), kdy(10))
    jlono(ic) = iadrs - 1
    call jjecrs(kat(10), ic, 10, 0, 'E',&
                imarq(jmarq(ic)+2*10-1))
    crnom = nomsys(1:8)//knom//nomsys(17:24)//cidbas(10)
    idts = hdfopd(idfic,ngrp,crnom)
    iret1 = hdftsd(idts,typei,ltypi,lonoi)
    call jjhrsv(idts, lonoi, kat(10))
!
    call jjalls(lon, ic, 'V', 'I', lois,&
                z, date, iadrs, kat(11), kdy(11))
    jdate(ic) = iadrs - 1
    call jjecrs(kat(11), ic, 11, 0, 'E',&
                imarq(jmarq(ic)+2*11-1))
    crnom = nomsys(1:8)//knom//nomsys(17:24)//cidbas(11)
    idts = hdfopd(idfic,ngrp,crnom)
    iret1 = hdftsd(idts,typei,ltypi,lonoi)
    call jjhrsv(idts, lonoi, kat(11))
!
    call jjalls(lon, ic, 'V', 'I', lois,&
                z, luti, iadrs, kat(12), kdy(12))
    jluti(ic) = iadrs - 1
    call jjecrs(kat(12), ic, 12, 0, 'E',&
                imarq(jmarq(ic)+2*12-1))
    crnom = nomsys(1:8)//knom//nomsys(17:24)//cidbas(12)
    idts = hdfopd(idfic,ngrp,crnom)
    iret1 = hdftsd(idts,typei,ltypi,lonoi)
    call jjhrsv(idts, lonoi, kat(12))
!
    lon = nrhcod(ic) * lois
    call jjalls(lon, ic, 'V', 'I', lois,&
                z, hcod, iadrs, kat(13), kdy(13))
    jhcod(ic) = iadrs - 1
    call jjecrs(kat(13), ic, 13, 0, 'E',&
                imarq(jmarq(ic)+2*13-1))
    crnom = nomsys(1:8)//knom//nomsys(17:24)//cidbas(13)
    idts = hdfopd(idfic,ngrp,crnom)
    iret1 = hdftsd(idts,typei,ltypi,lonoi)
    call jjhrsv(idts, lonoi, kat(13))
!
    lon = 3*nblma2 * lois
    call jjalls(lon, ic, 'V', 'I', lois,&
                z, iusadi, iadrs, kat(14), kdy(14))
    jusadi(ic) = iadrs - 1
    call jjecrs(kat(14), ic, 14, 0, 'E',&
                imarq(jmarq(ic)+2*14-1))
!
!     LA BASE ETANT RECREE, IL FAUT RETABLIR L'ETAT D'USAGE DES
!     ENREGISTREMENTS A -1
!
    do 14 i = 1, nblma2
        iusadi( iadrs + (3*i-2) - 1 ) = -1
        iusadi( iadrs + (3*i-1) - 1 ) = -1
        iusadi( iadrs + (3*i ) - 1 ) = 0
14  end do
    do 20 i = 1, lidbas
        iadm(jiadm(ic) + 2*i-1 ) = kat(i)
        iadm(jiadm(ic) + 2*i ) = kdy(i)
20  end do
!
!     IL FAUT AJUSTER LA LONGUEUR DU TYPE ENTIER AVANT ECRITURE
!
!
    do 49 k = 1, lideff
        if (type(jtype(ic)+k) .eq. 'I') then
            ltyp(jltyp(ic)+k) = lois
        endif
49  end do
!
    iadd (jiadd(ic)+1) = 0
    iadd (jiadd(ic)+2) = 0
    call jxecro(ic, kat(1), iadd(jiadd(ic)+1), ncar*lois, 0,&
                1)
    do 21 i = 2, lideff
        iadd (jiadd(ic)+2*i-1) = 0
        iadd (jiadd(ic)+2*i ) = 0
        lso(i) = lono(jlono(ic)+i) * ltyp(jltyp(ic)+i)
        call jxecro(ic, kat(i), iadd(jiadd(ic)+2*i-1), lso(i), 0,&
                    i)
21  end do
    cara(jcara(ic)+6) = iadd(jiadd(ic) + 2*2-1 )
    cara(jcara(ic)+7) = iadd(jiadd(ic) + 2*2 )
!
    ipgc = ipgca
    lon = nbobj*32
    call jjalls(lon, ic, 'V', 'K', 32,&
                'INIT', ik32, jk32, ktemp1, idyn32)
    iszon(jiszon+ktemp1-1) = istat(2)
    iszon(jiszon+iszon(jiszon+ktemp1-4)-4) = istat(4)
    iret1 = hdfnom(idfic,ngrp,k32(jk32))
    lon = nbobj*8
    svuse = svuse + (iszon(jiszon+ktemp1-4) - ktemp1 + 4)
    smxuse = max(smxuse,svuse)
    call jjalls(lon, ic, 'V', 'K', 8,&
                'INIT', ik8, jk8, ktemp2, idyn8)
    iszon(jiszon+ktemp2-1) = istat(2)
    iszon(jiszon+iszon(jiszon+ktemp2-4)-4) = istat(4)
    svuse = svuse + (iszon(jiszon+ktemp2-4) - ktemp2 + 4)
    smxuse = max(smxuse,svuse)
    iret2 = hdftyp(idfic,ngrp,nbobj,k8(jk8))
    call assert(iret2 .eq. 0)
!
!     ON AJUSTE LA LONGUEUR DU TYPE POUR LES INTEGER *8 OU *4
!     SUIVANT LA PLATE-FORME, ET LA LONGUEUR DES OBJETS DE GENRE
!     REPERTOIRE
!
    irt = 0
    do 51 k = lideff+1, nremax(ic)
        if (type(jtype(ic)+k) .eq. 'I') then
            ltyp(jltyp(ic)+k) = lois
        endif
        if (genr(jgenr(ic)+k) .eq. 'N') then
            ltypi = ltyp(jltyp(ic)+k)
            longj = long(jlong(ic)+k)
            lonok = (idehc + jjprem(longj,irt))*lois + (longj+1)* ltypi
            if (mod(lonok,ltypi) .gt. 0) then
                lonok = (lonok/ltypi + 1 )
            else
                lonok = lonok/ltypi
            endif
            lono(jlono(ic)+k) = lonok
        endif
51  end do
!
!     ON TRAITE EN PREMIER LES COLLECTIONS AFIN DE POUVOIR LES LIBERER
!
    call jemarq()
    do 101 k = 1, nbobj
        if (k8(jk8+k-1) .eq. 'dataset') then
            idts=hdfopd(idfic,ngrp,k32(jk32+k-1))
            iret1=hdfrat(idts,nomatr,5,kattr)
            if (kattr(1) .eq. 'COLLECTION') then
                read(kattr(2),'(16X,I8)') idco
                call jjlchd(idco, ic, idfic, idts, ngrp)
            endif
            iret3=hdfcld(idts)
            call assert(iret3 .eq. 0)
        endif
101  end do
!
!     ON TRAITE MAINTENANT LES OBJETS SIMPLES (OBJETS SYSTEMES EXCLUS)
!
    do 201 k = 1, nbobj
        if (k8(jk8+k-1) .eq. 'dataset') then
            idts=hdfopd(idfic,ngrp,k32(jk32+k-1))
            iret1=hdftsd(idts,typeb,ltypb,nbval)
            iret1=hdfrat(idts,nomatr,5,kattr)
            if (kattr(1) .eq. 'OBJET SIMPLE') then
                read(kattr(2),'(16X,I8)') idos
                genri = genr (jgenr(ic)+idos)
                typei = type (jtype(ic)+idos)
                ltypi = ltyp (jltyp(ic)+idos)
                lon = lono (jlono(ic)+idos)
                lonoi = lon * ltypi
                iadd (jiadd(ic)+2*idos-1) = 0
                iadd (jiadd(ic)+2*idos ) = 0
                call jjlihd(idts, lon, lonoi, genri, typei,&
                            ltypi, ic, idos, 0, imarq(jmarq(ic)+2*idos-1),&
                            iadmi, iadyn)
                iadm(jiadm(ic)+2*idos-1) = iadmi
                iadm(jiadm(ic)+2*idos ) = iadyn
                iclas = ic
                iclaos = ic
                idatos = idos
                nomos = rnom(jrnom(ic)+idos)
                nomco = d32(1:24)
                nomoc = d32
                call jjlide('JELIBE', rnom(jrnom(ic)+idos), 1)
            endif
            iret1=hdfcld(idts)
        else if (k8(jk8+k-1) .eq. 'group') then
            nomo = k32(jk32+k-1)
            idgr = hdfopg(idfic,nomo)
            iret1= hdfrat(idgr,nomatr,5,kattrg)
            if (kattrg(1) .eq. 'OBJET SIMPLE') then
                read(kattrg(2),'(16X,I8)') idos
                genri = genr (jgenr(ic)+idos)
                typei = type (jtype(ic)+idos)
                ltypi = ltyp (jltyp(ic)+idos)
                lon = lono (jlono(ic)+idos)
                lonoi = lon * ltypi
                iadd (jiadd(ic)+2*idos-1) = 0
                iadd (jiadd(ic)+2*idos ) = 0
!
!           ON TRAITE UN GROUPE CONTENANT LES ELEMENTS T_HCOD ET T_NOM
!           CORRESPONDANT A UN REPERTOIRE DE NOMS
!
                idt1=hdfopd(idfic,nomo,nrep(1))
                idt2=hdfopd(idfic,nomo,nrep(2))
                call jjalls(lonoi, ic, genri, typei, ltypi,&
                            'INIT', itab, jtab, iadmi, iadyn)
                call jjecrs(iadmi, ic, idos, 0, 'E',&
                            imarq(jmarq(ic)+2* idos-1))
                iret1=hdftsd(idt1,typeb,ltypb,nbval)
                call jjhrsv(idt1, nbval, iadmi)
!
!           ON AJUSTE LA POSITION DES NOMS EN FONCTION DU TYPE D'ENTIER
!
                iszon(jiszon+iadmi-1+ideno)= (idehc+iszon(jiszon+&
                iadmi-1+ilorep))*lois
                iret1=hdftsd(idt2,typeb,ltypb,nbval)
                kitab=jk1zon+(iadmi-1)*lois+iszon(jiszon+iadmi-1+&
                ideno)+1
                iret1=hdfrsv(idt2,nbval,k1zon(kitab),iconv)
                iret1 = hdfcld(idt2)
                iadm(jiadm(ic)+2*idos-1) = iadmi
                iadm(jiadm(ic)+2*idos ) = iadyn
                iclas = ic
                iclaos = ic
                idatos = idos
                nomos = rnom(jrnom(ic)+idos)
                nomco = d32(1:24)
                nomoc = d32
                call jjlide('JELIBE', rnom(jrnom(ic)+idos), 1)
            endif
            iret1=hdfclg(idgr)
        endif
201  end do
    iret1 = hdfclf (idfic)
    if (iret1 .ne. 0) then
        call u2mesk('F', 'JEVEUX_55', 1, nhdf)
    else
        call u2mesk('I', 'JEVEUX_56', 1, nhdf)
    endif
    call jjlidy(idyn32, ktemp1)
    call jjlidy(idyn8, ktemp2)
    call jedema()
!
! FIN ------------------------------------------------------------------
end subroutine
