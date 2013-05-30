subroutine op0144()
    implicit none
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!
!     OPERATEUR "CALC_FLUI_STRU"
!
!-----------------------------------------------------------------------
    include 'jeveux.h'
!
    include 'asterc/getfac.h'
    include 'asterc/getres.h'
    include 'asterc/getvid.h'
    include 'asterc/getvis.h'
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterc/indik8.h'
    include 'asterfort/assert.h'
    include 'asterfort/cmpcha.h'
    include 'asterfort/crprno.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/flust1.h'
    include 'asterfort/flust2.h'
    include 'asterfort/flust3.h'
    include 'asterfort/flust4.h'
    include 'asterfort/infmaj.h'
    include 'asterfort/iunifi.h'
    include 'asterfort/jecreo.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetc.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexatr.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/pteequ.h'
    include 'asterfort/tbajli.h'
    include 'asterfort/tbajpa.h'
    include 'asterfort/tbcrsd.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
!     UN COMMON AJOUTE POUR RESORBER UNE GLUTE ANTIQUE (VOIR HISTOR):
    character(len=8) :: typflu
    common  / kop144 / typflu
!
    integer :: ibid, nbconn, iconn
    integer :: ncmp, ncmpmx, jcorr2
    real(kind=8) :: r8b
    complex(kind=8) :: c16b
    logical :: tmode, calcul(2)
    character(len=8) :: nombm, mailla, k8b, gran, nomcmp(6)
    character(len=16) :: concep, cmd, nompar
    character(len=19) :: nomu, cham19, prchno
    character(len=24) :: desc, numo, vite, freq, masg, fact
    character(len=24) :: numoi, fsic, nomcha, matria, refebm, chrefe, chdesc
    character(len=24) :: chvale
    character(len=32) :: nomvar
    integer :: iarg
!
!-----------------------------------------------------------------------
    integer :: i, iacmp, iamor, iav, icmp, idec, idesc
    integer :: iec, ier, ifact, ifm, ifr, ifreq, ifsic
    integer :: ii, imasg, inec, ino, inumo, io, ipar
    integer :: irefbm, irefe, iret, itypfl, iv, ivite
    integer :: j, jcdesc, jcrefe, jdesc, jj, jprno, long
    integer :: nbam, nbcomp, nbno, nbnoeu, nbocc, nbpar, nbpv
    integer :: nec, nivdef, nivpar, numgd
    real(kind=8) :: amor, rbid, umin, vmax, vmin, vmoy, vpas
!
!-----------------------------------------------------------------------
    data         nomcmp /'DX      ','DY      ','DZ      ',&
     &                     'DRX     ','DRY     ','DRZ     '/
!
!-----------------------------------------------------------------------
!     OBJETS CREES SUR LA BASE GLOBALE
!
!     NOMU//'.DESC'
!     NOMU//'.REMF'
!     NOMU//'.NUMO'
!     NOMU//'.VITE'
!     NOMU//'.FREQ'
!     NOMU//'.MASG'
!     NOMU//'.FACT'
!
!     TABLE DES NOMS DES CHAMPS DE DEPLACEMENTS AUX NOEUDS
!
!     OBJETS ASSOCIES AU PROF_CHNO COMMUN A TOUS LES CHAMPS DE
!     DEPLACEMENTS AUX NOEUDS
!     NOMU(1:8)//'.C01.YY1XX1.LILI'
!     NOMU(1:8)//'.C01.YY1XX1.LPRN'
!     NOMU(1:8)//'.C01.YY1XX1.PRNO'
!     NOMU(1:8)//'.C01.YY1XX1.NUEQ'
!     NOMU(1:8)//'.C01.YY1XX1.DEEQ'
!     CES OBJETS SONT CREES PAR LA ROUTINE CRPRNO
!
!     OBJETS ASSOCIES AUX CHAMPS DE DEPLACEMENTS AUX NOEUDS
!     NOMU(1:8)//'.C01.YYYXXX.DESC'
!     NOMU(1:8)//'.C01.YYYXXX.REFE'
!     NOMU(1:8)//'.C01.YYYXXX.VALE'
!
!-----------------------------------------------------------------------
!
    call jemarq()
    call infmaj()
!
    call getres(nomu, concep, cmd)
    call getvis('BASE_MODALE', 'NUME_ORDRE', 1, iarg, 0,&
                ibid, nbno)
    call getvr8('BASE_MODALE', 'AMOR_REDUIT', 1, iarg, 0,&
                rbid, nbam)
    call getvr8('BASE_MODALE', 'AMOR_REDUIT_CONN', 1, iarg, 0,&
                rbid, nbconn)
    ifr = iunifi('RESULTAT')
    ifm = iunifi('MESSAGE')
    write(ifr,1000)
!
!
! --- 0.VERIFICATIONS AVANT EXECUTION ---
!
    if (nbno .ne. 0 .and. nbam .ne. 0 .and. nbam .ne. nbno) then
        call u2mess('F', 'ALGELINE2_82')
    endif
!
    if (nbno .ne. 0 .and. nbconn .ne. 0 .and. nbconn .ne. nbno) then
        call u2mess('F', 'ALGELINE2_83')
    endif
!
! --- 1.RECUPERATION DES CARACTERISTIQUES MODALES AVANT COUPLAGE ---
!
    call getvid('BASE_MODALE', 'MODE_MECA', 1, iarg, 1,&
                nombm, zi)
!
    tmode = .false.
    calcul(1)=.false.
    calcul(2)=.false.
    if (nbno .eq. 0) then
        tmode = .true.
        numoi = nombm//'           .ORDR'
        call jelira(numoi, 'LONUTI', nbno, k8b)
        call assert(nbam.eq.0 .or. abs(nbam).eq.nbno)
    else
        nbno = abs(nbno)
    endif
!
! --- 1.1.CREATION ET REMPLISSAGE DE L'OBJET .NUMO
!
    numo = nomu//'.NUMO'
    call wkvect(numo, 'G V I', nbno, inumo)
    if (tmode) then
        do 10 i = 1, nbno
            zi(inumo+i-1) = i
10      continue
    else
        call getvis('BASE_MODALE', 'NUME_ORDRE', 1, iarg, nbno,&
                    zi(inumo), ibid)
    endif
!
! --- 1.2.CREATION D'UN VECTEUR TEMPORAIRE POUR LES AMORTISSEMENTS
!
    call wkvect('&&OP0144.TEMP.AMOR', 'V V R', nbno, iamor)
    call wkvect('&&OP0144.CONNORS.AMOR', 'V V R', nbno, iconn)
    if (nbam .ne. 0) then
        call getvr8('BASE_MODALE', 'AMOR_REDUIT', 1, iarg, 0,&
                    amor, ibid)
        call getvr8('BASE_MODALE', 'AMOR_REDUIT', 1, iarg, nbno,&
                    zr(iamor), ibid)
        calcul(1)=.true.
    else if (nbconn.eq.0) then
        calcul(1)=.true.
        call getvr8('BASE_MODALE', 'AMOR_UNIF', 1, iarg, 1,&
                    amor, ibid)
        do 20 i = 1, nbno
            zr(iamor+i-1) = amor
20      continue
    endif
!
    if (nbconn .ne. 0) then
        call getvr8('BASE_MODALE', 'AMOR_REDUIT_CONN', 1, iarg, nbno,&
                    zr(iconn), ibid)
        calcul(2)=.true.
    endif
!
!
! --- 2.RECUPERATION DE LA PLAGE DE VITESSES D'ECOULEMENT ---
!
! --- 2.1.CREATION ET REMPLISSAGE DE L'OBJET .VITE
!
    call getvr8('VITE_FLUI', 'VITE_MIN', 1, iarg, 1,&
                vmin, zi)
    call getvr8('VITE_FLUI', 'VITE_MAX', 1, iarg, 1,&
                vmax, zi)
    call getvis('VITE_FLUI', 'NB_POIN ', 1, iarg, 1,&
                nbpv, zi)
    if (vmin .gt. vmax) then
        umin = vmin
        vmin = vmax
        vmax = umin
        call u2mess('A', 'ALGELINE2_85')
    endif
!
    vite = nomu//'.VITE'
    call wkvect(vite, 'G V R', nbpv, ivite)
    if (nbpv .eq. 1) then
        vmoy = (vmin+vmax)/2.d0
        write(ifm,*)'UNE SEULE VITESSE D''ECOULEMENT ETUDIEE :'//&
        ' VMOY = (VMIN+VMAX)/2'
        zr(ivite) = vmoy
    else
        vpas = (vmax-vmin)/(nbpv-1)
        do 30 iv = 1, nbpv
            zr(ivite+iv-1) = vmin + (iv-1)*vpas
30      continue
    endif
!
! --- 2.2.CREATION DE L'OBJET .FREQ
!
    freq = nomu//'.FREQ'
    call wkvect(freq, 'G V R', 2*nbno*nbpv, ifreq)
!
!
! --- 3.RECUPERATION DU CONCEPT TYPE_FLUI_STRU   ---
!
! --- 3.1.CREATION ET REMPLISSAGE DE L'OBJET .REMF
!
    call wkvect(nomu//'.REMF', 'G V K8', 2, irefe)
    call getvid(' ', 'TYPE_FLUI_STRU', 0, iarg, 1,&
                typflu, ibid)
    zk8(irefe) = typflu
    zk8(irefe+1) = nombm
!
! --- 3.2.DETERMINATION DU TYPE DE LA CONFIGURATION ETUDIEE
!
    fsic = typflu//'           .FSIC'
    call jeveuo(fsic, 'L', ifsic)
    itypfl = zi(ifsic)
!
!
! --- 4.RECUPERATION DES NIVEAUX D'IMPRESSION ---
!
    nivpar = 0
    nivdef = 0
    call getfac('IMPRESSION', nbocc)
    if (nbocc .ne. 0) then
        call getvtx('IMPRESSION', 'PARA_COUPLAGE', 1, iarg, 1,&
                    k8b, ibid)
        if (k8b(1:3) .eq. 'OUI') nivpar = 1
        call getvtx('IMPRESSION', 'DEFORMEE', 1, iarg, 1,&
                    k8b, ibid)
        if (k8b(1:3) .eq. 'OUI') nivdef = 1
    endif
!
!
! --- 5.CREATION ET REMPLISSAGE DE L'OBJET .DESC ---
!
    desc = nomu//'.DESC'
    call wkvect(desc, 'G V K16', 1, idesc)
    zk16(idesc) = 'DEPL'
!
!
! --- 6.CREATION ET REMPLISSAGE DE LA TABLE DES NOMS DES CHAMPS ---
! ---   DE DEPLACEMENTS MODAUX                                  ---
! ---   SIMULTANEMENT ON CREE LES OBJETS .REFE , .DESC ET .VALE ---
! ---   ASSOCIES A CHACUN DES CHAMPS                            ---
! ---   POUR LE PREMIER CHAMP ON CREE LE PROF_CHNO QUI VAUT     ---
! ---   ENSUITE POUR TOUS LES AUTRES CHAMPS                     ---
!
! --- 6.1.RECUPERATION D'INFORMATIONS NECESSAIRES
! ---     A LA CREATION DES OBJETS ASSOCIES AUX CHAMPS
! ---     A LA CREATION DU PROF_CHNO COMMUN
!
    refebm = nombm//'           .REFD'
    call jeveuo(refebm, 'L', irefbm)
    matria = zk24(irefbm)
!
    call dismoi('F', 'NOM_MAILLA', matria, 'MATR_ASSE', ibid,&
                mailla, iret)
    call jelira(mailla//'.NOMNOE', 'NOMUTI', nbnoeu, k8b)
    long = 6*nbnoeu
!
    gran = 'DEPL_R  '
    call jenonu(jexnom('&CATA.GD.NOMGD', gran), numgd)
    call jeveuo(jexnum('&CATA.GD.NOMCMP', numgd), 'L', iacmp)
    call jeveuo(jexatr('&CATA.GD.NOMCMP', 'LONCUM'), 'L', iav)
    nbcomp = zi(iav+numgd) - zi(iav+numgd-1)
!
    call dismoi('F', 'NB_EC', gran, 'GRANDEUR', nec,&
                k8b, ier)
    call wkvect('&&OP0144.DESC_NOEUD', 'V V I', nec*nbnoeu, jdesc)
    do 40 ino = 1, nbnoeu
        do 50 icmp = 1, 6
            j = indik8(zk8(iacmp),nomcmp(icmp),1,nbcomp)
            if (j .ne. 0) then
                iec = (j-1)/30 + 1
                jj = j - 30*(iec-1)
                zi(jdesc+(ino-1)*nec+iec-1) = ior( zi(jdesc+(ino-1)* nec+iec-1), 2**jj )
            endif
50      continue
40  end do
!
! --- 6.2.CREATION DE LA STRUCTURE TABLE
!
    if (itypfl .eq. 3) then
        nbpar = nbpv
    else
        nbpar = 1
    endif
    call tbcrsd(nomu, 'G')
    call tbajpa(nomu, 1, 'NOM_CHAM', 'K24')
!
! --- 6.31.CREATION DE L'OBJET .MASG
!
    masg = nomu//'.MASG'
    call wkvect(masg, 'G V R', nbno*nbpar, imasg)
!
! --- 6.32.CREATION DE L'OBJET .FACT
!
    fact = nomu//'.FACT'
    call wkvect(fact, 'G V R', 3*nbno*nbpar, ifact)
!
! --- 6.4.REMPLISSAGE DE LA TABLE (NOMS DES CHAMPS) ET CREATION
! ---     SIMULTANEE DES OBJETS ASSOCIES AUX CHAMPS
!
    nomcha(1:13) = nomu(1:8)//'.C01.'
!
    do 60 io = 1, nbno
!
        write(nomvar,'(I3.3)') zi(inumo+io-1)
        nomcha(14:16) = nomvar(1:3)
!
        do 70 ipar = 1, nbpar
!
            write(nompar,'(I3.3)') ipar
            nomcha(17:24) = nompar(1:3)//'     '
!
            call tbajli(nomu, 1, 'NOM_CHAM', ibid, r8b,&
                        c16b, nomcha, 0)
!
! --------CREATION DU CHAMP
            cham19 = nomcha(1:19)
! .DESC
            chdesc = cham19//'.DESC'
            call wkvect(chdesc, 'G V I', 2, jcdesc)
            call jeecra(chdesc, 'DOCU', 0, 'CHNO')
            zi(jcdesc) = numgd
            zi(jcdesc+1) = 6
! .VALE
            chvale = cham19//'.VALE'
            call jecreo(chvale, 'G V R')
            call jeecra(chvale, 'LONMAX', long, k8b)
! .REFE
            chrefe = cham19//'.REFE'
            call wkvect(chrefe, 'G V K24', 4, jcrefe)
            zk24(jcrefe) = mailla
!
! --------AU PREMIER PASSAGE CREATION DU PROF_CHNO
            if (io .eq. 1 .and. ipar .eq. 1) then
                zk24(jcrefe+1) = cham19
                call crprno(cham19, 'G', nbnoeu, long)
                call jeveuo(cham19//'.PRNO', 'E', jprno)
                idec = 1
                ii = 0
                do 80 ino = 1, nbnoeu
                    zi(jprno-1+(nec+2)*(ino-1)+1) = idec
                    zi(jprno-1+(nec+2)*(ino-1)+2) = 6
                    do 90 inec = 1, nec
                        ii = ii + 1
                        zi(jprno-1+(nec+2)*(ino-1)+2+inec) = zi(jdesc+ ii-1)
90                  continue
                    idec = idec + 6
80              continue
                prchno = cham19
!
!           -- CALCUL DE L'OBJET .DEEQ :
                call cmpcha(cham19, '&&OP0144.NOMCMP', '&&OP0144.CORR1', '&&OP0144.CORR2', ncmp,&
                            ncmpmx)
                call jeveuo('&&OP0144.CORR2', 'L', jcorr2)
                call pteequ(prchno, 'G', long, numgd, ncmp,&
                            zi(jcorr2))
                call jedetr('&&OP0144.NOMCMP')
                call jedetr('&&OP0144.CORR1')
                call jedetr('&&OP0144.CORR2')
!
            else
                zk24(jcrefe+1) = prchno
            endif
!
70      continue
!
60  end do
!
!
! --- 7.LANCEMENT DU CALCUL EN FONCTION DU TYPE DE LA CONFIGURATION ---
! ---   ETUDIEE                                                     ---
!
    if (itypfl .eq. 1) then
!
        call flust1(nomu, typflu, nombm, zi(inumo), zr(iamor),&
                    zr(iconn), zr(ifreq), zr(imasg), zr(ifact), zr(ivite),&
                    nbno, calcul, nbpv, nivpar, nivdef)
!
    else if (itypfl.eq.2) then
!
        call flust2(nomu, typflu, nombm, mailla, zi(inumo),&
                    zr( iamor), zr(ifreq), zr(imasg), zr(ifact), zr(ivite),&
                    nbno, nbpv, nivpar, nivdef)
!
    else if (itypfl.eq.3) then
!
        call flust3(nomu, typflu, nombm, zi(inumo), zr(iamor),&
                    zr(ifreq), zr(imasg), zr(ifact), zr(ivite), nbno,&
                    nbpv, nivpar, nivdef)
!
    else
!
        call flust4(nomu, typflu, nombm, mailla, zi(inumo),&
                    zr( iamor), zr(ifreq), zr(imasg), zr(ifact), zr(ivite),&
                    nbno, nbpv, nivpar, nivdef)
!
    endif
!
!
    call jedema()
    call jedetc('G', '&&MEFCEN', 1)
    1000 format(/,80('-'))
end subroutine
