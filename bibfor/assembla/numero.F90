subroutine numero(nuposs, modelz, infchz, solveu, base,&
                  nu)
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
! ----------------------------------------------------------------------
! IN  K14  NUPOSS  : NOM D'UN NUME_DDL CANDIDAT (OU ' ')
!                    SI NUPOSS != ' ', ON  REGARDE SI LE PROF_CHNO
!                    DE NUPOSS EST CONVENABLE.
!                    (POUR EVITER DE CREER SYTEMATIQUEMENT 1 PROF_CHNO)
! IN  K8   MODELE  : NOM DU MODELE
! IN  K19  INFCHA  : NOM DE L'OBJET DE TYPE INFCHA
! IN  K19  SOLVEU  : NOM DE L'OBJET DE TYPE SOLVEUR
! IN  K2   BASE    : BASE(1:1) : BASE POUR CREER LE NUME_DDL
!                    (SAUF LE PROF_CHNO)
!                  : BASE(2:2) : BASE POUR CREER LE PROF_CHNO
! VAR/JXOUT K14 NU : NOM DU NUME_DDL.
!                    SI NUPOSS !=' ', NU PEUT ETRE MODIFIE (NU=NUPOSS)
!----------------------------------------------------------------------
! person_in_charge: jacques.pellet at edf.fr
! CORPS DU PROGRAMME
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
    include 'jeveux.h'
!
    include 'asterfort/detrsd.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/exlim1.h'
    include 'asterfort/exlim2.h'
    include 'asterfort/fetmpi.h'
    include 'asterfort/gcncon.h'
    include 'asterfort/gnomsd.h'
    include 'asterfort/infniv.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/numer2.h'
    include 'asterfort/u2mesi.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/utimsd.h'
    include 'asterfort/uttcpr.h'
    include 'asterfort/uttcpu.h'
    include 'asterfort/wkvect.h'
    character(len=*) :: modelz, solveu, infchz
    character(len=*) :: nu, nuposs
    character(len=2) :: base
!
!
! DECLARATION VARIABLES LOCALES
    integer :: nchar, nblig, iret, jchar, jlligr, k, jtypch, islvk, idime, i
    integer :: ilima, nbma, nbsd, ifm, niv, ibid, isolfs, irefe, ideeq, ifetn
    integer :: nequa, nbpb, ncharf, l, ivligr, inueq, ifel1, ldeeqg, iinf, ifcpu
    integer :: idd, jmult, ier, nbproc, rang, ilimpi, nivmpi, nbchat, iffcc
    integer :: nequag, nbi2, ifeti, iaux, ino, icmp, imult, vali(2)
    real(kind=8) :: temps(6), rbid
    character(len=1) :: k1
    character(len=3) :: verif
    character(len=8) :: moloc, nomcha, k8bid, method, nomsd, modele
    character(len=14) :: nuposb, nomfe2
    character(len=16) :: pheno
    character(len=19) :: infcha, ligrsd, ligrcf
    character(len=24) :: lcharg, lligr, nomlig, sdfeti, nomsda, k24b, k24cf
    character(len=24) :: ksolvf, lligrs, infofe, noobj, k24mul
    logical :: lfeti, lfetic, lcf
!
! RECUPERATION ET MAJ DU NIVEAU D'IMPRESSION
    call infniv(ifm, niv)
!-----------------------------------------------------------------------
! CONSTRUCTION D'UN OBJET JEVEUX CONTENANT LA LISTE DES CHARGES ET
! LE NOM DU MODELE DE CALCUL
!-----------------------------------------------------------------------
    call jemarq()
    call uttcpu('CPU.RESO.1', 'DEBUT', ' ')
    call uttcpu('CPU.RESO.2', 'DEBUT', ' ')
    k24cf = '&&NUMERO.FETI.CONTACT'
!
!     DECO. FETI+CONTACT
    lcf = .false.
    ligrcf = ' '
!
    infcha = infchz
    modele = modelz
    lcharg = infcha//'.LCHA'
    nchar = 0
    call jeexin(lcharg, iret)
    if (iret .ne. 0) then
        call jelira(lcharg, 'LONMAX', nchar, k8bid)
        call jeveuo(lcharg, 'L', jchar)
    endif
    lligr = '&&NUMERO.LISTE_LIGREL'
!
!     LISTE
    call wkvect(lligr, 'V V K24', nchar+1, jlligr)
    nblig = 0
!     ON INSERE LE LIGREL DE MODELE
    call jeexin(modele//'.MODELE    .NBNO', iret)
    if (iret .gt. 0) then
        zk24(jlligr) = modele(1:8)//'.MODELE'
        nblig = nblig + 1
    endif
!     PUIS LES CHARGES A MAILLES ET/OU A NOEUDS TARDIFS
    do 10 k = 1, nchar
        nomcha = zk24(jchar+k-1)(1:8)
        call jeexin(nomcha(1:8)//'.TYPE', ier)
        if (ier .gt. 0) then
            call jeveuo(nomcha(1:8)//'.TYPE', 'L', jtypch)
            nomlig = nomcha(1:8)//'.CH'//zk8(jtypch) (1:2)// '.LIGRE.LIEL'
            call jeexin(nomlig, iret)
        else
            iret=0
        endif
        if (iret .gt. 0) then
            zk24(jlligr+nblig) = nomlig(1:19)
            nblig = nblig + 1
        endif
10  end do
!
    call jeecra(lligr, 'LONUTI', nblig, k8bid)
!
! SOLVEUR FETI ?
    call jeveuo(solveu(1:19)//'.SLVK', 'L', islvk)
    method=zk24(islvk)(1:8)
    nivmpi=1
    lfetic=.false.
    if (method(1:4) .eq. 'FETI') then
        lfeti=.true.
        nuposb=' '
        call jeveuo('&FETI.FINF', 'L', iinf)
        infofe=zk24(iinf)
        if (infofe(11:11) .eq. 'T') lfetic=.true.
    else
        lfeti=.false.
        infofe='FFFFFFFFFFFFFFFFFFFFFFFF'
    endif
! CALCUL TEMPS
    if (lfetic) then
        call uttcpu('CPU.NUMERO.FETI', 'INIT', ' ')
        call uttcpu('CPU.NUMERO.FETI', 'DEBUT', ' ')
    endif
! --------------------------------------------------------------
! CREATION ET REMPLISSAGE DE LA SD NUME_DDL "MAITRE"
! --------------------------------------------------------------
    call numer2(nuposs, nblig, zk24(jlligr), ' ', solveu,&
                base, nu, nequag)
!
    if (lfetic) then
        call uttcpu('CPU.NUMERO.FETI', 'FIN', ' ')
        call uttcpr('CPU.NUMERO.FETI', 6, temps)
        call jeveuo('&FETI.INFO.CPU.FACS', 'E', ifcpu)
        zr(ifcpu)=temps(5)+temps(6)
    endif
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    if (lfeti) then
!
! STRUCTURE DE DONNEES DE TYPE SD_FETI
        sdfeti=' '
        sdfeti(1:8)=zk24(islvk+5)
        verif=zk24(islvk+2)(1:3)
!
! MONITORING
        if (infofe(1:1) .eq. 'T') write(ifm, *)'<FETI/NUMERO> DOMAINE GLOBAL ', nu(1:14)
        if (infofe(2:2) .eq. 'T') call utimsd(ifm, 2, .false., .true., nu(1: 14),&
                                              1, ' ')
!
! VERIFICATION COHERENCE SD_FETI AVEC PARAMETRAGE OPERATEUR
        call jeveuo(sdfeti(1:19)//'.FREF', 'L', irefe)
        call jelira(sdfeti(1:19)//'.FREF', 'LONMAX', ncharf, k8bid)
        ncharf=ncharf-1
        nbpb=0
        if (zk8(irefe) .ne. modele) nbpb=nbpb+1
        do 17 k = 1, nchar
            nomcha=zk24(jchar+k-1)(1:8)
            do 15 l = 1, ncharf
                if (nomcha .eq. zk8(irefe+l)) goto 17
15          continue
            nbpb=nbpb+1
17      continue
        do 19 l = 1, ncharf
            nomcha=zk8(irefe+l)
            do 18 k = 1, nchar
                if (nomcha .eq. zk24(jchar+k-1)(1:8)) goto 19
18          continue
            nbpb=nbpb+1
19      continue
        if (verif .eq. 'OUI') then
            k1='F'
        else
            k1='A'
        endif
        if (nbpb .ne. 0) then
            vali(1)=nbpb
            call u2mesi(k1, 'ASSEMBLA_68', 1, vali)
        endif
! RECHERCHE DU PHENOMENE POUR LES NOUVEAUX LIGRELS DE SOUS-DOMAINE
! CF DISMPH.F
        call dismoi('F', 'PHENOMENE', modele, 'MODELE', ibid,&
                    pheno, iret)
        moloc=' '
        if (pheno(1:9) .eq. 'MECANIQUE') then
            moloc='DDL_MECA'
        else if (pheno(1:9).eq.'THERMIQUE') then
            moloc='DDL_THER'
        else if (pheno(1:9).eq.'ACOUSTIQU') then
            moloc='DDL_ACOU'
        else
            call u2mess('F', 'ASSEMBLA_32')
        endif
        if (infofe(1:1) .eq. 'T') then
            write(ifm,*)
            write (ifm,*)'<FETI/NUMERO> PHENOMENE ',pheno,moloc
            write(ifm,*)
        endif
!
!
! PREPARATION BOUCLE SUR LES SOUS-DOMAINES
        call jeveuo(sdfeti(1:19)//'.FDIM', 'L', idime)
        nbsd=zi(idime)
! K24CF: OBJET JEVEUX STOCKANT LES NOMS DES LIGRELS DE MODELE POUR NE
!  PAS LES RECONSTRUIRE A CHAQUE PASSE DU CONTACT CONTINUE
! ON EN PROFITE POUR DETRUIRE LES LIGRELS DE CONTACT PROJETES SI ON EST
! AU MOINS A LA 3IEME PASSE (LIGRCF.FEL1 EXISTE) + TOUS LES .FEL
! ANNEXES. SINON ON VA GASPILLER DE LA MEMOIRE ET EPUISER LE COMPTEUR
! DE GCNCON.
        if (lcf) then
            call jeveuo(k24cf, 'E', iffcc)
            call jeexin(ligrcf(1:19)//'.FEL1', iret)
            if (iret .ne. 0) then
                call jeveuo(ligrcf(1:19)//'.FEL1', 'L', ifel1)
                do 35 idd = 1, nbsd
                    k24b=zk24(ifel1+idd-1)
                    if ((k24b.ne.' ') .and. (k24b(1:19).ne.ligrcf(1:19))) call detrsd('LIGREL',&
                                                                                      k24b)
35              continue
                call jedetr(ligrcf(1:19)//'.FEL1')
                call jedetr(ligrcf(1:19)//'.FEL2')
                call jedetr(ligrcf(1:19)//'.FEL4')
            endif
        else
            call wkvect(k24cf, 'V V K24', nbsd, iffcc)
        endif
        nomsda=sdfeti(1:19)//'.FETA'
! ADRESSE DANS L'OBJET JEVEUX SOLVEUR.FETS DES NOMS DES OBJETS
! JEVEUX REPRESENTANT LES SOLVEURS LOCAUX
        call jeveuo(solveu(1:19)//'.FETS', 'L', isolfs)
!
! CONSTITUTION OBJET STOCKAGE.FETN
        call wkvect(nu(1:14)//'.FETN', base(1:1)//' V K24', nbsd, ifetn)
! OBJET JEVEUX FETI & MPI
        call jeveuo('&FETI.LISTE.SD.MPI', 'L', ilimpi)
! APPEL MPI POUR DETERMINER LE NOMBRE DE PROCESSEURS
        if (infofe(10:10) .eq. 'T') then
            nivmpi=2
        else
            nivmpi=1
        endif
        call fetmpi(3, nbsd, ifm, nivmpi, rang,&
                    nbproc, k24b, k24b, k24b, rbid)
!========================================
! BOUCLE SUR LES SOUS-DOMAINES + IF MPI:
!========================================
        do 30 i = 1, nbsd
            if (zi(ilimpi+i) .eq. 1) then
!
                if (lfetic) then
                    call uttcpu('CPU.NUMERO.FETI', 'INIT', ' ')
                    call uttcpu('CPU.NUMERO.FETI', 'DEBUT', ' ')
                endif
                call jemarq()
                call jeveuo(jexnum(nomsda, i), 'L', ilima)
                call jelira(jexnum(nomsda, i), 'LONMAX', nbma, k8bid)
!
! OBJET TEMPORAIRE CONTENANT LES NOMS DES MAILLES DU SD I
                call jenuno(jexnum(nomsda, i), nomsd)
!
                if (.not.lcf) then
! CAS SANS CONTACT OU CONTACT PREMIERE PASSE
! CREATION DU LIGREL TEMPORAIRE ASSOCIE AU SOUS-DOMAINE
! LES 16 PREMIERS CHARACTERES SONT OBLIGEATOIRES COMPTE-TENU DES
! PRE-REQUIS DES ROUTINES DE CONSTRUCTION DU NUM_DDL.
! NOUVELLE CONVENTION POUR LES LIGRELS FILS, GESTION DE NOMS
! ALEATOIRES
                    call gcncon('.', k8bid)
                    k8bid(1:2)='&F'
                    ligrsd=k8bid//'.MODELE   '
                    call exlim1(zi(ilima), nbma, modele, 'V', ligrsd)
                    zk24(iffcc+i-1)=ligrsd
                else
                    ligrsd=zk24(iffcc+i-1)(1:19)
                endif
! DETECTION DES LIGRELS DE CHARGES CONCERNES PAR CE SOUS-DOMAINE
! ET REMPLISSAGE DE LA LISTE DE LIGRELS AD HOC POUR NUEFFE VIA
! NUMER2.F
                lligrs='&&NUMERO.LIGREL_SD'
                call exlim2(sdfeti, nomsd, lligrs, ligrsd, nbchat,&
                            i, nbsd, infofe, nbproc, ligrcf)
                call jeveuo(lligrs, 'L', ivligr)
!
! --------------------------------------------------------------
! CREATION ET REMPLISSAGE DE LA SD NUME_DDL "ESCLAVE" LIEE A
! CHAQUE SOUS-DOMAINE
! --------------------------------------------------------------
! CREATION DU NUME_DDL ASSOCIE AU SOUS-DOMAINE
!         DETERMINATION DU NOM DU NUME_DDL  ASSOCIE AU SOUS-DOMAINE
                noobj ='12345678.00000.NUME.PRNO'
                call gnomsd(' ', noobj, 10, 14)
                nomfe2=noobj(1:14)
                ksolvf = zk24(isolfs+i-1)
!
                call numer2(nuposb, nbchat, zk24(ivligr), moloc, ksolvf,&
                            'VV', nomfe2, nequa)
                call jedetr(lligrs)
!
! REMPLISSAGE OBJET NU.FETN
                zk24(ifetn+i-1)=nomfe2
!
! MONITORING
                if (infofe(1:1) .eq. 'T') write(ifm, *)'<FETI/NUMERO> SD ', i, ' ', nomfe2
                if (infofe(2:2) .eq. 'T') call utimsd(ifm, 2, .false., .true., nomfe2,&
                                                      1, ' ')
                if (lfetic) then
                    call uttcpu('CPU.NUMERO.FETI', 'FIN', ' ')
                    call uttcpr('CPU.NUMERO.FETI', 6, temps)
                    zr(ifcpu+i)=temps(5)+temps(6)
                endif
                call jedema()
!
            endif
30      continue
!========================================
! FIN BOUCLE SUR LES SOUS-DOMAINES + IF MPI:
!========================================
!
! FIN DE IF METHOD='FETI'
    endif
!-----------------------------------------------------------------------
! PRE-REMPLISSAGE DE L'OBJET '&&FETI.MULT' QUI EST LE PENDANT FETI DU
! .CONL. IL SERT A TENIR COMPTE DE LA MULTIPLICITE DES NOEUDS D'INTERFA
! CE LORS DE LA RECONSTRUCTION DU CHAMP SOLUTION.
! ATTENTION, ON NE TIENT COMPTE QUE DES DDLS PHYSIQUES JUSQU'A 6.
! SI LCF=.TRUE., CONTACT METHODE CONTINUE, ON RECALCULE LE NUM_DDL MAIS
! ON NE CHANGE PAS L'ORDRE DES INCONNUES. C'EST JUSTE LE STOCKAGE QUI
! CHANGE, DONC PAS BESOIN DE REFAIRE CET OBJET
    if ((lfeti) .and. (.not.lcf)) then
        k24mul='&&FETI.MULT'
! ON TESTE AU CAS OU
        call jeexin(k24mul, iret)
        if (iret .ne. 0) call jedetr(k24mul)
        call wkvect(k24mul, 'V V I', nequag, jmult)
        nequag=nequag-1
        call jeveuo(nu(1:14)//'.NUME.DEEQ', 'L', ideeq)
        call jeveuo(sdfeti(1:19)//'.FETI', 'L', ifeti)
! NOMBRE DE LAGRANGE D'INTERFACE
        nbi2=zi(idime+1)
        do 50 k = 0, nequag
            ino=zi(ideeq+2*k)
            icmp=zi(ideeq+2*k+1)
            imult=1
            if ((ino*icmp.gt.0) .and. (icmp.le.6)) then
                do 48 i = 1, nbi2
                    iaux=ifeti+4*(i-1)
                    if (ino .eq. zi(iaux)) then
                        imult=zi(iaux+1)
                        goto 49
                    endif
48              continue
49              continue
            endif
            zi(jmult+k)=imult
50      continue
    endif
! APPEL MPI POUR DETERMINER LE RANG DU PROCESSEUR
    call fetmpi(2, nbsd, ifm, nivmpi, rang,&
                nbproc, k24b, k24b, k24b, rbid)
    if ((lfeti) .and. (rang.eq.0)) then
! POUR EVITER QUE LA RECONSTRUCTION DU CHAMP GLOBAL SOIT FAUSSE DANS
! FETRIN OU ON TRAVAILLE DIRECTEMENT SUR LES .VALE (GLOBAL ET LOCAUX)
! TEST DE L'OBJET .NUEQ DU PROF_CHNO DU NUME_DDL. POUR FETI IL DOIT
! ETRE EGALE A L'IDENTITE (EN THERORIE CE PB SE PRESENTE QUE POUR LA
! SOUS-STRUCTURATION QUI EST ILLICITE AVEC FETI, MAIS ON NE SAIT JAMAIS)
        k24b=nu(1:14)//'.NUME.NUEQ'
        call jeveuo(k24b, 'L', inueq)
        call jelira(k24b, 'LONMAX', ldeeqg, k8bid)
        do 40 i = 1, ldeeqg
            ibid=zi(inueq+i-1)
            if (ibid .ne. i) then
                vali(1)=i
                vali(2)=ibid
                call u2mesi('F', 'ASSEMBLA_67', 2, vali)
            endif
40      continue
    endif
    call jedetr(lligr)
!
!
    call uttcpu('CPU.RESO.1', 'FIN', ' ')
    call uttcpu('CPU.RESO.2', 'FIN', ' ')
    call jedema()
end subroutine
