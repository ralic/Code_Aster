subroutine fettsd(infofe, nbi, nbsd, vddl, sdfeti,&
                  colaux, irex, nbi2, ifeti, ifm,&
                  lpara, itps, nivmpi, rang, chsol,&
                  option, ltest)
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
!-----------------------------------------------------------------------
!    - FONCTION REALISEE:  TEST VALIDITE DE SD_FETI OU SORTIES FICHIER
!                          POUR SOULAGER ALFETI.F
!     ------------------------------------------------------------------
!     IN  INFOFE : CH19 : CHAINE DE CHARACTERES POUR MONITORING FETI
!     IN  NBI    : IN   : NBRE DE DDLS D'INTERFACE (SI OPTION=1)
!                         NUMERO DE SD, IDD (SI OPTION=3/4/6/7/8/10)
!     IN  NBSD   : IN   : NBRE DE SOUS-DOMAINES (SI OPTION=1)
!                         NOMBRE DE DDLS, NEQUA (OPTION=3/4/5/7/8/9/10)
!     IN VDDL    : VIN  : VECTEUR DES NBRES DE DDLS DES SOUS-DOMAINES
!     IN SDFETI  : CH19 : SD DECRIVANT LE PARTIONNEMENT FETI
!     IN COLAUX  : K24  : COLLECTION TEMPORAIRE DE REEL (SI OPTION=1)
!                         NOM DU NUM_DDL GLOBAL, NUDEV (OPT=3/4/9/10)
!     IN IREX    : IN   : ADRESSES VECTEURS AUX POUR JEVEUX (SI OPT=1)
!                         ADRESSE .FETN (SI OPT=3/4/6/7) OU .DEEQ (8)
!                         NBRE DE MODE RIGIDE NBMR (SI OPT=10)
!     IN NBI2    : IN   : NBRE DE LAGRANGES D'INTERFACE (SI OPT=1)
!                         ADRESSE .VALE, IADVAL (OPT=3/4/5/6/7/8/9/10)
!     IN IFETI   : IN   : ADRESSE JEVEUX OBJET SDFETI.FETI
!     IN IFM     : IN   : UNITE D'IMPRESSION
!     IN LPARA   : LOG  : .TRUE. SI PARALLELE, .FALSE. SINON
!     IN ITPS    : IN   : INDICE DE PAS DE TEMPS
!     IN NIVMPI  : IN   : NIVEAU D'IMPRESSION MPI
!     IN RANG    : IN   : RANG DU PROCESSEUR
!     IN CHSOL   : CH19 : CHAM_NO SOLUTION GLOBAL
!     IN OPTION  : IN   : OPTION D'UTILISATION DE LA ROUTINE FETTSD
!                   1   --> TEST SD_FETI 1 ET SD_FETI 2 DANS ALFETI
!                   2   --> PREPARATION DES DONNEES POUR OPTION 3/4
!                   3   --> TEST SD_FETI 3 DANS ASSMAM
!                   4   --> TEST SD_FETI 4 DANS ASSVEC
!                   5   --> VERIFICATION SOLUTION PRECEDENTE
!                   6   --> ECRITURE NUME_DDL/MATR_ASSE DANS 18
!                   7   --> ECRITURE NUME_DDL/CHAM_NO RHS DANS 18
!                   8   --> ECRITURE NUME_DDL/CHAM_NO SOL LOCALE DS 18
!                   9   --> IDEM SOLUTION GLOBALE
!                  10   --> ECRITURE NUME_DDL/MODES RIGIDES LOC DS 18
!     IN LTEST   :  LOG  : .TRUE. SI TEST ACTIVE
!----------------------------------------------------------------------
! person_in_charge: mickael.abbas at edf.fr
! CORPS DU PROGRAMME
! aslint: disable=W1304,W1304,W1501
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
    include 'jeveux.h'
!
    include 'asterfort/fetmpi.h'
    include 'asterfort/fetrex.h'
    include 'asterfort/fetsca.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jerazo.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    include 'blas/daxpy.h'
    integer :: nbi, nbsd, vddl(nbsd), irex, nbi2, ifeti, ifm, itps, nivmpi, rang
    integer :: option
    character(len=19) :: sdfeti, chsol
    character(len=24) :: infofe, colaux
    logical :: lpara, ltest
!
!
! DECLARATION VARIABLES LOCALES
    integer :: iret1, iret2, iret3, iret4, idd, nb, i, ibid, ddlhi, multp
    integer :: nbddl1, ddlbi, ilimpi, ddlneg, ifetc, ddlnel, irefe, numl, j
    integer :: compt, iret5, iret, iret6, ifetj, nbii, itest, ifeti1, ino, ifetn
    integer :: ideeq, nequa, iadval, jsmdi, nsmdi, nber, jsmhc, nz, jcol, nbmrma
    integer :: kterm, ilig, nbchar, ifm18, icmp, nberm, nbmr, k, neq, l, ino1
    integer(kind=4) :: nbi4
    real(kind=8) :: raux, rbid, dii, dii2, dii3, tol, ecarmi, ecarma, ecarmo
    character(len=8) :: nomsd, k8bid
    character(len=14) :: k14b
    character(len=24) :: sdfetg, k24b, sdfeth, sdfetl, sdfett, nudev, chref
    character(len=24) :: chfetc, sdfetb, knequg, knequl, k24bs
    logical :: ldeja
!
! CORPS DU PROGRAMME
!
    call jemarq()
    ltest=.false.
    idd=nbi
    nequa=nbsd
    nudev=colaux
    ifetn=irex
    ideeq=irex
    nbmr=irex
    iadval=nbi2
    if ((infofe(12:12).eq.'T') .and. ((option.eq.3).or.(option.eq.4))) then
        call jeveuo('&TEST.DIAG.FETI', 'L', itest)
        call jelira('&TEST.DIAG.FETI', 'LONMAX', nbii, k8bid)
        compt=zi(itest-1+nbii)
    endif
! POUR ECRITURE DANS UN FICHIER DE LA MATRICE, DU SECOND MEMBRE,
! DU NUME_DDL. IFM18 UNITE LOGIQUE ASSOCIEE
    ifm18=18
!-----------------------------
! VALIDATION COHERENCE DE LA SD_FETI
!-----------------------------
    if ((infofe(12:12).eq.'T') .and. (itps.eq.1) .and. (option.eq.1)) then
!
! INIT
        call jeveuo('&FETI.LISTE.SD.MPI', 'L', ilimpi)
        nbi4=nbi
        sdfeth=sdfeti//'.FETH'
        sdfetl=sdfeti//'.FLII'
        sdfett=sdfeti//'.FETI'
        sdfetb=sdfeti//'.FETB'
        sdfetg=sdfeti//'.FETG'
!-----------------------------
! VALIDATION .FETB/.FETG/.FETI VIA FETREX/FETSCA
!-----------------------------
        k24b='&&FETTSD.VERIF1'
        call wkvect(k24b, 'V V R', nbi, iret2)
        call jerazo(k24b, nbi, 1)
        call wkvect('&&FETTSD.VERIF2', 'V V R', nbi, iret3)
        call wkvect('&&FETTSD.VERIF3', 'V V R', nbi, iret4)
!
        k24bs='MULT'
        do 10 idd = 1, nbsd
            if (zi(ilimpi+idd) .eq. 1) then
                nb=vddl(idd)
                call jenuno(jexnum(sdfetg, idd), nomsd)
                call jeveuo(jexnom(colaux, nomsd), 'E', iret1)
                do 8 i = 1, nb
                    zr(iret1+i-1)=1.d0
 8              continue
                call fetrex(1, idd, nb, zr(iret1), nbi,&
                            zr(iret3), irex)
                call fetsca(nbi, zr(iret3), zr(iret4), k24bs, infofe,&
                            nbi2, ifeti, ifm)
                call daxpy(nbi4, 1.d0, zr(iret4), 1, zr(iret2),&
                           1)
            endif
10      continue
        if (lpara) call fetmpi(71, nbi4, ifm, nivmpi, ibid,&
                               ibid, k24b, k24b, k24b, rbid)
        raux=0.d0
        do 12 i = 1, nbi
            raux=raux+zr(iret2+i-1)
12      continue
!
        call u2mess('E', 'ALGELINE5_5')
        if (rang .eq. 0) then
            write(ifm,*)
            write(ifm,*)'TEST SD_FETI 1 (IL DOIT ETRE EGAL A 0 !) ',&
            raux
            write(ifm,*)
        endif
! POUR MONITORING EVENTUEL
!      CALL JEIMPO(6,'&&ALFETI.VERIF1',' ','')
!
        call jedetr(k24b)
        call jedetr('&&FETTSD.VERIF2')
        call jedetr('&&FETTSD.VERIF3')
!
!-----------------------------
! VALIDATION .FETB/.FETH/.FLII
!-----------------------------
!
! LE JUGE DE PAIX, LE NOMBRE DE DDLS TOTAL VIA LE CHAM_NO GLOBAL
        chref=chsol//'.REFE'
        call jeveuo(chref, 'L', irefe)
        knequg=zk24(irefe+1)(1:19)//'.NEQU'
        call jeveuo(knequg, 'L', iret1)
        ddlneg=zi(iret1)
!
! NOMBRE DE DDLS (PHYSIQUES+TARDIFS) MENTIONNES DANS LES CHAM_NOS LOCAUX
        ddlnel=0
        chfetc=chsol//'.FETC'
        call jeveuo(chfetc, 'L', ifetc)
        do 18 idd = 1, nbsd
            if (zi(ilimpi+idd) .eq. 1) then
                chref=zk24(ifetc+idd-1)(1:19)//'.REFE'
                call jeveuo(chref, 'L', irefe)
                knequl=zk24(irefe+1)(1:19)//'.NEQU'
                call jeveuo(knequl, 'L', iret1)
                ddlnel=ddlnel+zi(iret1)
            endif
18      continue
        if (lpara) then
            k24b='&&FETTSD.VERIF6'
            call wkvect(k24b, 'V V I', 1, iret6)
            zi(iret6)=ddlnel
            call fetmpi(6, 1, ifm, nivmpi, ibid,&
                        ibid, k24b, k24b, k24b, rbid)
            ddlnel=zi(iret6)
            call jedetr(k24b)
        endif
! NOMBRE DE DDLS (PHYSIQUES+TARDIFS) MENTIONNES DANS .FETH
        ddlhi=0
        call jeveuo(sdfeth, 'L', iret1)
! ON CUMUL CEUX DU .FETH
        do 20 idd = 1, nbsd
            ddlhi=ddlhi+zi(iret1+idd-1)
20      continue
! IDEM AVEC LES DDLS (PHYSIQUES+TARDIFS) DE .FETB/.FLII
        ddlbi=0
        do 30 idd = 1, nbsd
            call jenuno(jexnum(sdfetb, idd), nomsd)
            call jeveuo(jexnom(sdfetb, nomsd), 'L', iret3)
            call jelira(jexnom(sdfetb, nomsd), 'LONMAX', nb, k8bid)
            nb=nb/2
            do 25 i = 1, nb
! ON NE COMPTE QUE LES NOEUDS PHYSIQUES HORS INTERFACE
                if (zi(iret3-1+2*(i-1)+1) .gt. 0) then
                    if (i .eq. 1) then
                        nbddl1=zi(iret3-1+2*(i-1)+2)
                    else
                        nbddl1=zi(iret3-1+2*(i-1)+2)-zi(iret3-1+2*(i-&
                        2)+2)
                    endif
                    ddlbi=ddlbi+nbddl1
                endif
25          continue
!
            call jeexin(jexnom(sdfetl, nomsd), iret)
            if (iret .ne. 0) then
                call jeveuo(jexnom(sdfetl, nomsd), 'L', iret4)
                call jelira(jexnom(sdfetl, nomsd), 'LONMAX', nb, k8bid)
                nb=nb/2
! ON SUPPOSE QUE CHAQUE MAILLE TARDIVE CORRESPOND A DEUX LAGRANGES ET
! QU'ILS NE SONT PAS SUR L'INTERFACE !
                do 27 i = 1, nb
                    ddlbi=ddlbi+2*zi(iret4-1+2*(i-1)+2)
27              continue
            endif
30      continue
! ON LEUR ENLEVE LES DOUBLONS DU AUX DDLS PHYSIQUES D'INTERFACE .FETI
        k24b='&&FETTSD.VERIF5'
        call wkvect(k24b, 'V V I', nbi2, iret5)
        call jerazo(k24b, nbi2, 1)
        call jeveuo(sdfett, 'L', iret2)
        compt=0
        do 35 i = 1, nbi2
            if (i .eq. 1) then
                nbddl1=zi(iret2-1+4*(i-1)+3)
            else
                nbddl1=zi(iret2-1+4*(i-1)+3)-zi(iret2-1+4*(i-2)+3)
            endif
            multp=zi(iret2-1+4*(i-1)+2)
            if (multp .ge. 3) then
                numl=zi(iret2-1+4*(i-1)+1)
                ldeja=.false.
                do 32 j = 1, compt
                    if (numl .eq. zi(iret5-1+j)) ldeja=.true.
32              continue
                if (.not.ldeja) then
                    compt=compt+1
                    zi(iret5-1+compt)=numl
                    ddlbi=ddlbi+nbddl1
                    ddlhi=ddlhi-nbddl1*(multp-1)
                    ddlnel=ddlnel-nbddl1*(multp-1)
                endif
            else
                ddlbi=ddlbi+nbddl1
                ddlhi=ddlhi-nbddl1
                ddlnel=ddlnel-nbddl1
            endif
35      continue
        if (rang .eq. 0) then
            write(ifm,*)
            write(ifm,*)'TEST SD_FETI 2'
            write(ifm,*)'LES ECARTS SUIVANTS DOIVENT ETRE NULS !'
            write(ifm,*)'!! ATTENTION LE TROISIEME NE TIENT PAS COMPTE '
            write(ifm,*)'D''EVENTUELS LAGRANGES SUR L''INTERFACE,'
            write(ifm,*)' DE LIAISON_* DES FORCES NODALES !!'
            write(ifm,*)'--> CHAM_NOS LOCAUX + .FETI      ',ddlnel-ddlneg
            write(ifm,*)'--> .FETH           + .FETI      ',ddlhi-ddlneg
            write(ifm,*)'--> .FETB + .FLII   + .FETI      ',ddlbi-ddlneg
            write(ifm,*)
        endif
        call jedetr(k24b)
!
!-----------------------------
! PREPARATION DU TERRAIN (VECTEUR &TEST.DIAG.FETI) POUR
! INSERER LA RESOLUTION DU SYSTEME DIAGONAL
! DIAG(1...1)*U=(1...1)T
! DONT LA SOLUTION TRIVIALE EST U=(1....1)T
!-----------------------------
    else if ((infofe(12:12).eq.'T').and.(option.eq.2)) then
        call jeexin('&TEST.DIAG.FETI', iret1)
        if (iret1 .eq. 0) then
            call jeveuo(sdfeti(1:19)//'.FETI', 'L', ifeti1)
            call jeveuo(sdfeti(1:19)//'.FETJ', 'L', ifetj)
            call jelira(sdfeti(1:19)//'.FETI', 'LONMAX', nbii, k8bid)
            nbii=nbii/4
            call wkvect('&TEST.DIAG.FETI', 'V V I', 3*nbii+1, itest)
            compt=0
            do 38 i = 1, nbii
                ino=zi(ifeti1+4*(i-1))
                do 36 j = 1, compt
                    if (zi(itest+3*(j-1)) .eq. ino) goto 37
36              continue
                compt=compt+1
                zi(itest+3*(compt-1))=ino
                zi(itest+3*(compt-1)+1)=zi(ifeti1+4*(i-1)+1)
                zi(itest+3*(compt-1)+2)=zi(ifetj-1+zi(ifeti1+4*(i-1)+&
                3))
37              continue
38          continue
            zi(itest+3*nbii)=compt
        endif
!-----------------------------
! ON N'ASSEMBLE PAS LA MATRICE ET ON LA REMPLACE PAR DIAG(1...1)
!-----------------------------
    else if ((infofe(12:12).eq.'T').and.(option.eq.3)) then
        ltest=.true.
        nbmrma=0
        k14b=zk24(ifetn+idd-1)(1:14)
        call jeveuo(k14b//'.NUME.DEEQ', 'L', ideeq)
        call jeveuo(k14b//'.SMOS.SMDI', 'L', jsmdi)
        call jelira(k14b//'.SMOS.SMDI', 'LONMAX', nsmdi, k8bid)
        call jeveuo(k14b//'.SMOS.SMHC', 'L', jsmhc)
        nz=zi(jsmdi-1+nsmdi)
        jcol=1
        nbmr=0
        do 60 kterm = 1, nz
            if (zi(jsmdi-1+jcol) .lt. kterm) jcol=jcol+1
            ilig=zi4(jsmhc-1+kterm)
            if (ilig .eq. jcol) then
                ino=zi(ideeq+2*(ilig-1))
!            DII=1.D0*INO
                dii=1.d0
                dii2=1.d0
                if (idd .ne. 0) then
                    do 58 i = 1, compt
                        if (zi(itest+3*(i-1)) .eq. ino) then
                            if (nbmr .lt. nbmrma) then
                                dii3=0.d0
                                nbmr=nbmr+1
                            else
                                dii3=1.d-5
                            endif
                            if (idd .eq. zi(itest+3*(i-1)+2)) then
                                dii=1.d0
!                    DII2=INO*1.D0-((ZI(ITEST+3*(I-1)+1)-1)*DII3)
                                dii2=1.d0-((zi(itest+3*(i-1)+1)-1)*&
                                dii3)
                            else
                                dii=1.d0
                                dii2=dii3
                            endif
                            goto 59
                        endif
58                  continue
59                  continue
                endif
                zr(iadval-1+kterm)=dii*dii2
            else
                zr(iadval-1+kterm)=0.d0
            endif
60      continue
!-----------------------------
! ON N'ASSEMBLE PAS LE SECOND MEMBRE ET ON LE REMPLACE PAR
! (1...1)T / NBRE DE SECOND MEMBRE (POUR FAIRE 1)
!-----------------------------
    else if ((infofe(12:12).eq.'T').and.(option.eq.4)) then
        call jelira(sdfeti(1:19)//'.FREF', 'LONMAX', nbchar, k8bid)
        nbchar=nbchar-1
        ltest=.true.
        if (idd .eq. 0) then
            k14b=nudev(1:14)
        else
            k14b=zk24(ifetn+idd-1)(1:14)
        endif
        call jeveuo(k14b//'.NUME.DEEQ', 'L', ideeq)
        do 50 i = 1, nequa
            ino=zi(ideeq+2*(i-1))
!          DII=1.D0*INO/NBCHAR
            dii=1.d0/nbchar
            dii2=1.d0
            if (idd .ne. 0) then
                do 48 j = 1, compt
                    if (zi(itest+3*(j-1)) .eq. ino) then
                        dii2=1.d0/zi(itest+3*(j-1)+1)
                        goto 49
                    endif
48              continue
49              continue
            endif
            zr(iadval+i-1)=dii*dii2
50      continue
!-----------------------------
! VERIFICATION DE U=(1....1)T DANS FETRIN
!-----------------------------
    else if ((infofe(12:12).eq.'T').and.(option.eq.5)) then
        call jeveuo(nudev(1:14)//'.NUME.DEEQ', 'L', ideeq)
! TOLERANCE DU TEST/ NBRE DE VALEURS AFFICHEES
        tol=1.d-6
        nberm=5
        write(k8bid,'(I6)')nberm
        nber=0
        raux=0.d0
        ecarmi=1.d6
        ecarmo=0.d0
        ecarma=0.d0
        do 70 i = 1, nequa
            icmp=zi(ideeq+2*(i-1)+1)
! ON NE S'INTERESSE QU'AUX COMPOSANTES PHYSIQUES
            if (icmp .gt. 0) then
                raux=abs(zr(iadval-1+i)-1.d0)
                ecarmo=ecarmo+raux
                if (raux .gt. ecarma) ecarma=raux
                if (raux .lt. ecarmi) ecarmi=raux
                if (raux .gt. tol) then
                    ino=zi(ideeq+2*(i-1))
                    if (nber .lt. nberm) then
                        write(6,*)'TESTDIAG ERREUR U',i,' = ',zr(&
                        iadval-1+i)
                        write(6,*)'INO/ICMP = ',ino,icmp
                    else if (nber.eq.nberm) then
                        write(6,*)'!! AFFICHAGE LIMITE A '//k8bid//' VALEURS !!'
                    endif
                    nber=nber+1
                endif
            endif
70      continue
        write(ifm,*)
        write(ifm,*)'TEST SD_FETI + ALGORITHME 3'
        write(ifm,*)'ECART MIN A LA SOLUTION',ecarmi
        write(ifm,*)'ECART MOYEN A LA SOLUTION',ecarmo/nequa
        write(ifm,*)'ECART MAX A LA SOLUTION',ecarma
        write(ifm,*)'NOMBRE D''ERREUR A TOLERANCE ',tol,' = ',nber
        write(ifm,*)
!-----------------------------
! ON ECRIT DANS IFM18 LA MATRICE LOCALE
!-----------------------------
        else if (((infofe(14:14).eq.'T').or.(infofe(15:15).eq.'T'))&
    .and.(option.eq.6).and.(idd.ne.0)) then
        k14b=zk24(ifetn+idd-1)(1:14)
        call jeveuo(k14b//'.SMOS.SMDI', 'L', jsmdi)
        call jelira(k14b//'.SMOS.SMDI', 'LONMAX', nsmdi, k8bid)
        call jeveuo(k14b//'.SMOS.SMHC', 'L', jsmhc)
        nz=zi(jsmdi-1+nsmdi)
        jcol=1
        write(ifm18,*)'MATRICE DU SOUS-DOMAINE ',idd,'I/J/KIJ'
        write(ifm18,*)'TAILLE DU PB/NOMBRE DE TERMES ',nsmdi,nz
        do 80 kterm = 1, nz
            if (zi(jsmdi-1+jcol) .lt. kterm) jcol=jcol+1
            ilig=zi4(jsmhc-1+kterm)
            write(ifm18,*)ilig,jcol,zr(iadval-1+kterm)
80      continue
!-----------------------------
! ON ECRIT DANS IFM18 LE SECOND MEMBRE LOCALE
!-----------------------------
        else if ((infofe(14:14).eq.'T').and.(option.eq.7).and. (idd.ne.0))&
    then
        write(ifm18,*)'SECOND MEMBRE ',idd,&
     &    ' I/NUM_NOEUD (<0 SI LAGR)/NUM_COMPOSANTE (<0 SI LAGR)/FI'
        write(ifm18,*)'NOMBRE DE TERMES ',nequa
        do 90 i = 1, nequa
            ino=zi(ideeq+2*(i-1))
            icmp=zi(ideeq+2*(i-1)+1)
            write(ifm18,*)i,ino,icmp,zr(iadval-1+i)
90      continue
!-----------------------------
! ON ECRIT DANS IFM18 LA SOLUTION LOCALE
!-----------------------------
    else if ((infofe(14:14).eq.'T').and.(option.eq.8)) then
        write(ifm18,*)'SOLUTION LOCALE ',idd,&
     &    ' I/NUM_NOEUD (<0 SI LAGR)/NUM_COMPOSANTE (<0 SI LAGR)/ULOCI'
        write(ifm18,*)'NOMBRE DE TERMES ',nequa
        do 91 i = 1, nequa
            ino=zi(ideeq+2*(i-1))
            icmp=zi(ideeq+2*(i-1)+1)
            write(ifm18,*)i,ino,icmp,zr(iadval-1+i)
91      continue
!-----------------------------
! ON ECRIT DANS IFM18 LA DESCRIPTION DES LAGRANGE D'INTERFACE
!-----------------------------
    else if ((infofe(14:14).eq.'T').and.(option.eq.11)) then
        call jeveuo(sdfeti(1:19)//'.FETI', 'L', ifeti1)
        call jelira(sdfeti(1:19)//'.FETI', 'LONMAX', nbii, k8bid)
        nbii=nbii/4
        compt=0
        do 97 i = 1, nequa
            ino=zi(ideeq+2*(i-1))
            icmp=zi(ideeq+2*(i-1)+1)
            if (icmp .gt. 0) then
                do 95 k = 1, nbii
                    ino1=zi(ifeti1+4*(k-1)-1+1)
                    if (ino .eq. ino1) then
                        compt=compt+1
                        goto 97
                    endif
95              continue
            endif
97      continue
        write(ifm18,*)'LAGRANGE D''INTERFACE '//&
     &    ' NOEUD_GLOBAL  /  NUM_SD  /  NUM_DDL_LOCAL / CMP'
        write(ifm18,*)'NOMBRE DE TERMES ',compt
        do 93 i = 1, nequa
            ino=zi(ideeq+2*(i-1))
            icmp=zi(ideeq+2*(i-1)+1)
            if (icmp .gt. 0) then
                do 94 k = 1, nbii
                    ino1=zi(ifeti1+4*(k-1)-1+1)
                    if (ino .eq. ino1) then
                        write(ifm18,*) ino, idd, i, icmp
                        goto 93
                    endif
94              continue
            endif
93      continue
!-----------------------------
! ON ECRIT DANS IFM18 LA SOLUTION GLOBALE
!-----------------------------
    else if ((infofe(14:14).eq.'T').and.(option.eq.9)) then
        k14b=nudev(1:14)
        call jeveuo(k14b//'.NUME.DEEQ', 'L', ideeq)
        write(ifm18,*)'SOLUTION GLOBALE '//&
     &    ' I/NUM_NOEUD (<0 SI LAGR)/NUM_COMPOSANTE (<0 SI LAGR)/UGLOI'
        write(ifm18,*)'NOMBRE DE TERMES ',nequa
        do 92 i = 1, nequa
            ino=zi(ideeq+2*(i-1))
            icmp=zi(ideeq+2*(i-1)+1)
            write(ifm18,*)i,ino,icmp,zr(iadval-1+i)
92      continue
!-----------------------------
! ON ECRIT DANS IFM18 UN MODE RIGIDE
!-----------------------------
    else if ((infofe(15:15).eq.'T').and.(option.eq.10)) then
        k14b=nudev(1:14)
        call jeveuo(k14b//'.NUME.DEEQ', 'L', ideeq)
        write(ifm18,*)'SOUS-DOMAINE/NBRE MODE RIGIDE ',idd,nbmr,&
        ' I/J/NUM_NOEUD (<0 SI LAGR)/NUM_COMPOSANTE (<0 SI LAGR)/UIJ'
        if (nbmr .gt. 0) then
            write(ifm18,*)'NOMBRE TOTAL DE TERMES ',nequa
            neq=nequa/nbmr
            k=1
            l=1
            do 96 i = 1, nequa
                ino=zi(ideeq+2*(k-1))
                icmp=zi(ideeq+2*(k-1)+1)
                write(ifm18,*)l,k,ino,icmp,zr(iadval-1+i)
                if (k .lt. neq) then
                    k=k+1
                else
                    k=1
                    l=l+1
                endif
96          continue
        else
            write(ifm18,*)'NOMBRE TOTAL DE TERMES ',0
        endif
    endif
!
    call jedema()
end subroutine
