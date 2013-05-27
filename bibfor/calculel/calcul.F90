subroutine calcul(stop, optio, ligrlz, nin, lchin,&
                  lpain, nou, lchou, lpaou, base,&
                  mpic)
    implicit none
!
! ======================================================================
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
! TOLE CRP_20 CRS_1404
!     ARGUMENTS:
!     ----------
    include 'jeveux.h'
!
    include 'asterc/getres.h'
    include 'asterc/getvli.h'
    include 'asterc/indik8.h'
    include 'asterfort/alchlo.h'
    include 'asterfort/alrslt.h'
    include 'asterfort/assert.h'
    include 'asterfort/caldbg.h'
    include 'asterfort/caundf.h'
    include 'asterfort/debca1.h'
    include 'asterfort/debcal.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/exisd.h'
    include 'asterfort/extrai.h'
    include 'asterfort/fetmpi.h'
    include 'asterfort/infniv.h'
    include 'asterfort/inigrl.h'
    include 'asterfort/inpara.h'
    include 'asterfort/iunifi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/kndoub.h'
    include 'asterfort/montee.h'
    include 'asterfort/mpicm0.h'
    include 'asterfort/mpicm1.h'
    include 'asterfort/nbelem.h'
    include 'asterfort/nbgrel.h'
    include 'asterfort/nucalc.h'
    include 'asterfort/sdmpic.h'
    include 'asterfort/te0000.h'
    include 'asterfort/typele.h'
    include 'asterfort/u2mesi.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/utimsd.h'
    include 'asterfort/uttcpr.h'
    include 'asterfort/uttcpu.h'
    include 'asterfort/vrcdec.h'
    include 'asterfort/wkvect.h'
    include 'asterfort/zechlo.h'
    integer :: nin, nou
    character(len=*) :: base, optio
    character(len=*) :: lchin(*), lchou(*), lpain(*), lpaou(*), ligrlz, mpic
! ----------------------------------------------------------------------
!     ENTREES:
!        STOP   :  /'S' : ON S'ARRETE SI AUCUN ELEMENT FINI DU LIGREL
!                         NE SAIT CALCULER L'OPTION.
!                  /'C' : ON CONTINUE SI AUCUN ELEMENT FINI DU LIGREL
!                         NE SAIT CALCULER L'OPTION. IL N'EXISTE PAS DE
!                         CHAMP "OUT" DANS CE CAS.
!        OPTIO  :  NOM D'1 OPTION
!        LIGRLZ :  NOM DU LIGREL SUR LEQUEL ON DOIT FAIRE LE CALCUL
!        NIN    :  NOMBRE DE CHAMPS PARAMETRES "IN"
!        NOU    :  NOMBRE DE CHAMPS PARAMETRES "OUT"
!        LCHIN  :  LISTE DES NOMS DES CHAMPS "IN"
!        LCHOU  :  LISTE DES NOMS DES CHAMPS "OUT"
!        LPAIN  :  LISTE DES NOMS DES PARAMETRES "IN"
!        LPAOU  :  LISTE DES NOMS DES PARAMETRES "OUT"
!        BASE   :  'G' , 'V' OU 'L'
!        MPIC   :  'OUI' / 'NON' :
!                  'OUI' : SI LE CALCUL EST DISTRIBUE, ON "COMPLETE" LES
!                          CHAM_ELEM "OUT" POUR QUE LE CHAMP SOIT LE
!                          MEME SUR TOUS LES PROCESSEURS.
!
!     SORTIES:
!       ALLOCATION ET CALCUL DES OBJETS CORRESPONDANT AUX CHAMPS "OUT"
!
! ----------------------------------------------------------------------
    common /caii02/iaoptt,lgco,iaopmo,ilopmo,iaopno,ilopno,iaopds,&
     &       iaoppa,npario,nparin,iamloc,ilmloc,iadsgd
    common /caii03/iamaco,ilmaco,iamsco,ilmsco,ialiel,illiel
    common /caii04/iachii,iachik,iachix
    common /caii05/ianoop,ianote,nbobtr,iaobtr,nbobmx
    character(len=16) :: option, nomte, nomtm, pheno, modeli
    common /cakk01/option,nomte,nomtm,pheno,modeli
    integer :: nbgr, igr, nbelgr, jcteat, lcteat, iawloc, iawlo2, iawtyp
    common /caii06/nbgr,igr,nbelgr,jcteat,lcteat,iawloc,iawlo2,iawtyp
    integer :: nbobj, iainel, ininel
    common /caii09/nbobj,iainel,ininel
    integer :: nute, jnbelr, jnoelr, iactif, jpnlfp, jnolfp, nblfpg
    common /caii11/nute,jnbelr,jnoelr,iactif,jpnlfp,jnolfp,nblfpg
!
!
    integer :: evfini, calvoi, jrepe, jptvoi, jelvoi
    common /caii19/evfini,calvoi,jrepe,jptvoi,jelvoi
!-----------------------------------------------------------------------
    logical :: lfetmo, lfetts, lfettd, ldist, lfeti, lfetic, dbg, ldgrel
    real(kind=8) :: rbid, temp1(6), temp2(6)
    complex(kind=8) :: cbid
    character(len=8) :: lpain2(nin), lpaou2(nou)
    character(len=19) :: lchin2(nin), lchou2(nou)
    character(len=19) :: ligrel
    character(len=24) :: k24b, infofe, valk(2), kfel
    character(len=1) :: stop
    integer :: iachii, iachik, iachix, iadsgd, ibid, nbproc, ifeti1, jparal
    integer :: ialiel, iamaco, iamloc, iamsco, ianoop, ianote, iaobtr, idd
    integer :: iaopds, iaopmo, iaopno, iaoppa, iaoptt, ima, ifcpu, rang, ifm
    integer :: niv
    integer :: ier, illiel, ilmaco, ilmloc, ilmsco, ilopmo, iinf, iret1, ifel1
    integer :: ilopno, iret, iuncod, j, lgco, ifel2, iret2, ilimpi
    integer :: npario, nbobmx, nparin, nbsd, jnumsd, n1
    integer :: vali(4)
    integer :: nbobtr, nval
    character(len=32) :: phemod
    integer :: opt, afaire
    integer :: iel, numc, numail
    integer :: i, ipar, nin2, nin3, nou2, nou3, jtypma, jprti, jprtk
    character(len=1) :: base2, kbid
    character(len=8) :: nompar, cas, exiele, k8bid, partit, tych
    character(len=10) :: k10b
    character(len=16) :: k16bid, cmde
    character(len=20) :: k20b1, k20b2, k20b3, k20b4
!
!
!     -- FONCTIONS FORMULES :
!     NUMAIL(IGR,IEL)=NUMERO DE LA MAILLE ASSOCIEE A L'ELEMENT IEL
    numail(igr,iel)=zi(ialiel-1+zi(illiel-1+igr)-1+iel)
!
! DEB-------------------------------------------------------------------
!
    call jemarq()
!
!     -- POUR QUE LES MESURES DE TEMPS EN // SOIENT COMPREHENSIBLES
!        PAR LES UTILISATEURS, IL FAUT FORCER UNE SYNCHRO AVANT LES
!        MESURES :
    call mpicm1('BARRIER', ' ', ibid, ibid, ibid,&
                rbid, cbid)
    call uttcpu('CPU.CALC.1', 'DEBUT', ' ')
    call uttcpu('CPU.CALC.2', 'DEBUT', ' ')
!
    call infniv(ifm, niv)
    ligrel=ligrlz
    iactif=1
    nbobtr=0
    base2=base
    option=optio
    call assert(mpic.eq.'OUI'.or.mpic.eq.'NON')
    call assert(stop.eq.'S'.or.stop.eq.'C')
!
    dbg=.false.
    lfetmo=.false.
    lfetts=.false.
    lfettd=.false.
    lfetic=.false.
!
!
!
!     -- S'IL N'Y A PAS D'ELEMENTS FINIS, ON SORT :
!     ----------------------------------------------
    call dismoi('F', 'EXI_ELEM', ligrel, 'LIGREL', ibid,&
                exiele, ibid)
    if (exiele .ne. 'OUI') then
        if (stop .eq. 'S') then
            call u2mesk('F', 'CALCULEL2_25', 1, ligrel)
        else
            goto 120
        endif
    endif
!
!
!     -- DEBCA1 MET CERTAINS OBJETS EN MEMOIRE (ET EN COMMON):
!     -----------------------------------------------------------------
    call debca1(option, ligrel, nin)
!
    call jeveuo('&CATA.TE.TYPEMA', 'L', jtypma)
    call jenonu(jexnom('&CATA.OP.NOMOPT', option), opt)
!
!     -- POUR SAVOIR L'UNITE LOGIQUE OU ECRIRE LE FICHIER ".CODE" :
    call getvli(cas)
    iuncod = iunifi('CODE')
    if (iuncod .gt. 0) call getres(k8bid, k16bid, cmde)
!
!
!     0.1- CAS D'UN CALCUL "FETI" :
!          CALCUL DE : LFETMO,LFETTS,LFETTD,LFETIC,INFOFE
!     -----------------------------------------------------------------
    infofe='FFFFFFFFFFFFFFFFFFFFFFFF'
    call jeexin('&FETI.MAILLE.NUMSD', iret)
    lfeti=(iret.gt.0)
    if (lfeti) then
!       - CALCUL DU RANG ET DU NBRE DE PROC
        call fetmpi(2, ibid, ibid, 1, rang,&
                    ibid, k24b, k24b, k24b, rbid)
        call fetmpi(3, ibid, ibid, 1, ibid,&
                    nbproc, k24b, k24b, k24b, rbid)
!       - ON PROFILE OU PAS ?
        call jeveuo('&FETI.FINF', 'L', iinf)
        infofe=zk24(iinf)
        if (infofe(11:11) .eq. 'T') then
            lfetic=.true.
            call uttcpr('CPU.CALC.2', 6, temp1)
        endif
!
!       - SI PARALLELISME ON VA "DISTRIBUER" LES CALCULS ELEMENTAIRES :
        if (nbproc .gt. 1) then
            if (ligrel(9:15) .eq. '.MODELE') then
!         -    FETI PARALLELE SUR LIGREL DE MODELE
!         -    ON VA DONC TRIER PAR MAILLE PHYSIQUE
                lfetmo=.true.
                call jeveuo('&FETI.MAILLE.NUMSD', 'L', ifeti1)
                ifeti1=ifeti1-1
            else
!         -    PROBABLEMENT FETI PARALLELE SUR LIGREL TARDIF
!         -    DANS LE DOUTE, ON S'ABSTIENT ET ON FAIT TOUT
                kfel=ligrel(1:19)//'.FEL1'
                call jeexin(kfel, iret1)
                if (iret1 .ne. 0) then
!         -   LIGREL A MAILLES TARDIVES
                    call jelira(kfel, 'LONMAX', nbsd, k8bid)
                    call jeveuo(kfel, 'L', ifel1)
                    call jeveuo('&FETI.LISTE.SD.MPI', 'L', ilimpi)
                    do 20 idd = 1, nbsd
                        if (zi(ilimpi+idd) .eq. 1) then
!         -   LE SOUS-DOMAINE IDD EST CONCERNE PAR CE PROC
                            if (zk24(ifel1+idd-1)(1:19) .eq. ligrel(1: 19)) then
!         -   LIGREL TARDIF CONCENTRE SUR LE SOUS DOMAINE IDD
!         -   IL FAUT TOUT FAIRE
                                lfetts=.true.
                                elseif (zk24(ifel1+idd-1)(1:19).ne.' ')&
                            then
!         -   LIGREL TARDIF DUPLIQUE, NOTAMMENT, SUR LE SOUS DOMAINE IDD
!             POINTE PAR UN .FEL2 (AUTRE QUE LIGREL DE CONTACT INIT)
                                kfel=ligrel(1:19)//'.FEL2'
                                call jeexin(kfel, iret2)
                                if (iret2 .ne. 0) then
!         -   ON VA TRIER PAR MAILLE TARDIVE, SINON ON FAIT TOUT PAR
!         -   PRUDENCE
                                    lfettd=.true.
                                    call jeveuo(kfel, 'L', ifel2)
                                endif
                            endif
                        endif
20                  continue
                endif
            endif
            if ((lfetmo.and.lfetts) .or. (lfetmo.and.lfettd) .or. (lfetts.and.lfettd)) &
            call u2mess('F', 'CALCULEL6_75')
        endif
!
!       -- MONITORING FETI :
        if (infofe(1:1) .eq. 'T') then
            write (ifm,*)'<FETI/CALCUL> RANG ',rang
            write (ifm,*)'<FETI/CALCUL> LIGREL/OPTION ',ligrel,' ',&
            option
            if (lfetmo) then
                write (ifm,*)'<FETI/CALCUL> LIGREL DE MODELE'
            else if (lfetts) then
                write (ifm,*)'<FETI/CALCUL> LIGREL TARDIF NON DUPLIQUE'
            else if (lfettd) then
                write (ifm,*)'<FETI/CALCUL> LIGREL TARDIF DUPLIQUE'
            else
                if (nbproc .gt. 1) write (ifm,*)'<FETI/CALCUL> AUTRE LIGREL'
            endif
        endif
    endif
!
!
!     0.2- CAS D'UN CALCUL "DISTRIBUE" :
!     -- CALCUL DE LDIST :
!          .TRUE.  : LES CALCULS ELEMENTAIRES SONT DISTRIBUES (PAS FETI)
!          .FALSE. : SINON
!     -- CALCUL DE LDGREL :
!          .TRUE.  : LES CALCULS ELEMENTAIRES SONT DISTRIBUES PAR GREL
!                    IGREL -> RANG=MOD(LIGREL,NBPROC)
!     -- SI LDIST  == .TRUE. : CALCUL DE  RANG, NBPROC, [JNUMSD]
!     -------------------------------------------------------------
    ldist=.false.
    ldgrel=.false.
    call dismoi('F', 'PARTITION', ligrel, 'LIGREL', ibid,&
                partit, ibid)
    call jeexin(partit//'.PRTK', iret)
    if ((iret.ne.0) .and. (.not.lfeti)) then
        ldist=.true.
        call mpicm0(rang, nbproc)
        call jeveuo(partit//'.PRTI', 'L', jprti)
        if (zi(jprti) .ne. nbproc) then
            vali(1)=zi(jprti)
            vali(2)=nbproc
            call u2mesi('F', 'CALCULEL_13', 2, vali)
        endif
!
        call jeveuo(partit//'.PRTK', 'L', jprtk)
        ldgrel=zk24(jprtk-1+1).eq.'GROUP_ELEM'
        if (.not.ldgrel) then
            call jeveuo(partit//'.NUPROC.MAILLE', 'L', jnumsd)
            call jelira(partit//'.NUPROC.MAILLE', 'LONMAX', n1, kbid)
        endif
    endif
!
!
!
!     1- SI AUCUN TYPE_ELEMENT DU LIGREL NE SAIT CALCULER L'OPTION,
!     -- ON VA DIRECTEMENT A LA SORTIE :
!     -------------------------------------------------------------
    afaire=0
    ier=0
    nbgr=nbgrel(ligrel)
    do 30,j=1,nbgr
    nute=typele(ligrel,j)
    call jenuno(jexnum('&CATA.TE.NOMTE', nute), nomte)
    nomtm=zk8(jtypma-1+nute)
    numc=nucalc(opt,nute,0)
!
!        -- SI LE NUMERO DU TEOOIJ EST NEGATIF :
    if (numc .lt. 0) then
        valk(1)=nomte
        valk(2)=option
        if (numc .eq. -1) then
            call u2mesk('F', 'CALCULEL_30', 2, valk)
        else
            call assert(.false.)
        endif
    endif
!
    afaire=max(afaire,numc)
    30 end do
    call assert(ier.le.0)
    if (afaire .eq. 0) then
        if (stop .eq. 'S') then
            call u2mesk('F', 'CALCULEL_34', 1, option)
        else
            goto 120
!
        endif
    endif
!
!     2- ON REND PROPRES LES LISTES : LPAIN,LCHIN,LPAOU,LCHOU :
!        EN NE GARDANT QUE LES PARAMETRES DU CATALOGUE DE L'OPTION
!        QUI SERVENT A AU MOINS UN TYPE_ELEMENT
!     ---------------------------------------------------------
!     TEST SUR ERREUR PROGRAMMEUR : TROP DE CHAMPS "IN"
    call assert(nin.le.80)
    nin3=zi(iaopds-1+2)
    nou3=zi(iaopds-1+3)
!
    nin2=0
    do 50,i=1,nin
    nompar=lpain(i)
    ipar=indik8(zk8(iaoppa),nompar,1,nin3)
    if (ipar .gt. 0) then
        do 40,j=1,nbgr
        nute=typele(ligrel,j)
        ipar=inpara(opt,nute,'IN ',nompar)
!
        if (ipar .eq. 0) goto 40
        call exisd('CHAMP_GD', lchin(i), iret)
        if (iret .eq. 0) goto 40
        nin2=nin2+1
        lpain2(nin2)=lpain(i)
        lchin2(nin2)=lchin(i)
        goto 50
!
40      continue
    endif
    50 end do
!
!     -- VERIF PAS DE DOUBLONS DANS LPAIN2 :
    call kndoub(8, lpain2, nin2, iret)
    call assert(iret.eq.0)
!
    nou2=0
    do 70,i=1,nou
    nompar=lpaou(i)
    ipar=indik8(zk8(iaoppa+nin3),nompar,1,nou3)
    if (ipar .gt. 0) then
        do 60,j=1,nbgr
        nute=typele(ligrel,j)
        ipar=inpara(opt,nute,'OUT',nompar)
!
        if (ipar .eq. 0) goto 60
        nou2=nou2+1
        lpaou2(nou2)=lpaou(i)
        lchou2(nou2)=lchou(i)
!           -- ON INTERDIT LA CREATION DU CHAMP ' ' :
        call assert(lchou2(nou2).ne.' ')
        goto 70
!
60      continue
    endif
    70 end do
!     -- VERIF PAS DE DOUBLONS DANS LPAOU2 :
    call kndoub(8, lpaou2, nou2, iret)
    call assert(iret.eq.0)
!
!     3- DEBCAL FAIT DES INITIALISATIONS ET MET LES OBJETS EN MEMOIRE :
!     -----------------------------------------------------------------
    call debcal(option, ligrel, nin2, lchin2, lpain2,&
                nou2, lchou2)
    if (dbg) call caldbg('IN', nin2, lchin2, lpain2)
!
!     4- ALLOCATION DES RESULTATS ET DES CHAMPS LOCAUX:
!     -------------------------------------------------
    call alrslt(opt, ligrel, nou2, lchou2, lpaou2,&
                base2, ldist, lfeti)
    call alchlo(opt, ligrel, nin2, lpain2, lchin2,&
                nou2, lpaou2)
!
!     5- AVANT BOUCLE SUR LES GREL :
!     QUELQUES ACTIONS HORS BOUCLE GREL DUES A CALVOI==1 :
!     -----------------------------------------------------
    call extrai(nin2, lchin2, lpain2, opt, nute,&
                ligrel, 'INIT')
!
!     6- BOUCLE SUR LES GREL :
!     -------------------------------------------------
    do 100 igr = 1, nbgr
!
!       -- SI PARALLELISME='GROUP_ELEM' : ON PEUT PARFOIS TOUT "SAUTER"
        if (ldgrel .and. mod(igr,nbproc) .ne. rang) goto 100
!
!       -- SI LE GREL EST VIDE, IL FAUT "SAUTER" :
        nbelgr=nbelem(ligrel,igr)
        if (nbelgr .eq. 0) goto 100
!
        nute=typele(ligrel,igr)
        call jenuno(jexnum('&CATA.TE.NOMTE', nute), nomte)
        nomtm=zk8(jtypma-1+nute)
        call dismoi('F', 'PHEN_MODE', nomte, 'TYPE_ELEM', ibid,&
                    phemod, ibid)
        pheno=phemod(1:16)
        modeli=phemod(17:32)
        call jelira(jexnum('&CATA.TE.CTE_ATTR', nute), 'LONMAX', lcteat, kbid)
        if (lcteat .gt. 0) then
            call jeveuo(jexnum('&CATA.TE.CTE_ATTR', nute), 'L', jcteat)
        else
            jcteat=0
        endif
!
        numc=nucalc(opt,nute,0)
        call assert(numc.ge.-10)
        call assert(numc.le.9999)
!
        if (numc .gt. 0) then
!
!         -- EN MODE PARALLELE
!         -- SI FETI OU CALCUL DISTRIBUE , ON VA REMPLIR
!         -- LE VECTEUR AUXILIAIRE '&CALCUL.PARALLELE'
            if (lfetmo .or. lfettd .or. ldist) then
                call wkvect('&CALCUL.PARALLELE', 'V V L', nbelgr, jparal)
                do 80 iel = 1, nbelgr
                    ima=numail(igr,iel)
                    if (lfetmo) then
!               - LIGREL DE MODELE, ON TAG EN SE BASANT SUR
!                '&FETI.MAILLE.NUMSD'
                        if (ima .le. 0) call u2mess('F', 'CALCULEL6_76')
                        if (zi(ifeti1+ima) .gt. 0) zl(jparal-1+iel)= .true.
                    else if (lfettd) then
                        if (ima .ge. 0) call u2mess('F', 'CALCULEL6_76')
                        idd=zi(ifel2+2*(-ima-1)+1)
!               - MAILLE TARDIVES, ON TAG EN SE BASANT SUR .FEL2
!                 (VOIR NUMERO.F)
                        if (idd .gt. 0) then
!                 - MAILLE NON SITUEE A L'INTERFACE
                            if (zi(ilimpi+idd) .eq. 1) zl(jparal-1+iel)= .true.
                        else if (idd.eq.0) then
!                 - MAILLE D'UN AUTRE PROC, ON NE FAIT RIEN
!                   ZL(JPARAL-1+IEL) INITIALISE A .FALSE.
                        else if (idd.lt.0) then
!                 - MAILLE A L'INTERFACE, ON NE S'EMBETE PAS ET ON FAIT
!                    TOUT (C'EST DEJA ASSEZ COMPLIQUE COMME CELA !)
                            zl(jparal-1+iel)=.true.
                        endif
!
                    else if (ldist) then
                        if (.not.ldgrel) then
!                 - LIGREL DE MODELE, ON TAGUE EN SE BASANT SUR
!                   PARTIT//'.NUPROC.MAILLE'
                            if (ima .lt. 0) then
                                if (rang .eq. 0) then
                                    zl(jparal-1+iel)=.true.
                                endif
                            else if (ima.gt.0) then
                                if (zi(jnumsd-1+ima) .eq. rang) then
                                    zl(jparal-1+iel)=.true.
                                endif
                            endif
                        else
!                 -- SI LDGREL, ON EST SUR LE BON PROC :
                            zl(jparal-1+iel)=.true.
                        endif
                    endif
80              continue
!
!           -- MONITORING
                if (infofe(2:2) .eq. 'T') call utimsd(ifm, 2, .false., .true.,&
                                                      '&CALCUL.PARALLELE', 1, ' ')
!
            endif
!
!         6.1 INITIALISATION DES TYPE_ELEM :
            call inigrl(ligrel, igr, nbobj, zi(iainel), zk24(ininel),&
                        nval)
!
!         6.2 ECRITURE AU FORMAT ".CODE" DU COUPLE (OPTION,TYPE_ELEM)
            if (iuncod .gt. 0) then
                k10b = cas
                k20b1 = '&&CALCUL'
                k20b2 = option
                k20b3 = nomte
                k20b4 = cmde
                write (iuncod, 1000) k10b, k20b1, k20b2, k20b3, k20b4
            endif
!
!         6.3 PREPARATION DES CHAMPS "IN"
            call extrai(nin2, lchin2, lpain2, opt, nute,&
                        ligrel, ' ')
!
!         6.4 MISE A ZERO DES CHAMPS "OUT"
            call zechlo(opt, nute)
!
!         6.5 ON ECRIT UNE VALEUR "UNDEF" AU BOUT DE
!             TOUS LES CHAMPS LOCAUX "IN" ET "OUT":
            call caundf('ECRIT', opt, nute)
!
!         6.6 ON FAIT LES CALCULS ELEMENTAIRES:
            if (dbg) write (6,*)'&&CALCUL OPTION= ',option,' ',nomte, ' ',numc
            call vrcdec()
            call te0000(numc, opt, nute)
!
!         6.7 ON VERIFIE LA VALEUR "UNDEF" DES CHAMPS LOCAUX "OUT" :
            call caundf('VERIF', opt, nute)
!
!         6.8 ON RECOPIE DES CHAMPS LOCAUX DANS LES CHAMPS GLOBAUX:
            call montee(opt, ligrel, nou2, lchou2, lpaou2,&
                        ' ')
!         IMPRESSIONS DE DEBUG POUR DETERMINER LES TEXXXX COUPABLES :
            if (dbg) call caldbg('OUTG', nou2, lchou2, lpaou2)
!
            call jedetr('&CALCUL.PARALLELE')
        endif
100  end do
!     ---FIN BOUCLE IGR
!
!     7- APRES BOUCLE SUR LES GREL :
!     QUELQUES ACTIONS HORS BOUCLE GREL DUES A CALVOI==1 :
!     -----------------------------------------------------
    call montee(opt, ligrel, nou2, lchou2, lpaou2,&
                'FIN')
!
!     8- ON "COMPLETE" LES CHAM_ELEM "OUT" SI NECESSAIRE :
!     ----------------------------------------------------
    if (mpic .eq. 'OUI' .and. ldist) then
        do 111,i=1,nou2
        call dismoi('F', 'TYPE_CHAMP', lchou2(i), 'CHAMP', ibid,&
                    tych, ibid)
        if (tych(1:2) .eq. 'EL') call sdmpic('CHAM_ELEM', lchou2(i))
111      continue
    endif
!
    if (dbg) call caldbg('OUTF', nou2, lchou2, lpaou2)
!
120  continue
!
!
!     9- ON DETRUIT LES OBJETS VOLATILES CREES PAR CALCUL:
!     ----------------------------------------------------
    do 110,i=1,nbobtr
    call jedetr(zk24(iaobtr-1+i))
    110 end do
    call jedetr('&&CALCUL.OBJETS_TRAV')
    iactif=0
!
!
!     9- MESURE DU TEMPS CONSOMME :
!     ----------------------------------
    call mpicm1('BARRIER', ' ', ibid, ibid, ibid,&
                rbid, cbid)
    call uttcpu('CPU.CALC.2', 'FIN', ' ')
    call uttcpu('CPU.CALC.1', 'FIN', ' ')
!
    if (lfetic) then
        call uttcpr('CPU.CALC.2', 6, temp2)
        call jeveuo('&FETI.INFO.CPU.ELEM', 'E', ifcpu)
        zr(ifcpu+rang)=zr(ifcpu+rang)+ (temp2(5)-temp1(5))+(temp2(6)-&
        temp1(6))
    endif
!
    call jedema()
!
    1000 format(1x,a10,a20,1x,a20,a20,a20)
!
end subroutine
