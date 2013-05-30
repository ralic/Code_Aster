subroutine exlim2(sdfeti, nomsd, lligrs, ligrsd, nbchat,&
                  numsd, nbsd, infofe, nbproc, ligrcf)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!-----------------------------------------------------------------------
! IN  : SDFETI : NOM DE LA SD_FETI
! IN  : NOMSD  : NOM DU SOUS-DOMAINE
! IN/OUT: LLIGRS : NOM DU VECTEUR CONTENANT LA LISTE DES LIGRELS
!                ASSOCIES A NOMSD POUR CONSTITUER SON NUME_DDL
! IN  : LIGRSD : NOM DU LIGREL TEMPORAIRE EGALE A LA RESTRICTION DU SD
!                AU MODELE
! IN  : NUMSD  : NUMERO DE SOUS-DOMAINE
! OUT : NBCHAT : NOMBRE DE LIGRELS ASSOCIEES A NOMSD
! IN  :  NBSD  : NOMBRE DE SOUS-DOMAINES
! IN  : NBPROC : NOMBRE DE PROCESSEURS
! IN  : LIGRCF : NOM DU LIGREL DE CHARGE DE CONTACT CONTINUE
!-----------------------------------------------------------------------
! TOLE CRP_20
! person_in_charge: olivier.boiteau at edf.fr
! CORPS DU PROGRAMME
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
    include 'jeveux.h'
!
    include 'asterfort/adalig.h'
    include 'asterfort/assert.h'
    include 'asterfort/cormgi.h'
    include 'asterfort/detrsd.h'
    include 'asterfort/gcncon.h'
    include 'asterfort/infniv.h'
    include 'asterfort/jecrec.h'
    include 'asterfort/jecroc.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jedupo.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jerazo.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/utimsd.h'
    include 'asterfort/wkvect.h'
    integer :: nbcha, numsd, nbsd, nbproc
    character(len=8) :: nomsd
    character(len=19) :: ligrsd, ligrcf
    character(len=24) :: sdfeti, lligrs, infofe
!
!
! DECLARATION VARIABLES LOCALES
    integer :: iret, k, iflii, nbmata, nbmat1, k1, iflim, nbgrel, i, ifel1
    integer :: nmgrel, igrel, j, numgre, nmgre1, l, numgr1, ladr, ifm, niv
    integer :: iflin, lont, nbno, nbnema, ibid, inbno, m, nbmat2, madr, nadr
    integer :: nbnoet, ifel2, ifel3, nbmas, ideca, ivligr, idd, iret1, nbcha2
    integer :: nbmas2, nbno2, ifetb, lofetb, lofel4, ifel4, ifel5, iaux1, iaux4
    integer :: lofel5, idime, ifeti, nbddli, nbnoi, iauxzi, imult, nbnoe2
    integer :: lofel6, lofel7, ifcfl, ifcfm, ifcfn, nbchac, nbchat, ifcfi
    integer :: nbcha1, isig, inem2, noeut, n, nboeut, nbcc2, nbpb
    character(len=8) :: k8bid, nomsd2
    character(len=19) :: ligrch
    character(len=24) :: k24b, k24dul, k24chl, k24dun, k24chn, k24cf1, k24cf2
    character(len=24) :: k24cf3, nomsda, k24cf4, k24cf5, k24b1
    logical :: lfcfl1, lfcfl2, lcc, lcc2, lok
    call jemarq()
!
! RECUPERATION ET MAJ DU NIVEAU D'IMPRESSION
    call infniv(ifm, niv)
! NOMBRE DE LIGREL DANS LA LISTE POUR NUMERO. ON COMPTE DEJA LE
! LIGREL DE MODELE
    nbchat=1
! SOUS-DOMAINE CONCERNE PAR DES LIGRELS TARDIFS DE CHARGE
! C'EST A DIRE COMPORTANT DES MAILLES TARDIVES (A NOEUDS TARDIFS OU PAS)
! A NOEUDS TARDIFS:    DDL_IMPO/FACE_IMPO/LIAISON_DDL/OBLIQUE....
! SANS NOEUD TARDIF:   FORCE_NODALE
    k24b=sdfeti(1:19)//'.FLIN'
    call jeexin(jexnom(k24b, nomsd), iret)
    if (iret .ne. 0) then
        call jeveuo(jexnom(k24b, nomsd), 'L', iflin)
        call jelira(jexnom(k24b, nomsd), 'LONMAX', nbcha, k8bid)
        call jeveuo(jexnom(k24b(1:23)//'I', nomsd), 'L', iflii)
        call jeveuo(jexnom(k24b(1:23)//'M', nomsd), 'L', iflim)
    else
        nbcha=0
    endif
    nbchat=nbchat+nbcha
! ON RAJOUTE EVENTUELLEMENT LE(S) LIGREL(S) DE CONTACT CONTINUE
    lfcfl1=.false.
    lfcfl2=.false.
    nbchac=0
! 1ERE PASSE ?
    k24b=sdfeti(1:19)//'.FCFL'
    call jeexin(jexnom(k24b, nomsd), iret)
    if (iret .ne. 0) then
        call jeveuo(jexnom(k24b, nomsd), 'L', ifcfl)
        call jelira(jexnom(k24b, nomsd), 'LONMAX', nbchac, k8bid)
        call jeveuo(jexnom(k24b(1:23)//'I', nomsd), 'L', ifcfi)
        call jeveuo(jexnom(k24b(1:23)//'M', nomsd), 'L', ifcfm)
        call jelira(jexnom(k24b(1:23)//'N', nomsd), 'LONMAX', nboeut, k8bid)
        call jeveuo(jexnom(k24b(1:23)//'N', nomsd), 'L', ifcfn)
        lfcfl1=.true.
    endif
    nbchat=nbchat+nbchac
! 2ND PASSE ET + ?
! ON NE FAIT PAS COMME EN MONO-DOMAINE ON NE CUMUL PAS LES LIGREL DE
!  CONTACT REDONDANTS.
    call jeexin(ligrcf//'.LIEL', iret)
    if (iret .ne. 0) then
        lfcfl2=.true.
! ON DETRUIT (LA PREMIERE FOIS, SOIT CONTACT 2 PASSE), LES LIGRELS DE
! CONTACT CONTINUE PROJETES EVENTUELS ET LES POINTEURS ASSOCIES
! ON LAISSE BIEN SUR INTACT LE LIGREL GLOBAL DE CHARGE DE LA BASE 'G'
        do 9 i = 1, nbchac
            k24b=zk24(ifcfl+i-1)
            call jeexin(k24b(1:19)//'.FEL1', iret)
            if (iret .ne. 0) then
                call jeveuo(k24b(1:19)//'.FEL1', 'L', ifel1)
                do 8 idd = 1, nbsd
                    k24b1=zk24(ifel1+idd-1)
                    if ((k24b1.ne.' ') .and. (k24b1(1:19).ne.k24b(1:19))) call detrsd('LIGREL',&
                                                                                      k24b1)
 8              continue
                call jedetr(k24b(1:19)//'.FEL1')
            endif
 9      continue
! GROSSE GLUTE POUR DETERMINER PAR AVANCE LES MAILLES TARDIVES DU
! LIGREL DE CHARGE CONTACT N+1 EME PASSE QUI APPARTIENNENT AU SD.
! ON UTILISE POUR CE FAIRE LE POINTEUR '&&EXLIM2.NEMA' DIMENSIONNE
! AU NBRE DE MAILLES TARDIVES DU LIGREL. LEUR NBRE TOTAL EST NBMAT1
        if (lfcfl1) then
            k24chn=ligrcf//'.NEMA'
            call jelira(k24chn, 'NUTIOC', nbmata, k8bid)
            nbcc2=0
            call wkvect('&&EXLIM2.NEMA', 'V V I', nbmata, inem2)
            call jerazo('&&EXLIM2.NEMA', nbmata, 1)
            do 40 l = 1, nbmata
                call jelira(jexnum(k24chn, l), 'LONMAX', nbnema, k8bid)
                nbnema=nbnema-1
                call jeveuo(jexnum(k24chn, l), 'L', madr)
! COMPTEUR POUR LES NOEUDS DE LA MAILLE TARDIVE APPARTENANT BIEN
! A .FCFN
                nbpb=0
! BOUCLE SUR LES NOEUDS DE LA MAILLE TARDIVE
                do 39 m = 1, nbnema
! NUMERO DU NOEUD DE LA MAILLE TARDIVE
                    noeut=zi(madr+m-1)
! IL FAUT QUE TOUTES LES NOEUDS DE LA MAILLE TARDIVE APPARTIENNENT
! AU SD POUR QU'ELLE SOIT PRISE EN COMPTE
! BOUCLE SUR LES NOEUDS DE CONTACT DU SD
                    do 37 n = 1, nboeut
                        if (zi(ifcfn+n-1) .eq. noeut) then
                            nbpb=nbpb+1
                            goto 38
                        endif
37                  continue
38                  continue
39              continue
                if (nbpb .eq. nbnema) then
! ET UNE MAILLE TARDIVE DE PLUS ATTRIBUEE AU SD
                    zi(inem2+nbcc2)=-l
                    nbcc2=nbcc2+1
                endif
40          continue
            lfcfl1=.false.
! SI LE SD EST CONCERNE PAR LE CONTACT N+1 PASSE
            if (nbcc2 .ne. 0) then
                nbchat=nbchat+1-nbchac
            else
                nbchat=nbchat-nbchac
            endif
!
        endif
    endif
!
! ALLOCATION DE LA LISTE DE LIGREL POUR NUME_DDL DU SD
    call wkvect(lligrs, 'V V K24', nbchat, ivligr)
    zk24(ivligr)='                        '
! LIGREL DE MODELE PROJETE
    zk24(ivligr)=ligrsd
!
!-------------
! PRETRAITEMENT POUR DIMENSIONNER D'EVENTUELS VECTEURS AXILIAIRES .FEL4
! ET .FEL5
    if (nbcha .ne. 0) then
        call jeveuo(jexnom(sdfeti(1:19)//'.FETB', nomsd), 'L', ifetb)
        call jelira(jexnom(sdfeti(1:19)//'.FETB', nomsd), 'LONMAX', lofetb, k8bid)
        lofetb=lofetb-1
        imult=2
! RECHERCHE DE LA MULTIPLICITE MAXIMALE (IMULT) DES NOEUDS D'INTERFACE
        call jeveuo(sdfeti(1:19)//'.FETI', 'L', ifeti)
        call jeveuo(sdfeti(1:19)//'.FDIM', 'L', idime)
        nbnoi=zi(idime+1)-1
        nbddli=zi(idime+3)
        do 305 i = 0, lofetb
            iauxzi=-zi(ifetb+i)
            if (iauxzi .gt. 0) then
                do 300 k = 0, nbnoi
                    if (zi(ifeti+4*k) .eq. iauxzi) then
                        if (zi(ifeti+4*k+1) .gt. imult) imult=zi(ifeti+ 4*k+1)
                    endif
300              continue
            endif
305      continue
!
        lofel6 = 3*imult*nbddli/(nbnoi+1)
! ON DIMENSIONNE .FEL5 EN TENANT COMPTE DE DEUX LAGRANGES PAR DDLS BLO
! CABLES
        lofel7=lofel6*2
    else
        lofel6=1
        lofel7=1
    endif
!-------------
!
!-----------------------------------------------------------------------
! BOUCLE SUR LES CHARGES
!-----------------------------------------------------------------------
    nbcha1=nbchat-1
    ideca=0
    do 100 k = 1, nbcha1
        k1=k-1
        isig=-1
! LIGREL DE CHARGE AVEC DU CONTACT CONTINU 1ERE PASSE, ON CHANGE DE
! POINTEURS
        lcc=.false.
        lcc2=.false.
        if ((k.gt.nbcha) .and. lfcfl1) then
! LCC INDIQUE SI LA CHARGE EST DE TYPE CONTACT CONTINUE 1ERE PASSE
            lcc=.true.
            iflin=ifcfl
            iflii=ifcfi
            iflim=ifcfm
            k1=k1-nbcha
            isig=1
            ideca=0
        else
! CHARGE TARDIVE DEJA PRISE EN COMPTE AU COUP D'AVANT (LIGREL DE CHARGES
! TARDIVES AVEC CONTACT CONTINUE)
            if ((lfcfl2) .and. (k.le.nbcha)) then
                k24cf1=zk24(iflin+k1)(1:19)//'.FEL1'
                call jeveuo(k24cf1, 'L', ifel1)
                zk24(ivligr+k)=zk24(ifel1+numsd-1)
                if (infofe(1:1) .eq. 'T') then
                    write(ifm,*)'<FETI/EXLIM2> LIGREL DEJA PROJETE ',&
                    zk24(iflin+k1)(1:19),' POUR SOUS-DOMAINE ',nomsd
                    write(ifm,*)'NOM DU PROJETE :',zk24(ifel1+numsd-1)
                endif
! ON PASSE EN FIN DE BOUCLE SUR LA CHARGE SUIVANTE
                goto 100
            endif
! LCC INDIQUE SI LA CHARGE EST DE TYPE CONTACT CONTINUE N+1ERE PASSE
            if ((lfcfl2) .and. (k.gt.nbcha)) then
                lcc2=.true.
            endif
        endif
        if (lcc2) then
! SI CONTACT 2 + N PASSE ON DETRUIT LES LIGRELS PROJETES PASSES ET LES
! POINTEURS
            ligrch=ligrcf(1:19)
            k24chl=ligrch//'.LIEL'
            k24chn=ligrch//'.NEMA'
            call jelira(k24chn, 'NUTIOC', nbmata, k8bid)
            nbmat1=nbcc2
        else
            ligrch=zk24(iflin+k1)(1:19)
            k24chl=ligrch//'.LIEL'
            k24chn=ligrch//'.NEMA'
            nbmata=zi(iflii+2*k1)
            nbmat1=zi(iflii+2*k1+1)
        endif
!
        if ((nbmata.ne.nbmat1) .and. (nbmat1.gt.0)) then
! **** ON PROJETE LA CHARGE
!
! LIGREL DE CHARGE PARTAGE ENTRE PLUSIEURS SOUS-DOMAINES, ON VA LE
! DUPLIQUER. SON NOUVEAU NOM TEMPORAIRE DU LIGREL DUPLIQUE EST
! 'F'//GNCON(2:8)//LIGRCH(9:19)
            call gcncon('.', k8bid)
            k8bid(1:1)='F'
            k24dul=k8bid//k24chl(9:24)
!
! ON VA STOCKER LE NOM DU NOUVEAU LIGREL DANS LE .FEL1 SI IL EXISTE
! SINON ON LE CREER
            k24cf1=k24chl(1:19)//'.FEL1'
            call jeexin(k24cf1, iret)
            if (iret .eq. 0) then
! CONSTITUTION OBJET STOCKAGE.FEL1
                call wkvect(k24cf1, 'V V K24', nbsd, ifel1)
                call jerazo(k24cf1, nbsd, 1)
            else
                call jeveuo(k24cf1, 'E', ifel1)
            endif
            zk24(ifel1+numsd-1)=k24dul(1:19)
!
! LIGREL.LIEL INITIAL: K24CHL
!             PROJETE: K24DUL
! LIGREL.NEMA INITIAL: K24CHN
!             PROJETE: K24DUN
! ON PREPARE LA PROJECTION DU .LIEL
            call jecrec(k24dul, 'V V I', 'NU', 'CONTIG', 'VARIABLE',&
                        nbmat1)
            lont=2*nbmat1
            call jeecra(k24dul, 'LONT', lont, ' ')
!
            k24dun=k24dul(1:19)//'.NEMA'
! ON PREPARE LA PROJECTION DU .NEMA
            if (.not.lcc) then
                call jelira(k24chn, 'NUTIOC', nbmas, k8bid)
                call jecrec(k24dun, 'V V I', 'NU', 'CONTIG', 'VARIABLE',&
                            nbmat1)
                call jelira(k24chn, 'LONT', lont, k8bid)
                call jeecra(k24dun, 'LONT', lont, ' ')
                call jeveuo(k24chl(1:19)//'.NBNO', 'L', inbno)
                nbno=zi(inbno)
! POUR DIMENSIONNER LES .FEL4 ET .FEL5
                nbnoe2=max(nbno,nbmas)
                lofel4=nbnoe2*lofel6
                lofel5=nbnoe2*lofel7
            else
                nbno=0
! INIT POUR CREER INFORMATIQUEMENT LES OBJETS .FEL4 ET .FEL5 AVANT DE
! DETRUIRE !
                lofel4=1
                lofel5=1
            endif
! SI LIGREL A MAILLE ET/OU A NOEUD TARDIF (TOUS SAUF CONTACT CONTINUE
! 1ERE PASSE DONC)
! ON VA STOCKER LES ANCIENS NUMEROS DE MAILLES TARDIVES DANS LE .FEL2
! ET LES ANCIENS NUMEROS DE NOEUDS TARDIFS DANS LE .FEL3
! SI IL EXISTE, SINON ON LE CREE.
!
! --- ON TRAITE DES MAILLES ET/OU NOEUDS TARDIFS, ON A DONC BESOIN
! --- DES OBJETS .FEL2, .FEL3, .FEL4 ET .FEL5
            if (.not.lcc) then
                k24cf2=k24chl(1:19)//'.FEL2'
                call jeexin(k24cf2, iret)
                if (iret .eq. 0) then
! CONSTITUTION OBJET STOCKAGE.FEL2
                    nbmas2=2*nbmas
                    call wkvect(k24cf2, 'V V I', nbmas2, ifel2)
                    call jerazo(k24cf2, nbmas2, 1)
                else
                    call jeveuo(k24cf2, 'E', ifel2)
                endif
! SI SEULEMENT MAILLE TARDIVE SANS NOEUD TARDIF, PAS BESOIN DE .FEL3
                if (nbno .gt. 0) then
                    k24cf3=k24chl(1:19)//'.FEL3'
                    call jeexin(k24cf3, iret)
                    if (iret .eq. 0) then
! CONSTITUTION OBJET STOCKAGE.FEL3
                        nbno2=2*nbno
                        call wkvect(k24cf3, 'V V I', nbno2, ifel3)
                        call jerazo(k24cf3, nbno2, 1)
                    else
                        call jeveuo(k24cf3, 'E', ifel3)
                    endif
                endif
                k24cf4=k24chl(1:19)//'.FEL4'
                call jeexin(k24cf4, iret)
                if (iret .eq. 0) then
! CONSTITUTION OBJET STOCKAGE.FEL4
                    call wkvect(k24cf4, 'V V I', lofel4, ifel4)
                    zi(ifel4)=0
                else
                    call jeveuo(k24cf4, 'E', ifel4)
                endif
! SI SEULEMENT MAILLE TARDIVE SANS NOEUD TARDIF, PAS BESOIN DE .FEL5
                if (nbno .gt. 0) then
                    k24cf5=k24chl(1:19)//'.FEL5'
                    call jeexin(k24cf5, iret)
                    if (iret .eq. 0) then
! CONSTITUTION OBJET STOCKAGE.FEL5
                        call wkvect(k24cf5, 'V V I', lofel5, ifel5)
                        zi(ifel5)=0
                    else
                        call jeveuo(k24cf5, 'E', ifel5)
                    endif
                endif
            endif
! ENDIF DU .NOT.LCFC1
!
!-----------------------------------------------------------------------
! --- BOUCLE SUR LES GRELS DU .LIEL A PROJETER
!-----------------------------------------------------------------------
            nbmat2=0
            nbnoet=0
            call jelira(k24chl, 'NUTIOC', nbgrel, k8bid)
            do 30 i = 1, nbgrel
                call jelira(jexnum(k24chl, i), 'LONMAX', nmgrel, k8bid)
                call jeveuo(jexnum(k24chl, i), 'L', igrel)
!
!-----------------------------------------------------------------------
! --- BOUCLE SUR LES MAILLES DES GRELS
!-----------------------------------------------------------------------
                nmgre1=nmgrel-2
                do 20 j = 0, nmgre1
!
! NUMERO DE LA MAILLE DANS LE LIGREL GLOBAL
                    numgre=zi(igrel+j)
                    lok=.false.
! ON DECIDE DE FAIRE LA BOUCLE OU NON
                    if (lcc2) then
! DANS LE CAS CONTACT 2EME PASSE OU PLUS
                        do 50 l = 1, nbcc2
                            if (zi(inem2+l-1) .eq. numgre) lok=.true.
50                      continue
                    else if (lcc) then
! DANS LE CAS CONTACT 1ERE PASSE
                        if (numgre .gt. 0) lok=.true.
                    else
! DANS LES AUTRES CAS
                        if (numgre .lt. 0) lok=.true.
                    endif
!
                    if (lok) then
! MAILLE A PROJETER EVENTUELLEMENT
! MAILLE TARDIVE SI LIGREL AVEC DIRICHLET OU FORCE NODALE OU CONTACT
! N+1 PASSE
! MAILLE PHYSIQUE SI CONTACT CONTINUE 1ERE PASSE
!
!-----------------------------------------------------------------------
! --- BOUCLE SUR LES MAILLES DES .FLIM OU .FCFM
!-----------------------------------------------------------------------
                        do 10 l = 1, nbmat1
!
                            if (lcc2) then
                                numgr1=isig*numgre
                            else
                                numgr1=zi(iflim+ideca+l-1)
                            endif
!
                            if (numgr1 .eq. isig*numgre) then
! ON LA DUPLIQUE DANS .LIEL EN CREEANT UN GREL LIMITE A CETTE MAILLE
                                nbmat2=nbmat2+1
                                call jecroc(jexnum(k24dul, nbmat2))
                                call jeecra(jexnum(k24dul, nbmat2), 'LONMAX', 2, k8bid)
                                call jeveuo(jexnum(k24dul, nbmat2), 'E', ladr)
! ON RECOPIE LE NUMERO NEGATIF DE MAILLE ET SON TYPE
                                if (lcc) then
                                    zi(ladr)=numgre
                                else
                                    zi(ladr)=-nbmat2
                                endif
                                zi(ladr+1)=zi(igrel+nmgre1+1)
!
!---------------
! TRAITEMENTS LIES AUX .FEL2/.FEL4 POUR MAILLE TARDIVE D'INTERFACE
                                if (.not.lcc) then
                                    iaux1=ifel2+2*(numgr1-1)+1
                                    if (zi(iaux1) .lt. 0) then
                                        iaux4=zi(ifel4)
                                        zi(ifel4+iaux4+1)=-nbmat2
                                        zi(ifel4+iaux4+2)=numsd
                                        zi(ifel4+iaux4+3)=numgr1
                                        zi(iaux1)=zi(iaux1)-1
                                        zi(ifel4)=iaux4+3
                                    else
                                        if ((zi(iaux1-1).eq.0) .and. (zi(iaux1).eq.0)) then
! C'EST LA PREMIERE FOIS QUE CETTE MAILLE TARDIVE EST ENUMEREE: ON
! L'ENREGISTRE SIMPLEMENT
                                            zi(iaux1-1)=-nbmat2
                                            zi(iaux1)=numsd
                                        else
! C'EST LA DEUXIEME FOIS: ON MET EN PLACE LE PROCESSUS VIA .FEL4 CAR
! C'EST UNE MAILLE TARDIVE CONCERNANT UN POINT D'INTERFACE
! DERNIERE ADRESSE PRISE PAR L'OBJET .FEL4
                                            iaux4=zi(ifel4)
                                            zi(ifel4+iaux4+1)=zi(iaux1-1)
                                            zi(ifel4+iaux4+2)=zi(iaux1)
                                            zi(ifel4+iaux4+3)=numgr1
                                            zi(ifel4+iaux4+4)=-nbmat2
                                            zi(ifel4+iaux4+5)=numsd
                                            zi(ifel4+iaux4+6)=numgr1
                                            zi(iaux1)=-2
                                            zi(ifel4)=iaux4+6
                                        endif
                                    endif
!------------------
!
! ON DUPLIQUE LES INFOS DU .NEMA (DESCRIPTION DES NOEUDS TARDIFS
! ASSOCIES A LA MAILLE TARDIVE
                                    call jelira(jexnum(k24chn, numgr1), 'LONMAX', nbnema, k8bid)
                                    call jeveuo(jexnum(k24chn, numgr1), 'L', madr)
                                    call jecroc(jexnum(k24dun, nbmat2))
                                    call jeecra(jexnum(k24dun, nbmat2), 'LONMAX', nbnema, k8bid)
                                    call jeveuo(jexnum(k24dun, nbmat2), 'E', nadr)
                                    nbnema=nbnema-1
!-----------------------------------------------------------------------
! --- BOUCLE SUR LES NOEUDS DU .NEMA
!-----------------------------------------------------------------------
                                    do 3 m = 0, nbnema
                                        if (zi(madr+m) .lt. 0) then
! NOEUD TARDIF
                                            ibid=abs(zi(madr+m))
! ON VA TESTER LA PRESENCE EVENTUELLE D'UNE VALEUR NON NULLE POUR
! TRAITER LE CAS DES DDL_IMPO QUI UTILISENT, POUR DES MAILLES TARDIVES
! DISTINCTES, LES MEMES LAGRANGES (IL NE FAUT DONC PAS INCREMENTER LEURS
! NOUVELLES VALEURS).
!---------------
! TRAITEMENTS LIES AUX .FEL3/.FEL5 POUR NOEUD TARDIF D'INTERFACE
                                            iaux1=ifel3+2*(ibid-1)+1
                                            if (zi(iaux1) .lt. 0) then
                                                iaux4=zi(ifel5)
                                                nbnoet=nbnoet+1
                                                zi(ifel5+iaux4+1)=-nbnoet
                                                zi(ifel5+iaux4+2)=numsd
                                                zi(ifel5+iaux4+3)=ibid
                                                zi(iaux1)=zi(iaux1)-1
                                                zi(ifel5)=iaux4+3
                                                zi(nadr+m)=-nbnoet
                                            else
                                                if ((zi(iaux1-1).eq.0) .and.&
                                                    ( zi(iaux1).eq.0)) then
! C'EST LA PREMIERE FOIS QUE CE NOEUD TARDIF EST ENUMERE: ON
! L'ENREGISTRE SIMPLEMENT
                                                    nbnoet=nbnoet+1
                                                    zi(iaux1-1)=-nbnoet
                                                    zi(iaux1)=numsd
                                                    zi(nadr+m)=-nbnoet
                                                    else if ((zi(iaux1-1).ne.0).and.&
                                    (numsd.eq.zi(iaux1))) then
! C'EST UN NOEUD TARDIF DE LIAISON_DDL POUR CE SD, IL NE FAUT PAS LE
! DUPLIQUER, IL EXISTE DEJA
                                                    zi(nadr+m)=zi(iaux1-1)
                                                else
! C'EST LA DEUXIEME FOIS: ON MET EN PLACE LE PROCESSUS VIA .FEL5 CAR
! C'EST UN NOEUD TARDIF CONCERNANT UN POINT D'INTERFACE
! DERNIERE ADRESSE PRISE PAR L'OBJET .FEL5
                                                    iaux4=zi(ifel5)
                                                    zi(ifel5+iaux4+1)=zi(iaux1-1)
                                                    zi(ifel5+iaux4+2)=zi(iaux1)
                                                    zi(ifel5+iaux4+3)=ibid
                                                    nbnoet=nbnoet+1
                                                    zi(ifel5+iaux4+4)=-nbnoet
                                                    zi(ifel5+iaux4+5)=numsd
                                                    zi(ifel5+iaux4+6)=ibid
                                                    zi(iaux1)=-2
                                                    zi(ifel5)=iaux4+6
                                                    zi(nadr+m)=-nbnoet
                                                endif
                                            endif
!------------------
!
                                        else
! NOEUD PHYSIQUE
                                            zi(nadr+m)=zi(madr+m)
                                        endif
 3                                  continue
! FIN DU IF SUR LFCFL1
                                endif
! ON A TROUVE LA MAILLE CORRESPONDANTE DU .FLIM A DUPLIQUER ON PEUT
! PASSER A LA MAILLE SUIVANTE DU GREL
                                goto 15
! FIN DU IF SUR NUMGR1
                            endif
! FIN BOUCLE SUR LES MAILLES DE .FLIM
10                      continue
! FIN DU IF SUR NUMGRE ET LFCFL1
                    endif
!
! LABEL DE SORTIE DE BOUCLE SUR LE MAILLE DE '.FLIM'
15                  continue
! FIN BOUCLE MAILLE DE GREL
20              continue
! FIN BOUCLE SUR LES GREL DU LIGREL DE CHARGE
30          continue
!
! ON RENTRE NUMERO DE NOEUD TARDIF MAX (EN VALEUR ABSOLUE) DANS .NBNO
            call wkvect(k24dul(1:19)//'.NBNO', 'V V I', 1, inbno)
            zi(inbno)=nbnoet
!
! ADAPTATION DE L'OBJET JEVEUX K24DUL (ON REGROUPE LES MAILLES PAR TYPE
! DANS UN SEUL GREL)
            call adalig(k24dul(1:19))
!
! ON DUPLIQUE LES AUTRES OBJETS DU LIGREL DE CHARGE
            call jedupo(ligrch//'.PRNS', 'V', k24dul(1:19)//'.PRNS', .false.)
            call jedupo(ligrch//'.LGNS', 'V', k24dul(1:19)//'.LGNS', .false.)
            call jedupo(ligrch//'.LGRF', 'V', k24dul(1:19)//'.LGRF', .false.)
            call jedupo(ligrch//'.PRNM', 'V', k24dul(1:19)//'.PRNM', .false.)
!
! CREATION DE LA CORRESPONDANCE MAILLE --> (IGREL,IM) POUR LE .REPE
            call cormgi('V', k24dul(1:19))
!
! ON STOCKE LE NOM DU LIGREL DE CHARGE TEMPORAIRE DANS LA LISTE
            zk24(ivligr+k)='                        '
            zk24(ivligr+k)=k24dul(1:19)
            if (infofe(1:1) .eq. 'T') then
                write(ifm,*)'<FETI/EXLIM2> PROJECTION DU LIGREL ',&
                ligrch,' POUR SOUS-DOMAINE ',nomsd
                write(ifm,*)'NOM DU PROJETE :',zk24(ivligr+k)
                if (lcc) then
                    write(ifm,*)'LIGREL DE CHARGE DE CONTACT 1ER PASSAGE'
                else
                    write(ifm,*)'LIGREL DE CHARGE AVEC DU TARDIF'
                endif
            endif
!
! ON DETRUIT LES .FEL4 TE .FEL5 INUTILES
            if (.not.lcc) then
                if (zi(ifel4) .eq. 0) call jedetr(k24cf4)
                if (nbno .gt. 0) then
                    if (zi(ifel5) .eq. 0) call jedetr(k24cf5)
                endif
            endif
!
        else if (nbmat1.gt.0) then
!
! **** ON CONSERVE LA CHARGE
!
! ON VA STOCKER LE NOM DU NOUVEAU LIGREL DANS LE .FEL1 SI IL EXISTE
! SINON ON LE CREER
            if (lcc2) then
                k24cf1=ligrcf(1:19)//'.FEL1'
            else
                k24cf1=zk24(iflin+k1)(1:19)//'.FEL1'
            endif
            call jeexin(k24cf1, iret)
            if (iret .eq. 0) then
! CONSTITUTION OBJET STOCKAGE.FEL1
                call wkvect(k24cf1, 'V V K24', nbsd, ifel1)
                call jerazo(k24cf1, nbsd, 1)
            else
! INCOHERENCE QUE POUR LE SEQUENTIEL, CF BOUCLE 150 CI-DESSOUS
                call assert(nbproc.ne.1)
                call jeveuo(k24cf1, 'E', ifel1)
            endif
            zk24(ifel1+numsd-1)=k24cf1(1:19)
!
! ON STOCKE LE NOM DU LIGREL DE CHARGE INITIAL DANS LA LISTE
            zk24(ivligr+k)='                        '
            zk24(ivligr+k)=k24cf1(1:19)
            if (infofe(1:1) .eq. 'T') then
                write(ifm,*)'<FETI/EXLIM2> ASSOCIATION DU LIGREL DE '//&
     &      'CHARGE ',zk24(ivligr+k)(1:19),'POUR SOUS-DOMAINE ',nomsd
                if (lcc) then
                    write(ifm,*)'LIGREL DE CHARGE A MAILLE/NOEUDS PHYSIQUES'
                else
                    write(ifm,*)'LIGREL DE CHARGE AVEC DU TARDIF'
                endif
            endif
! FIN DU IF NBMATA.NE.NBMAT1
        endif
        ideca=ideca+nbmat1
!
!
        if (infofe(2:2) .eq. 'T') then
            call utimsd(ifm, 2, .false., .true., zk24(ivligr+k)(1:19),&
                        1, 'V')
            if (lcc2) then
                call utimsd(ifm, 2, .false., .true., ligrcf(1:19),&
                            1, 'V')
            else
                call utimsd(ifm, 2, .false., .true., zk24(iflin+k1)(1:19),&
                            1, 'V')
            endif
        endif
! FIN BOUCLE SUR LA LISTE DES CHARGES
100  end do
!
!
!-----------------------------------------------------------------------
! MONITORING
!-----------------------------------------------------------------------
    if (infofe(1:1) .eq. 'T') write(ifm, * )'<FETI/EXLIM2> REMPLISSAGE OBJET JEVEUX ',&
                              lligrs(1:19)
    if (infofe(2:2) .eq. 'T') call utimsd(ifm, 2, .false., .true., lligrs(1:19),&
                                          1, 'V')
!
!-----------------------------------------------------------------------
! SPECIAL PARALLELISME POUR .FEL1
!-----------------------------------------------------------------------
    k24b=sdfeti(1:19)//'.FLIN'
    if (nbproc .gt. 1) then
! INIT POUR JENUNO
        nomsda=sdfeti(1:19)//'.FETA'
! SI ON EST EN PARALLELE, COMPTE-TENU DE LA GESTION PARTIELLE DES
! DONNEES, POUR UN PROCESSEUR DONNE, IL FAUT GENERER TOUT LES .FEL1
! POTENTIELS, SINON LES BOUCLES DE ASSMAM, QUI ELLES FONCTIONNENT SUR
! TOUS LES CHARGEMENTS, SONT PERDUES !
        do 150 idd = 1, nbsd
! POUR NE PAS REFAIRE LE TRAVAIL FAIT JUSTE AU DESSUS
            if (idd .ne. numsd) then
                call jenuno(jexnum(nomsda, idd), nomsd2)
                call jeexin(jexnom(k24b, nomsd2), iret)
                if (iret .ne. 0) then
! SOUS-DOMAINE CONCERNE PAR DES LIGRELS TARDIFS DE CHARGE
                    call jeveuo(jexnom(k24b, nomsd2), 'L', iflin)
                    call jelira(jexnom(k24b, nomsd2), 'LONMAX', nbcha2, k8bid)
                    do 140 k = 1, nbcha2
                        k1=k-1
                        k24cf1=zk24(iflin+k1)(1:19)//'.FEL1'
                        call jeexin(k24cf1, iret1)
                        if (iret1 .eq. 0) then
! CONSTITUTION OBJET STOCKAGE.FEL1
                            call wkvect(k24cf1, 'V V K24', nbsd, ifel1)
                            call jerazo(k24cf1, nbsd, 1)
                        endif
140                  continue
                endif
                if (lfcfl2) then
                    k24cf1=ligrcf(1:19)//'.FEL1'
                    call jeexin(k24cf1, iret1)
                    if (iret1 .eq. 0) then
! CONSTITUTION OBJET STOCKAGE.FEL1
                        call wkvect(k24cf1, 'V V K24', nbsd, ifel1)
                        call jerazo(k24cf1, nbsd, 1)
                    endif
                endif
            endif
150      continue
    endif
!
    call jedetr('&&EXLIM2.NEMA')
    call jedema()
!
end subroutine
