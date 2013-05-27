subroutine ircers(ifi, ligrel, nbgrel, longr, ncmpmx,&
                  vale, nomgd, nomcmp, titr, nomel,&
                  loc, celd, nbnoma, permut, maxnod,&
                  typma, nomsd, nomsym, ir, nbmat,&
                  nummai, lmasu, ncmpu, nucmp, nbcmp,&
                  ncmps, nocmpl)
    implicit none
!
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/codent.h'
    include 'asterfort/dgmode.h'
    include 'asterfort/digdel.h'
    include 'asterfort/ecrtes.h'
    include 'asterfort/exisdg.h'
    include 'asterfort/irgags.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexatr.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/lxlgut.h'
    include 'asterfort/lxliis.h'
    include 'asterfort/nbec.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    integer :: ifi, ligrel(*), nbgrel, longr(*), ncmpmx, celd(*), ncmpu
    integer :: nucmp(*), nbnoma(*), typma(*), permut(maxnod, *), nbmat
    integer :: nummai(*), nbcmp, ncmps(*)
    character(len=*) :: nomgd, nomcmp(*), nomel(*), loc, titr, nomsym, nomsd
    character(len=*) :: nocmpl(*)
    real(kind=8) :: vale(*)
    logical :: lmasu
!--------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! TOLE CRP_21
! TOLE CRP_20
!        ECRITURE D'UN CHAMELEM SUR FICHIER UNIVERSEL, DATASET TYPE 56
!                                                                OU 57
!        A VALEURS REELLES
!  ENTREE:
!     IFI   : UNITE LOGIQUE DU FICHIER UNIVERSEL
!     LIGREL: LIGREL COMPLET
!     NBGREL: NOMBRE DE GRELS
!     LONGR : POINTEUR DE LONGUEUR DE LIGREL
!     NCMPMX: NOMBRE MAXI DE CMP DE LA GRANDEUR NOMGD
!     VALE  : VALEURS DU CHAM_NO
!     NOMGD : NOM DE LA GRANDEUR: SIEF_R, EPSI_R,...
!     NOMCMP: NOMS DES CMP
!     TITR  : 1 LIGNE  DE TITRE
!     NOMEL : NOMS DES MAILLES SUPPORTS DES ELEMENTS
!     LOC   : LOCALISATION DES VALEURS (ELNO =>57, ELGA=>56,ELEM=>56)
!     CELD  : DESCRIPTEUR DU CHAM_ELEM (MODES LOCAUX,ADRESSES->.CELV)
!     NBNOMA: NOMBRE DE NOEUDS DE CHAQUE MAILLE
!     PERMUT: TABLEAU DES PERMUTATIONS DES NOEUDS DE CHAQUE TYPE-MA
!     MAXNOD: NOMBRE MAXI DE NOEUDS POUR LES DIFF. TYPE_MAILLES
!     TYPMA : TYPE_MAILLES
!     IR    : NUMERO D'ORDRE DU CHAMP
!     NBMAT : NOMBRE DE MAILLES A IMPRIMER
!     NUMMAI: NUMEROS DES MAILLES A IMPRIMER
!     LMASU : INDIQUE SI MAILLAGE ISSU DE SUPERTAB  .TRUE.
!     NBCMP : NOMBRE DE COMPOSANTES A IMPRIMER
!
!     ------------------------------------------------------------------
    character(len=3) :: toto
    character(len=4) :: nomgs
    character(len=8) :: nocmp, ktype
    character(len=24) :: nomst
    character(len=80) :: entete(10), titre, texte
    integer :: nbchs, nbcmpt, entier, nbspt, nnoe, ilong, imodel
    integer :: impre, iente, impel
    logical :: afaire, lcmp, lnocen
!
!  --- INITIALISATIONS ----
!
!-----------------------------------------------------------------------
    integer :: i, iachml, iad, iaec, iast, ibcmps, ic
    integer :: ichs, icm, icmax0, icmp, icmpl, icmps, icms
    integer :: icmsup, ico, icoef, icomax, icomm, icou, icp
    integer :: ida, idebu, idern, iel, ielg, ier, ies
    integer :: ifin, igre, igrel, ilig, imai, inbcg, inbctg
    integer :: indats, inoa, inochs, inogds, inos, inper, iperm
    integer :: ipg, ipoin1, ipoin2, ipos, ir, ires, irvg
    integer :: irvn, is0, isnbcs, isp, ispt, isup, itabl
    integer :: itseg2, itype, iutil, j, jj, jmax, jmod
    integer :: jspt, jt, jtitr, k, l, ll
    integer :: maxnod, mode, nbcou, nbdats, nbelgr, nbpg, ncmpg
    integer :: ncmpp, nec, ni, npcalc, nsca, nscal
!-----------------------------------------------------------------------
    call jemarq()
!
    call wkvect('&&IRCERS.NOMGDS', 'V V K8', ncmpmx, inogds)
    call wkvect('&&IRCERS.NOMCHS', 'V V K8', ncmpmx, inochs)
    call wkvect('&&IRCERS.NBCMPS', 'V V I', ncmpmx, ibcmps)
    call wkvect('&&IRCERS.IPCMPS', 'V V I', ncmpmx*ncmpmx, icmps)
    call wkvect('&&IRCERS.LTABL', 'V V L', ncmpmx, itabl)
    call jeveuo('&CATA.TE.MODELOC', 'L', imodel)
    call jeveuo(jexatr('&CATA.TE.MODELOC', 'LONCUM'), 'L', ilong)
!
    nomst= '&&IRECRI.SOUS_TITRE.TITR'
    call jeveuo(nomst, 'L', jtitr)
    titre = zk80(jtitr)
    do 1 i = 1, ncmpmx
        zl(itabl-1+i)=.false.
 1  end do
    lnocen=.false.
    lcmp=.false.
!
!  --- RECHERCHE DES GRANDEURS SUPERTAB ----
!
    call irgags(ncmpmx, nomcmp, nomsym, nbchs, zk8(inochs),&
                zi(ibcmps), zk8(inogds), zi(icmps))
!
!
!     NOM DE LA GRANDEUR SUPERTAB
    if (nbcmp .ne. 0) then
        lcmp=.true.
        if (nomgd .ne. 'VARI_R') then
            do 897 i = 1, nbchs
                do 898 j = 1, zi(ibcmps+i-1)
                    if (ncmps(1) .eq. zi(icmps-1+(i-1)*ncmpmx+j)) goto 899
898              continue
897          continue
899          continue
            nomgs=zk8(inogds-1+i)
        endif
    endif
! --- DETERMINATION DU NOMBRE MAXIMUM DE SOUS-POINTS ---
    icomax = 0
    do 8 igre = 1, nbgrel
        icoef=max(1,celd(4))
        if (icoef .gt. icomax) icomax=icoef
 8  end do
    icomm = 6
    if (ncmpu .eq. 0) then
        icmax0 = icomax
    else
        icmax0 = ncmpu
    endif
!
! -- ALLOCATION DES TABLEAUX DE TRAVAIL ---
!
    if (loc .eq. 'ELNO') then
        call jedetr('&&IRCERS.VALNOE')
        call wkvect('&&IRCERS.VALNOE', 'V V R', ncmpmx*icomm, irvn)
    else if (loc.eq.'ELGA'.or.loc.eq.'ELEM') then
        call jedetr('&&IRCERS.VALGAU')
        call wkvect('&&IRCERS.VALGAU', 'V V R', ncmpmx*icomm, irvg)
    endif
!
! ---- BOUCLE SUR LES DIVERSES GRANDEURS SUPERTAB ----
    if (nbcmp .ne. 0 .and. nomgd .ne. 'VARI_R') nbchs=1
    do 10 ichs = 1, nbchs
        if (ichs .gt. 1) then
            afaire = .false.
            do 2 icp = 1, zi(ibcmps-1+ichs)
                afaire= (afaire.or.zl(itabl-1+(zi(icmps-1 +(ichs-1)*&
                ncmpmx+icp))))
 2          continue
            if (.not. afaire) goto 10
        endif
!
        call jedetr('&&IRCERS.SOUS_PT')
        call wkvect('&&IRCERS.SOUS_PT', 'V V I', icomax, jspt)
!
! --- GROUPEMENT DES VARIABLES SCALAIRES 6 PAR 6 ----
!  ---  DETERMINATION DU NOMBRE DE DATASETS SUPERTAB A IMPRIMER --
!
        if (zi(ibcmps-1+ichs) .eq. 1 .and. icomax .gt. 1) then
            do 42 i = 1, icmax0
                zi(jspt-1+i)=6
42          continue
            ilig=icmax0/6
            ires=icmax0-ilig*6
            if (ires .eq. 0) then
                nbdats=ilig
            else
                nbdats=ilig+1
                zi(jspt-1+nbdats)=ires
            endif
            nbcmpt=6
            nomgs='VARI'
        else if (nbcmp.ne.0 .and. nomgd.ne.'VARI_R') then
            call wkvect('&&IRCERS.SCMP_DATS', 'V V I', nbcmp, indats)
            ilig=nbcmp/6
            ires=nbcmp-ilig*6
            ni=0
            zi(indats)=ni
            if (ires .eq. 0) then
                nbdats=ilig
                do 901 i = 1, nbdats
                    zi(ibcmps+i-1)=6
                    ni=ni+6
                    zi(indats+i)=ni
901              continue
            else
                nbdats=ilig+1
                do 902 i = 1, nbdats-1
                    zi(ibcmps+i-1)=6
                    ni=ni+6
                    zi(indats+i)=ni
902              continue
                zi(ibcmps+nbdats-1)=ires
                zi(indats+nbdats)=ni+ires
            endif
            nbcmpt=6
        else
            nbdats=icomax
            do 3 i = 1, nbdats
                zi(jspt-1+i)=1
 3          continue
            nbcmpt=zi(ibcmps-1+ichs)
            nomgs=zk8(inogds-1+ichs)
        endif
!
!
!
! --- ECRITURE DE L'ENTETE SUPERTAB ----
!
        call ecrtes(nomsd, titr, nomgs, ir, loc,&
                    nbcmpt, 2, entete, lcmp)
!
!
! --- POSITION DES COMPOSANTES SELECTIONNEES ---
!
        if (nbcmp .ne. 0 .and. nomgd .ne. 'VARI_R') then
            k=0
            l=0
            ll=0
            call wkvect('&&IRCERS.PERM', 'V V I', nbcmp*nbgrel, iperm)
            call wkvect('&&IRCERS.NBCMPS_GREL', 'V V I', nbgrel, inbcg)
            call wkvect('&&IRCERS.POS', 'V V I', nbcmp*nbgrel, ipos)
            call wkvect('&&IRCERS.NBCMPT_GREL', 'V V I', nbgrel, inbctg)
            call wkvect('&&IRCERS.SNBCPS', 'V V I', nbgrel+1, isnbcs)
            call wkvect('&&IRCERS.CMP_GREL', 'V V I', nbcmp*nbgrel, inper)
            zi(isnbcs)=0
            do 904 igrel = 1, nbgrel
                mode=celd(celd(4+igrel)+2)
                ipoin1=longr(igrel)
                ipoin2=longr(igrel+1)
                nbelgr=ipoin2-ipoin1-1
                if (mode .eq. 0) goto 904
                jmod = imodel+zi(ilong-1+mode)-1
                nec = nbec (zi(jmod-1+2))
                call jedetr('&&IRCERS.ENT_COD')
                call wkvect('&&IRCERS.ENT_COD', 'V V I', nec, iaec)
                call dgmode(mode, imodel, ilong, nec, zi(iaec))
                ncmpg=0
!             POSITIONS DES COMPOSANTES SELECTIONNEES PRESENTES
!             DANS LE  GREL PARMI LES COMPOSANTES SELECTIONNEES
                do 905 icmpl = 1, nbcmp
                    if (exisdg(zi(iaec),ncmps(icmpl))) then
                        zi(inper+k)=ncmps(icmpl)
                        k=k+1
                        ncmpg=ncmpg+1
                    endif
905              continue
!             SOMME DES COMPOSANTES SELECTIONNEES PAR GREL
                zi(isnbcs+igrel)=k
!             NOMBRE DE COMPOSANTES SELECTIONNEES PRESENTES
!             DANS LE GREL
                zi(inbcg+igrel-1)=ncmpg
                if (igrel .eq. nbgrel .and. k .lt. nbcmp) call u2mess('F', 'PREPOST_83')
                ncmpp=0
                do 906 i = 1, ncmpmx
                    if (exisdg(zi(iaec),i)) then
                        ncmpp=ncmpp+1
!                   POSITIONS DES COMPOSANTES SELECTIONNEES PRESENTES
!                   DANS LE GREL PARMI LES COMPOSANTES DU GREL
                        do 915 j = 1, zi(inbcg+igrel-1)
                            if (i .eq. zi(inper+j-1+zi(isnbcs+igrel-1))) then
                                zi(ipos+l)=ncmpp
                                l=l+1
                            endif
915                      continue
!                   POSITION DES COMPOSANTES SELECTIONNEES
!                   DANS LES DATASETS
                        do 789 j = 1, nbcmp
                            if (i .eq. ncmps(j)) then
                                zi(iperm+ll)=j
                                ll=ll+1
                            endif
789                      continue
                    endif
906              continue
!             NOMBRE DE COMPOSANTES DANS LE GREL
                zi(inbctg+igrel-1)=ncmpp
904          continue
        endif
!
!
! --- IMPRESSION DES DATASETS SUPERTAB ---
!
!
        do 11 ida = 1, nbdats
!
            iente = 1
            impre = 0
            ifin = 1
            idebu = 1
            entete(4) = ' '
            texte = ' '
!
! --- ECRITURE DANS L'ENTETE SUPERTAB DES NOMS DE COMPOSANTES---
!
            if (nbcmp .ne. 0 .and. nomgd .ne. 'VARI_R') then
                do 907 icp = 1, zi(ibcmps+ida-1)
                    nocmp=nocmpl(zi(indats+ida-1)+icp)
                    iutil=lxlgut(nocmp)
                    ifin = idebu+iutil
                    texte(idebu:ifin)=nocmp(1:iutil)//' '
                    idebu = ifin + 1
907              continue
                texte(ifin+2:ifin+7)= '('//loc//')'
            else
                nbspt=zi(jspt-1+ida)
                do 5 icp = 1, zi(ibcmps-1+ichs)
                    if (zi(ibcmps-1+ichs) .eq. 1 .and. icomax .gt. 1) then
                        do 6 ispt = 1, nbspt
                            if (ncmpu .eq. 0) then
                                entier=(ida-1)*6+ispt
                            else
                                entier=nucmp((ida-1)*6+ispt)
                            endif
                            nocmp = nomcmp(zi(icmps-1+(ichs-1)*ncmpmx+ icp))
                            iutil=lxlgut(nocmp)
                            call codent(entier, 'G', toto)
                            ifin = idebu+iutil+3
                            texte(idebu:ifin)= nocmp(1:iutil) //'_'//&
                            toto//' '
                            idebu = ifin + 1
 6                      continue
                    else
                        nocmp = nomcmp(zi(icmps-1+(ichs-1)*ncmpmx+icp) )
                        iutil=lxlgut(nocmp)
                        ifin = idebu+iutil
                        texte(idebu:ifin)=nocmp(1:iutil)//' '
                        idebu = ifin + 1
                    endif
 5              continue
                texte(ifin+2:ifin+7)= '('//loc
                idern = ifin+7
                if (zi(ibcmps-1+ichs) .gt. 1 .and. icomax .gt. 1) then
                    call codent(ida, 'G', toto)
                    texte(ifin+8:ifin+14)= 'SPT_'//toto
                    idern = ifin + 14
                endif
                texte(idern:idern+1)= ')'
            endif
!
            iutil = lxlgut(texte)
            jmax = lxlgut(titre)
            jmax = min(jmax,(80-iutil-3))
            entete(4)= titre(1:jmax)//' - '//texte(1:iutil)
!
            do 12 igrel = 1, nbgrel
                mode=celd(celd(4+igrel)+2)
                ipoin1=longr(igrel)
                ipoin2=longr(igrel+1)
                nbelgr=ipoin2-ipoin1-1
                if (mode .eq. 0) goto 12
                jmod = imodel+zi(ilong-1+mode)-1
                nec = nbec (zi(jmod-1+2))
                call jedetr('&&IRCERS.ENT_COD')
                call wkvect('&&IRCERS.ENT_COD', 'V V I', nec, iaec)
                call dgmode(mode, imodel, ilong, nec, zi(iaec))
                iad=celd(celd(4+igrel)+8)
                nscal = digdel(mode)
!
                if (nbcmp .ne. 0 .and. nomgd .ne. 'VARI_R') then
                    nsca=nscal
                    if (zi(inbcg+igrel-1) .ne. 0) then
                        goto 62
                    else
                        goto 12
                    endif
                endif
!
                icoef=max(1,celd(4))
                if (zi(ibcmps-1+ichs) .eq. 1 .and. icomax .gt. 1) then
                    if (ncmpu .eq. 0) then
                        ico = (ida-1)*6+1
                    else
                        ico = 1
                    endif
                else
                    ico=ida
                endif
                if (icoef .lt. ico) goto 12
                nsca = nscal*icoef
                ncmpp=0
                do 23 i = 1, ncmpmx
                    if (exisdg(zi(iaec),i)) then
                        ncmpp=ncmpp+1
                        if (ichs .eq. 1) zl(itabl-1+i)=.true.
                    endif
23              continue
                do 61 i = 1, zi(ibcmps-1+ichs)
                    if (exisdg(zi(iaec),zi(icmps-1+(ichs-1) *ncmpmx+i) )) goto 62
61              continue
                goto 12
62              continue
                do 13 ielg = 1, nbelgr
                    iel=ligrel(ipoin1+ielg-1)
                    if (iel .le. 0) goto 13
!
! --- IMPRESSION DU CHAMELEM SUR UNE LISTE DE MAILLES ---
!
                    if (nbmat .ne. 0) then
                        do 14 imai = 1, nbmat
                            if (iel .eq. nummai(imai)) goto 15
14                      continue
                        goto 13
                    endif
15                  continue
                    impel = 1
!
!           RECHERCHE DE L'ADRESSE DANS VALE DU DEBUT DES VALEURS
!
                    iachml = iad + nsca * (ielg-1)
!
!
!    --- CHAMELEM AUX NOEUDS ET AU POINTS DE GAUSS
!
!       - ELEMENTS NON DISPONIBLES DANS IDEAS
!
                    itype = typma(iel)
                    call jenuno(jexnum('&CATA.TM.NOMTM', itype), ktype)
                    if (ktype .eq. 'PYRAM5') then
                        call u2mess('A', 'PREPOST_84')
                    else if (ktype .eq. 'PYRAM13') then
                        call u2mess('A', 'PREPOST_85')
                    else
!
!    --- CHAMELEM AUX NOEUDS ---
!
                        if (loc .eq. 'ELNO') then
!
                            if (nbcmp .ne. 0 .and. nomgd .ne. 'VARI_R') then
!
                                npcalc=nscal/zi(inbctg+igrel-1)
                                nnoe = nbnoma(iel)
                                itype = typma(iel)
                                call jenuno(jexnum('&CATA.TM.NOMTM', itype), ktype)
                                if (ktype .eq. 'HEXA27') then
                                    nnoe = nnoe - 7
                                    lnocen=.true.
                                else if (ktype .eq. 'TRIA7') then
                                    nnoe = nnoe - 1
                                    lnocen=.true.
                                else if (ktype .eq. 'PENTA18') then
                                    nnoe = nnoe - 3
                                    lnocen=.true.
                                else if (ktype .eq. 'QUAD9') then
                                    nnoe = nnoe - 1
                                    lnocen=.true.
                                else if (ktype .eq. 'SEG4') then
                                    nnoe = nnoe - 2
                                    call jenonu(jexnom('&CATA.TM.NOMTM', 'SEG2' ), itseg2)
                                    itype=itseg2
                                endif
                                nbcou = npcalc / nnoe
!
                                do 516 inos = 1, nnoe
                                    inoa=0
                                    do 528 iast = 1, nnoe
                                        isup=permut(iast,itype)
                                        if (inos .eq. isup) then
                                            inoa=iast
                                            goto 529
                                        endif
528                                  continue
!
529                                  continue
                                    call assert(inoa.ne.0)
!
                                    do 561 icou = 1, nbcou
                                        jj=iachml-1+zi(inbctg+igrel-1)&
                                        *(inoa-1)+ (icou-1)*zi(inbctg+&
                                        igrel-1)*nnoe
!
                                        do 521 i = 1, nbcmpt
                                            zr(irvn-1+i)=0.d0
521                                      continue
!
                                        do 509 icm = 1, zi(inbcg+igrel-1)
                                            j=zi(iperm+zi(isnbcs+igrel-1)+&
                                        icm-1)
                                            if (j .le. 6*ida .and. j .ge. (6* ida-5)) then
                                                jt=j-6*(ida-1)
                                                ic=zi(ipos+zi(isnbcs+igrel-1)+&
                                        icm-1)
                                                zr(irvn-1+jt)= vale(jj+ic)
                                            endif
509                                      continue
!
                                        if (iente .eq. 1) then
                                            write(ifi,'(A80)') (entete(i),&
                                        i=1,10)
                                            iente=0
                                        endif
                                        if (impel .eq. 1) then
                                            if (lmasu) then
                                                call lxliis(nomel(iel)(2:8), ies, ier)
                                            else
                                                ies=iel
                                            endif
                                            write (ifi,'(4I10,5X,A,A)')&
                                        ies,1,nnoe,nbcmpt,'% MAILLE ',&
                                        nomel(iel)
                                            impel=0
                                        endif
                                        write (ifi,'(6(1PE13.5E3))') (&
                                        zr(irvn-1+i), i=1,nbcmpt)
!
561                                  continue
516                              continue
!
                            else
!
                                npcalc = nscal / ncmpp
                                nnoe = nbnoma(iel)
                                itype = typma(iel)
                                call jenuno(jexnum('&CATA.TM.NOMTM', itype), ktype)
                                if (ktype .eq. 'HEXA27') then
                                    nnoe = nnoe - 7
                                    lnocen=.true.
                                else if (ktype .eq. 'TRIA7') then
                                    nnoe = nnoe - 1
                                    lnocen=.true.
                                else if (ktype .eq. 'PENTA18') then
                                    nnoe = nnoe - 3
                                    lnocen=.true.
                                else if (ktype .eq. 'QUAD9') then
                                    nnoe = nnoe - 1
                                    lnocen=.true.
                                else if (ktype .eq. 'SEG4') then
                                    nnoe = nnoe - 2
                                    call jenonu(jexnom('&CATA.TM.NOMTM', 'SEG2' ), itseg2)
                                    itype=itseg2
                                endif
                                nbcou = npcalc / nnoe
!
                                do 16 inos = 1, nnoe
                                    inoa=0
                                    do 28 iast = 1, nnoe
                                        isup=permut(iast,itype)
                                        if (inos .eq. isup) then
                                            inoa=iast
                                            goto 29
                                        endif
28                                  continue
29                                  continue
                                    call assert(inoa.ne.0)
                                    do 161 icou = 1, nbcou
                                        j=iachml-1+ncmpp*icoef*(inoa-&
                                        1)+ (icou-1)*ncmpp*icoef*nnoe+&
                                        ncmpp*(ico-1)
                                        do 21 i = 1, nbcmpt
                                            zr(irvn-1+i)=0.d0
21                                      continue
                                        ic=0
                                        do 22 icmp = 1, ncmpmx
                                            if (exisdg(zi(iaec),icmp)) then
                                                ic=ic+1
                                                do 43 icms = 1, zi(ibcmps-1+ichs)
                                                    icmsup = zi( icmps-1+(ichs-1 )* ncmpmx+icms )
                                                    if (icmp .eq. icmsup) then
                                                        impre=1
                                                        do 26 isp = 1, zi(jspt-1+ida)
                                                            zr(irvn-1+icms-1+isp)=&
                                        vale(j+ic+ncmpp*(isp-1))
26                                                      continue
                                                        goto 22
                                                    endif
43                                              continue
                                            endif
22                                      continue
                                        if (impre .eq. 1) then
                                            if (iente .eq. 1) then
                                                write(ifi,'(A80)') (entete(i),&
                                        i=1,10)
                                                iente=0
                                            endif
                                            if (impel .eq. 1) then
                                                if (lmasu) then
                                                    call lxliis(nomel(iel)(2:8), ies, ier)
                                                else
                                                    ies=iel
                                                endif
                                                write (ifi,'(4I10,5X,A,A)')&
                                        ies,1,nnoe,nbcmpt,'% MAILLE ',&
                                        nomel(iel)
                                                impel=0
                                            endif
                                            write (ifi,'(6(1PE13.5E3))') (&
                                        zr(irvn-1+i), i=1,nbcmpt)
                                        endif
161                                  continue
16                              continue
                            endif
!
!  --- CHAMELEM AUX POINTS DE GAUSS---
!
                        else if (loc.eq.'ELGA'.or.loc.eq.'ELEM') then
!
                            if (nbcmp .ne. 0 .and. nomgd .ne. 'VARI_R') then
                                nbpg=nscal/zi(inbctg+igrel-1)
!
                                do 908 i = 1, nbcmpt
                                    zr(irvg-1+i)=0.d0
908                              continue
!
                                do 909 icm = 1, zi(inbcg+igrel-1)
                                    j=zi(iperm+zi(isnbcs+igrel-1)+icm-&
                                    1)
!
                                    if (j .le. 6*ida .and. j .ge. (6*ida- 5)) then
                                        jt=j-6*(ida-1)
!
                                        do 910 ipg = 1, nbpg
                                            jj=zi(inbctg+igrel-1)*(ipg-1)+&
                                        zi(ipos+zi(isnbcs+igrel-1)+&
                                        icm-1)
                                            zr(irvg-1+jt)=zr(irvg-1+jt)+&
                                        vale(iachml-1+jj)
910                                      continue
                                        zr(irvg-1+jt)=zr(irvg-1+jt)/&
                                        nbpg
!
                                    endif
909                              continue
!
                                if (iente .eq. 1) then
                                    write(ifi,'(A80)') (entete(i),i=1,&
                                    10)
                                    iente=0
                                endif
                                if (lmasu) then
                                    call lxliis(nomel(iel)(2:8), ies, ier)
                                else
                                    ies=iel
                                endif
                                write(ifi,'(2I10,5X,2A)')ies,nbcmpt,&
                                '% MAILLE ',nomel(iel)
                                write (ifi,'(6(1PE13.5E3))') (zr(irvg-&
                                1+i),i=1,nbcmpt)
!
!
                            else
                                npcalc = nscal/ncmpp
                                nbpg=npcalc
                                do 18 i = 1, nbcmpt
                                    zr(irvg-1+i)=0.d0
18                              continue
                                ic=0
                                do 19 icmp = 1, ncmpmx
                                    if (exisdg(zi(iaec),icmp)) then
                                        ic=ic+1
                                        do 37 icms = 1, zi(ibcmps-1+ichs)
                                            icmsup = zi( icmps-1+(ichs-1 )* ncmpmx+icms )
                                            if (icmp .eq. icmsup) then
                                                impre=1
                                                do 36 isp = 1, zi(jspt-1+ida)
                                                    if (ncmpu .eq. 0) then
                                                        is0 = isp
                                                    else
                                                        is0 = nucmp( (ida-1)*6+isp)
                                                    endif
                                                    do 17 ipg = 1, nbpg
!
                                                        j=iachml-1+ncmpp*icoef*(ipg-1)&
                                        +ncmpp*(ico-1)
                                                        zr(irvg-1+icms-1+isp)= zr(&
                                        irvg-1+icms-1+isp)+ vale(j+ic+&
                                        ncmpp*(is0-1))
!
17                                                  continue
                                                    zr(irvg-1+icms-1+isp)=zr(irvg-&
                                        1+icms-1+isp) / nbpg
!
36                                              continue
                                                goto 19
                                            endif
37                                      continue
                                    endif
19                              end do
                                if (impre .eq. 1) then
                                    if (iente .eq. 1) then
                                        write(ifi,'(A80)') (entete(i),&
                                        i=1,10)
                                        iente=0
                                    endif
                                    if (impel .eq. 1) then
                                        if (lmasu) then
                                            call lxliis(nomel(iel)(2:8), ies, ier)
                                        else
                                            ies=iel
                                        endif
                                        write(ifi,'(2I10,5X,2A)')ies,&
                                        nbcmpt, '% MAILLE ',nomel(iel)
                                        impel=0
                                    endif
                                    write (ifi,'(6(1PE13.5E3))') (zr(&
                                    irvg-1+i),i=1,nbcmpt)
                                    impre=0
                                endif
                            endif
                        endif
                    endif
13              end do
12          end do
            if (iente .eq. 0) write (ifi,'(A)') '    -1'
11      end do
10  end do
!
    if (lnocen) then
        call u2mess('A', 'PREPOST_86')
    endif
!
    call jedetr('&&IRCERS.VALNOE')
    call jedetr('&&IRCERS.VALGAU')
    call jedetr('&&IRCERS.SOUS_PT')
    call jedetr('&&IRCERS.ENT_COD')
    call jedetr('&&IRCERS.NOMGDS')
    call jedetr('&&IRCERS.NOMCHS')
    call jedetr('&&IRCERS.NBCMPS')
    call jedetr('&&IRCERS.IPCMPS')
    call jedetr('&&IRCERS.LTABL')
    call jedetr('&&IRCERS.PERM')
    call jedetr('&&IRCERS.NBCMPS_GREL')
    call jedetr('&&IRCERS.POS')
    call jedetr('&&IRCERS.NBCMPT_GREL')
    call jedetr('&&IRCERS.SNBCPS')
    call jedetr('&&IRCERS.CMP_GREL')
    call jedetr('&&IRCERS.SCMP_DATS')
    call jedema()
end subroutine
