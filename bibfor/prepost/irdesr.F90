subroutine irdesr(ifi, nbno, prno, nueq, nec,&
                  dg, ncmpmx, vale, nomcmp, titr,&
                  nomnoe, nomsd, nomsym, ir, numnoe,&
                  lmasu, nbcmp, ncmps, nocmpl)
    implicit none
!
    include 'jeveux.h'
!
    include 'asterfort/ecrtes.h'
    include 'asterfort/exisdg.h'
    include 'asterfort/irgags.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/lxlgut.h'
    include 'asterfort/lxliis.h'
    include 'asterfort/wkvect.h'
    integer :: ifi, nbno, prno(*), nueq(*), nec, dg(*), ncmpmx
    integer :: ir, numnoe(*), ncmps(*), nbcmp
    real(kind=8) :: vale(*)
    character(len=*) :: nomcmp(*), nocmpl(*)
    character(len=*) :: titr, nomnoe(*), nomsd, nomsym
    logical :: lmasu
!
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
!--------------------------------------------------------------------
!        ECRITURE D'UN CHAM_NO SUR FICHIER UNIVERSEL, DATASET TYPE 55
!        A VALEURS REELLES
!      ENTREE:
!         IFI   : UNITE LOGIQUE DU FICHIER UNIVERSEL
!                NBNO  : NOMBRE DE NOEUDS DU LIGREL ( DU MAILLAGE)
!         PRNO  : OBJET .PRNO DU PROF_CHNO
!         NUEQ  : OBJET .NUEQ DU PROF_CHNO
!         NEC   : NOMBRE D'ENTIERS-CODES
!         DG    : ENTIERS CODES
!         NCMPMX: NOMBRE MAXI DE CMP DE LA GRANDEUR
!         VALE  : VALEURS DU CHAM_NO
!         NOMCMP: NOMS DES CMP
!         TITR  : 1 LIGNE DE TITRE
!         NOMNOE: NOMS DES NOEUDS
!         NUMNOE: NUMEROS DES NOEUDS
!         NOMSD : NOM DU RESULTAT
!         NOMSYM: NOM SYMBOLIQUE
!         IR    : NUMERO D'ORDRE DU CHAMP
!         LMASU : INDIQUE SI MAILLAGE SUPERTAB  .TRUE. MAILLAGE SUPERTAB
!         NOCMPL: NOMS DES COMPOSANTES SELECTIONNEES
!         NBCMP : NOMBRE DE COMPOANTES SELECTIONNEES
!         NCMPS : NUMEROS DES COMPOSANTES SELECTIONNEES
!
!     ------------------------------------------------------------------
    character(len=8) :: nocmp, nomgs
    character(len=24) :: nomst
    character(len=80) :: entete(10), titre, texte
    integer :: nbchs, nbcmpt
    integer :: impre, iente, iutil
    logical :: afaire, lcmp
!
!  --- INITIALISATIONS ----
!
!-----------------------------------------------------------------------
    integer :: i, ibcmps, ic, ichs, icmp, icmps, icms
    integer :: icmsup, icompt, icp, ida, idebu, iec, ier
    integer :: ifin, ilig, indats, inno, ino, inochs, inogds
    integer :: ires, irval, itabl, ival, j, jadm
    integer :: jj, jl, jmax, jpos, jtitr, k, l
    integer :: ll, nbdats, ncmp, ni
!-----------------------------------------------------------------------
    call jemarq()
!
    call wkvect('&&IRDESR.NOMGDS', 'V V K8', ncmpmx, inogds)
    call wkvect('&&IRDESR.NOMCHS', 'V V K8', ncmpmx, inochs)
    call wkvect('&&IRDESR.NBCMPS', 'V V I', ncmpmx, ibcmps)
    call wkvect('&&IRDESR.IPCMPS', 'V V I', ncmpmx*ncmpmx, icmps)
    call wkvect('&&IRDESR.LTABL', 'V V L', ncmpmx, itabl)
!
    nomst= '&&IRECRI.SOUS_TITRE.TITR'
    call jeveuo(nomst, 'L', jtitr)
    titre = zk80(jtitr)
    do 1 i = 1, ncmpmx
        zl(itabl-1+i)=.false.
 1  end do
!
! --- ALLOCATION DES TABLEAUX DE TRAVAIL ---
!
    call jedetr('&&IRDESR.VAL')
    call wkvect('&&IRDESR.VAL', 'V V R', ncmpmx, irval)
!
! ---- RECHERCHE DES GRANDEURS SUPERTAB -----
!
    call irgags(ncmpmx, nomcmp, nomsym, nbchs, zk8(inochs),&
                zi(ibcmps), zk8(inogds), zi(icmps))
!
!      ==================
! ---- PARTIE 1 : NBCMP=0
!      ===================
!
    if (nbcmp .eq. 0) then
!
! ---- BOUCLE SUR LES DIVERSES GRANDEURS SUPERTAB ----
        do 10 ichs = 1, nbchs
            if (ichs .gt. 1) then
                afaire=.false.
                do 2 icp = 1, zi(ibcmps-1+ichs)
                    afaire = ( afaire .or. zl(itabl-1+ zi(icmps-1+(ichs- 1)*ncmpmx+icp)) )
 2              continue
                if (.not. afaire) goto 10
            endif
            iente = 1
            impre = 0
            lcmp=.false.
            call ecrtes(nomsd, titr, zk8(inogds-1+ichs), ir, 'NOEU',&
                        zi(ibcmps-1+ichs), 2, entete, lcmp)
            idebu = 1
            entete(4) = ' '
            texte = ' '
            do 5 icp = 1, zi(ibcmps-1+ichs)
                nocmp = nomcmp(zi(icmps-1+(ichs-1)*ncmpmx+icp))
                iutil = lxlgut(nocmp)
                ifin = idebu+iutil
                texte(idebu:ifin) = nocmp(1:iutil)//' '
                idebu = ifin+1
 5          continue
            iutil = lxlgut(texte)
            jmax = lxlgut(titre)
            jmax = min(jmax,(80-iutil-2))
            entete(4)= titre(1:jmax)//' - '//texte(1:iutil)
!
            do 11 inno = 1, nbno
                ino = numnoe(inno)
                do 17 iec = 1, nec
                    dg(iec)=prno((ino-1)*(nec+2)+2+iec)
17              continue
!
!         NCMP : NOMBRE DE CMPS SUR LE NOEUD INO
!         IVAL : ADRESSE DU DEBUT DU NOEUD INO DANS .NUEQ
                ival = prno((ino-1)* (nec+2)+1)
                ncmp = prno((ino-1)* (nec+2)+2)
                if (ncmp .eq. 0) goto 11
!
                do 25 ic = 1, zi(ibcmps-1+ichs)
                    zr(irval-1+ic) = 0.0d0
25              continue
!
                icompt = 0
                do 12 icmp = 1, ncmpmx
                    if (exisdg(dg,icmp)) then
                        if (ichs .eq. 1) zl(itabl-1+icmp)= .true.
                        icompt = icompt + 1
                        do 13 icms = 1, zi(ibcmps-1+ichs)
                            icmsup = zi(icmps-1+(ichs-1)*ncmpmx+icms)
                            if (icmp .eq. icmsup) then
                                impre = 1
                                zr(irval-1+icms) = vale(nueq(ival-1+ icompt))
                                goto 12
                            endif
13                      continue
                    endif
12              continue
!
                if (impre .eq. 1) then
                    if (iente .eq. 1) then
                        write(ifi,'(A80)') (entete(i),i=1,10)
                        iente=0
                    endif
                    if (lmasu) then
                        call lxliis(nomnoe(inno)(2:8), ino, ier)
                    endif
                    write (ifi,'(I10,5X,A,A)') ino,'% NOEUD ',nomnoe(&
                    inno)
                    write (ifi,'(6(1PE13.5E3))') (zr(irval-1+i),&
                    i=1,zi(ibcmps-1+ichs))
                    impre=0
                endif
11          end do
            if (iente .eq. 0) write (ifi,'(A)') '    -1'
10      end do
!
!      =====================
! ---- PARTIE 2 : NBCMP.NE.0
!      =====================
!
    else
!
! --- GRANDEUR SUPERTAB
        do 897 i = 1, nbchs
            do 898 j = 1, zi(ibcmps+i-1)
                if (ncmps(1) .eq. zi(icmps-1+(i-1)*ncmpmx+j)) goto 899
898          continue
897      end do
899      continue
        nomgs=zk8(inogds-1+i)
!
! --- NOMBRE DE DATASET
        call wkvect('&&IRDESR.CMP_DATS', 'V V I', nbcmp, indats)
        nbcmpt=6
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
901          continue
        else
            nbdats=ilig+1
            do 902 i = 1, nbdats-1
                zi(ibcmps+i-1)=6
                ni=ni+6
                zi(indats+i)=ni
902          continue
            zi(ibcmps+nbdats-1)=ires
            zi(indats+nbdats)=ni+ires
        endif
!
! --- ECRITURE DE L'ENTETE SUPERTAB ----
        lcmp=.true.
        call ecrtes(nomsd, titr, nomgs, ir, 'NOEU',&
                    nbcmpt, 2, entete, lcmp)
!
! --- BOUCLE SUR LES DATASETS
!     -----------------------
        do 810 ida = 1, nbdats
!
            iente = 1
            idebu = 1
            entete(4) = ' '
            texte = ' '
!
            do 865 icp = 1, zi(ibcmps+ida-1)
                nocmp = nocmpl(icp+zi(indats+ida-1))
                iutil=lxlgut(nocmp)
                ifin = idebu+iutil
                texte(idebu:ifin)=nocmp(1:iutil)//' '
                idebu = ifin + 1
865          continue
!
            iutil = lxlgut(texte)
            jmax = lxlgut(titre)
            jmax = min(jmax,(80-iutil-2))
            entete(4)= titre(1:jmax)//' - '//texte(1:iutil)
!
            do 811 inno = 1, nbno
!
                ino = numnoe(inno)
                do 817 iec = 1, nec
                    dg(iec)=prno((ino-1)*(nec+2)+2+iec)
817              continue
!
                ncmp = prno((ino-1)*(nec+2)+2)
                if (ncmp .eq. 0) goto 811
!
!
                do 825 ic = 1, 6
                    zr(irval-1+ic) = 0.0d0
825              continue
!
! ---       COMPOSANTES ADMISES
                call jedetr('&&IRDESR.CMP')
                call wkvect('&&IRDESR.CMP', 'V V I', ncmpmx, jadm)
                call jedetr('&&IRDESR.POS')
                call wkvect('&&IRDESR.POS', 'V V I', nbcmp, jpos)
                k=0
                do 777 icmp = 1, ncmpmx
                    if (exisdg(dg,icmp)) then
                        zi(jadm+k)=icmp
                        k=k+1
                    endif
777              continue
!
! ---       POSITIONS DES COMPOSANTES SELECTIONNEES PARMI LES
!           COMPOSANTES ADMISES
                l=0
                do 778 j = 1, zi(ibcmps+ida-1)
                    ll=0
                    do 779 jl = 1, k
                        ll=ll+1
                        if (zi(jadm+jl-1) .eq. ncmps(j+zi(indats+ida-1))) goto 780
779                  continue
780                  continue
                    zi(jpos+l)=ll
                    l=l+1
778              continue
!
                ival = prno((ino-1)*(nec+2)+1)
                do 812 icmp = 1, zi(ibcmps+ida-1)
                    jj=zi(jpos+icmp-1)
                    zr(irval-1+icmp)=vale(nueq(ival-1+jj))
812              continue
!
                if (iente .eq. 1) then
                    write(ifi,'(A80)') (entete(i),i=1,10)
                    iente=0
                endif
!
                if (lmasu) then
                    call lxliis(nomnoe(inno)(2:8), ino, ier)
                endif
!
                write (ifi,'(I10,5X,A,A)') ino,'% NOEUD ',nomnoe(inno)
                write (ifi,'(6(1PE13.5E3))') (zr(irval-1+i),i=1,6)
!
811          continue
            if (iente .eq. 0) write (ifi,'(A)') '    -1'
810      continue
!
    endif
!
! --- MENAGE
!
    call jedetr('&&IRDESR.NOMGDS')
    call jedetr('&&IRDESR.NOMCHS')
    call jedetr('&&IRDESR.NBCMPS')
    call jedetr('&&IRDESR.IPCMPS')
    call jedetr('&&IRDESR.LTABL')
    call jedetr('&&IRDESR.VAL')
    call jedetr('&&IRDESR.CMP_DATS')
    call jedetr('&&IRDESR.CMP')
    call jedetr('&&IRDESR.POS')
!
    call jedema()
end subroutine
