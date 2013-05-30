subroutine irdesc(ifi, nbno, prno, nueq, nec,&
                  dg, ncmpmx, vale, nomcmp, titr,&
                  nomnoe, nomsd, nomsym, ir, numnoe,&
                  lmasu)
    implicit none
!
    include 'jeveux.h'
    include 'asterfort/ecrtes.h'
    include 'asterfort/exisdg.h'
    include 'asterfort/irgags.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/lxlgut.h'
    include 'asterfort/lxliis.h'
    include 'asterfort/wkvect.h'
    integer :: ifi, nbno, nueq(*), prno(*), nec, dg(*), ncmpmx
    integer :: ir, numnoe(*)
    complex(kind=8) :: vale(*)
    character(len=*) :: nomcmp(*)
    character(len=*) :: titr, nomnoe(*), nomsd, nomsym
    logical :: lmasu
!
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
!        ECRITURE D'UN CHAM_NO SUR FICHIER UNIVERSEL, DATASET TYPE 55
!        A VALEURS COMPLEXES
!      ENTREE:
!         IFI   : UNITE LOGIQUE DU FICHIER UNIVERSEL
!                NBNO  : NOMBRE DE NOEUDS DU LIGREL ( DU MAILLAGE)
!         PRNO  : PROFIL-NOEUDS
!         NUEQ  : OBJET .NUEQ DU PROF_CHNO
!         NEC   : NOMBRE D'ENTIERS-CODES
!         DG    : ENTIERS CODES
!         NCMPMX: NOMBRE MAXI DE CMP DE LA GRANDEUR
!         VALE  : VALEURS DU CHAM_NO
!         NOMCMP: NOMS DES CMP (9 MAXI)
!         TITR  : 1 LIGNES DE TITRE
!         NOMNOE: NOMS DES NOEUDS
!         NOMSD : NOMS DU RESULTAT
!         NOMSYM: NOM SYMBOLIQUE
!         IR    : NUMERO D'ORDRE DU CHAMP
!         NUMNOE: NUMEROS DES NOEUDS
!         LMASU : INDIQUE SI MAILLAGE SUPERTAB  .TRUE. MAILLAGE SUPERTAB
!
! --------------------------------------------------------------------
!     ------------------------------------------------------------------
    character(len=8) :: nocmp
    character(len=24) :: nomst
    character(len=80) :: entete(10), titre, texte
    integer :: nbchs
    integer :: impre, iente, iutil
    logical :: afaire, lcmp
!
!
!  --- INITIALISATIONS ----
!
!-----------------------------------------------------------------------
    integer :: i, ibcmps, ic, ichs, icmp, icmps, icms
    integer :: icmsup, icompt, icp, icval, idebu, iec, ier
    integer :: ifin, inno, ino, inochs, inogds, iret, irval
    integer :: itabl, ival, jmax, jtitr, ncmp
!-----------------------------------------------------------------------
    call jemarq()
!
    call wkvect('&&IRDESC.NOMGDS', 'V V K8', ncmpmx, inogds)
    call wkvect('&&IRDESC.NOMCHS', 'V V K8', ncmpmx, inochs)
    call wkvect('&&IRDESC.NBCMPS', 'V V I', ncmpmx, ibcmps)
    call wkvect('&&IRDESC.IPCMPS', 'V V I', ncmpmx*ncmpmx, icmps)
    call wkvect('&&IRDESC.LTABL', 'V V L', ncmpmx, itabl)
!
    nomst= '&&IRECRI.SOUS_TITRE.TITR'
    call jeveuo(nomst, 'L', jtitr)
    titre = zk80(jtitr)
    do 1 i = 1, ncmpmx
        zl(itabl-1+i)=.false.
 1  end do
!
! --- ALLOCATION DES TABLEAUX DE TRAVAIL ----
!
    call jeexin('&&IRDESC.VALR', iret)
    if (iret .ne. 0) call jedetr('&&IRDESC.VALR')
    call wkvect('&&IRDESC.VALR', 'V V R', ncmpmx, irval)
    call jeexin('&&IRDESC.VALC', iret)
    if (iret .ne. 0) call jedetr('&&IRDESC.VALC')
    call wkvect('&&IRDESC.VALC', 'V V R', ncmpmx, icval)
! ---- RECHERCHE DES GRANDEURS SUPERTAB -----
!
    call irgags(ncmpmx, nomcmp, nomsym, nbchs, zk8(inochs),&
                zi(ibcmps), zk8(inogds), zi(icmps))
!
! ---- BOUCLE SUR LES DIVERSES GRANDEURS SUPERTAB ----
    do 10 ichs = 1, nbchs
        if (ichs .gt. 1) then
            afaire=.false.
            do 2 icp = 1, zi(ibcmps-1+ichs)
                afaire = ( afaire .or. zl(itabl-1+ zi(icmps-1+(ichs-1)* ncmpmx+icp)) )
 2          continue
            if (.not. afaire) goto 10
        endif
        iente = 1
        impre = 0
        lcmp=.false.
        call ecrtes(nomsd, titr, zk8(inogds-1+ichs), ir, 'NOEU',&
                    zi( ibcmps-1+ichs), 5, entete, lcmp)
        idebu=1
        entete(4) = ' '
        texte = ' '
        do 5 icp = 1, zi(ibcmps-1+ichs)
            nocmp = nomcmp(zi(icmps-1+(ichs-1)*ncmpmx+icp))
            iutil = lxlgut(nocmp)
            ifin = idebu+iutil
            texte(idebu:ifin) = nocmp(1:iutil)//' '
            idebu = ifin+1
 5      continue
        iutil = lxlgut(texte)
        jmax = lxlgut(titre)
        jmax = min(jmax,(80-iutil-2))
        entete(4)= titre(1:jmax)//' - '//texte(1:iutil)
        do 11 inno = 1, nbno
            ino = numnoe(inno)
            do 17 iec = 1, nec
                dg(iec)=prno((ino-1)*(nec+2)+2+iec)
17          continue
!
!              NCMP : NOMBRE DE CMPS SUR LE NOEUD INO
!              IVAL : ADRESSE DU DEBUT DU NOEUD INO DANS .NUEQ
            ival = prno((ino-1)* (nec+2)+1)
            ncmp = prno((ino-1)* (nec+2)+2)
            if (ncmp .eq. 0) goto 11
!
            do 25 ic = 1, zi(ibcmps-1+ichs)
                zr(irval-1+ic) = 0.0d0
                zr(icval-1+ic) = 0.0d0
25          continue
            icompt = 0
            do 12 icmp = 1, ncmpmx
                if (exisdg(dg,icmp)) then
                    if (ichs .eq. 1) zl(itabl-1+icmp)= .true.
                    icompt = icompt + 1
                    do 13 icms = 1, zi(ibcmps-1+ichs)
                        icmsup = zi(icmps-1+(ichs-1)*ncmpmx+icms)
                        if (icmp .eq. icmsup) then
                            impre = 1
                            zr(irval-1+icms)=dble(vale(nueq(ival-1+&
                            icompt)))
                            zr(icval-1+icms)=dimag(vale(nueq(ival-1+&
                            icompt)))
                            goto 12
                        endif
13                  continue
                endif
12          continue
!
            if (impre .eq. 1) then
                if (iente .eq. 1) then
                    write(ifi,'(A80)') (entete(i),i=1,10)
                    iente=0
                endif
                if (lmasu) then
                    call lxliis(nomnoe(inno)(2:8), ino, ier)
                endif
                write (ifi,'(I10,5X,A,A)') ino,'% NOEUD ',nomnoe(inno)
                write (ifi,'(6(1PE13.5))') (zr(irval-1+i), zr(icval-1+&
                i),i=1,zi(ibcmps-1+ichs))
                impre=0
            endif
11      end do
        if (iente .eq. 0) write (ifi,'(A)') '    -1'
10  end do
    call jedetr('&&IRDESC.VALR')
    call jedetr('&&IRDESC.VALC')
    call jedetr('&&IRDESC.NOMGDS')
    call jedetr('&&IRDESC.NOMCHS')
    call jedetr('&&IRDESC.NBCMPS')
    call jedetr('&&IRDESC.IPCMPS')
    call jedetr('&&IRDESC.LTABL')
    call jedema()
end subroutine
