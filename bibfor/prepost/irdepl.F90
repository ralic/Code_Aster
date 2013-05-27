subroutine irdepl(chamno, partie, ifi, form, titre,&
                  nomsd, nomsym, numord, lcor, nbnot,&
                  numnoe, nbcmp, nomcmp, lsup, borsup,&
                  linf, borinf, lmax, lmin, lresu,&
                  formr, nive)
    implicit none
!
    include 'jeveux.h'
!
    include 'asterc/getres.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/imprsd.h'
    include 'asterfort/irccmp.h'
    include 'asterfort/ircnc8.h'
    include 'asterfort/ircnrl.h'
    include 'asterfort/ircrrl.h'
    include 'asterfort/irdeca.h'
    include 'asterfort/irdesc.h'
    include 'asterfort/irdesr.h'
    include 'asterfort/irdrca.h'
    include 'asterfort/irdrsr.h'
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
    include 'asterfort/lxcaps.h'
    include 'asterfort/lxlgut.h'
    include 'asterfort/nbec.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    character(len=*) :: chamno, form, titre, nomsd, nomsym
    character(len=*) :: nomcmp(*), formr, partie
    integer :: nbnot, ifi, numnoe(*), nbcmp, nive
    integer :: numord
    logical :: lcor
    logical :: lsup, linf, lmax, lmin
    logical :: lresu
    real(kind=8) :: borsup, borinf
!_____________________________________________________________________
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
!        IMPRESSION D'UN CHAMNO A COMPOSANTES REELLES OU COMPLEXES
!         AU FORMAT IDEAS, ...
!     ENTREES:
!        CHAMNO : NOM DU CHAMNO A ECRIRE
!        PARTIE : IMPRESSION DE LA PARTIE COMPLEXE OU REELLE DU CHAMP
!        IFI    : NUMERO LOGIQUE DU FICHIER DE SORTIE
!        FORM   : FORMAT DES SORTIES: IDEAS, RESULTAT
!        TITRE  : TITRE POUR IMPRESSION IDEAS
!        NOMSD  : NOM DU RESULTAT D'OU PROVIENT LE CHAMNO A IMPRIMER.
!        NOMSYM : NOM SYMBOLIQUE
!        NUMORD : NUMERO DE CALCUL, MODE,  CAS DE CHARGE
!        LCOR   : IMPRESSION DES COORDONNEES  .TRUE. IMPRESSION
!        NBNOT  : NOMBRE DE NOEUDS A IMPRIMER
!        NUMNOE : NUMEROS DES NOEUDS A IMPRIMER
!        NBCMP  : NOMBRE DE COMPOSANTES A IMPRIMER AU FORMAT RESULTAT
!        NOMCMP : NOMS DES COMPOSANTES A IMPRIMER AU FORMAT RESULTAT
!        LSUP   : =.TRUE. INDIQUE PRESENCE D'UNE BORNE SUPERIEURE
!        BORSUP : VALEUR DE LA BORNE SUPERIEURE
!        LINF   : =.TRUE. INDIQUE PRESENCE D'UNE BORNE INFERIEURE
!        BORINF : VALEUR DE LA BORNE INFERIEURE
!        LMAX   : =.TRUE. INDIQUE IMPRESSION VALEUR MAXIMALE
!        LMIN   : =.TRUE. INDIQUE IMPRESSION VALEUR MINIMALE
!        LRESU  : =.TRUE. INDIQUE IMPRESSION D'UN CONCEPT RESULTAT
!        FORMR  : FORMAT D'ECRITURE DES REELS SUR "RESULTAT"
!        NIVE   : NIVEAU IMPRESSION CASTEM 3 OU 10
!     ------------------------------------------------------------------
!
    character(len=1) :: k1bid, type
    integer :: gd, lgconc, lgch16
    integer :: jvale, nuti
    logical :: lmasu
    character(len=8) :: nomsdr, nomma, nomgd, cbid, forma
    character(len=16) :: nomcmd, nosy16
    character(len=19) :: chamn
    character(len=24) :: nomnu
    character(len=24) :: valk(3)
    character(len=80) :: titmai
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, iad, iadesc, iaec, ianueq, iaprno, iarefe
    integer :: iavale, ibid, ier, ino, iret, itype, jcoor
    integer :: jncmp, jno, jnu, jtitr, nbcmpt
    integer :: nbno, nbnot2, nbtitr, ncmpmx, ndim, nec, num
!
!-----------------------------------------------------------------------
    call jemarq()
!
    chamn = chamno(1:19)
    forma = form
    nosy16 = nomsym
    nbcmpt=0
    call jeveuo(chamn//'.REFE', 'L', iarefe)
!     --- NOM DU MAILLAGE
    nomma = zk24(iarefe-1+1) (1:8)
!     --- NOM DU PROFIL AUX NOEUDS ASSOCIE S'IL EXISTE
    nomnu = zk24(iarefe-1+2)
!
    call jelira(chamn//'.VALE', 'TYPE', ibid, type)
    if (type(1:1) .eq. 'R') then
        itype = 1
    else if (type(1:1).eq.'C') then
        itype = 2
    else if (type(1:1).eq.'I') then
        itype = 3
    else if (type(1:1).eq.'K') then
        itype = 4
    else
        call getres(cbid, cbid, nomcmd)
        call u2mesk('A', 'PREPOST_97', 1, type(1:1))
        goto 9999
    endif
!
    call jeveuo(chamn//'.VALE', 'L', iavale)
!
    call jeveuo(chamn//'.DESC', 'L', iadesc)
    gd = zi(iadesc-1+1)
    num = zi(iadesc-1+2)
!
    call jenuno(jexnum('&CATA.GD.NOMGD', gd), nomgd)
!
!     --- NOMBRE D'ENTIERS CODES POUR LA GRANDEUR NOMGD
    nec = nbec(gd)
!
    call jeexin('&&IRDEPL.ENT_COD', iret)
    if (iret .ne. 0) call jedetr('&&IRDEPL.ENT_COD')
    call wkvect('&&IRDEPL.ENT_COD', 'V V I', nec, iaec)
    call jelira(jexnum('&CATA.GD.NOMCMP', gd), 'LONMAX', ncmpmx, k1bid)
    call jeveuo(jexnum('&CATA.GD.NOMCMP', gd), 'L', iad)
    call wkvect('&&IRDEPL.NUM_CMP', 'V V I', ncmpmx, jncmp)
!
    if (nbcmp .ne. 0) then
!       - NOMBRE ET NOMS DES COMPOSANTES DE LA LISTE DES COMPOSANTES
!         DONT ON DEMANDE L'IMPRESSION PRESENTES DANS LA GRANDEUR NOMGD
        call irccmp(' ', nomgd, ncmpmx, zk8(iad), nbcmp,&
                    nomcmp, nbcmpt, jncmp)
!       - SI SELECTION SUR LES COMPOSANTES ET AUCUNE PRESENTE DANS LE
!       - CHAMP A IMPRIMER ALORS IL N'Y A RIEN A FAIRE
        if (nbcmpt .eq. 0) goto 9997
    endif
!
!     --- SI LE CHAMP EST A REPRESENTATION CONSTANTE: RIEN DE SPECIAL
!
!     --- SI LE CHAMP EST DECRIT PAR UN "PRNO":
    if (num .ge. 0) then
        call jeveuo(nomnu(1:19)//'.NUEQ', 'L', ianueq)
        call jenonu(jexnom(nomnu(1:19)//'.LILI', '&MAILLA'), ibid)
        call jeveuo(jexnum(nomnu(1:19)//'.PRNO', ibid), 'L', iaprno)
    endif
!
!     --- NOMBRE DE NOEUDS DU MAILLAGE: NBNO
    call dismoi('F', 'NB_NO_MAILLA', nomma, 'MAILLAGE', nbno,&
                cbid, ier)
!
!     --- CREATION LISTES DES NOMS ET DES NUMEROS DES NOEUDS A IMPRIMER
    call wkvect('&&IRDEPL.NOMNOE', 'V V K8', nbno, jno)
    call wkvect('&&IRDEPL.NUMNOE', 'V V I', nbno, jnu)
    if (nbnot .eq. 0) then
!       - IL N'Y A PAS EU DE SELECTION SUR ENTITES TOPOLOGIQUES EN
!         OPERANDE DE IMPR_RESU => ON PREND TOUS LES NOEUDS DU MAILLAGE
        do 11 ino = 1, nbno
            call jenuno(jexnum(nomma//'.NOMNOE', ino), zk8(jno-1+ino))
            zi(jnu-1+ino) = ino
            nbnot2= nbno
11      continue
!
    else
!       - IL Y A EU SELECTION SUR DES ENTITES TOPOLOGIQUES => ON NE
!         PREND QUE LES NOEUDS DEMANDES (APPARTENANT A UNE LISTE DE
!         NOEUDS, DE MAILLES, DE GPES DE NOEUDS OU DE GPES DE MAILLES)
        do 12 ino = 1, nbnot
            zi(jnu-1+ino) = numnoe(ino)
            call jenuno(jexnum(nomma//'.NOMNOE', numnoe(ino)), zk8(jno- 1+ino))
12      continue
        nbnot2= nbnot
    endif
! --- RECHERCHE DES COORDONNEES ET DE LA DIMENSION -----
    call dismoi('F', 'DIM_GEOM_B', nomma, 'MAILLAGE', ndim,&
                cbid, ier)
    call jeveuo(nomma//'.COORDO    .VALE', 'L', jcoor)
!
    if (form .eq. 'RESULTAT') then
        if (itype .eq. 1 .and. num .ge. 0) then
            call ircnrl(ifi, nbnot2, zi(iaprno), zi(ianueq), nec,&
                        zi(iaec), ncmpmx, zr(iavale), zk8(iad), zk8(jno),&
                        lcor, ndim, zr(jcoor), zi(jnu), nbcmpt,&
                        zi(jncmp), lsup, borsup, linf, borinf,&
                        lmax, lmin, formr)
        else if (itype.eq.1.and.num.lt.0) then
            call ircrrl(ifi, nbnot2, zi(iadesc), nec, zi(iaec),&
                        ncmpmx, zr( iavale), zk8(iad), zk8(jno), lcor,&
                        ndim, zr(jcoor), zi(jnu), nbcmpt, zi(jncmp),&
                        lsup, borsup, linf, borinf, lmax,&
                        lmin, formr)
        else if (itype.eq.2.and.num.ge.0) then
            call ircnc8(ifi, nbnot2, zi(iaprno), zi(ianueq), nec,&
                        zi(iaec), ncmpmx, zc(iavale), zk8(iad), zk8(jno),&
                        lcor, ndim, zr(jcoor), zi(jnu), nbcmpt,&
                        zi(jncmp), lsup, borsup, linf, borinf,&
                        lmax, lmin, formr)
        else if (itype.eq.2.and.num.lt.0) then
            call u2mesk('E', 'PREPOST2_35', 1, forma)
        else if ((itype.eq.3).or.(itype.eq.4)) then
            call imprsd('CHAMP', chamno, ifi, nomsd)
        endif
!
    else if (form(1:6).eq.'CASTEM') then
!
! ------ AU FORMAT CASTEM, PAS DE MINUSCULES
!        LE NOM DU CHAM_GD EST DANS LA VARIABLE NOMSYM
        if (.not. lresu) call lxcaps(nosy16)
!
        if (itype .eq. 1 .and. num .ge. 0) then
            call irdeca(ifi, nbnot2, zi(iaprno), zi(ianueq), nec,&
                        zi(iaec), ncmpmx, zr(iavale), nomgd, zk8(iad),&
                        nosy16, zi(jnu), lresu, nbcmp, nomcmp,&
                        nive)
        else if (itype.eq.1.and.num.lt.0) then
            call irdrca(ifi, nbnot2, zi(iadesc), nec, zi(iaec),&
                        ncmpmx, zr( iavale), nomgd, zk8(iad), nosy16,&
                        zi(jnu), lresu, nbcmp, nomcmp, nive)
        else if (itype.eq.2.and.num.ge.0) then
            call jelira(chamn//'.VALE', 'LONUTI', nuti, k1bid)
            call wkvect('&&IRDEPL.VALE', 'V V R', nuti, jvale)
!
            if (partie .eq. 'REEL') then
                do 10 i = 1, nuti
                    zr(jvale-1+i)=dble(zc(iavale-1+i))
10              continue
            else if (partie.eq.'IMAG') then
                do 20 i = 1, nuti
                    zr(jvale-1+i)=dimag(zc(iavale-1+i))
20              continue
            else
                call u2mess('F', 'PREPOST2_4')
            endif
            call irdeca(ifi, nbnot2, zi(iaprno), zi(ianueq), nec,&
                        zi(iaec), ncmpmx, zr(jvale), nomgd, zk8(iad),&
                        nosy16, zi(jnu), lresu, nbcmp, nomcmp,&
                        nive)
        else
            valk(1) = chamn
            valk(2) = forma
            call u2mesk('E', 'PREPOST2_36', 2, valk)
        endif
!
    else if (form(1:5).eq.'IDEAS') then
!  ---  ON CHERCHE SI MAILLAGE IDEAS ---
        lmasu=.false.
        call jeexin(nomma//'           .TITR', iret)
        if (iret .ne. 0) then
            call jeveuo(nomma//'           .TITR', 'L', jtitr)
            call jelira(nomma//'           .TITR', 'LONMAX', nbtitr, k1bid)
            if (nbtitr .ge. 1) then
                titmai=zk80(jtitr-1+1)
                if (titmai(10:31) .eq. 'AUTEUR=INTERFACE_IDEAS') then
                    lmasu=.true.
                endif
            endif
        endif
!
        if (itype .eq. 1 .and. num .ge. 0) then
            call irdesr(ifi, nbnot2, zi(iaprno), zi(ianueq), nec,&
                        zi(iaec), ncmpmx, zr(iavale), zk8(iad), titre,&
                        zk8(jno), nomsd, nomsym, numord, zi(jnu),&
                        lmasu, nbcmp, zi(jncmp), nomcmp)
        else if (itype.eq.1.and.num.lt.0) then
            call irdrsr(ifi, nbnot2, zi(iadesc), nec, zi(iaec),&
                        ncmpmx, zr( iavale), zk8(iad), titre, zk8(jno),&
                        nomsd, nomsym, numord, zi( jnu), lmasu,&
                        nbcmp, zi(jncmp), nomcmp)
        else if (itype.eq.2.and.num.ge.0) then
            call irdesc(ifi, nbnot2, zi(iaprno), zi(ianueq), nec,&
                        zi(iaec), ncmpmx, zc(iavale), zk8(iad), titre,&
                        zk8(jno), nomsd, nomsym, numord, zi(jnu),&
                        lmasu)
        else if (itype.eq.2.and.num.lt.0) then
            call u2mesk('E', 'PREPOST2_35', 1, forma)
        endif
!
    endif
    goto 9998
9997  continue
    lgch16=lxlgut(nosy16)
    if (.not.lresu) then
        valk(1) = nosy16(1:lgch16)
        valk(2) = nomgd
        call u2mesk('A', 'PREPOST2_40', 2, valk)
    else
        nomsdr=nomsd
        lgconc=lxlgut(nomsdr)
        valk(1) = nosy16(1:lgch16)
        valk(2) = nomsdr(1:lgconc)
        valk(3) = nomgd
        call u2mesk('A', 'PREPOST2_41', 3, valk)
    endif
9998  continue
    call jedetr('&&IRDEPL.ENT_COD')
    call jedetr('&&IRDEPL.NUM_CMP')
    call jedetr('&&IRDEPL.NOMNOE')
    call jedetr('&&IRDEPL.NUMNOE')
    call jedetr('&&IRDEPL.VALE')
9999  continue
    call jedema()
end subroutine
