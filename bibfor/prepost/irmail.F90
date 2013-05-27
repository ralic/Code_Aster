subroutine irmail(form, ifi, versio, noma, lmod,&
                  nomo, nive, infmai, formar)
!
    implicit none
!     ------------------------------------------------------------------
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
!
!     BUT: ECRITURE DU MAILLAGE AU FORMAT RESULTAT, IDEAS, ENSIGHT, MED,
!          OU CASTEM
!     ENTREE:
!        FORM  : FORMAT DES IMPRESSIONS: IDEAS, ENSIGHT, ...
!        IFI   : UNITE LOGIQUE D'IMPRESSION
!        VERSIO: VERSION IDEAS 4 OU 5 PAR DEFAUT 5
!        NOMA  : NOM UTILISATEUR DU MAILLAGE A ECRIRE
!        LMOD  : LOGIQUE INDIQUANT SI IMPRESSION MODELE OU MAILLAGE
!                 .TRUE. MODELE
!        NOMO  : NOM UTILISATEUR DU MODELE ' ' SI SEULEMENT MAILLAGE
!        NIVE  : NIVEAU IMPRESSION CASTEM 3 OU 10
!        INFMAI: POUR LE FORMAT MED, NIVEAU DES INFORMATIONS A IMPRIMER
!     ------------------------------------------------------------------
    include 'jeveux.h'
!
    include 'asterfort/dismoi.h'
    include 'asterfort/iradhs.h'
    include 'asterfort/irmaca.h'
    include 'asterfort/irmare.h'
    include 'asterfort/irmasu.h'
    include 'asterfort/irmgms.h'
    include 'asterfort/irmhdf.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexatr.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/wkvect.h'
!---------------- ARGUMENTS --------------------------------------------
    integer :: versio, nive, infmai
    logical :: lmod
    character(len=8) :: noma, nomo
    character(len=16) :: formar
    character(len=*) :: form
!---------------- VARIABLES LOCALES ------------------------------------
!
    integer :: ier, ifi, igm, ign
    integer :: ima, ino, iret
    integer :: jcod1, jcod2, jcodd, jconx
    integer :: jcoor, jnogm, jnogn
    integer :: jnomai, jnonoe, jperm, jpoin
    integer :: jtitr, jtypl, jtypm
!
    integer :: lon1, maxnod, nbgrm, nbgrn
    integer :: nbmai, nbnoe, nbtitr, ndim
!
    logical :: lmasu, lgmsh
!
    character(len=1) :: k1bid
    character(len=8) :: cbid
    character(len=80) :: titmai
!     ------------------------------------------------------------------
!
    call jemarq()
!
!     --- RECUPERATION DE LA DIMENSION DU PROBLEME
    call dismoi('F', 'DIM_GEOM_B', noma, 'MAILLAGE', ndim,&
                cbid, ier)
!
!     --- RECUPERATION DU NOMBRE DE MAILLES
    call jelira(noma//'.NOMMAI', 'NOMUTI', nbmai, k1bid)
!
!     --- NBNOE = NOMBRE DE NOEUDS DU MAILLAGE (RECUPERATION VALEUR)
    call dismoi('F', 'NB_NO_MAILLA', noma, 'MAILLAGE', nbnoe,&
                cbid, ier)
!
!     --- RECUPERATION DES VECTEURS COORDONNEES DES NOEUDS JCOOR
!                      DU  VECTEUR DES CONNECTIVITES
!                      DU  POINTEUR SUR LES CONNECTIVITES
!                      DU  POINTEUR SUR LES TYPES DE MAILLE
    call jeveuo(noma//'.COORDO    .VALE', 'L', jcoor)
    call jeveuo(noma//'.CONNEX', 'L', jconx)
    call jeveuo(jexatr(noma//'.CONNEX', 'LONCUM'), 'L', jpoin)
    call jeveuo(noma//'.TYPMAIL        ', 'L', jtypm)
!
!     --- CONSTITUTION DU TITRE (SUR PLUSIEURS LIGNES EVENTUELLEMENT)
    call jeexin(noma//'           .TITR', iret)
    if (iret .gt. 0) then
        call jeveuo(noma//'           .TITR', 'L', jtitr)
        call jelira(noma//'           .TITR', 'LONMAX', nbtitr, k1bid)
    else
        nbtitr=1
        call wkvect(noma//'           .TITR', 'V V K80', nbtitr, jtitr)
        zk80(jtitr)='MAILLAGE RESTREINT'
    endif
!
!
!       - DESTRUCTION PUIS ALLOCATION DE ZONES DE TRAVAIL
    call jeexin('&&IRMAIL.NOMMAI', iret)
    if (iret .ne. 0) call jedetr('&&IRMAIL.NOMMAI')
    call jeexin('&&IRMAIL.NOMNOE', iret)
    if (iret .ne. 0) call jedetr('&&IRMAIL.NOMNOE')
    call wkvect('&&IRMAIL.NOMMAI', 'V V K8', nbmai, jnomai)
    call wkvect('&&IRMAIL.NOMNOE', 'V V K8', nbnoe, jnonoe)
!       - RECUPERATION DES NOMS DES MAILLES
    do 10 ima = 1, nbmai
        call jenuno(jexnum(noma//'.NOMMAI', ima), zk8(jnomai-1+ima))
10  end do
!       - RECUPERATION DES NOMS DES NOEUDS
    do 20 ino = 1, nbnoe
        call jenuno(jexnum(noma//'.NOMNOE', ino), zk8(jnonoe-1+ino))
20  end do
!       - TEST EXISTENCE DE GROUPES DE NOEUDS
    call jeexin(noma//'.GROUPENO', iret)
    if (iret .ne. 0) then
!         - RECUPERATION DU NOMBRE ET DES NOMS DES GROUPES DE NOEUDS
        call jelira(noma//'.GROUPENO', 'NUTIOC', nbgrn, k1bid)
        if (nbgrn .ne. 0) then
            call wkvect('&&IRMAIL.NOMGRNO', 'V V K24', nbgrn, jnogn)
            do 30 ign = 1, nbgrn
                call jenuno(jexnum(noma//'.GROUPENO', ign), zk24(jnogn- 1+ign))
30          continue
        else
!           - SI PAS DE GROUPE DE NOEUDS - NOMBRE DE GROUPES = 0
            nbgrn=0
        endif
    else
        jnogn=1
        nbgrn=0
    endif
!       - TEST EXISTENCE DE GROUPES DE MAILLE
    call jeexin(noma//'.GROUPEMA', iret)
    if (iret .ne. 0) then
!         - RECUPERATION DU NOMBRE ET DES NOMS DES GROUPES DE MAILLES
        call jelira(noma//'.GROUPEMA', 'NUTIOC', nbgrm, k1bid)
        if (nbgrm .ne. 0) then
            call wkvect('&&IRMAIL.NOMGRMA', 'V V K24', nbgrm, jnogm)
            do 40 igm = 1, nbgrm
                call jenuno(jexnum(noma//'.GROUPEMA', igm), zk24(jnogm- 1+igm))
40          continue
        else
            nbgrm=0
        endif
    else
        jnogm=1
        nbgrm=0
    endif
    if (lmod) then
!       - IMPRESSION DU MODELE
!         --> ON RECUPERE LE TYPE D'ELEMENT FINI DES MAILLES
        call jeveuo(nomo//'.MAILLE', 'L', jtypl)
    else
        jtypl=1
    endif
!
    if (form .eq. 'RESULTAT') then
!       - TRAITEMENT DU FORMAT 'RESULTAT'
        call irmare(ifi, ndim, nbnoe, zr(jcoor), nbmai,&
                    zi(jconx), zi(jpoin), noma, zi(jtypm), zi(jtypl),&
                    lmod, zk80(jtitr), nbtitr, nbgrn, nbgrm,&
                    zk8(jnomai), zk8(jnonoe), formar)
!
    else if (form.eq.'ASTER') then
!       - TRAITEMENT DU FORMAT 'ASTER'
        call irmare(ifi, ndim, nbnoe, zr(jcoor), nbmai,&
                    zi(jconx), zi(jpoin), noma, zi(jtypm), zi(jtypl),&
                    lmod, zk80(jtitr), nbtitr, nbgrn, nbgrm,&
                    zk8(jnomai), zk8(jnonoe), formar)
!
    else if (form.eq.'MED') then
!       - TRAITEMENT DU FORMAT ECHANGE DE DONNEES 'MED'
        call irmhdf(ifi, ndim, nbnoe, zr(jcoor), nbmai,&
                    zi(jconx), zi(jpoin), noma, zi(jtypm), zk80(jtitr),&
                    nbtitr, nbgrn, zk24(jnogn), nbgrm, zk24(jnogm),&
                    zk8(jnomai), zk8(jnonoe), infmai)
!
    else if (form.eq.'CASTEM') then
!       - TRAITEMENT DU FORMAT 'CASTEM'
        call irmaca(ifi, ndim, nbnoe, zr(jcoor), nbmai,&
                    zi(jconx), zi(jpoin), noma, zi(jtypm), lmod,&
                    nbgrn, zk24(jnogn), nbgrm, zk24(jnogm), nive)
!
    else if (form.eq.'GMSH') then
!       - TRAITEMENT DU FORMAT 'GMSH'
!         ON REGARDE SI LE MAILLAGE EST UN MAILLAGE GMSH (LGMSH)
        lgmsh=.false.
        call jeexin(noma//'           .TITR', iret)
        if (iret .ne. 0) then
            call jeveuo(noma//'           .TITR', 'L', jtitr)
            call jelira(noma//'           .TITR', 'LONMAX', nbtitr, k1bid)
            if (nbtitr .ge. 1) then
                titmai=zk80(jtitr-1+1)
                if (titmai(10:31) .eq. 'AUTEUR=INTERFACE_GMSH') then
                    lgmsh=.true.
                endif
            endif
        endif
        call irmgms(ifi, ndim, nbnoe, noma, nbgrm,&
                    zk8(jnonoe), lgmsh, versio)
!
    else if (form(1:5).eq.'IDEAS') then
!       - TRAITEMENT FORMAT 'IDEAS'
!         ON REGARDE SI LE MAILLAGE EST UN MAILLAGE SUPERTAB (LMASU)
        lmasu=.false.
        call jeexin(noma//'           .TITR', iret)
        if (iret .ne. 0) then
            call jeveuo(noma//'           .TITR', 'L', jtitr)
            call jelira(noma//'           .TITR', 'LONMAX', nbtitr, k1bid)
            if (nbtitr .ge. 1) then
                titmai=zk80(jtitr-1+1)
                if (titmai(10:31) .eq. 'AUTEUR=INTERFACE_IDEAS') then
                    lmasu=.true.
                endif
            endif
        endif
!       - SOUS PROGRAMME : TRAITER LES ADHERENCES SUPERTAB
        call iradhs(versio)
        call jeveuo('&&IRADHS.CODEGRA', 'L', jcod1)
        call jeveuo('&&IRADHS.CODEPHY', 'L', jcod2)
        call jeveuo('&&IRADHS.CODEPHD', 'L', jcodd)
        call jeveuo('&&IRADHS.PERMUTA', 'L', jperm)
        call jelira('&&IRADHS.PERMUTA', 'LONMAX', lon1, k1bid)
        maxnod=zi(jperm-1+lon1)
        call irmasu(ifi, ndim, nbnoe, zr(jcoor), nbmai,&
                    zi(jconx), zi(jpoin), zi(jtypm), zi(jtypl), zi(jcod1),&
                    zi(jcod2), zi(jcodd), zi(jperm), maxnod, lmod,&
                    noma, nbgrn, zk24(jnogn), nbgrm, zk24(jnogm),&
                    lmasu, zk8(jnomai), zk8(jnonoe), versio)
!       - DESTRUCTION ZONE ALLOUEE POUR GPES DE NOEUDS SI ELLE EXISTE
        call jeexin('&&IRMASU.NOMGRNO', iret)
        if (iret .ne. 0) then
            call jedetr('&&IRMASU.NOMGRNO')
        endif
!       - DESTRUCTION ZONE ALLOUEE POUR GPES DE MAILLES SI ELLE EXISTE
        call jeexin('&&IRMASU.NOMGRMA', iret)
        if (iret .ne. 0) then
            call jedetr('&&IRMASU.NOMGRMA')
        endif
        call jedetr('&&IRADHS.PERMUTA')
        call jedetr('&&IRADHS.CODEGRA')
        call jedetr('&&IRADHS.CODEPHY')
        call jedetr('&&IRADHS.CODEPHD')
        call jedetr('&&IRMAIL.NOMMAI')
        call jedetr('&&IRMAIL.NOMNOE')
!
    endif
!
! --- MENAGE
    call jedetr('&&IRMAIL.NOMMAI')
    call jedetr('&&IRMAIL.NOMNOE')
    call jedetr('&&IRMAIL.NOMGRMA')
    call jedetr('&&IRMAIL.NOMGRNO')
!
    call jedema()
end subroutine
