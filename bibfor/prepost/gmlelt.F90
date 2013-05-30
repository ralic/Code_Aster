subroutine gmlelt(igmsh, maxnod, nbtyma, nbmail, nbnoma,&
                  nuconn, versio)
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/iunifi.h'
    include 'asterfort/jecrec.h'
    include 'asterfort/jecroc.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/wkvect.h'
    integer :: igmsh, maxnod, nbtyma, nbmail, nbnoma(nbtyma), nuconn(19, 32)
    integer :: versio
! ----------------------------------------------------------------------
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
! TOLE CRS_512
!
!      GMLELT --   LECTURE DES NUMEROS DES ELEMENTS, DE LEUR TYPE,
!                  DE LEUR NUMERO DE GROUPE, DU NOMBRE DE LEURS
!                  CONNECTIVITES ET DE LEURS CONNECTIVITES
!
!   ARGUMENT        E/S  TYPE         ROLE
!    IGMSH          IN    I         UNITE LOGIQUE DU FICHIER GMSH
!    MAXNOD         IN    I         NOMBRE MAXIMUM DE NOEUDS POUR
!                                   UNE MAILLE DONNEE
!    NBTYMA         IN    I         NOMBRE  DE TYPES DE MAILLES
!    NBMAIL         OUT   I         NOMBRE TOTAL DE MAILLES
!    NBNOMA         IN    I         NOMBRE DE NOEUDS DE LA MAILLE
!                                    POUR UN TYPE DE MAILLE DONNEE
!    NUCONN         IN    I         PASSAGE DE LA NUMEROTATION DES NDS
!                                     D'UNE MAILLE : ASTER -> GMSH
!    VERSIO         IN    I         VERSION DU FICHIER GMSH
!
! ......................................................................
!
!
!
!
    character(len=8) :: k8bid
    logical :: exisgr
    integer :: imes, nbmxte, nbtag, i, ij, k, icurgr
    integer :: nbgrou, indgro, ima, ibid, ityp, ino, node, indmax
    integer :: jnuma, jtypma, jgroma, jnbnma, jnoma, jnbmag, jnbtym
    integer :: jindma, jdetr, jtag, jgr
!
    parameter   (nbmxte=19)
    integer :: nbno(nbmxte)
    data        nbno/ 2, 3, 4, 4, 8, 6, 5, 3, 6, 9,10,27,&
     &                      18,14, 1, 8,20,15,13/
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATION :
!     --------------
    k8bid = '        '
!
! --- RECUPERATION DES NUMEROS D'UNITE LOGIQUE :
!     ----------------------------------------
    imes = iunifi('MESSAGE')
!
! --- LECTURE DU NOMBRE D'ELEMENTS :
!     ----------------------------
    read(igmsh,'(I10)') nbmail
!
! --- CREATION DE VECTEURS DE TRAVAIL :
!     -------------------------------
    call jedetr('&&PREGMS.NUMERO.MAILLES')
    call jedetr('&&PREGMS.TYPE.MAILLES')
    call jedetr('&&PREGMS.GROUPE.MAILLES')
    call jedetr('&&PREGMS.NBNO.MAILLES')
    call jedetr('&&PREGMS.CONNEC.MAILLES')
    call jedetr('&&PREGMS.NBMA.GROUP_MA')
    call jedetr('&&PREGMS.NBTYP.MAILLES')
    call jedetr('&&PREGMS.LISTE.GROUP_MA')
    call jedetr('&&PREGMS.INDICE.GROUP_MA')
    call jedetr('&&PREGMS.TAGS')
!
! ---   VECTEUR DES NUMEROS DES MAILLES
    call wkvect('&&PREGMS.NUMERO.MAILLES', 'V V I', nbmail, jnuma)
! ---   VECTEUR DU TYPE DES MAILLES
    call wkvect('&&PREGMS.TYPE.MAILLES', 'V V I', nbmail, jtypma)
! ---   VECTEUR DU NUMERO DE GROUPE DES MAILLES
    call wkvect('&&PREGMS.GROUPE.MAILLES', 'V V I', nbmail, jgroma)
! ---   VECTEUR DU NOMBRE DE CONNECTIVITES DES MAILLES
    call wkvect('&&PREGMS.NBNO.MAILLES', 'V V I', nbmail, jnbnma)
! ---   VECTEUR DES CONNECTIVITES DES MAILLES
    call wkvect('&&PREGMS.CONNEC.MAILLES', 'V V I', maxnod*nbmail, jnoma)
! ---   VECTEUR DU NOMBRE DE MAILLES POUR UN GROUPE DE MAILLES
    call wkvect('&&PREGMS.NBMA.GROUP_MA', 'V V I', nbmail, jnbmag)
! ---   VECTEUR DU NOMBRE DE MAILLES PAR TYPE DE MAILLES
    call wkvect('&&PREGMS.NBTYP.MAILLES', 'V V I', nbtyma, jnbtym)
! --- CREATION DU VECTEUR FAISANT CORRESPONDRE LES INDICES AUX
! --- NUMEROS DES GROUPES DE MAILLES :
    call wkvect('&&PREGMS.INDICE.GROUP_MA', 'V V I', nbmail, jindma)
! --- INDICATION DE DESTRUCTION DES NOEUDS
    call jeveuo('&&PREGMS.DETR.NOEUDS', 'E', jdetr)
! --- TAGS POUR LE FORMAT VERSION 2 :
! --- DIMENSIONNE A 2*NBMAIL CAR 2 TAGS PAS DEFAUT DANS GMSH
    if (versio .eq. 2) then
        call jecrec('&&PREGMS.TAGS', 'V V I', 'NU', 'DISPERSE', 'VARIABLE',&
                    nbmail)
    endif
!
! --- LECTURE DES ENREGISTREMENTS RELATIFS AUX MAILLES ET AFFECTATION
! --- DES VECTEURS DE TRAVAIL :
!     -----------------------
    k = 0
    ij = 0
!
! --- ICURGR : NUMERO DU GROUPE GMSH
!     NBGROU : NBRE DE GROUPES TROUVES
!     INDGRO : INDICE DU GROUPE
    icurgr = 0
    nbgrou = 0
    indgro = 0
    do 10 ima = 1, nbmail
!
        if (versio .eq. 1) then
!
            read(igmsh,*) zi(jnuma+ima-1),zi(jtypma+ima-1), zi(jgroma+&
            ima-1),ibid,zi(jnbnma+ima-1), (zi(jnoma+ij+k-1),k=1,zi(&
            jnbnma+ima-1))
!
        else if (versio.eq.2) then
!
            read(igmsh,*) ibid,ibid,nbtag
!
            call jecroc(jexnum('&&PREGMS.TAGS', ima))
            call jeecra(jexnum('&&PREGMS.TAGS', ima), 'LONMAX', nbtag, k8bid)
            call jeveuo(jexnum('&&PREGMS.TAGS', ima), 'E', jtag)
!
            backspace(igmsh)
            read(igmsh,*) zi(jnuma+ima-1),zi(jtypma+ima-1), nbtag,(zi(&
            jtag-1+k),k=1,nbtag), (zi(jnoma+ij+k-1),k=1,nbno(zi(&
            jtypma+ima-1)))
!
            zi(jnbnma+ima-1)=nbno(zi(jtypma+ima-1))
            zi(jgroma+ima-1)=zi(jtag-1+1)
            if (nbtag .eq. 0) then
                zi(jgroma+ima-1)=0
            endif
!
        else
            call assert(.false.)
        endif
!
!      INDICATION DES NOEUDS QUI NE SONT PAS ORPHELINS
        ityp = zi(jtypma+ima-1)
        do 12 ino = 1, nbnoma(ityp)
            node = zi(jnoma+ij+nuconn(ityp,ino)-1)
            zi(jdetr+node) = 1
12      continue
!
        if (icurgr .ne. zi(jgroma+ima-1)) then
            icurgr = zi(jgroma+ima-1)
            exisgr = .false.
            do 20 i = 1, nbgrou
                if (icurgr .eq. zi(jindma+i-1)) then
                    exisgr = .true.
                    indgro = i
                    goto 30
                endif
20          continue
30          continue
            if (.not.exisgr) then
                nbgrou = nbgrou + 1
                indgro = nbgrou
                zi(jindma+indgro-1) = zi(jgroma+ima-1)
            endif
        endif
        zi(jnbmag+indgro-1) = zi(jnbmag+indgro-1) + 1
!
        ij = ij + zi(jnbnma+ima-1)
        zi(jnbtym+zi(jtypma+ima-1)-1) = zi(jnbtym+zi(jtypma+ima-1)-1)+ 1
10  end do
!
    if (nbgrou .ne. 0) then
!
        indmax = nbgrou
        call jeecra('&&PREGMS.INDICE.GROUP_MA', 'LONUTI', indmax, k8bid)
!
! --- CREATION DE LA COLLECTION DES GROUPES DE MAILLES :
!     ------------------------------------------------
        call jecrec('&&PREGMS.LISTE.GROUP_MA', 'V V I', 'NU', 'CONTIG', 'VARIABLE',&
                    indmax)
        call jeecra('&&PREGMS.LISTE.GROUP_MA', 'LONT', nbmail, k8bid)
!
        do 40 i = 1, indmax
            call jeecra(jexnum('&&PREGMS.LISTE.GROUP_MA', i), 'LONMAX', zi(jnbmag+i-1), k8bid)
            zi(jnbmag+i-1) = 0
40      continue
!
! --- AFFECTATION DES OBJETS RELATIFS AUX GROUPES DE MAILLES :
!     ------------------------------------------------------
        k = 0
! --- ICURGR : NUMERO DU GROUPE GMSH
!     NBGROU : NBRE DE GROUPES TROUVES
!     INDGRO : INDICE DU GROUPE
        icurgr = 0
        nbgrou = 0
        indgro = 0
        do 50 ima = 1, nbmail
            if (icurgr .ne. zi(jgroma+ima-1)) then
                icurgr = zi(jgroma+ima-1)
                exisgr = .false.
                do 60 i = 1, nbgrou
                    if (icurgr .eq. zi(jindma+i-1)) then
                        exisgr = .true.
                        indgro = i
                        goto 70
                    endif
60              continue
70              continue
                if (.not.exisgr) then
                    nbgrou = nbgrou + 1
                    indgro = nbgrou
                endif
            endif
            zi(jnbmag+indgro-1) = zi(jnbmag+indgro-1) + 1
!
            zi(jindma+indgro-1) = zi(jgroma+ima-1)
            call jeveuo(jexnum('&&PREGMS.LISTE.GROUP_MA', indgro), 'E', jgr)
            zi(jgr+zi(jnbmag+indgro-1)-1) = zi(jnuma+ima-1)
50      continue
!
    endif
!
    write(imes,*) 'NOMBRE DE MAILLES : ',nbmail
!
    call jedema()
!
! ============================ FIN DE LA ROUTINE ======================
end subroutine
