subroutine ircmpe(nofimd, ncmpve, numcmp, exicmp, nbvato,&
                  nbmaec, limaec, adsd, adsl, nbimpr,&
                  ncaimi, ncaimk, tyefma, typmai, typgeo,&
                  nomtyp, typech, profas, promed, prorec,&
                  nroimp, chanom, sdcarm)
    implicit none
    include 'asterfort/celfpg.h'
    include 'asterfort/cesexi.h'
    include 'asterfort/infniv.h'
    include 'asterfort/ircael.h'
    include 'asterfort/ircmpf.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/juveca.h'
    include 'asterfort/wkvect.h'
    integer :: nbvato, ncmpve, numcmp(ncmpve), nbmaec, typmai(*), adsd
    integer :: limaec(*), nbimpr, typgeo(*), profas(nbvato), tyefma(*)
    integer :: nroimp(nbvato), promed(nbvato), prorec(nbvato), adsl
    character(len=*) :: nofimd
    character(len=8) :: nomtyp(*), typech, sdcarm
    character(len=19) :: chanom
    character(len=24) :: ncaimi, ncaimk
    logical :: exicmp(nbvato)
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
! person_in_charge: nicolas.sellenet at edf.fr
! TOLE CRP_21
!_______________________________________________________________________
!     ECRITURE D'UN CHAMP - MAILLES ET PROFIL SUR LES ELEMENTS
!        -  -       -       -          -              -
!_______________________________________________________________________
!     ENTREES :
!       NOFIMD : NOM DU FICHIER MED
!       NCMPVE : NOMBRE DE COMPOSANTES VALIDES EN ECRITURE
!       NUMCMP : NUMEROS DES COMPOSANTES VALIDES
!       EXICMP : EXISTENCE DES COMPOSANTES PAR MAILLES
!       NBVATO : NOMBRE DE VALEURS TOTALES
!       NBMAEC : NOMBRE D'ENTITES A ECRIRE (O, SI TOUTES)
!       LIMAEC : LISTE DES ENTITES A ECRIRE SI EXTRAIT
!       ADSK, D, ... : ADRESSES DES TABLEAUX DES CHAMPS SIMPLIFIES
!       TYPMAI : TYPE ASTER POUR CHAQUE MAILLE
!       TYEFMA : NRO D'ELEMENT FINI OU DE MAILLE ASSOCIE A CHAQUE MAILLE
!       TYPGEO : TYPE GEOMETRIQUE DE MAILLE ASSOCIEE AU TYPE ASTER
!       NOMTYP : NOM DES TYPES DE MAILLES ASTER
!       PROREC : PROFIL RECIPROQUE. AUXILIAIRE.
!     SORTIES :
!       NBIMPR : NOMBRE D'IMPRESSIONS
!       NCAIMI : STRUCTURE ASSOCIEE AU TABLEAU CAIMPI
!         CAIMPI : ENTIERS POUR CHAQUE IMPRESSION
!                  CAIMPI(1,I) = TYPE D'EF / MAILLE ASTER (0, SI NOEUD)
!                  CAIMPI(2,I) = NOMBRE DE POINTS (GAUSS OU NOEUDS)
!                  CAIMPI(3,I) = NOMBRE DE SOUS-POINTS
!                  CAIMPI(4,I) = NOMBRE DE COUCHES
!                  CAIMPI(5,I) = NOMBRE DE SECTEURS
!                  CAIMPI(6,I) = NOMBRE DE FIBTRES
!                  CAIMPI(7,I) = NOMBRE DE MAILLES A ECRIRE
!                  CAIMPI(8,I) = TYPE DE MAILLES ASTER (0, SI NOEUD)
!                  CAIMPI(9,I) = TYPE GEOMETRIQUE AU SENS MED
!                  CAIMPI(10,I) = NOMBRE TOTAL DE MAILLES IDENTIQUES
!       NCAIMK : STRUCTURE ASSOCIEE AU TABLEAU CAIMPK
!         CAIMPK : CARACTERES POUR CHAQUE IMPRESSION
!                  CAIMPK(1,I) = NOM DE LA LOCALISATION ASSOCIEE
!                  CAIMPK(2,I) = NOM DU PROFIL AU SENS MED
!                  CAIMPK(3,I) = NOM DE L'ELEMENT DE STRUCTURE
!       PROFAS : PROFIL ASTER. C'EST LA LISTE DES NUMEROS ASTER DES
!                ELEMENTS POUR LESQUELS LE CHAMP EST DEFINI
!       PROMED : PROFIL MED. C'EST LA LISTE DES NUMEROS MED DES
!                ELEMENTS POUR LESQUELS LE CHAMP EST DEFINI
!       NROIMP : NUMERO DE L'IMPRESSION ASSOCIEE A CHAQUE MAILLE
!_______________________________________________________________________
!
    include 'jeveux.h'
    character(len=80) :: ednopf, ednoga
    parameter (ednopf=' ')
    parameter (ednoga=' ')
!
    integer :: ntymax, nmaxfi
    parameter (ntymax=69)
    parameter (nmaxfi=10)
    integer :: ifm, nivinf, ibid, iret, iaux, jaux, kaux, ima, jcesd, laux
    integer :: jcesc, jcesl, jcesv, nrefma, jnofpg, jchfpg
    integer :: nrcmp, nrpg, nrsp, nbpg, nbsp, nval, typmas, nbimp0, nrimpr
    integer :: nmaty0(ntymax), adraux(ntymax), nbcou, nbsec, nbfib
    integer :: adcaii, adcaik, nbgrf, nugrfi(nmaxfi)
    integer :: nbgrf2, nbcou2, nbsec2, nbfib2, ima2
    integer :: nugrf2(nmaxfi), igrfi, imafib
!
    character(len=16) :: nomfpg
    character(len=64) :: noprof
!
    logical :: exicar, grfidt
!
!====
! 1. PREALABLES
!====
    call infniv(ifm, nivinf)
!
    if (nivinf .gt. 1) write (ifm,1001) 'DEBUT DE IRCMPE'
    1001 format(/,4x,10('='),a,10('='),/)
!
!====
! 2. ON REMPLIT UN PREMIER TABLEAU PAR MAILLE :
!    . VRAI DES QU'UNE DES COMPOSANTES DU CHAMP EST PRESENTE SUR
!      LA MAILLE
!    . FAUX SINON
!    REMARQUE : ON EXAMINE LES NCMPVE COMPOSANTES QUI SONT DEMANDEES,
!    MAIS IL FAUT BIEN TENIR COMPTE DE LA NUMEROTATION DE REFERENCE
!====
    laux = adsd + 1
    do 21 , iaux = 1 , nbvato
    laux = laux + 4
    nbpg = zi(laux)
    nbsp = zi(laux+1)
    do 211 , nrcmp = 1 , ncmpve
    kaux = numcmp(nrcmp)
    do 2111 , nrpg = 1 , nbpg
    do 2112 , nrsp = 1 , nbsp
    call cesexi('C', adsd, adsl, iaux, nrpg,&
                nrsp, kaux, jaux)
    if (jaux .gt. 0) then
        exicmp(iaux) = .true.
        goto 21
    endif
2112  continue
2111  continue
211  continue
    21 end do
!
!====
! 3. PROFAS : LISTE DES MAILLES POUR LESQUELS ON AURA IMPRESSION
!    UNE MAILLE EST PRESENTE SI ET SEULEMENT SI AU MOINS UNE COMPOSANTE
!    Y EST DEFINIE ET SI ELLE FAIT PARTIE DU FILTRAGE DEMANDE
!====
    nval = 0
!
! 3.1. ==> SANS FILTRAGE : C'EST LA LISTE DES MAILLES QUI POSSEDENT
!          UNE COMPOSANTE VALIDE
    if (nbmaec .eq. 0) then
        do 31 , iaux = 1 , nbvato
        if (exicmp(iaux)) then
            nval = nval + 1
            profas(nval) = iaux
        endif
31      continue
!
! 3.2. ==> AVEC FILTRAGE : C'EST LA LISTE DES MAILLES REQUISES ET AVEC
!          UNE COMPOSANTE VALIDE
    else
        do 32 , jaux = 1 , nbmaec
        iaux = limaec(jaux)
        if (exicmp(iaux)) then
            nval = nval + 1
            profas(nval) = iaux
        endif
32      continue
    endif
!
!====
! 4. CARACTERISATIONS DES IMPRESSIONS
!    ON TRIE SELON DEUX CRITERES :
!    1. LE NOMBRE DE SOUS-POINTS
!    2. LE TYPE D'ELEMENT FINI POUR UN CHAMP ELGA, OU LE TYPE DE LA
!       MAILLE, POUR UN AUTRE TYPE DE CHAMP. LE TABLEAU TYEFMA VAUT DONC
!       EFMAI OU TYPMAI A L'APPEL , SELON LE TYPE DE CHAMP.
!====
! 4.1. ==> TABLEAU DES CARACTERISATIONS ENTIERES DES IMPRESSIONS
!          ALLOCATION INITIALE
    nbimp0 = 20
    iaux = 10*nbimp0
    call wkvect(ncaimi, 'V V I', iaux, adcaii)
!
! 4.2. ==> PARCOURS DES MAILLES QUI PASSENT LE FILTRE
    nbimpr = 0
!     SI ON EST SUR UN CHAMP ELGA, LE TRI DOIT SE FAIRE SUR LES FAMILLES
!     DE POINTS DE GAUSS
    if (typech(1:4) .eq. 'ELGA') then
        call celfpg(chanom, '&&IRCMPE.NOFPGMA', ibid)
        call wkvect('&&IRCMPE.TABNOFPG', 'V V K16', nval, jnofpg)
        call jeveuo('&&IRCMPE.NOFPGMA', 'L', jchfpg)
    endif
!
    call jeexin(sdcarm//'.CANBSP    .CESV', iret)
    exicar=.false.
    if (iret .ne. 0 .and. typech(1:4) .eq. 'ELGA') then
        call jeveuo(sdcarm//'.CANBSP    .CESD', 'L', jcesd)
        call jeveuo(sdcarm//'.CANBSP    .CESC', 'L', jcesc)
        call jeveuo(sdcarm//'.CANBSP    .CESL', 'L', jcesl)
        call jeveuo(sdcarm//'.CANBSP    .CESV', 'L', jcesv)
        exicar=.true.
    endif
!
    do 42 , iaux = 1 , nval
    ima = profas(iaux)
    nrefma = tyefma(ima)
!
    laux = adsd + 4*ima + 1
    nbpg = zi(laux)
    nbsp = zi(laux+1)
    if (typech(1:4) .eq. 'ELGA') then
        nomfpg = zk16(jchfpg+ima-1)
    endif
    nbcou = 0
    nbsec = 0
    nbfib = 0
    nbgrf = 0
    imafib = 0
    if (nbsp .gt. 1 .and. exicar) then
        call ircael(jcesd, jcesl, jcesv, jcesc, ima,&
                    nbcou, nbsec, nbfib, nbgrf, nugrfi)
        if (nbfib .ne. 0) imafib = ima
    endif
!
! 4.2.1. ==> RECHERCHE D'UNE IMPRESSION SEMBLABLE
!
    do 421 , jaux = 1 , nbimpr
    if (typech(1:4) .eq. 'ELGA') then
!           POUR LES ELGA, TRI SUR LES FAMILLES DE POINTS DE GAUSS
        if (.not.exicar) then
!             SI ON N'A PAS DE CARA_ELEM, LE CAS EST SIMPLE
!             ON COMPARE LE NOM DE LA FAMILLE DE PG ET NBSP
            if (zk16(jnofpg+jaux-1) .eq. nomfpg .and. zi( adcaii+10*(jaux-1)+2) .eq. nbsp) then
                nrimpr = jaux
                goto 423
            endif
        else
!             SINON, ON A DEUX CAS, PMF ET AUTRES
            if (nbfib .ne. 0) then
                if (zk16(jnofpg+jaux-1) .eq. nomfpg) then
!                 POUR LES PMF, ON COMPARE AUSSI LES GROUPES DE FIBRES
                    ima2 = zi(adcaii+10*(jaux-1)+5)
                    call ircael(jcesd, jcesl, jcesv, jcesc, ima2,&
                                nbcou2, nbsec2, nbfib2, nbgrf2, nugrf2)
                    if (nbfib2 .eq. nbfib .and. nbgrf2 .eq. nbgrf) then
                        grfidt = .true.
                        do 10, igrfi = 1, nbgrf2
                        if (nugrf2(igrfi) .ne. nugrfi( igrfi)) grfidt = .false.
10                      continue
                        if (grfidt) then
                            nrimpr = jaux
                            goto 423
                        endif
                    endif
                endif
            else
                if (zk16(jnofpg+jaux-1) .eq. nomfpg .and. zi(adcaii+10*(jaux-1)+3) .eq.&
                    nbcou .and. zi(adcaii+10*(jaux-1)+4) .eq. nbsec) then
                    nrimpr = jaux
                    goto 423
                endif
            endif
        endif
    else
        if (zi(adcaii+10*(jaux-1)) .eq. nrefma .and. zi(adcaii+ 10*(jaux-1)+2) .eq. nbsp) then
            nrimpr = jaux
            goto 423
        endif
    endif
421  continue
!
! 4.2.2. ==> ON CREE UNE NOUVELLE IMPRESSION
!            SI ON DEPASSE LA LONGUEUR RESERVEE, ON DOUBLE
!
    if (nbimpr .eq. nbimp0) then
        nbimp0 = 2*nbimp0
        call juveca(ncaimi, 10*nbimp0)
        call jeveuo(ncaimi, 'E', adcaii)
    endif
!
    nbimpr = nbimpr + 1
    jaux = adcaii+10*(nbimpr-1)
!                  CAIMPI(1,I) = TYPE D'EF / MAILLE ASTER (0, SI NOEUD)
    zi(jaux) = nrefma
!                  CAIMPI(2,I) = NOMBRE DE POINTS (DE GAUSS OU NOEUDS)
    zi(jaux+1) = nbpg
!                  CAIMPI(3,I) = NOMBRE DE SOUS-POINTS
    zi(jaux+2) = nbsp
!                  CAIMPI(4,I) = NOMBRE DE COUCHES
    zi(jaux+3) = nbcou
!                  CAIMPI(5,I) = NOMBRE DE SECTEURS
    zi(jaux+4) = nbsec
!                  CAIMPI(6,I) = NUMERO DE LA MAILLE 'EXEMPLE' POUR
!                                LES PMF
    zi(jaux+5) = imafib
!                  CAIMPI(7,I) = NOMBRE DE MAILLES A ECRIRE
    zi(jaux+6) = 0
!                  CAIMPI(8,I) = TYPE DE MAILLES ASTER (0, SI NOEUD)
    zi(jaux+7) = typmai(ima)
!                  CAIMPI(9,I) = TYPE GEOMETRIQUE AU SENS MED
    zi(jaux+8) = typgeo(typmai(ima))
!
    if (typech(1:4) .eq. 'ELGA') then
        zk16(jnofpg+nbimpr-1) = nomfpg
    endif
    nrimpr = nbimpr
!
! 4.2.3. ==> MEMORISATION DE L'IMPRESSION DE CETTE MAILLE
!            CUMUL DU NOMBRE DE MAILLES POUR CETTE IMPRESSION
423  continue
!
    nroimp(ima) = nrimpr
    jaux = adcaii+10*(nrimpr-1)+6
    zi(jaux) = zi(jaux) + 1
!
    42 end do
!
    if (typech(1:4) .eq. 'ELGA') then
        call jedetr('&&IRCMPE.TABNOFPG')
        call jedetr('&&IRCMPE.NOFPGMA')
    endif
!
!====
! 5. CONVERSION DU PROFIL EN NUMEROTATION MED
!    PROMED : ON STOCKE LES VALEURS DES NUMEROS DES MAILLES AU SENS MED
!    PAR TYPE DE MAILLES.
!    IL FAUT REORDONNER LE TABLEAU PROFAS PAR IMPRESSION SUCCESSIVE :
!    LE TABLEAU EST ORGANISE EN SOUS-TABLEAU CORRESPONDANT A CHAQUE
!    IMPRESSION. ON REPERE CHAQUE DEBUT DE SOUS-TABLEAU AVEC ADRAUX.
!====
! 5.1. ==> PROREC : C'EST LA LISTE RECIPROQUE. POUR LA MAILLE NUMERO
!                   IAUX EN NUMEROTATION ASTER, ON A SA POSITION DANS LE
!                   TABLEAU DES VALEURS S'IL FAIT PARTIE DE LA LISTE
!                   ET 0 SINON.
    do 51 , iaux = 1 , nval
    ima = profas(iaux)
    prorec(ima) = iaux
    51 end do
!
! 5.2. ==> ADRESSES DANS LE TABLEAU PROFAS
!          ADRAUX(IAUX) = ADRESSE DE LA FIN DE LA ZONE DE L'IMPRESSION
!                         PRECEDENTE, IAUX-1
    adraux(1) = 0
    do 52 , iaux = 2 , nbimpr
    adraux(iaux) = adraux(iaux-1) + zi(adcaii+10*(iaux-2)+6)
    52 end do
!
! 5.3. ==> DECOMPTE DU NOMBRE DE MAILLES PAR TYPE DE MAILLES ASTER
!          NMATY0(IAUX) = NUMERO MED DE LA MAILLE COURANTE, DANS LA
!                         CATEGORIE ASTER IAUX. A LA FIN, NMATY0(IAUX)
!                         VAUT LE NOMBRE DE MAILLES PAR TYPE DE MAILLES
!                         ASTER, POUR TOUTES LES MAILLES DU MAILLAGE
!          ADRAUX(JAUX) = ADRESSE DANS LES TABLEAUX PROMED ET PROFAS
!                         DE LA MAILLE COURANTE ASSOCIEE A L'IMPRESSION
!                         NUMERO JAUX
    do 531 , iaux = 1 , ntymax
    nmaty0(iaux) = 0
    531 end do
!
    do 532 , ima = 1 , nbvato
!
    typmas = typmai(ima)
    nmaty0(typmas) = nmaty0(typmas) + 1
    if (prorec(ima) .ne. 0) then
        jaux = nroimp(ima)
        adraux(jaux) = adraux(jaux) + 1
        promed(adraux(jaux)) = nmaty0(typmas)
        profas(adraux(jaux)) = ima
    endif
!
    532 end do
!
!====
! 6. MEMORISATION DANS LES CARACTERISTIQUES DE L'IMPRESSION
!====
! 6.1. ==> NOMBRE DE MAILLES DU MEME TYPE
    do 61 , iaux = 1 , nbimpr
!
    jaux = adcaii+10*(iaux-1)
    typmas = zi(jaux+7)
!                  CAIMPI(10,I) = NOMBRE DE MAILLES IDENTIQUES
    zi(jaux+9) = nmaty0(typmas)
!
    61 end do
!
! 6.2. ==> CARACTERISTIQUES CARACTERES
    iaux = 3*nbimpr
    call wkvect(ncaimk, 'V V K80', iaux, adcaik)
    do 62 , iaux = 1 , nbimpr
    jaux = adcaik+2*(iaux-1)
!                  CAIMPK(1,I) = NOM DE LA LOCALISATION ASSOCIEE
    zk80(jaux) = ednoga
!                  CAIMPK(2,I) = NOM DU PROFIL AU SENS MED
    zk80(jaux+1) = ednopf
!                  CAIMPK(3,I) = NOM DE L'ELEMENT DE STRUCTURE
    zk80(jaux+2) = ednopf
    62 end do
!
!====
! 7. STOCKAGE DES EVENTUELS PROFILS DANS LE FICHIER MED
!====
    kaux = 1
!
    do 71 , iaux = 1 , nbimpr
!
    jaux = adcaii+10*(iaux-1)
!
!       SI LE NOMBRE DE MAILLES A ECRIRE EST >0
    if (zi(jaux+6) .gt. 0) then
!
!         SI LE NOMBRE DE MAILLES A ECRIRE EST DIFFERENT
!         DU NOMBRE TOTAL DE MAILLES DE MEME TYPE:
        if (zi(jaux+6) .ne. zi(jaux+9)) then
            call ircmpf(nofimd, zi(jaux+6), promed(kaux), noprof)
!
!                  CAIMPK(2,I) = NOM DU PROFIL AU SENS MED
            zk80(adcaik+3*(iaux-1)+1) = noprof
        endif
!
!         KAUX := POINTEUR PERMETTANT DE SE PLACER DANS PROMED
!         POUR LA PROCHAINE IMPRESSION
        kaux = kaux + zi(jaux+6)
    endif
    71 end do
!
!====
! 8. LA FIN
!====
    if (nivinf .gt. 1) then
        if (typech(1:4) .eq. 'ELGA') then
            write (ifm,8001)
        else
            write (ifm,8004)
        endif
        do 81 , iaux = 1 , nbimpr
        jaux = adcaii+10*(iaux-1)
        if (zi(jaux+6) .gt. 0) write (ifm, 8002) nomtyp(zi(jaux+7)), zi(jaux+6), zi(jaux+1),&
                               zi(jaux+2)
81      continue
        write (ifm,8003)
        write (ifm,1001) 'FIN DE IRCMPE'
    endif
    8001 format(4x,65('*'),/,4x,'*  TYPE DE *',22x,'NOMBRE DE',21x,'*',&
     &/,4x,'*  MAILLE  *  VALEURS   * POINT(S) DE GAUSS *',&
     &     '   SOUS_POINT(S)   *',/,4x,65('*'))
    8002 format(4x,'* ',a8,' *',i11,' *',i15,'    *',i15,'    *')
    8003 format(4x,65('*'))
    8004 format(4x,65('*'),/,4x,'*  TYPE DE *',22x,'NOMBRE DE',21x,'*',&
     &/,4x,'*  MAILLE  *  VALEURS   *      POINTS       *',&
     &     '   SOUS_POINT(S)   *',/,4x,65('*'))
!
end subroutine
