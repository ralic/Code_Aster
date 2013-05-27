subroutine lrcmva(ntvale, nbvato, ntproa, lgproa, ncmprf,&
                  nomcmr, nbcmfi, nmcmfi, nbcmpv, ncmpvm,&
                  numcmp, nochmd, adsl, adsv, codret)
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
! person_in_charge: nicolas.sellenet at edf.fr
! ======================================================================
!     LECTURE D'UN CHAMP - FORMAT MED - CREATION DES VALEURS AUX NOEUDS
!     -    -       -              -                  --
!-----------------------------------------------------------------------
!     ENTREES:
!       NTVALE : TABLEAU QUI CONTIENT LES VALEURS LUES
!       NBVATO : NOMBRE DE VALEURS TOTAL
!       NTPROA : TABLEAU QUI CONTIENT LE PROFIL ASTER
!       LGPROA : LONGUEUR DU PROFIL ASTER
!                SI NUL, PAS DE PROFIL
!       NCMPRF : NOMBRE DE COMPOSANTES DU CHAMP DE REFERENCE
!       NOMCMR : NOMS DES COMPOSANTES DE REFERENCE
!       NBCMFI : NOMBRE DE COMPOSANTES DANS LE FICHIER      .
!       NMCMFI : NOM DES COMPOSANTES DANS LE FICHIER
!       NBCMPV : NOMBRE DE COMPOSANTES VOULUES
!                SI NUL, ON LIT LES COMPOSANTES A NOM IDENTIQUE
!       NCMPVM : LISTE DES COMPOSANTES VOULUES DANS MED
!       NUMCMP : TABLEAU DES NUMEROS DES COMPOSANTES VALIDES
!       NOCHMD : NOM MED DU CHAMP A LIRE
!     SORTIES:
!       ADSL, ADSV : ADRESSES DES TABLEAUX DES CHAMPS SIMPLIFIES
!        CODRET : CODE DE RETOUR (0 : PAS DE PB, NON NUL SI PB)
!
!   REMARQUE :
!    LE TABLEAU DE VALEURS EST UTILISE AINSI : TV(NBCMFI,NVALUT)
!    EN FORTRAN, CELA CORRESPOND AU STOCKAGE MEMOIRE SUIVANT :
!    TV(1,1), TV(2,1), ..., TV(NBCMFI,1), TV(1,2), TV(2,2), ...,
!    TV(NBCMFI,2) , TV(1,3), TV(2,3), ..., TV(1,NVALUT), TV(2,NVALUT),
!    TV(NBCMFI,NVALUT)
!    C'EST CE QUE MED APPELLE LE MODE ENTRELACE
!_____________________________________________________________________
!
    implicit none
!
! 0.1. ==> ARGUMENTS
!
    include 'jeveux.h'
    include 'asterc/indik8.h'
    include 'asterfort/infniv.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/lxlgut.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/wkvect.h'
    integer :: nbvato, lgproa
    integer :: ncmprf, nbcmpv
    integer :: adsl, adsv
    integer :: codret
!
    character(len=*) :: nochmd
    character(len=*) :: nomcmr(*)
    character(len=*) :: ntvale, ntproa, ncmpvm, nmcmfi, numcmp
!
! 0.2. ==> COMMUNS
!
! 0.3. ==> VARIABLES LOCALES
!
    character(len=6) :: nompro
    parameter ( nompro = 'LRCMVA' )
!
!
    integer :: iaux, jaux, kaux, laux, maux
    integer :: nrcmp, ncmpdb
    integer :: nuval
    integer :: nbcmfi
    integer :: adremp, advale, adncfi, adnucm, adncvm, adproa
    integer :: ifm, nivinf
!
    character(len=8) :: saux08
    character(len=24) :: ntcmpl
    character(len=24) :: valk(2)
!
    call infniv(ifm, nivinf)
!
!====
! 1. ON BOUCLE SUR LES NBCMFI COMPOSANTES DU CHAMP QUI A ETE LU
!    DANS LE FICHIER
!====
!
    codret = 0
!
!               12   345678   9012345678901234
    ntcmpl = '&&'//nompro//'.LOGIQUECMP     '
    call wkvect(ntcmpl, 'V V L', ncmprf, adremp)
!
    call jeveuo(ntvale, 'L', advale)
    if (nbcmpv .ne. 0) then
        call jeveuo(ncmpvm, 'L', adncvm)
    endif
    call jeveuo(nmcmfi, 'L', adncfi)
    call jeveuo(numcmp, 'L', adnucm)
!
    if (lgproa .ne. 0) then
        call jeveuo(ntproa, 'L', adproa)
    endif
!
    do 11 , iaux = 1 , nbcmfi
!
    ncmpdb = 1
!
! 1.1. ==> REPERAGE DU NUMERO DE LA COMPOSANTE DU CHAMP ASTER DANS
!          LAQUELLE AURA LIEU LE TRANSFERT DE LA IAUX-EME COMPOSANTE LUE
!
110  continue
!
    nrcmp = 0
!
! 1.1.1. ==> QUAND ON VEUT UNE SERIE DE COMPOSANTES :
!            BOUCLE 111 : ON CHERCHE, DANS LA LISTE VOULUE EN MED,
!            NCMPVM, A QUEL ENDROIT SE TROUVE LA COMPOSANTE LUE EN
!            COURS DE TRAITEMENT, ZK16(ADNCFI-1+IAUX).
!            ON EXPLORE LA LISTE DE NCMPDB, QUI VAUT 1 AU DEBUT, A
!            NBCMPV, QUI EST LE NOMBRE DE COMPOSANTES VOULUES.
!
!            DEUX CAS DE FIGURE :
!            . SI AUCUN ELEMENT DE CETTE LISTE NE CORRESPOND A LA
!              COMPOSANTE LUE, CELA VEUT DIRE QUE CETTE COMPOSANTE N'EST
!              PAS SOUHAITEE. EN QUELQUE SORTE, ELLE A ETE LUE POUR
!              RIEN. ON PASSE A LA COMPOSANTE LUE SUIVANTE (GOTO 11)
!
!            . QUAND ON TOMBE SUR UNE COMPOSANTE MED VOULUE IDENTIQUE,
!              ON VA REMPLIR LA COMPOSANTE ASTER ASSOCIEE AVEC LES
!              VALEURS DE LA COMPOSANTE LUE. POUR CELA :
!              . ON DEDUIT LE NUMERO DANS LA LISTE OFFICIELLE DE LA
!                COMPOSANTE ASTER ASSOCIEE, ZI(ADNUCM-1+JAUX).
!              . ON MEMORISE QUE LA COMPOSANTE ASTER EST REMPLIE GRACE
!                AU BOOLEN ZL(ADREMP+NRCMP-1).
!              . ON MEMORISE A QUEL ENDROIT DE LA LISTE VOULUE EN MED ON
!                EN EST. EN EFFET, LES VALEURS DE LA COMPOSANTE LUE
!                PEUVENT ETRE MISES DANS PLUSIEURS COMPOSANTES ASTER.
!                UNE FOIS LE TRANSFERT EFFECTUE, ON REPRENDRA
!                L'EXPLORATION DE LA LISTE A L'ENDROIT OU ON S'ETAIT
!                ARRETE : NCMPDB.
!              . ON FAIT LE TRANSFERT DES VALEURS (GOTO 12).
!
    if (nbcmpv .ne. 0) then
!
        do 111 , jaux = ncmpdb , nbcmpv
        if (zk16(adncvm-1+jaux) .eq. zk16(adncfi-1+iaux)) then
            nrcmp = zi(adnucm-1+jaux)
            ncmpdb = jaux + 1
            goto 12
        endif
111      continue
        goto 11
!
! 1.1.2. ==> QUAND ON STOCKE A L'IDENTIQUE, ON RECHERCHE LE NUMERO DE
!            COMPOSANTE DE REFERENCE QUI A LE MEME NOM QUE LA
!            COMPOSANTE LUE. ON EST OBLIGE DE FAIRE CETTE RECHERCHE CAR
!            RIEN NE GARANTIT QUE L'ORDRE DES COMPOSANTES SOIT LE MEME
!            DANS LA REFERENCE ASTER ET DANS LE CHAMP ECRIT DANS LE
!            FICHIER.
    else
!
        saux08 = zk16(adncfi-1+iaux)(1:8)
        if (lxlgut(zk16(adncfi-1+iaux)) .gt. 8) then
            valk(1) = zk16(adncfi-1+iaux)
            valk(2) = saux08
            call u2mesk('A', 'MED_72', 2, valk)
        endif
        nrcmp = indik8 ( nomcmr, saux08, 1, ncmprf )
!
    endif
!
! 1.2. ==> SI AUCUNE COMPOSANTE N'A ETE TROUVEE, MALAISE ...
!
12  continue
!
    if (nrcmp .eq. 0) then
        call u2mesk('F', 'MED_73', 1, zk16(adncfi-1+iaux))
    endif
!
! 1.3. ==> TRANSFERT DES VALEURS DANS LA COMPOSANTE NRCMP
!
    zl(adremp+nrcmp-1) = .true.
!
! 1.3.1. ==> SANS PROFIL : ON PARCOURT LES NOEUDS DU PREMIER AU DERNIER
!
    if (lgproa .eq. 0) then
        kaux = -ncmprf+nrcmp-1
        laux = advale-nbcmfi+iaux-1
        do 131 , jaux = 1 , nbvato
        kaux = kaux + ncmprf
        laux = laux + nbcmfi
        zl(adsl+kaux) = .true.
        zr(adsv+kaux) = zr(laux)
131      continue
!
    else
!
! 1.3.2. ==> AVEC PROFIL : ON PARCOURT LES NOEUDS STOCKES DANS LE PROFIL
!            ON RECUPERE LEURS NUMEROS ET ON PLACE LA VALEUR AU BON
!            ENDROIT. ATTENTION, IL N'Y A AUCUNE RAISON POUR QUE LES
!            NUMEROS APPARAISENT DANS L'ORDRE, DONC IL FAUT RECALCULER
!            LA POSITION A CHAQUE FOIS.
!
        maux = -ncmprf+nrcmp-1
        laux = advale-nbcmfi+iaux-1
        do 132 , nuval = 0 , lgproa-1
        jaux = zi(adproa+nuval)
        kaux = maux + jaux*ncmprf
        laux = laux + nbcmfi
        zl(adsl+kaux) = .true.
        zr(adsv+kaux) = zr(laux)
132      continue
!
    endif
!
! 1.4. ==> QUAND ON VEUT UNE SERIE DE COMPOSANTES, ON REPREND
!          L'EXPLORATION DE LA LISTE VOULUE
!
    if (nbcmpv .ne. 0) then
        goto 110
    endif
!
    11 end do
!
!====
! 2. ON INFORME SUR LES COMPOSANTES QUI ONT ETE REMPLIES
!====
!
    kaux = 0
    do 21 , jaux = 1 , ncmprf
    if (.not.zl(adremp+jaux-1)) then
        kaux = kaux + 1
    endif
    21 end do
!
    if (kaux .gt. 0 .and. nivinf .gt. 1) then
        write(ifm,2001) nochmd
        do 22 , jaux = 1 , ncmprf
        if (zl(adremp+jaux-1)) then
            saux08 = nomcmr(jaux)
            write(ifm,2002) saux08
        endif
22      continue
        write(ifm,*) ' '
    endif
!
    2001 format('CHAMP ',a)
    2002 format('. LA COMPOSANTE LUE : ',a8,'.')
!
!====
! 3. MENAGE
!====
!
    call jedetr(ntcmpl)
!
end subroutine
