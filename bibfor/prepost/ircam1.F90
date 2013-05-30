subroutine ircam1(nofimd, nochmd, existc, ncmprf, numpt,&
                  instan, numord, adsd, adsv, adsl,&
                  adsk, partie, ncmpve, ntlcmp, ntncmp,&
                  ntucmp, ntproa, nbimpr, caimpi, caimpk,&
                  typech, nomamd, nomtyp, modnum, nuanom,&
                  codret)
! ======================================================================
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: nicolas.sellenet at edf.fr
! TOLE CRP_21
!     ECRITURE D'UN CHAMP - FORMAT MED - PHASE 1
!        -  -       - -            -           -
!-----------------------------------------------------------------------
!     ENTREES :
!       NOFIMD : NOM DU FICHIER MED
!       NOCHMD : NOM MED DU CHAMP A ECRIRE
!       NCMPRF : NOMBRE DE COMPOSANTES DU CHAMP DE REFERENCE
!       NUMPT  : NUMERO DE PAS DE TEMPS
!       INSTAN : VALEUR DE L'INSTANT A ARCHIVER
!       NUMORD : NUMERO D'ORDRE DU CHAMP
!       ADSK, D, ... : ADRESSES DES TABLEAUX DES CHAMPS SIMPLIFIES
!       PARTIE: IMPRESSION DE LA PARTIE IMAGINAIRE OU REELLE POUR
!               UN CHAMP COMPLEXE
!       NCMPVE : NOMBRE DE COMPOSANTES VALIDES EN ECRITURE
!       NTPROA : PROFIL ASTER. C'EST LA LISTE DES NUMEROS ASTER DES
!                ELEMENTS/NOEUDS POUR LESQUELS LE CHAMP EST DEFINI
!       NBIMPR : NOMBRE D'IMPRESSIONS
!         NCAIMI : ENTIERS POUR CHAQUE IMPRESSION
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
!         NCAIMK : CARACTERES POUR CHAQUE IMPRESSION
!                  CAIMPK(1,I) = NOM DE LA LOCALISATION ASSOCIEE
!                  CAIMPK(2,I) = NOM DU PROFIL AU SENS MED
!                  CAIMPK(3,I) = NOM DE L'ELEMENT DE STRUCTURE
!       NOMAMD : NOM DU MAILLAGE MED
!       NOMTYP : NOM DES TYPES POUR CHAQUE MAILLE
!       MODNUM : INDICATEUR SI LA SPECIFICATION DE NUMEROTATION DES
!                NOEUDS DES MAILLES EST DIFFERENTES ENTRE ASTER ET MED:
!                     MODNUM = 0 : NUMEROTATION IDENTIQUE
!                     MODNUM = 1 : NUMEROTATION DIFFERENTE
!       NUANOM : TABLEAU DE CORRESPONDANCE DES NOEUDS MED/ASTER.
!                NUANOM(ITYP,J): NUMERO DANS ASTER DU J IEME NOEUD DE LA
!                MAILLE DE TYPE ITYP DANS MED.
!     SORTIES:
!       CODRET : CODE DE RETOUR (0 : PAS DE PB, NON NUL SI PB)
!_______________________________________________________________________
!
    implicit none
!
    include 'jeveux.h'
    include 'asterc/utflsh.h'
    include 'asterfort/infniv.h'
    include 'asterfort/ircmcc.h'
    include 'asterfort/ircmec.h'
    include 'asterfort/ircmva.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/mfferm.h'
    include 'asterfort/mfnpdt.h'
    include 'asterfort/mfouvr.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    integer :: ntymax
    parameter (ntymax = 69)
!
! 0.1. ==> ARGUMENTS
!
    integer :: nbimpr
    integer :: caimpi(10, nbimpr)
    integer :: numpt, numord
    integer :: adsd, adsv, adsl, adsk
    integer :: existc, ncmprf
    integer :: ncmpve, jcomp, junit
    integer :: typent, tygeom
    integer :: modnum(ntymax), nuanom(ntymax, *)
!
    character(len=8) :: typech
    character(len=8) :: nomtyp(*)
    character(len=24) :: ntlcmp, ntncmp, ntucmp, ntproa
    character(len=*) :: nofimd, partie
    character(len=*) :: nomamd
    character(len=*) :: caimpk(3, nbimpr)
    character(len=64) :: nochmd
    character(len=64) :: nompb(2)
!
    real(kind=8) :: instan
!
    integer :: codret
!
! 0.2. ==> COMMUNS
!
!
! 0.3. ==> VARIABLES LOCALES
!
    character(len=6) :: nompro
    parameter ( nompro = 'IRCAM1' )
!
    integer :: edleaj
    integer :: ednoeu
    parameter (ednoeu=3)
    integer :: edmail
    parameter (edmail=0)
    integer :: edelst
    parameter (edelst=5)
    integer :: typnoe
    parameter (typnoe=0)
    integer :: ednopg
    parameter (ednopg=1)
    integer :: ednoma
    parameter (ednoma=4)
!
    character(len=8) :: saux08
    character(len=24) :: ntvale
    character(len=64) :: nomprf, nolopg, nomam2
!
    integer :: nbrepg, nbpt, iret
    integer :: ifm, nivinf
    integer :: nbenty, nvalec, nbpg, nbsp, nbco, nbse, nbfi
    integer :: tymast
    integer :: advale
    integer :: adproa, adnucm
    integer :: nrimpr, codre2, retsav
    integer :: ideb, ifin
!
    integer :: idfimd
    integer :: iaux
    logical :: ficexi
!
!====
! 1. PREALABLES
!====
!
! 1.1. ==> RECUPERATION DU NIVEAU D'IMPRESSION
!
    call infniv(ifm, nivinf)
!
    if (nivinf .gt. 1) then
        write (ifm,1001) 'DEBUT DE '//nompro
        write (ifm,*) 'ECRITURE DE '//nochmd
    endif
    1001 format(/,4x,10('='),a,10('='),/)
!
! 1.2. ==> NOMS DES TABLEAUX DE TRAVAIL
!               12   345678   9012345678901234
    ntvale = '&&'//nompro//'.VALEURS        '
!
! 1.3. ==> ADRESSES
!
    call jeveuo(ntproa, 'L', adproa)
    call jeveuo(ntlcmp, 'L', adnucm)
!
!====
! 2. OUVERTURE FICHIER MED EN MODE 'LECTURE_AJOUT'
!    CELA SIGNIFIE QUE LE FICHIER EST ENRICHI MAIS ON NE PEUT PAS
!    ECRASER UNE DONNEE DEJA PRESENTE.
!    (LE FICHIER EXISTE DEJA CAR SOIT ON A TROUVE UN MAILLAGE DEDANS,
!     SOIT ON EST PASSE PAR IRMAIL/IRMHDF)
!====
!
    inquire(file=nofimd,exist=ficexi)
    if (ficexi) then
        edleaj = 1
        call mfouvr(idfimd, nofimd, edleaj, codret)
        if (codret .ne. 0) then
            edleaj = 3
            call mfouvr(idfimd, nofimd, edleaj, codret)
        endif
    else
        edleaj = 3
        call mfouvr(idfimd, nofimd, edleaj, codret)
    endif
    if (codret .ne. 0) then
        saux08='MFOUVR  '
        call u2mesg('F', 'DVP_97', 1, saux08, 1,&
                    codret, 0, 0.d0)
    endif
!
!====
! 3. CREATION DU CHAMP
!====
!
! 3.1. ==> CREATION DU TABLEAUX DES COMPOSANTES
!
    nbpt=0
    call wkvect('&&IRCAM1.CNAME', 'V V K16', ncmprf, jcomp)
    call wkvect('&&IRCAM1.CUNIT', 'V V K16', ncmprf, junit)
    nomam2 = ' '
    iret=0
    call mfnpdt(idfimd, nochmd, nomam2, nbpt, zk16(junit),&
                zk16(jcomp), iret)
    if (iret .eq. 0 .and. nbpt .ne. 0 .and. nomam2 .ne. nomamd) then
        call u2mess('F', 'MED_94')
    endif
!
! 3.2. ==> CREATION DU CHAMP DANS LE FICHIER
!
    call ircmcc(idfimd, nomamd, nochmd, existc, ncmpve,&
                ntncmp, ntucmp, codret)
!
!====
! 4. ECRITURE POUR CHAQUE IMPRESSION SELECTIONNEE
!====
!
    ifin = 0
!
    retsav = 0
    do 41 , nrimpr = 1 , nbimpr
!
    if (codret .eq. 0) then
!
!GN        PRINT *,'IMPRESSION NUMERO ',NRIMPR
!GN        PRINT *,'CAIMPI : ',(CAIMPI(IAUX,NRIMPR),IAUX = 1 , 7)
!GN        PRINT *,'CAIMPK (LOCA GAUSS) : ',CAIMPK(1,NRIMPR)
!GN        PRINT *,'CAIMPK (PROFIL)     : ',CAIMPK(2,NRIMPR)
! 4.0. ==> NOMBRE DE VALEURS A ECRIRE
!
        nvalec = caimpi(7,nrimpr)
!
        if (nvalec .gt. 0) then
!
! 4.1. ==> ON DOIT ECRIRE DES VALEURS CORRESPONDANTS A NVALEC SUPPORTS
!          DU TYPE EN COURS.
!
            if (nivinf .gt. 1) then
                write (ifm,4000)
                call utflsh(codret)
            endif
!
            tygeom = caimpi(9,nrimpr)
            tymast = caimpi(8,nrimpr)
            ideb = ifin + 1
            ifin = ideb + nvalec - 1
!
            if (tygeom .eq. typnoe) then
                typent = ednoeu
            else
                if (typech .eq. 'ELNO') then
                    typent = ednoma
                else
                    typent = edmail
                endif
            endif
!
            if (tygeom .eq. typnoe) then
                nbpg = 1
                nbsp = 1
                nbco = 0
                nbse = 0
                nbfi = 0
                if (nivinf .gt. 1) then
                    write (ifm,4001)
                endif
            else
                nbpg = caimpi(2,nrimpr)
                nbsp = caimpi(3,nrimpr)
                nbco = caimpi(4,nrimpr)
                nbse = caimpi(5,nrimpr)
                nbfi = caimpi(6,nrimpr)
                if ((nbpg.eq.18) .and. (typech.eq.'ELNO    ')) then
                    nompb(1) = nochmd(9:22)
                    nompb(2) = 'PENTA18'
                    call u2mesk('A', 'PREPOST2_84', 2, nompb(1))
                    goto 41
                endif
                if (nivinf .gt. 1) then
                    write (ifm,4002) nomtyp(tymast), tygeom
                endif
            endif
!
            nbenty = caimpi(10,nrimpr)
            nolopg = caimpk(1,nrimpr)
            nomprf = caimpk(2,nrimpr)
!
! 4.2. ==> CREATION DES TABLEAUX DE VALEURS A ECRIRE
!
            if (codret .eq. 0) then
!
                iaux = ncmpve*nbsp*nbpg*nvalec
                call wkvect(ntvale, 'V V R', iaux, advale)
!
                call ircmva(zi(adnucm), ncmpve, ncmprf, nvalec, nbpg,&
                            nbsp, adsv, adsd, adsl, adsk,&
                            partie, tymast, modnum, nuanom, typech,&
                            zr(advale), zi( adproa), ideb, ifin, codre2)
                if (codre2 .ne. 0) retsav = 100
!
            endif
!
! 4.4. ==> ECRITURE VRAIE
!
            if (codret .eq. 0) then
!
                nbrepg = ednopg
                if ((tygeom.ne.typnoe) .and. (nbpg*nbsp.ne.1)) then
                    nbrepg = nbpg
                    if (nbco .eq. 0 .and. nbse .eq. 0 .and. nbfi .eq. 0) then
                        nbrepg = nbpg*nbsp
                    else
                        typent = edelst
                    endif
                endif
!
                call ircmec(idfimd, nochmd, nomprf, nolopg, numpt,&
                            instan, numord, zr(advale), ncmpve, nbenty,&
                            nbrepg, nvalec, typent, tygeom, codret)
!
                call jedetr(ntvale)
!
            endif
!
        endif
!
    endif
!
    41 end do
!
    if (nivinf .gt. 1) then
        call utflsh(codret)
        write (ifm,4000)
    endif
!
    4000 format(/,80('-'),/)
    4001 format('  * VALEURS AUX NOEUDS',/)
    4002 format(&
     &/,'  * VALEURS SUR LES MAILLES DE TYPE ASTER ',a,' ET MED',i4)
!
!====
! 5. FERMETURE DU FICHIER MED
!====
!
    call mfferm(idfimd, codret)
    if (codret .ne. 0) then
        saux08='MFFERM  '
        call u2mesg('F', 'DVP_97', 1, saux08, 1,&
                    codret, 0, 0.d0)
    endif
    if (retsav .eq. 100) codret=100
!
!====
! 6. LA FIN
!====
!
!     MENAGE
    call jedetr('&&IRCAM1.CNAME')
    call jedetr('&&IRCAM1.CUNIT')
!
    if (nivinf .gt. 1) then
        write (ifm,1001) 'FIN DE '//nompro
    endif
!
end subroutine
