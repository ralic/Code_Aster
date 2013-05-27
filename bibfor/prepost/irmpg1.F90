subroutine irmpg1(nofimd, nomfpg, nbnoto, nbrepg, nbsp,&
                  ndim, typgeo, refcoo, gscoo, wg,&
                  raux1, raux2, raux3, nolopg, nomasu,&
                  codret)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: nicolas.sellenet at edf.fr
!_______________________________________________________________________
!     ECRITURE AU FORMAT MED - LOCALISATION POINTS DE GAUSS - PHASE 1
!        -  -            -                  -         -             -
!_______________________________________________________________________
!     ENTREES :
!       NOFIMD : NOM DU FICHIER MED
!       NOMFPG : NOM DE LA FAMILLE DES POINTS DE GAUSS
!       NBNOTO : NOMBRE DE NOEUDS TOTAL
!       NBREPG : NOMBRE DE POINTS DE GAUSS
!       NBSP   : NOMBRE DE SOUS-POINTS
!       NDIM   : DIMENSION DE L'ELEMENT
!       TYPGEO : TYPE GEOMETRIQUE DE LA MAILLE ASSOCIEE
!       REFCOO : COORDONNEES DES NBNOTO NOEUDS
!       GSCOO  : COORDONNEES DES POINTS DE GAUSS, SI CHAMP ELGA
!       WG     : POIDS DES POINTS DE GAUSS, SI CHAMP ELGA
!       RAUX1, RAUX2, RAUX3 : TABLEAUX REELS AUXILIAIRES
!     SORTIES :
!       NOLOPG : NON DE LA LOCALISATION ECRITE
!       CODRET : CODE DE RETOUR (0 : PAS DE PB, NON NUL SI PB)
!
!     REMARQUE :
!     ON DOIT FOURNIR LES COORDONNEES SOUS LA FORME :
!     . ELEMENT 1D : X1 X2 ... ... XN
!     . ELEMENT 2D : X1 Y1 X2 Y2 ... ... XN YN
!     . ELEMENT 3D : X1 Y1 Z1 X2 Y2 Z2 ... ... XN YN ZN
!     C'EST CE QUE MED APPELLE LE MODE ENTRELACE
!     ON DOIT FOURNIR LES POIDS SOUS LA FORME :
!     WG1 WG2 ... ... WGN
!_______________________________________________________________________
!
    implicit none
!
! 0.1. ==> ARGUMENTS
!
    include 'asterfort/codent.h'
    include 'asterfort/infniv.h'
    include 'asterfort/lxlgut.h'
    include 'asterfort/mfferm.h'
    include 'asterfort/mfgaue.h'
    include 'asterfort/mfgaui.h'
    include 'asterfort/mfgaul.h'
    include 'asterfort/mfngau.h'
    include 'asterfort/mfouvr.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mess.h'
    integer :: nbnoto, nbrepg, nbsp, ndim, typgeo
!
    real(kind=8) :: refcoo(*), gscoo(*), wg(*)
    real(kind=8) :: raux1(*), raux2(*), raux3(*)
!
    character(len=16) :: nomfpg
    character(len=*) :: nolopg, nomasu
    character(len=*) :: nofimd
!
    integer :: codret
!
! 0.2. ==> COMMUNS
! 0.3. ==> VARIABLES LOCALES
!
    character(len=6) :: nompro
    parameter ( nompro = 'IRMPG1' )
!
    integer :: edfuin
    parameter (edfuin=0)
    integer :: edleaj
    parameter (edleaj=1)
!
    integer :: ifm, nivinf
    integer :: iaux, jaux, kaux
    integer :: nblopg
    integer :: idfimd
    integer :: typgel, nbrepl, ndim2
    integer :: lgnofa
!
!
    character(len=8) :: saux08
    character(len=16) :: saux16
    character(len=64) :: saux64, nomas2
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
! 1.2. ==> INFORMATION
!
    if (nivinf .gt. 1) then
        write (ifm,1001) 'DEBUT DE '//nompro
        write (ifm,12001) nomfpg
        12001 format(/,'ECRITURE D''UNE LOCALISATION DES POINTS DE GAUSS',&
     &       /,'==> NOM DE LA FAMILLE D''ELEMENT FINI ASSOCIEE : ',a)
    endif
    1001 format(/,4x,10('='),a,10('='),/)
!
! 1.3. ==> OUVERTURE FICHIER MED EN MODE 'LECTURE_AJOUT'
!      CELA SIGNIFIE QUE LE FICHIER EST ENRICHI MAIS ON NE PEUT PAS
!      Y ECRASER UNE DONNEE DEJA PRESENTE.
!      REMARQUE : LE FICHIER EXISTE DEJA CAR IL CONTIENT LE MAILLAGE.
!
    inquire(file=nofimd,exist=ficexi)
    if (.not. ficexi) then
        call u2mess('F', 'MED2_3')
    endif
    call mfouvr(idfimd, nofimd, edleaj, codret)
    if (codret .ne. 0) then
        saux08='MFOUVR  '
        call u2mesg('F', 'DVP_97', 1, saux08, 1,&
                    codret, 0, 0.d0)
    endif
!
! 1.4. ==> CREATION DE LA PRMIERE MOITIE DU NOM DE LA LOCALISATION :
!          ON L'IDENTIFIE AU NOM DE LA FAMILLE, EN REMPLACANT LES BLANCS
!          INTERNES PAR DES '_'
!
    saux16 = '                '
!               1234567890123456
    lgnofa = lxlgut(nomfpg)
    saux16(1:lgnofa) = nomfpg(1:lgnofa)
    do 14 , iaux = 1 , lgnofa
    if (saux16(iaux:iaux) .eq. ' ') then
        saux16(iaux:iaux) = '_'
    endif
    14 end do
!
!====
! 2. ON CHERCHE SI UNE DESCRIPTION IDENTIQUE EXISTE DEJA DANS LE
!    FICHIER. SI OUI, ON LA DECLARE COMME ETANT CELLE A UTILISER.
!====
!
! 2.1. ==> COMBIEN DE LOCALISATIONS SONT PRESENTES DANS LE FICHIER
!
    call mfngau(idfimd, nblopg, codret)
!
    if (codret .ne. 0) then
        saux08='MFNGAU  '
        call u2mesg('F', 'DVP_97', 1, saux08, 1,&
                    codret, 0, 0.d0)
    endif
!
!
    if (nivinf .gt. 1) then
        if (nblopg .eq. 0) then
            write (ifm,21001)
        else
            write (ifm,21002) nblopg
        endif
    endif
    21001 format(/,2x,'LE FICHIER NE CONTIENT PAS DE',&
     &' LOCALISATION DE POINTS DE GAUSS.')
    21002 format(/,2x,'LE FICHIER CONTIENT DEJA',i8,&
     &' LOCALISATION(S) DE POINTS DE GAUSS : ')
!
! 2.2. ==> PARCOURS DES LOCALISATIONS DEJA ENREGISTREES
!
    do 22 , iaux = 1 , nblopg
!
! 2.2.1. ==> LECTURE DES CARACTERISTIQUES DE LA IAUX-EME LOCALISATION
!              PRESENTE DANS LE FICHIER :
!              SAUX64 = NOM
!              TYPGEL = TYPGEO
!              NBREPL = NOMBRE DE POINTS DE GAUSS
!
    call mfgaui(idfimd, iaux, saux64, typgel, nbrepl,&
                ndim2, nomas2, codret)
!
    if (codret .ne. 0) then
        saux08='MFGAUI  '
        call u2mesg('F', 'DVP_97', 1, saux08, 1,&
                    codret, 0, 0.d0)
    endif
!
    if (nivinf .gt. 1) then
        write (ifm,22101) iaux, saux64, typgel, nbrepl
    endif
    22101 format(&
     & /,2x,'. CARACTERISTIQUES DE LA LOCALISATION NUMERO',i4,' : ',&
     & /,2x,'... NOM    : ',a,&
     & /,2x,'... TYPGEO :',i4,&
     & /,2x,'... NBREPG :',i4)
!
! 2.2.2. ==> ON REPERE SI LA LOCALISATION EST BATIE SUR LA MEME
!            FAMILLE D'ELEMENT FINI ASTER.
!            ON PEUT DEJA FILTRER SUR LE TYPE D'ELEMENT FINI
!            ENSUITE, SI LES CARACTERISTIQUES SONT LES MEMES, ON LIT LES
!            POIDS ET LES COORDONNEES ET LES ON COMPARE A CEUX DE
!            LA LOCALISATION VOULUE.
!            DES QUE L'ON TROUVE UN TERME DIFFERENT, ON PASSE A LA
!            LOCALISATION SUIVANTE.
!
    if (saux64(1:lgnofa) .eq. saux16(1:lgnofa) .and. nomas2 .eq. nomasu) then
!
!
        if (typgel .eq. typgeo .and. nbrepl .eq. nbrepg) then
!
            call mfgaul(idfimd, raux1, raux2, raux3, edfuin,&
                        saux64, codret)
!
            if (codret .ne. 0) then
                saux08='MFGAUL  '
                call u2mesg('F', 'DVP_97', 1, saux08, 1,&
                            codret, 0, 0.d0)
            endif
!
            kaux = nbnoto*ndim
            do 2221 , jaux = 1 , kaux
            if (refcoo(jaux) .ne. raux1(jaux)) then
                goto 22
            endif
2221          continue
            kaux = nbrepg*ndim
            do 2222 , jaux = 1 , kaux
            if (gscoo(jaux) .ne. raux2(jaux)) then
                goto 22
            endif
2222          continue
            do 2223 , jaux = 1 , nbrepg
            if (wg(jaux) .ne. raux3(jaux)) then
                goto 22
            endif
2223          continue
!
!           SI ON ARRIVE ICI, C'EST QUE LES LOCALISATIONS SONT
!           IDENTIQUES ; ON LE NOTIFIE ET ON TERMINE LE PROGRAMME
!
            if (nivinf .gt. 1) then
                write (ifm,22201) saux64
            endif
            22201 format(/,2x,'LA LOCALISATION ',a,' EST LA BONNE.')
!
            nolopg = saux64
!
            goto 40
!
        endif
!
    endif
!
    22 end do
!
!====
! 3. ON DOIT ECRIRE UNE NOUVELLE LOCALISATION
!    ON LUI CREE UN NOM AUTOMATIQUE.
!    ON S'ASSURE QUE LE NOM CREE N'EXISTE PAS DEJA DANS LE FICHIER
!====
!
! 3.0. ==> IMPRESSION EVENTUELLE DES CARACTERISTIQUES
!
    if (nivinf .gt. 1) then
!
        write (ifm,*) ' '
        write (ifm,61000) 'FAMILLE DE POINTS DE GAUSS', nomfpg
        write (ifm,61001) 'NOMBRE DE NOEUDS          ', nbnoto
        write (ifm,61001) 'NOMBRE DE POINTS DE GAUSS ', nbrepg
!
!     6.1. DIMENSION 1
!
        if (ndim .eq. 1) then
!                            123456789012345
            write (ifm,60001) 'NOEUDS         '
            do 6011 , iaux = 1 , nbnoto
            write (ifm,60011) iaux,refcoo(iaux)
6011          continue
            write (ifm,60021)
            write (ifm,60001) 'POINTS DE GAUSS'
            do 6021 , iaux = 1 , nbrepg
            write (ifm,60011) iaux,gscoo(iaux)
6021          continue
            write (ifm,60021)
!
!     6.2. DIMENSION 2
!
        else if (ndim.eq.2) then
            write (ifm,60002) 'NOEUDS         '
            do 6012 , iaux = 1 , nbnoto
            write (ifm,60012) iaux, refcoo(ndim*(iaux-1)+1),&
                refcoo(ndim*(iaux-1)+2)
6012          continue
            write (ifm,60022)
            write (ifm,60002) 'POINTS DE GAUSS'
            do 6022 , iaux = 1 , nbrepg
            write (ifm,60012) iaux, gscoo(ndim*(iaux-1)+1),&
                gscoo(ndim*(iaux-1)+2)
6022          continue
            write (ifm,60022)
!
!     6.3. DIMENSION 3
!
        else
            write (ifm,60003) 'NOEUDS         '
            do 6013 , iaux = 1 , nbnoto
            write (ifm,60013) iaux, refcoo(ndim*(iaux-1)+1),&
                refcoo(ndim*(iaux-1)+2), refcoo(ndim*(iaux-1)+3)
6013          continue
            write (ifm,60023)
            write (ifm,60003) 'POINTS DE GAUSS'
            do 6023 , iaux = 1 , nbrepg
            write (ifm,60013) iaux, gscoo(ndim*(iaux-1)+1),&
                gscoo(ndim*(iaux-1)+2), gscoo(ndim*(iaux-1)+3)
6023          continue
            write (ifm,60023)
        endif
!
        write (ifm,60004)
        do 6024 , iaux = 1 , nbrepg
        write (ifm,60011) iaux, wg(iaux)
6024      continue
        write (ifm,60021)
!
    endif
!
    60001 format(&
     &/,28('*'),&
     &/,'*      COORDONNEES DES     *',&
     &/,'*      ',a15        ,'     *',&
     &/,28('*'),&
     &/,'*  NUMERO  *       X       *',&
     &/,28('*'))
    60002 format(&
     &/,44('*'),&
     &/,'*       COORDONNEES DES ',a15        ,'    *',&
     &/,44('*'),&
     &/,'*  NUMERO  *       X       *       Y       *',&
     &/,44('*'))
    60003 format(&
     &/,60('*'),&
     &/,'*            COORDONNEES DES ',a15         ,&
     &'               *',&
     &/,60('*'),&
     &/,'*  NUMERO  *       X       *       Y       *',&
     &'       Z       *',&
     &/,60('*'))
    60004 format(&
     &/,28('*'),&
     &/,'*      POINTS DE GAUSS     *',&
     &/,'*  NUMERO  *     POIDS     *',&
     &/,28('*'))
    60011 format('* ',i5,'    *',1pg12.5,'    *')
    60012 format('* ',i5,2('    *',1pg12.5),'    *')
    60013 format('* ',i5,3('    *',1pg12.5),'    *')
    60021 format(28('*'))
    60022 format(44('*'))
    60023 format(60('*'))
    61000 format(a,' : ',a)
    61001 format(a,' : ',i4)
!
    kaux = -1
!
30  continue
!
! 3.1. ==> CREATION D'UN NOM POUR LA LOCALISATION
!      NOLOPG( 1:16) = NOM DE LA FAMILLE DES POINTS DE GAUSS
!      NOLOPG(17:25) = NOMBRE DE SOUS-POINTS SI > 1
!      NOLOPG(26:32) = COMPTEUR AU DELA DE 1
!      LES MANQUES INTERNES SONT COMPLETES PAR DES '_'
!
!     EXEMPLE : HE8_____FPG8
!               QU4_____FPG4 __________________3
!               12345678901234567890123456789012
!
! 3.1.1 ==> MISE A BLANC
!
    nolopg =&
     &'                                                                '
!      1234567890123456789012345678901234567890123456789012345678901234
!
! 3.1.2 ==> INSERTION DU NOM DE LA FAMILLE
!
    nolopg(1:lgnofa) = nomfpg(1:lgnofa)
!
    do 312 , iaux = 1, lgnofa
    if (nolopg(iaux:iaux) .eq. ' ') then
        nolopg(iaux:iaux) = '_'
    endif
    312 end do
!
! 3.1.3 ==> INSERTION DU NOMBRE DE SOUS-POINTS SI > 1
!
    if (nbsp .gt. 1) then
        call codent(nbsp, 'D', saux08)
        nolopg (17:24) = saux08
        do 313 , iaux = 1, 24
        if (nolopg(iaux:iaux) .eq. ' ') then
            nolopg(iaux:iaux) = '_'
        endif
313      continue
    endif
!
! 3.1.4 ==> INSERTION DU COMPTEUR AU DELA DE 1
!
    kaux = kaux + 1
    if (kaux .gt. 0) then
        call codent(kaux, 'D', saux08)
        nolopg (25:32) = saux08
        do 314 , iaux = 1, 32
        if (nolopg(iaux:iaux) .eq. ' ') then
            nolopg(iaux:iaux) = '_'
        endif
314      continue
    endif
!
! 3.2. ==> LECTURE DES CARACTERISTIQUES DE LA IAUX-EME LOCALISATION
!            PRESENTE DANS LE FICHIER :
!              SAUX64 = NOM
!              TYPGEL = TYPGEO
!              NBREPL = NOMBRE DE POINTS DE GAUSS
!          SI ON EN TROUVE UNE QUI PORTE LE MEME NOM, ON RECOMMENCE
!
    do 32 , iaux = 1 , nblopg
!
    call mfgaui(idfimd, iaux, saux64, typgel, nbrepl,&
                ndim2, nomas2, codret)
!
    if (codret .ne. 0) then
        saux08='MFGAUI  '
        call u2mesg('F', 'DVP_97', 1, saux08, 1,&
                    codret, 0, 0.d0)
    endif
!
    if (saux64 .eq. nolopg) then
        goto 30
    endif
!
    32 end do
!
! 3.3. ==> ECRITURE DE LA NOUVELLE DESCRIPTION
!
    if (nivinf .gt. 1) then
!
        write (ifm,33000) typgeo, nbnoto, nbrepg, nolopg
        33000 format(&
     &/,'TYPE DE MAILLES MED :', i4,&
     &/,'NOMBRE DE NOEUDS          :', i4&
     &/,'NOMBRE DE POINTS DE GAUSS :', i4,&
     &/,'ECRITURE D''UNE NOUVELLE LOCALISATION DES POINTS DE GAUSS, ',&
     &  'NOMMEE : ',/,a,/)
!
    endif
!
    if (nomasu .ne. ' ') then
        nolopg(32:64)=nomasu(1:32)
    endif
!
    call mfgaue(idfimd, typgeo, refcoo, edfuin, nbrepg,&
                gscoo, wg, nolopg, ndim, nomasu,&
                codret)
!
    if (codret .ne. 0) then
        saux08='MFGAUE  '
        call u2mesg('F', 'DVP_97', 1, saux08, 1,&
                    codret, 0, 0.d0)
    endif
!
!====
! 4. LA FIN
!====
!
40  continue
!
! 4.1. ==> FERMETURE DU FICHIER MED
!
    call mfferm(idfimd, codret)
    if (codret .ne. 0) then
        saux08='MFFERM  '
        call u2mesg('F', 'DVP_97', 1, saux08, 1,&
                    codret, 0, 0.d0)
    endif
!
    if (nivinf .gt. 1) then
        write (ifm,1001) 'FIN DE '//nompro
    endif
!
end subroutine
