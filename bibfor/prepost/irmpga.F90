subroutine irmpga(nofimd, chanom, typech, nomtyp, nbimpr,&
                  caimpi, caimpk, modnum, nuanom, sdcarm,&
                  codret)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!_______________________________________________________________________
!     ECRITURE AU FORMAT MED - LOCALISATION POINTS DE GAUSS
!        -  -            -                  -         --
!_______________________________________________________________________
!     ENTREES :
!       NOFIMD : NOM DU FICHIER MED
!       CHANOM : NOM ASTER DU CHAMP
!       TYPECH : TYPE DU CHAMP ('ELEM', 'ELGA')
!       NOMTYP : NOM DES TYPES DE MAILLES ASTER
!       NBIMPR : NOMBRE D'IMPRESSIONS
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
!       MODNUM : INDICATEUR SI LA SPECIFICATION DE NUMEROTATION DES
!                NOEUDS DES MAILLES EST DIFFERENTES ENTRE ASTER ET MED:
!                     MODNUM = 0 : NUMEROTATION IDENTIQUE
!                     MODNUM = 1 : NUMEROTATION DIFFERENTE
!       NUANOM : TABLEAU DE CORRESPONDANCE DES NOEUDS MED/ASTER.
!                NUANOM(ITYP,J): NUMERO DANS ASTER DU J IEME NOEUD DE LA
!                MAILLE DE TYPE ITYP DANS MED.
!     SORTIES :
!         CAIMPK : CARACTERES POUR CHAQUE IMPRESSION
!                  CAIMPK(1,I) = NOM DE LA LOCALISATION ASSOCIEE
!                  CAIMPK(2,I) = NOM DU PROFIL AU SENS MED
!                  CAIMPK(3,I) = NOM DE L'ELEMENT DE STRUCTURE
!       CODRET : CODE DE RETOUR (0 : PAS DE PB, NON NUL SI PB)
!_______________________________________________________________________
!
    implicit none
!
    include 'jeveux.h'
!
    include 'asterc/utflsh.h'
    include 'asterfort/assert.h'
    include 'asterfort/infniv.h'
    include 'asterfort/irmase.h'
    include 'asterfort/irmpg1.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/uteref.h'
    integer :: ntymax
    parameter (ntymax = 69)
!
! 0.1. ==> ARGUMENTS
!
    integer :: nbimpr
    integer :: caimpi(10, nbimpr)
    integer :: modnum(ntymax), nuanom(ntymax, *)
!
    character(len=8) :: nomtyp(*)
    character(len=8) :: typech, sdcarm
    character(len=19) :: chanom
    character(len=80) :: caimpk(3, nbimpr)
    character(len=*) :: nofimd
!
    integer :: codret
!
! 0.2. ==> COMMUNS
!
!
! 0.3. ==> VARIABLES LOCALES
!
    character(len=6) :: nompro
    parameter ( nompro = 'IRMPGA' )
!
    integer :: ifm, nivinf, nbcou, nbsec, numai
    integer :: iaux, jaux, kaux, laux
    integer :: nbrepg, nbnoso, nbnoto, ndim
    integer :: ntypef, tygeom, tymast
    integer :: nbpg, nbsp
    integer :: nrimpr
!
    integer :: lgmax
    parameter (lgmax=1000)
!
    real(kind=8) :: refcoo(3*lgmax), gscoo(3*lgmax), wg(lgmax)
    real(kind=8) :: raux1(3*lgmax), raux2(3*lgmax), raux3(lgmax)
!
    character(len=4) :: chnbco, chnbse
    character(len=10) :: nonuma
    character(len=16) :: nomtef, nomfpg, typsec
    character(len=64) :: nolopg, nomasu
!
!====
! 1. PREALABLES
!====
!
    codret = 0
!
    call infniv(ifm, nivinf)
!
    if (nivinf .gt. 1) then
        write (ifm,1001) 'DEBUT DE '//nompro
        call u2mess('I', 'MED_74')
        call utflsh(codret)
    endif
    1001 format(/,4x,10('='),a,10('='),/)
!
!====
! 2. BOUCLAGE SUR LES IMPRESSIONS DEMANDEES
!    ON NE S'INTERESSE QU'A CELLES QUI COMPORTENT PLUS DE 1 POINT DE
!    GAUSS ET/OU PLUS DE 1 SOUS_POINT
!====
!
    do 20 , nrimpr = 1 , nbimpr
!
    nbpg = caimpi(2,nrimpr)
    nbsp = caimpi(3,nrimpr)
!
    nomasu = ' '
!
    tymast = caimpi(8,nrimpr)
    tygeom = caimpi(9,nrimpr)
!
! 2.1. ==> CARACTERISATIONS DE L'ELEMENT FINI
!          ON DOIT RECUPERER LES COORDONNEES SOUS LA FORME :
!          . ELEMENT 1D : X1 X2 ... ... XN
!          . ELEMENT 2D : X1 Y1 X2 Y2 ... ... XN YN
!          . ELEMENT 3D : X1 Y1 Z1 X2 Y2 Z2 ... ... XN YN ZN
!          C'EST CE QUE MED APPELLE LE MODE ENTRELACE
!          ON DOIT RECUPERER LES POIDS SOUS LA FORME :
!          WG1 WG2 ... ... WGN
!
    if (codret .eq. 0) then
!
        if (nivinf .gt. 1) then
            if (typech(1:4) .eq. 'ELGA') then
                write (ifm,2100) typech, nbpg, nbsp
            else
                write (ifm,2101) typech, nbpg, nbsp
            endif
        endif
        2100  format('CHAMP DE TYPE ',a,', AVEC,',i5,' POINTS DE GAUSS ET',&
     &           i5,' SOUS_POINTS')
        2101  format('CHAMP DE TYPE ',a,', AVEC,',i5,' POINTS  ET',&
     &           i5,' SOUS_POINTS')
!
! 2.1.1. ==> CARACTERISATIONS DE L'ELEMENT FINI QUAND C'EST UN CHAMP
!            AUX POINTS DE GAUSS AVEC PLUS DE 1 POINT DE GAUSS
!
        if (typech(1:4) .eq. 'ELGA') then
!
! 2.1.1.1. ==> FORMULE GENERALE
!       CODRET : CODE DE RETOUR
!                0 : PAS DE PB
!                1 : LE CHAMP N'EST PAS DEFINI SUR CE TYPE D'ELEMENT
!
            ntypef = caimpi(1,nrimpr)
            call jenuno(jexnum('&CATA.TE.NOMTE', ntypef), nomtef)
!
            call uteref(chanom, typech, ntypef, nomtef, nomfpg,&
                        nbnoso, nbnoto, nbrepg, ndim, refcoo,&
                        gscoo, wg, codret)
!
            if (codret .eq. 1) then
                codret = 0
                goto 20
            endif
!
! 2.1.1.2. ==> SI CE TYPE DE MAILLE EST RENUMEROTEE ENTRE ASTER ET MED,
!              IL FAUT MODIFIER LA REPARTITION DES NOEUDS
!
            if (modnum(tymast) .eq. 1) then
!
                kaux = ndim*nbnoto
                do 2111 , iaux = 1 , kaux
                raux1(iaux) = refcoo(iaux)
2111              continue
                do 2112 , iaux = 1 , nbnoto
                kaux = ndim*(iaux-1)
                laux = ndim*(nuanom(tymast,iaux)-1)
                do 2113 , jaux = 1 , ndim
                refcoo(kaux+jaux) = raux1(laux+jaux)
2113              continue
2112              continue
!
            endif
!
            nbcou = caimpi(4,nrimpr)
            nbsec = caimpi(5,nrimpr)
            numai = caimpi(6,nrimpr)
            typsec = ' '
            if (nbcou .ne. 0 .or. nbsec .ne. 0 .or. numai .ne. 0) then
                if (nbcou .ne. 0 .and. nbsec .eq. 0 .and. numai .eq. 0) then
                    typsec = 'COQUE'
                    write(chnbco,'(I4)')nbcou
                    nomasu = 'SECT COQ '//chnbco//'C'
                    elseif ( nbcou.ne.0.and.nbsec.ne.0.and. numai.eq.0&
                    )then
                    typsec = 'TUYAU'
                    write(chnbco,'(I4)')nbcou
                    write(chnbse,'(I4)')nbsec
                    nomasu = 'SECT TUY '//chnbco// 'C '//chnbse// 'S'
                    elseif ( nbcou.eq.0.and.nbsec.eq.0.and. numai.ne.0&
                    )then
                    typsec = 'PMF'
                    write(nonuma,'(I10)')numai
                    nomasu = 'SECT PMF '//nonuma
                else
                    call assert(.false.)
                endif
                call irmase(nofimd, typsec, nbcou, nbsec, numai,&
                            sdcarm, nomasu)
                nbrepg = nbpg
            else
!
! 2.1.1.3. ==> EXTENSION AVEC DES SOUS_POINTS : ON REPRODUIT LA MEME
!              DESCRIPTION DANS CHAQUE 'COUCHE'.
!              C'EST UNE SOLUTION TEMPORAIRE, DANS L'ATTENTE DE
!              L'EVOLUTION MED
                if (nbsp .gt. 1) then
!
                    do 2114 , jaux = 2 , nbsp
!
                    kaux = ndim*nbpg*(jaux-1)
                    do 2115 , iaux = 1 , ndim*nbpg
                    gscoo(kaux+iaux) = gscoo(iaux)
2115                  continue
                    kaux = nbpg*(jaux-1)
                    do 2116 , iaux = 1 , nbpg
                    wg(kaux+iaux) = wg(iaux)
2116                  continue
!
2114                  continue
!
                endif
                nbrepg = nbpg*nbsp
            endif
!
! 2.1.2. ==> CARACTERISATIONS DE L'ELEMENT FINI QUAND
!            . C'EST UN CHAMP AUX POINTS DE GAUSS AVEC UN SEUL POINT DE
!              GAUSS MAIS PLUSIEURS SOUS_POINTS
!            . UN CHAMP ELEM
!            ON DEFINIT UNE PSEUDO-LOCALISATION AUX POINTS DE GAUSS
!            ON DEDUIT LA DIMENSION DU CODAGE MED DU TYPE DE MAILLE
!            SOUS-JACENTE
!
        else if ((typech(1:4).eq.'ELEM')) then
!
            iaux = mod(caimpi(9,nrimpr),100)
            ndim = ( caimpi(9,nrimpr) - iaux ) / 100
!
            ntypef = 0
            jaux = 3*lgmax
            do 2121 , iaux = 1 , jaux
            refcoo(iaux) = 0.d0
            gscoo(iaux) = 0.d0
2121          continue
            do 2122 , iaux = 1 , lgmax
            wg(iaux) = 0.d0
2122          continue
!
            nomfpg(1: 8) = nomtyp(tymast)
            do 2123 , iaux = 1 , 8
            if (nomfpg(iaux:iaux) .eq. ' ') then
                nomfpg(iaux:iaux) = '_'
            endif
2123          continue
            nomfpg(9:16) = typech
!
            nbnoto = mod(tygeom,100)
            nbrepg = nbsp
!
        endif
!
! 2.2. ==> ON ECRIT LA LOCALISATION
!
        if (ndim .gt. 0) then
            call irmpg1(nofimd, nomfpg, nbnoto, nbrepg, nbsp,&
                        ndim, tygeom, refcoo, gscoo, wg,&
                        raux1, raux2, raux3, nolopg, nomasu,&
                        codret)
!
            caimpk(1,nrimpr) = nolopg
            caimpk(3,nrimpr) = nomasu
        endif
!
    endif
!
    20 end do
!
!====
! 3. LA FIN
!====
!
    if (nivinf .gt. 1) then
        write (ifm,1001) 'FIN DE '//nompro
    endif
!
end subroutine
