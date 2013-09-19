subroutine ircmpn(nofimd, ncmprf, ncmpve, numcmp, exicmp,&
                  nbvato, nbnoec, linoec, adsl, caimpi,&
                  caimpk, profas, innoce)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: nicolas.sellenet at edf.fr
!_______________________________________________________________________
!  ECRITURE D'UN CHAMP - FORMAT MED - PROFIL POUR LES NOEUDS
!     -  -       -              -     -               -
!_______________________________________________________________________
!     ENTREES :
!       NOFIMD : NOM DU FICHIER MED
!       NCMPRF : NOMBRE DE COMPOSANTES DU CHAMP DE REFERENCE
!       NCMPVE : NOMBRE DE COMPOSANTES VALIDES EN ECRITURE
!       NUMCMP : NUMEROS DES COMPOSANTES VALIDES
!       EXICMP : EXISTENCE DES COMPOSANTES PAR MAILLES
!       NBVATO : NOMBRE DE VALEURS TOTALES
!       NBNOEC : NOMBRE D'ENTITES A ECRIRE (O, SI TOUTES)
!       LINOEC : LISTE DES ENTITES A ECRIRE SI EXTRAIT
!       ADSK, D, ... : ADRESSES DES TABLEAUX DES CHAMPS SIMPLIFIES
!       INNOCE : TABLEAU INDICATEUR DE NOEUD CENTRE
!                INNOCE(INO)=1 SI LE NOEUD INO EST UN NOEUD CENTRE
!                D'UNE MAILLE TRIA7,QUAD9,PENTA18 OU HEXA27
!
!     SORTIES :
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
!         CAIMPK : CARACTERES POUR CHAQUE IMPRESSION
!                  CAIMPK(1) = NOM DE LA LOCALISATION ASSOCIEE
!                  CAIMPK(2) = NOM DU PROFIL AU SENS MED
!                  CAIMPK(3) = NOM DE L'ELEMENT DE STRUCTURE
!       PROFAS : PROFIL ASTER. C'EST LA LISTE DES NUMEROS ASTER DES
!                NOEUDS POUR LESQUELS LE CHAMP EST DEFINI
!
!  COMMENTAIRE : C'EST AVEC L'USAGE DE ZL(ADSL) QU'IL FAUT FILTRER LES
!                COMPOSANTES. SEUL MOYEN FIABLE AVEC UN CHAMELEM
!                SIMPLIFIE.
!_______________________________________________________________________
!
    implicit none
!
! 0.1. ==> ARGUMENTS
!
#include "jeveux.h"
#include "asterfort/infniv.h"
#include "asterfort/ircmpf.h"
    integer :: nbvato, ncmprf, ncmpve
    integer :: numcmp(ncmprf), innoce(nbvato)
    integer :: nbnoec
    integer :: linoec(*)
    integer :: adsl
    integer :: caimpi(10)
    integer :: profas(nbvato)
!
    character(len=*) :: nofimd
    character(len=80) :: caimpk(3)
!
    logical :: exicmp(nbvato)
!
! 0.2. ==> COMMUNS
!
!
! 0.3. ==> VARIABLES LOCALES
!
    character(len=6) :: nompro
    parameter ( nompro = 'IRCMPN' )
!
    character(len=80) :: ednopf
    parameter ( ednopf=' ' )
    character(len=80) :: ednoga
    parameter ( ednoga=' ' )
!                         12345678901234567890123456789012
!
    integer :: typnoe
    parameter (typnoe=0)
    integer :: ednopg
    parameter (ednopg=1)
!
    character(len=64) :: noprof
!
    integer :: ifm, nivinf
!
    integer :: iaux, jaux
    integer :: nrcmp
    integer :: nval
!
!====
! 1. PREALABLES
!====
!
    call infniv(ifm, nivinf)
!
    if (nivinf .gt. 1) then
        write (ifm,1001) 'DEBUT DE '//nompro
    endif
    1001 format(/,4x,10('='),a,10('='),/)
!
!====
! 2. ON REMPLIT UN PREMIER TABLEAU PAR NOEUD :
!    VRAI DES QU'UNE DES COMPOSANTES DU CHAMP EST PRESENTE SUR LE NOEUD
!    FAUX SINON
!    REMARQUES: 1- ON EXAMINE LES NCMPVE COMPOSANTES QUI SONT DEMANDEES,
!    MAIS IL FAUT BIEN TENIR COMPTE DE NCMPRF, NOMBRE DE COMPOSANTES DE
!    REFERENCE, POUR L'ADRESSAGE DANS LE TABLEAU ADSL
!               2- SI LE NOEUD EST UN NOEUD CENTRE, ON L'OUBLIE
!====
!
    do 21 , iaux = 0 , nbvato-1
!
    if (innoce(iaux+1) .eq. 1) then
        exicmp(iaux+1) = .false.
        goto 21
    endif
!
    jaux = adsl-1+iaux*ncmprf
    do 211 , nrcmp = 1 , ncmpve
    if (zl(jaux+numcmp(nrcmp))) then
        exicmp(iaux+1) = .true.
        goto 21
    endif
211  continue
!
    21 end do
!
!====
! 3. PROFAS : LISTE DES NOEUDS POUR LESQUELS ON AURA IMPRESSION
!    UN NOEUD EN FAIT PARTIE SI ET SEULEMENT SI AU MOINS UNE COMPOSANTE
!    Y EST DEFINIE ET S'IL FAIT PARTIE DU FILTRAGE DEMANDE
!====
!
    nval = 0
!
! 3.1. ==> SANS FILTRAGE : C'EST LA LISTE DES NOEUDS AVEC UNE COMPOSANTE
!          VALIDE
!
    if (nbnoec .eq. 0) then
!
        do 31 , iaux = 1 , nbvato
        if (exicmp(iaux)) then
            nval = nval + 1
            profas(nval) = iaux
        endif
31      continue
!
! 3.2. ==> AVEC FILTRAGE
!
    else
!
        do 32 , jaux = 1 , nbnoec
        iaux = linoec(jaux)
        if (exicmp(iaux)) then
            nval = nval + 1
            profas(nval) = iaux
        endif
32      continue
!
    endif
!
!====
! 4. CARACTERISATIONS DES IMPRESSIONS
!====
!
!                  CAIMPI(1,I) = TYPE D'EF / MAILLE ASTER (0, SI NOEUD)
    caimpi(1) = 0
!                  CAIMPI(2,I) = NOMBRE DE POINTS DE GAUSS
    caimpi(2) = ednopg
!                  CAIMPI(3,I) = NOMBRE DE SOUS-POINTS
    caimpi(3) = ednopg
!                  CAIMPI(4,I) = NOMBRE DE MAILLES A ECRIRE
    caimpi(4) = ednopg
!                  CAIMPI(4,I) = NOMBRE DE MAILLES A ECRIRE
    caimpi(5) = ednopg
!                  CAIMPI(4,I) = NOMBRE DE MAILLES A ECRIRE
    caimpi(6) = ednopg
!                  CAIMPI(4,I) = NOMBRE DE MAILLES A ECRIRE
    caimpi(7) = nval
!                  CAIMPI(5,I) = TYPE DE MAILLES ASTER (0, SI NOEUD)
    caimpi(8) = 0
!                  CAIMPI(6,I) = TYPE GEOMETRIQUE AU SENS MED
    caimpi(9) = typnoe
!                  CAIMPI(7,I) = NOMBRE DE MAILLES IDENTIQUES
    caimpi(10) = nbvato
!                  CAIMPK(1) = NOM DE LA LOCALISATION ASSOCIEE
    caimpk(1) = ednoga
!                  CAIMPK(2) = NOM DU PROFIL AU SENS MED
    caimpk(2) = ednopf
!                  CAIMPK(3) = NOM DE L'ELEMENT DE STRUCTURE AU SENS MED
    caimpk(3) = ednopf
!
!GN      WRITE(IFM,*) 'A LA FIN DE 4, CAIMPI :'
!GN      WRITE(IFM,4000) (CAIMPI(IAUX,1),IAUX = 1 , 7)
!GN 4000 FORMAT('TYPN =',I4,', NB PG =',I4,', NB SPT =',I4,
!GN     >       ', NBVAL ECR =',I8,', MA ASTER =',I4,
!GN     >       ', TYPE GEO MED =',I4,', NBVAL TOT =',I8)
!
    if (nivinf .gt. 1) then
        write (ifm,3301) nompro, ' : NOMBRE TOTAL DE VALEURS    : ',&
     &                   nbvato
        write (ifm,3301) nompro, ' : NOMBRE DE VALEURS A ECRIRE : ',&
        nval
    endif
    3301 format(4x,a6,a,i8)
!
!====
! 5. STOCKAGE DU PROFIL DANS LE FICHIER MED
!    REMARQUE : DANS LE CAS DES NOEUDS, IL Y A IDENTITE ENTRE LES
!               NUMEROTATIONS ASTER ET MED DES NOEUDS (CF IRMMNO)
!====
!
    if (nval .ne. nbvato) then
!
        iaux = 0
        call ircmpf(nofimd, nval, profas, noprof)
!
        caimpk(2) = noprof
!
    endif
!
!====
! 6. LA FIN
!====
!
    if (nivinf .gt. 1) then
        write (ifm,1001) 'FIN DE '//nompro
    endif
!
end subroutine
