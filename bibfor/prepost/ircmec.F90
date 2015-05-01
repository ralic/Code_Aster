subroutine ircmec(idfimd, nochmd, nomprf, nolopg, numpt,&
                  instan, numord, val, ncmpve, nbenty,&
                  nbrepg, nvalec, typent, typgeo, codret)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!_______________________________________________________________________
!     ECRITURE D'UN CHAMP -  FORMAT MED - ECRITURE
!        -  -       -               -     --
!_______________________________________________________________________
!     ENTREES :
!       IDFIMD : IDENTIFIANT DU FICHIER MED
!       NOMAM2 : NOM DU MAILLAGE MED
!       NOCHMD : NOM MED DU CHAMP A ECRIRE
!       NOMPRF : NOM MED DU PROFIL ASSOCIE AU CHAMP
!       NOLOPG : NOM MED LOCALISATION DES PTS DE GAUSS ASSOCIEE AU CHAMP
!       NUMPT  : NUMERO DE PAS DE TEMPS
!       INSTAN : VALEUR DE L'INSTANT A ARCHIVER
!       NUMORD : NUMERO D'ORDRE DU CHAMP
!       VAL    : VALEURS EN MODE ENTRELACE
!       NCMPVE : NOMBRE DE COMPOSANTES VALIDES EN ECRITURE
!       NBENTY : NOMBRE D'ENTITES DU TYPE CONSIDERE
!       NBREPG : NOMBRE DE POINTS DE GAUSS
!       NVALEC : NOMBRE DE VALEURS A ECRIRE EFFECTIVEMENT
!       TYPENT : TYPE D'ENTITE MED DU CHAMP A ECRIRE
!       TYPGEO : TYPE GEOMETRIQUE MED DU CHAMP A ECRIRE
!     SORTIES:
!       CODRET : CODE DE RETOUR (0 : PAS DE PB, NON NUL SI PB)
!_______________________________________________________________________
!
    implicit none
!
! 0.1. ==> ARGUMENTS
!
#include "jeveux.h"
#include "asterfort/as_mfdrpw.h"
#include "asterfort/infniv.h"
#include "asterfort/utmess.h"
    character(len=*) :: nochmd, nomprf, nolopg
!
    integer :: idfimd
    integer :: numpt, numord
    integer :: ncmpve, nbenty, nbrepg, nvalec
    integer :: typent, typgeo
!
    real(kind=8) :: instan
    real(kind=8) :: val(*)
!
    integer :: codret
!
! 0.2. ==> COMMUNS
!
!
! 0.3. ==> VARIABLES LOCALES
!
!
    character(len=6) :: nompro
    parameter ( nompro = 'IRCMEC' )
!
    character(len=32) :: ednopf
!                         12345678901234567890123456789012
    parameter ( ednopf='                                ' )
    integer :: edfuin
    parameter (edfuin=0)
    integer :: edall
    parameter (edall=0)
    integer :: ednopt
    parameter (ednopt=-1)
    integer :: ednopg
    parameter (ednopg=1)
    integer :: edcomp
    parameter (edcomp=2)
    character(len=32) :: ednoga
    parameter ( ednoga='                                ' )
!
    integer :: ifm, nivinf
    integer :: iaux
!
    character(len=8) :: saux08
    character(len=14) :: saux14
    character(len=35) :: saux35
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
        1001 format(/,4x,10('='),a,10('='),/)
        call utmess('I', 'MED_49')
        write (ifm,13001) nbrepg, typent, typgeo
        do 13 , iaux = 1 , ncmpve
        write (ifm,13002)&
     &    '. PREMIERE ET DERNIERE VALEURS A ECRIRE POUR LA COMPOSANTE',&
     &    iaux, ' : ',val(iaux),val((nvalec*nbrepg-1)*ncmpve+iaux)
13      continue
    endif
    13001 format(2x,'. NBREPG =',i4,', TYPENT =',i4,', TYPGEO =',i4)
    13002 format(2x,a,i3,a3,5g16.6)
!
!====
! 2. ECRITURE DES VALEURS
!    LE TABLEAU DE VALEURS EST UTILISE AINSI :
!        TV(NCMPVE,NBSP,NBPG,NVALEC)
!    TV(1,1,1,1), TV(2,1,1,1), ..., TV(NCMPVE,1,1,1),
!    TV(1,2,1,1), TV(2,2,1,1), ..., TV(NCMPVE,2,1,1),
!            ...     ...     ...
!    TV(1,NBSP,NBPG,NVALEC), TV(2,NBSP,NBPG,NVALEC), ... ,
!                                      TV(NCMPVE,NBSP,NBPG,NVALEC)
!    C'EST CE QUE MED APPELLE LE MODE ENTRELACE
!    REMARQUE : LE 6-EME ARGUMENT DE as_mfdrpw EST LE NOMBRE DE VALEURS
!               C'EST LE PRODUIT DU NOMBRE TOTAL D'ENTITES DU TYPE EN
!               COURS PAR LE PRODUIT DES NOMBRES DE POINTS DE GAUSS
!               ET DE SOUS-POINT.
!               ATTENTION, CE N'EST DONC PAS LE NOMBRE DE VALEURS
!               REELLEMENT ECRITES MAIS PLUTOT LE NOMBRE MAXIMUM QU'ON
!               POURRAIT ECRIRE.
!====
!
! 2.1. ==> MESSAGES
!
!GN      PRINT *,'TABLEAU REELLEMENT ECRIT'
!GN      PRINT 1789,(VAL(IAUX),
!GN     >  IAUX=1,NVALEC*NBREPG*NCMPVE-1)
!GN 1789  FORMAT(10G12.5)
!
    if (nivinf .gt. 1) then
!                  12345678901235
        saux14 = '. ECRITURE DES'
!                  12345678901234567890123456789012345
        saux35 = ' VALEURS POUR LE NUMERO D''ORDRE : '
!
        if (nbrepg .eq. ednopg) then
            write (ifm,20001) saux14, ncmpve, nvalec, saux35, numord
        else
            write (ifm,20002) saux14, ncmpve, nbrepg, nvalec, saux35,&
            numord
        endif
        if (numpt .ne. ednopt) then
            write (ifm,20003) numpt, instan
        endif
        if (nomprf .eq. ednopf) then
            write (ifm,20004)
        else
            write (ifm,20005) nomprf
        endif
        if (nolopg .eq. ednoga) then
            write (ifm,20006)
        else
            write (ifm,20007) nolopg
        endif
    endif
!
    20001 format(2x,a14,i3,' * ',i8,a35,i5)
    20002 format(2x,a14,2(i3,' * '),i8,a35,i5)
    20003 format(5x,'( PAS DE TEMPS NUMERO :',i5,', T = ',g13.5,' )')
    20004 format(2x,'. PAS DE PROFIL')
    20005 format(2x,'. NOM DU PROFIL : ',a)
    20006 format(2x,'. PAS DE LOCALISATION DE POINTS DE GAUSS')
    20007 format(2x,'. NOM DE LA LOCALISATION DES POINTS DE GAUSS : ',a)
!
! 2.2. ==> NOMBRE DE VALEURS
!
    iaux = nbenty
!
! 2.3. ==> ECRITURE VRAIE
!
    call as_mfdrpw(idfimd, nochmd, val, edfuin, iaux,&
                   nolopg, edall, nomprf, edcomp, typent,&
                   typgeo, numpt, instan, numord, codret)
!
    if (codret .ne. 0) then
        saux08='mfdrpw'
        call utmess('F', 'DVP_97', sk=saux08, si=codret)
    endif
!
    if (nivinf .gt. 1) then
        write (ifm,1001) 'FIN DE '//nompro
    endif
!
end subroutine
