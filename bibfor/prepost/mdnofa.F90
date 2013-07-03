subroutine mdnofa(numfam, nogrf, nbgf, nbfaex, nofaex,&
                  nomfam)
! person_in_charge: nicolas.sellenet at edf.fr
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
!  ENTREES :
!     NUMFAM = NUMERO DE LA FAMILLE
!     NOGRF  = NOMS DES GROUPES D ENTITES DE LA FAMILLE
!     NBGF   = NOMBRE DE GROUPES DE LA FAMILLE
!     NBFAEX = NOMBRE DE FAMILLES EXISTANTES POUR CETTE SERIE
!     NOFAEX = NOMS DES FAMILLES DEJA CREEES
!  SORTIES :
!     NOMFAM = NOM DE LA FAMILLE
!     ------------------------------------------------------------------
!
!    LE NOM DE LA FAMILLE SERA LA CONCATENATION DES NOMS DES GROUPES.
!    ON PLACE UN '_' ENTRE DEUX NOMS
!    EXEMPLE :
!       GROUPE 1 : 'HAUT'
!       GROUPE 2 : 'FACE_X'
!       FAMILLE ==> 'HAUT_FACE_X'
!                    12345678901234567890123456789012
!
!     SI ON N'A PAS LA PLACE, ON MET UN NOM ARBITRAIRE
!
    implicit none
!
! 0.1. ==> ARGUMENTS
!
#include "asterfort/codent.h"
#include "asterfort/lxlgut.h"
    integer :: numfam
    integer :: nbgf
    integer :: nbfaex
    character(len=*) :: nogrf(*)
    character(len=*) :: nomfam
    character(len=*) :: nofaex(*)
!
! 0.2. ==> COMMUNS
!
! 0.3. ==> VARIABLES LOCALES
!
    integer :: iaux, jaux
    integer :: ideb, ifin
    integer :: lgnofa, lgnofx
!
    character(len=8) :: saux08
!
!
!
!====
! 1. ON INITIALISE AVEC DES ' '
!====
!
    lgnofx = len(nomfam)
    do 11 , iaux = 1 , lgnofx
    nomfam(iaux:iaux) = ' '
    11 end do
!
!====
! 2. CALCUL DE LA TAILLE NECESSAIRE A LA CONCATENATION
!====
!
    lgnofa = 0
    do 21 , iaux = 1 , nbgf
    lgnofa = lgnofa + lxlgut(nogrf(iaux))
    if (iaux .lt. nbgf) then
        lgnofa = lgnofa + 1
    endif
21  continue
!
!====
! 2. ON A LA PLACE : FABRICATION DU NOM
!====
!
    if (lgnofa .le. lgnofx) then
!
        ifin = 0
        do 31 , iaux = 1 , nbgf
        jaux = lxlgut(nogrf(iaux))
        ideb = ifin + 1
        ifin = ifin + jaux
        nomfam(ideb:ifin) = nogrf(iaux)(1:jaux)
        if (iaux .lt. nbgf) then
            ifin = ifin + 1
            nomfam(ifin:ifin) = '_'
        endif
31      continue
!
!====
! 4. SINON, C'EST UN NOM ARBITRAIRE, CONSTRUIT AVEC LE NUMERO DE
!    LA FAMILLE
!====
!
    else
!
!                      12345678
        nomfam(1:8) = 'FAMILLE_'
!
        call codent(numfam, 'G', saux08)
!
        jaux = lxlgut(saux08)
        nomfam(9:8+jaux) = saux08(1:jaux)
!
    endif
!
!====
! 5. MEMORISATION DU NOM DANS LE TABLEAU
!====
!
    nofaex(nbfaex+1) = nomfam
!
end subroutine
