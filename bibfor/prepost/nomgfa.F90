subroutine nomgfa(nogr, nbgr, dgf, nogrf, nbgf)
!     ------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! person_in_charge: nicolas.sellenet at edf.fr
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!  ENTREES :
!     NOGR   = NOMS DES GROUPES D ENTITES
!     NBGR   = NOMBRE DE GROUPES
!     DGF    = DESCRIPTEUR-GROUPE DE LA FAMILLE (VECTEUR ENTIERS)
!  SORTIES :
!     NOGRF  = NOMS DES GROUPES D ENTITES DE LA FAMILLE
!     NBGF   = NOMBRE DE GROUPES DE LA FAMILLE
!     ------------------------------------------------------------------
!
    implicit none
!
! 0.1. ==> ARGUMENTS
!
    include 'asterfort/exigfa.h'
    integer :: nbgf, nbgr
    integer :: dgf(*)
    character(len=80) :: nogrf(*)
    character(len=24) :: nogr(nbgr)
!
! 0.2. ==> COMMUNS
!
! 0.3. ==> VARIABLES LOCALES
!
    character(len=56) :: saux56
    integer :: iaux
!
!     ------------------------------------------------------------------
!
    saux56 = ' '
!
    nbgf = 0
    do 10 , iaux = 1,nbgr
    if (exigfa(dgf,iaux)) then
        nbgf = nbgf + 1
        nogrf(nbgf)(1:24) = nogr(iaux)
        nogrf(nbgf)(25:80) = saux56
    endif
    10 end do
!
end subroutine
