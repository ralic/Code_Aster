subroutine utnbnv(typmav, nbsv, nbnv)
!-----------------------------------------------------------------------
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
! person_in_charge: olivier.boiteau at edf.fr
!-----------------------------------------------------------------------
!    - FONCTION REALISEE:  UTILITAIRE CALCULANT LE TYPE DE FACE VOISINE
!                          ET SON NBRE DE SOMMETS. POUR AERER TE0003
!
! IN TYPMAV  : NOM DE LA MAILLE VOISINE.
! OUT NBSV   : TYPE DE MAILLE VOISINE (POUR 2D, TRAITEMENT MAILLE SYM.).
! OUT NBNV   : NOMBRE DE NOEUDS.
!   -------------------------------------------------------------------
!
!     FONCTIONS INTRINSEQUES:
!       AUCUNE.
!   -------------------------------------------------------------------
!     ASTER INFORMATIONS:
!       25/09/01 (OB): CREATION POUR SIMPLIFIER TE0003.F.
!----------------------------------------------------------------------
! CORPS DU PROGRAMME
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
#include "asterfort/assert.h"
    integer :: nbnv, nbsv
    character(len=8) :: typmav
!
! DECLARATION VARIABLES LOCALES
    character(len=1) :: noeuv
    character(len=2) :: formv
!
! INIT.
    formv = typmav(1:2)
    noeuv = typmav(5:5)
!
! DETERMINATION DE NBNV ET NBSV (EN 2D)
!
! TRIANGLE
    if ((formv.eq.'TR') .or. (formv.eq.'TL')) then
        nbsv = 3
        call assert(noeuv.eq.'3'.or.noeuv.eq.'6')
        if (noeuv .eq. '3') then
            nbnv = 3
        else if (noeuv.eq.'6') then
            nbnv = 6
        endif
! QUADRANGLE
    else if ((formv.eq.'QU').or.(formv.eq.'QL')) then
        nbsv = 4
        call assert(noeuv.eq.'4'.or.noeuv.eq.'8'.or.noeuv.eq.'9')
        if (noeuv .eq. '4') then
            nbnv = 4
        else if (noeuv.eq.'8') then
            nbnv = 8
        else if (noeuv.eq.'9') then
            nbnv = 9
        endif
! HEXAEDRE
    else if (formv.eq.'HE') then
        call assert(typmav(5:5) .eq. '8' .or. typmav(5:6) .eq. '20' .or. typmav(5:6) .eq. '27')
        if (typmav(5:5) .eq. '8') then
            nbnv = 8
        else if (typmav(5:6).eq.'20') then
            nbnv = 20
        else if (typmav(5:6).eq.'27') then
            nbnv = 27
        endif
! PENTAEDRE
    else if (formv.eq.'PE') then
        call assert(typmav(6:6).eq.'6'.or.typmav(6:7).eq.'15')
        if (typmav(6:6) .eq. '6') then
            nbnv = 6
        else if (typmav(6:7).eq.'15') then
            nbnv = 15
        endif
! TETRAEDRE
    else if (formv.eq.'TE') then
        call assert(typmav(6:6).eq.'4'.or.typmav(6:7).eq.'10')
        if (typmav(6:6) .eq. '4') then
            nbnv = 4
        else if (typmav(6:7).eq.'10') then
            nbnv = 10
        endif
    else
        call assert(.false.)
    endif
!
end subroutine
