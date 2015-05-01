subroutine cgvemf(modele, typfis, nomfis, typdis)
    implicit none
!
#include "asterf_types.h"
#include "asterfort/exixfe.h"
#include "asterfort/utmess.h"
#include "asterfort/xvfimo.h"
    character(len=8) :: modele, typfis, nomfis
    character(len=16) :: typdis
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: samuel.geniaut at edf.fr
!
!     SOUS-ROUTINE DE L'OPERATEUR CALC_G
!
!     BUT : VERIFICATION DE LA COMPATIBILITE ENTRE LA SD ASSOCIEE AU
!           FOND DE FISSURE ET LE MODELE
!
!  IN :
!    MODELE : NOM DE LA SD_MODELE
!    TYPFIS : TYPE DE LA SD DECRIVANT LE FOND DE FISSURE
!            ('THETA' OU 'FONDIFSS' OU 'FISSURE')
!    NOMFIS : NOM DE LA SD DECRIVANT LE FOND DE FISSURE
! ======================================================================
!
    integer :: ixfem
    aster_logical :: fiinmo
    character(len=8) :: valk(2)
!
!     LE MODELE EST-IL X-FEM : SI OUI IXFEM=1
    call exixfe(modele, ixfem)
!
!     ERREUR SI FOND_FISS EST DONNE AVEC UN MODELE X-FEM
    if (typfis .eq. 'FONDFISS' .and. ixfem .eq. 1) then
        call utmess('F', 'RUPTURE0_95', sk=modele)
    endif
!
!     ERREUR SI FISSURE EST DONNE AVEC UN MODELE NON X-FEM
    if (typfis .eq. 'FISSURE' .and. ixfem .eq. 0) then
        call utmess('F', 'RUPTURE0_96', sk=modele)
    endif
!
!     ERREUR SI FISSURE N'EST PAS ASSOCIEE AU MODELE X-FEM
!     SAUF SI MODELE COHESIF
    if (typfis .eq. 'FISSURE'.and.typdis.eq.'FISSURE') then
        fiinmo = xvfimo(modele,nomfis)
        if (.not.fiinmo) then
            valk(1)=nomfis
            valk(2)=modele
            call utmess('F', 'RUPTURE0_97', nk=2, valk=valk)
        endif
    endif
!
end subroutine
