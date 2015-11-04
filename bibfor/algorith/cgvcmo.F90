subroutine cgvcmo(modele, nomfis, typfis, ndim)
    implicit none
!
#include "asterfort/assert.h"
#include "asterfort/exixfe.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"

    character(len=8), intent(in) :: modele, nomfis, typfis
    integer, intent(in) :: ndim
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
!     BUT : VERIFICATION DE LA COMPATIBILITE ENTRE LA DIMENSION DU MODELE
!           ET DE CELLES DES ELEMENTS EN FOND DE FISSURE POUR DETECTER
!           LES ELEMENTS DE STRUCTURE.
!
!  IN :
!    MODELE : NOM DE LA SD_MODELE
!    NOMFIS : NOM DE LA SD DECRIVANT LE FOND DE FISSURE
!    TYPFIS : TYPE DE DEFINITION DE LA FISSURE 'FISSURE', 'FOND_FISS' ou 'THETA'
!    NDIM   : DIMENSION DE LA MODELISATION
! ======================================================================
!
    integer :: idimom, idielm, ixfem, iret, lfonfi

    character(len=19) :: fonfis

    idielm=-1
    idimom=-2

!   LE MODELE EST-IL X-FEM : SI OUI IXFEM=1
    call exixfe(modele, ixfem)
!
!   RECUPERATION DE LA DIMENSION MAX DU MODELE
    if ((ndim .eq. 2).or.(ndim .eq. 120)) then
        idimom=2
    else if ((ndim .eq. 3).or.(ndim .eq. 23).or.(ndim .eq. 103).or.(ndim .eq. 123)) then
        idimom=3
    else
!       APPEL DE CALC_G POUR UNE MODELISATION 1D, AUCUN SENS
        ASSERT(.false.)
    endif

!   ON NE FAIT LA VERIFICATION QUE SI LA FISSURE EST DEFINIE GEOMETRIQUEMENT DONC PAS SI
!   LE CHAMP THETA EST RENTRE
!   PAR AILLEURS ON NE TESTE QUE LE CAS FEM, CE TEST ETANT REALISE DANS MODI_MODELE_XFEM
!   POUR XFEM
    if ((ixfem .eq. 0) .and. (typfis .ne. 'THETA')) then
        fonfis=nomfis//'.FONDFISS'
        call jeexin(fonfis, iret)

!       EN FEM AVEC UNE FISSURE NON DEFINIE PAR THETA, FONDFISS
!       EXISTE FORCEMENT
        ASSERT(iret .ne. 0)
        call jelira(fonfis,'LONMAX', lfonfi)

!       ON TESTE LE NOMBRE DE POINTS EN FOND DE FISSURE, SACHANT QUE LE VECTEUR
!       FONDFISS EST DE TAILLE 4*NOMBRE DE POINT FOND
        if (lfonfi/4 .le. 1) then
            idielm=2
        else
            idielm=3
        endif

        if (idielm .ne. idimom) then
            call utmess('F', 'RUPTURE0_40', ni=2, vali=[idimom, idielm])
        endif
    endif

end subroutine
