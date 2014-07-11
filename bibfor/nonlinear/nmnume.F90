subroutine nmnume(modele, result, lischa, lcont, defico,&
                  compor, solveu, numedd, sdnume)
implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/cfdisl.h"
#include "asterfort/nmprof.h"
#include "asterfort/nuendo.h"
#include "asterfort/nunuco.h"
#include "asterfort/nurota.h"
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
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=24) :: modele, numedd, compor, defico
    character(len=8) :: result
    character(len=19) :: lischa, solveu
    character(len=19) :: sdnume
    aster_logical :: lcont
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (INITIALISATION)
!
! CREATION DE LA NUMEROTATION
!
! ----------------------------------------------------------------------
!
!
! IN  RESULT : NOM UTILISATEUR DU RESULTAT DE MECA_NON_LINE
! IN  DEFICO : NOM DE LA SD DEFINITION DU CONTACT
! IN  LCONT  : IL Y A DU CONTACT
! IN  MODELE : MODELE MECANIQUE
! IN  LISCHA : LISTE DES CHARGES
! IN  SOLVEU : NOM DU SOLVEUR
! IN  NUMEDD : NOM DE LA NUMEROTATION
! IN  COMPOR : NOM DE LA CARTE COMPOR
! IN  SDNUME : NOM DE LA SD NUMEROTATION
!
!
!
!
    character(len=24) :: sdnuro, sdnuen, sdnuco
    aster_logical :: lctcc
!
! ----------------------------------------------------------------------
!
!
! --- CREATION PROFIL NUME_DDL
!
    call nmprof(modele, result, lischa, solveu, numedd)
!
! - Get position of large rotation dof
!
    sdnuro = sdnume(1:19)//'.NDRO'
    call nurota(modele, numedd, compor, sdnuro)
!
! - Get position of damaged dof 
!
    sdnuen = sdnume(1:19)//'.ENDO'
    call nuendo(modele, numedd, sdnuen)
!
! - Get position of contact dof 
!
    sdnuco = sdnume(1:19)//'.NUCO'
    if (lcont) then
        lctcc = cfdisl(defico,'FORMUL_CONTINUE')
        if (lctcc) then
            call nunuco(numedd, sdnuco)
        endif
    endif  
!
end subroutine
