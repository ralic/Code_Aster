subroutine diinit(meshz , modelz, result, mate  , carele,&
                  fonact, sddyna, parcri, instin, solveu,&
                  defico, sddisc)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/getvid.h"
#include "asterfort/isfonc.h"
#include "asterfort/ndynlo.h"
#include "asterfort/nmcrar.h"
#include "asterfort/nmcrdd.h"
#include "asterfort/nmcrli.h"
#include "asterfort/nmcrsu.h"
#include "asterfort/pascom.h"
#include "asterfort/pascou.h"
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=*), intent(in) :: meshz
    character(len=*), intent(in) :: modelz
    real(kind=8) :: instin, parcri(*)
    character(len=8) :: result
    character(len=19) :: sddisc, sddyna, solveu
    character(len=24) :: carele, mate
    character(len=24) :: defico
    integer :: fonact(*)
!
! --------------------------------------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (STRUCTURES DE DONNES)
!
! CREATION SD DISCRETISATION ET ARCHIVAGE
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  model            : name of model
! IN  SDDYNA : NOM DE LA SD DEDIEE A LA DYNAMIQUE (CF NDLECT)
! IN  PARCRI : PARAMETRES DES CRITERES DE CONVERGENCE (CF NMDOCN)
! IN  RESULT : NOM UTILISATEUR DU RESULTAT
! IN  NOMA   : NOM DU MAILLAGE
! IN  NOMO   : NOM DU MODELE
! IN  FONACT : FONCTIONNALITES ACTIVEES (VOIR NMFONC)
! IN  INSTIN : INSTANT INITIAL QUAND ETAT_INIT
! IN  SOLVEU : SD SOLVEUR
! IN  DEFICO : SD DE DEFINITION DU CONTACT
! OUT SDDISC : SD DISCRETISATION
!
! --------------------------------------------------------------------------------------------------
!
    integer :: n1, nm
    aster_logical :: lexpl, lprmo
    aster_logical :: limpex, lctcd
    character(len=8) :: meca
    character(len=19) :: lisins
    character(len=8) :: model, mesh
!
! --------------------------------------------------------------------------------------------------
!
    call getvid('INCREMENT', 'LIST_INST', iocc=1, scal=lisins, nbret=n1)
    ASSERT(n1.ne.0)
    model = modelz
    mesh  = meshz
!
! --- FONCTIONNALITES ACTIVEES
!
    lexpl = ndynlo(sddyna,'EXPLICITE' )
    lprmo = ndynlo(sddyna,'PROJ_MODAL')
    limpex = isfonc(fonact,'IMPLEX')
    lctcd = isfonc(fonact,'CONT_DISCRET')
!
! --- CREATION SD DISCRETISATION
!
    call nmcrli(instin, lisins, sddisc)
!
! --- EVALUATION DE LA CONDITION DE COURANT EN EXPLICITE
!
    if (lexpl) then
        if (lprmo) then
            call getvid('PROJ_MODAL', 'MODE_MECA', iocc=1, scal=meca, nbret=nm)
            call pascom(meca, sddyna, sddisc)
        else
            call pascou(mate, carele, sddyna, sddisc)
        endif
    endif
!
! --- CREATION SD ARCHIVAGE
!
    call nmcrar(result, sddisc, fonact)
!
! --- SUBDIVISION AUTOMATIQUE DU PAS DE TEMPS
!
    call nmcrsu(sddisc, lisins, parcri, limpex, lctcd,&
                solveu, defico)
!
end subroutine
