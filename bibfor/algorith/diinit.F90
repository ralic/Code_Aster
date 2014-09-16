subroutine diinit(meshz , modelz, result, mate  , carele  ,&
                  fonact, sddyna, parcri, instin, sd_inout,&
                  solveu, defico, sddisc, sdobse, sdsuiv)
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
#include "asterfort/nmcrob.h"
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
    character(len=19) :: sddisc, sddyna, sdobse, solveu
    character(len=24) :: carele, mate
    character(len=24) :: sdsuiv, defico
    integer :: fonact(*)
    character(len=24), intent(in) :: sd_inout
!
! --------------------------------------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (STRUCTURES DE DONNES)
!
! CREATION SD DISCRETISATION, ARCHIVAGE ET OBSERVATION
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  model            : name of model
! IN  SDDYNA : NOM DE LA SD DEDIEE A LA DYNAMIQUE (CF NDLECT)
! IN  PARCRI : PARAMETRES DES CRITERES DE CONVERGENCE (CF NMDOCN)
! IN  RESULT : NOM UTILISATEUR DU RESULTAT
! In  sd_inout         : datastructure for input/output parameters
! IN  NOMA   : NOM DU MAILLAGE
! IN  NOMO   : NOM DU MODELE
! IN  FONACT : FONCTIONNALITES ACTIVEES (VOIR NMFONC)
! IN  INSTIN : INSTANT INITIAL QUAND ETAT_INIT
! IN  SOLVEU : SD SOLVEUR
! IN  DEFICO : SD DE DEFINITION DU CONTACT
! OUT SDDISC : SD DISCRETISATION
! OUT SDSUIV : NOM DE LA SD POUR SUIVI DDL
! OUT SDOBSE : NOM DE LA SD POUR OBSERVATION
!
! --------------------------------------------------------------------------------------------------
!
    integer :: n1, nm
    aster_logical :: lexpl, lprmo
    aster_logical :: limpex, lctcd
    character(len=8) :: meca
    character(len=19) :: lisins
    integer :: numreo
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
    call nmcrar(result, sddisc, fonact, numreo)
!
! --- SUBDIVISION AUTOMATIQUE DU PAS DE TEMPS
!
    call nmcrsu(sddisc, lisins, parcri, limpex, lctcd,&
                solveu, defico)
!
! --- CREATION SD OBSERVATION
!
    call nmcrob(mesh  , model, result, numreo, sd_inout,&
                sdobse)
!
! --- CREATION SD SUIVI_DDL
!
    call nmcrdd(mesh  , model, sd_inout, sdsuiv)
!
end subroutine
