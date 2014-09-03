subroutine tiinit(resulz, lreuse, instin, lisins, sddisc)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/ntcrar.h"
#include "asterfort/ntcrli.h"
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    real(kind=8) :: instin
    character(len=19) :: sddisc
    aster_logical :: lreuse
    character(len=19) :: lisins
    character(len=24) :: resulz
!
! --------------------------------------------------------------------------------------------------
!
! ROUTINE THER_* (STRUCTURES DE DONNES)
!
! CREATION SD DISCRETISATION ET ARCHIVAGE
!
! --------------------------------------------------------------------------------------------------
!
! IN  RESULT : NOM UTILISATEUR DU RESULTAT
! IN  INSTIN : INSTANT INITIAL QUAND ETAT_INIT
! IN  LISINS : list of time step
! IN  LREUSE : .TRUE. SI REUSE
! OUT SDDISC : SD DISCRETISATION
!
! --------------------------------------------------------------------------------------------------
!
    character(len=8) :: result
!
! --------------------------------------------------------------------------------------------------
!
    result = resulz(1:8)
!
! --- CREATION SD DISCRETISATION
!
    call ntcrli(instin, lisins, sddisc)
!
! --- CREATION SD ARCHIVAGE
!
    call ntcrar(result, sddisc, lreuse)
!
end subroutine
