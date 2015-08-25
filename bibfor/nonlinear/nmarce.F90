subroutine nmarce(sdieto, result   , sddisc, instan, numarc,&
                  force , ds_print_)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmeteo.h"
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
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=24) :: sdieto
    character(len=8) :: result
    character(len=19) :: sddisc
    integer :: numarc
    real(kind=8) :: instan
    aster_logical :: force
    type(NL_DS_Print), optional, intent(in) :: ds_print_
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - ARCHIVAGE )
!
! ARCHIVAGE DES CHAMPS
!
! ----------------------------------------------------------------------
!
!
! IN  SDIETO : SD GESTION IN ET OUT
! In  ds_print         : datastructure for printing parameters
! IN  RESULT : NOM UTILISATEUR DU CONCEPT RESULTAT
! IN  FORCE  : VRAI SI ON SOUHAITE FORCER L'ARCHIVAGE DE TOUS LES CHAMPS
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  INSTAN : INSTANT D'ARCHIVAGE
! IN  NUMARC : NUMERO D'ARCHIVAGE
!
! ----------------------------------------------------------------------
!
    character(len=24) :: sdieto_info
    integer, pointer :: v_sdieto_info(:) => null()
    integer :: nbcham
    integer :: icham
!
! ----------------------------------------------------------------------
!
    sdieto_info = sdieto(1:19)//'.INFO'
    call jeveuo(sdieto_info, 'L', vi = v_sdieto_info)
    nbcham = v_sdieto_info(1)
!
! - Loop on fields
!
    do icham = 1, nbcham
        if (present(ds_print_)) then
            call nmeteo(result, sddisc, sdieto   , force, numarc, &
                        instan, icham , ds_print_)
        else
            call nmeteo(result, sddisc, sdieto   , force, numarc, &
                        instan, icham)
        endif
    end do
!
end subroutine
