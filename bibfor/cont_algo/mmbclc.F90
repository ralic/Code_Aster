subroutine mmbclc(mesh  , model , numedd  , iterat    , numins,&
                  sddisc, sddyna, ds_print, ds_contact, valinc,&
                  solalg, sdtime, sdstat  , mmcvca    , instan)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/cfdisl.h"
#include "asterfort/copisd.h"
#include "asterfort/mmappa.h"
#include "asterfort/mmchml.h"
#include "asterfort/mmligr.h"
#include "asterfort/mmmbca.h"
#include "asterfort/mmctcg.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmimci.h"
#include "asterfort/nmrinc.h"
#include "asterfort/nmtime.h"
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
    character(len=8), intent(in) :: mesh
    character(len=8), intent(in) :: model
    integer, intent(in) :: numins, iterat
    character(len=19), intent(in) :: sddisc, sddyna
    type(NL_DS_Contact), intent(inout) :: ds_contact
    character(len=24), intent(in) :: sdtime, sdstat, numedd
    type(NL_DS_Print), intent(inout) :: ds_print
    character(len=19), intent(in) :: valinc(*), solalg(*)
    aster_logical, intent(out) :: mmcvca
    real(kind=8) :: instan
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - ALGORITHME)
!
! CHANGEMENT DES STATUTS SI CONTACT CONTINU NEWTON GENERALISE
!
! ----------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  model            : name of model
! IN  ITERAT : NUMERO D'ITERATION DE NEWTON
! IN  NUMEDD : NUMEROTATION NUME_DDL
! IN  NUMINS : NUMERO D'INSTANT
! IN  SDDISC : SD DISCRETISATION
! IN  SDDYNA : SD DYNAMIQUE
! IO  ds_print         : datastructure for printing parameters
! IO  ds_contact       : datastructure for contact management
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! IN  SDTIME : SD TIMER
! IN  SDSTAT : SD STATISTIQUES
! OUT MMCVCA : INDICATEUR DE CONVERGENCE POUR BOUCLE DES
!              CONTRAINTES ACTIVES
!               .TRUE. SI LA BOUCLE DES CONTRAINTES ACTIVES A CONVERGE
!
! ----------------------------------------------------------------------
!
    aster_logical :: lallv, lnewtc, lnewtg
    integer :: ctcsta
    character(len=19) :: depgeo, depplu
!
! ----------------------------------------------------------------------
!
    lallv = cfdisl(ds_contact%sdcont_defi,'ALL_VERIF')
    lnewtc = cfdisl(ds_contact%sdcont_defi,'CONT_NEWTON')
    lnewtg = cfdisl(ds_contact%sdcont_defi,'GEOM_NEWTON')
    ctcsta = 0
    if (lallv) then
        mmcvca = .true.
        goto 999
    endif
!
! --- DECOMPACTION VARIABLES CHAPEAUX
!
    call nmchex(valinc, 'VALINC', 'DEPPLU', depplu)
    depgeo = ds_contact%sdcont_solv(1:14)//'.DEPG'
!
! --- NOUVEL APPARIEMENT
!
    if (lnewtg) then
        call copisd('CHAMP_GD', 'V', depplu, depgeo)
        call mmctcg(mesh, ds_contact, numedd, sdstat, sdtime)
    endif
!
! --- NOUVELLE NUMEROTATION (ELEMENTS TARDIFS DE CONTACT)
!
    if (lnewtg) then
        call nmtime(sdtime, 'INI', 'CTCC_PREP')
        call nmtime(sdtime, 'RUN', 'CTCC_PREP')
        call mmligr(mesh, model, ds_contact)
        call nmtime(sdtime, 'END', 'CTCC_PREP')
    endif
!
! --- CHANGEMENT DES STATUTS
!
    if (lnewtc .or. lnewtg) then
        call nmtime(sdtime, 'INI', 'CTCC_CONT')
        call nmtime(sdtime, 'RUN', 'CTCC_CONT')
        call mmmbca(mesh  , sddyna, iterat, ds_contact, sdstat,&
                    valinc, solalg, instan, ctcsta    , mmcvca)
        call nmtime(sdtime, 'END', 'CTCC_CONT')
        call nmrinc(sdstat, 'CTCC_CONT')
    endif
!
! --- MISE A JOUR DE LA CARTE
!
    if (lnewtc .or. lnewtg) then
        call nmtime(sdtime, 'INI', 'CTCC_PREP')
        call nmtime(sdtime, 'RUN', 'CTCC_PREP')
        call mmchml(mesh, ds_contact, sddisc, sddyna, numins)
        call nmtime(sdtime, 'END', 'CTCC_PREP')
        call nmrinc(sdstat, 'CTCC_PREP')
    endif
!
! - Contact status for generalized Newton
!
    if (lnewtc) then
        call nmimci(ds_print, 'CONT_NEWT', ctcsta, .true._1)
    endif
!
999 continue
!
end subroutine
