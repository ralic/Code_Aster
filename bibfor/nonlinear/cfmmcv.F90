subroutine cfmmcv(mesh    , modele, numedd    , fonact, sddyna,&
                  ds_print, sdstat, sddisc    , sdtime, sderro,&
                  numins  , iterat, ds_contact, valinc, solalg,&
                  instan)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/cfconv.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/isfonc.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mm_cycl_print.h"
#include "asterfort/mmbclc.h"
#include "asterfort/nmcrel.h"
#include "asterfort/nmimck.h"
#include "asterfort/nmimcr.h"
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
    integer :: fonact(*)
    integer :: iterat, numins
    character(len=19) :: sddisc, sddyna
    character(len=8) :: mesh
    character(len=24) :: numedd, modele
    type(NL_DS_Contact), intent(inout) :: ds_contact
    character(len=24) :: sderro, sdstat, sdtime
    character(len=19) :: solalg(*), valinc(*)
    real(kind=8) :: instan
    type(NL_DS_Print), intent(inout) :: ds_print
!
! --------------------------------------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! CRITERES DE CONVERGENCE POUR LE CONTACT
!
! --------------------------------------------------------------------------------------------------
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  MODELE : NOM DU MODELE
! IN  NUMEDD : NUMEROTATION NUME_DDL
! IN  FONACT : FONCTIONNALITES ACTIVEES (VOIR NMFONC)
! IN  SDDYNA : SD DYNAMIQUE
! IO  ds_print         : datastructure for printing parameters
! IN  SDSTAT : SD STATISTIQUES
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  SDTIME : SD TIMER
! IN  SDERRO : GESTION DES ERREURS
! IN  ITERAT : NUMERO D'ITERATION
! IN  NUMINS : NUMERO D'INSTANT
! IO  ds_contact       : datastructure for contact management
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
!
! --------------------------------------------------------------------------------------------------
!
    aster_logical :: lctcd=.false._1, lctcc=.false._1, lnewtc=.false._1
    aster_logical :: mmcvca=.false._1
    character(len=8) :: nomo=' '
    real(kind=8) :: r8bid=0.d0
    integer :: ntpc=0
!
! --------------------------------------------------------------------------------------------------
!
    mmcvca = .false.
    nomo = modele(1:8)
    ntpc = cfdisi(ds_contact%sdcont_defi,'NTPC' )
!
! --- FONCTIONNALITES ACTIVEES
!
    lctcd = isfonc(fonact,'CONT_DISCRET')
    lctcc = isfonc(fonact,'CONT_CONTINU')
    lnewtc = isfonc(fonact,'CONT_NEWTON')
!
! - Values in convergence table: not affected
!
    call nmimck(ds_print, 'BOUC_NOEU', ' '  , .false._1)
    call nmimcr(ds_print, 'BOUC_VALE', r8bid, .false._1)
!
! --- CONVERGENCE ADAPTEE AU CONTACT DISCRET
!
    if (lctcd) then
        call cfconv(mesh  , sdstat, ds_print, sderro, ds_contact,&
                    solalg)
    endif
!
! --- CONVERGENCE ADAPTEE AU CONTACT CONTINU
!
    if (lnewtc) then
        call mmbclc(mesh  , nomo  , numedd  , iterat    , numins,&
                    sddisc, sddyna, ds_print, ds_contact, valinc,&
                    solalg, sdtime, sdstat  , mmcvca    , instan)
        if (mmcvca) then
            call nmcrel(sderro, 'DIVE_CTCC', .false._1)
        else
            call nmcrel(sderro, 'DIVE_CTCC', .true._1)
        endif
    endif
!
! - Cycling informations printing in convergence table
!
    if (lctcc) then
        call mm_cycl_print(ds_print, sdstat)
    endif
!
end subroutine
