subroutine nmctcf(mesh     , model_, ds_print, sderro, ds_contact,&
                  hval_incr, loop_frot_conv)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfdisl.h"
#include "asterfort/cfdisr.h"
#include "asterfort/copisd.h"
#include "asterfort/infdbg.h"
#include "asterfort/mmbouc.h"
#include "asterfort/mmmcri.h"
#include "asterfort/mmreas.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmcrel.h"
#include "asterfort/nmimck.h"
#include "asterfort/nmimcr.h"
#include "asterfort/xreacl.h"
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
    character(len=24), intent(in) :: model_
    type(NL_DS_Print), intent(inout) :: ds_print
    character(len=24), intent(in) :: sderro
    type(NL_DS_Contact), intent(in) :: ds_contact
    character(len=19), intent(in) :: hval_incr(*)
    aster_logical, intent(out) :: loop_frot_conv
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGO - BOUCLE CONTACT)
!
! SEUIL DE FROTTEMENT
!
! ----------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  model            : name of model
! IO  ds_print         : datastructure for printing parameters
! In  sderro           : datastructure for errors during algorithm
! In  ds_contact       : datastructure for contact management
! In  hval_incr        : hat-variable for incremental values fields
! Out loop_frot_conv
!
! ----------------------------------------------------------------------
!
    integer :: ifm, niv
    aster_logical :: ltfcm, lctcc, lxfcm
    aster_logical :: lerrof
    integer :: maxfro
    real(kind=8) :: epsfro
    integer :: mmitfr
    character(len=19) :: depplu, deplam, depmoi
    character(len=8) :: model
    character(len=16) :: cvgnoe
    real(kind=8) :: cvgval
!
! ----------------------------------------------------------------------
!
    call infdbg('MECANONLINE', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> MISE A JOUR DU SEUIL DE TRESCA'
    endif
!
! --- INITIALISATIONS
!
    model  = model_(1:8)
    loop_frot_conv = .false.
    deplam = ds_contact%sdcont_solv(1:14)//'.DEPF'
    lerrof = .false.
!
! --- DECOMPACTION DES VARIABLES CHAPEAUX
!
    call nmchex(hval_incr, 'VALINC', 'DEPMOI', depmoi)
    call nmchex(hval_incr, 'VALINC', 'DEPPLU', depplu)
!
! --- INFOS BOUCLE FROTTEMENT
!
    call mmbouc(ds_contact, 'FROT', 'READ', mmitfr)
    maxfro = cfdisi(ds_contact%sdcont_defi,'ITER_FROT_MAXI')
    epsfro = cfdisr(ds_contact%sdcont_defi,'RESI_FROT' )
!
! --- TYPE DE CONTACT
!
    lctcc = cfdisl(ds_contact%sdcont_defi,'FORMUL_CONTINUE')
    lxfcm = cfdisl(ds_contact%sdcont_defi,'FORMUL_XFEM')
    ltfcm = cfdisl(ds_contact%sdcont_defi,'CONT_XFEM_GG')
!
! --- MISE A JOUR DES SEUILS
!
    if (lxfcm) then
        if (.not.ltfcm) then
            call xreacl(mesh, model, hval_incr, ds_contact)
        endif
    else if (lctcc) then
        call mmreas(mesh, ds_contact, hval_incr)
    else
        ASSERT(.false.)
    endif
!
! --- CONVERGENCE SEUIL FROTTEMENT
!
    call mmmcri('FROT', mesh, depmoi, deplam, depplu,&
                ds_contact, epsfro, cvgnoe, cvgval, loop_frot_conv)
!
    if ((.not.loop_frot_conv) .and. (mmitfr.eq.maxfro)) then
        lerrof = .true.
    endif
!
! --- CONVERGENCE ET ERREUR
!
    call nmcrel(sderro, 'ERRE_CTCF', lerrof)
    if (loop_frot_conv) then
        call nmcrel(sderro, 'DIVE_FIXF', .false._1)
    else
        call nmcrel(sderro, 'DIVE_FIXF', .true._1)
    endif
!
! - Set values in convergence table for contact geoemtry informations
!
    call nmimck(ds_print, 'BOUC_NOEU', cvgnoe, .true._1)
    call nmimcr(ds_print, 'BOUC_VALE', cvgval, .true._1)
!
! --- MISE A JOUR DU SEUIL DE REFERENCE
!
    if (.not.loop_frot_conv) then
        call copisd('CHAMP_GD', 'V', depplu, deplam)
    endif
!
end subroutine
