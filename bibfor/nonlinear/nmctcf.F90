subroutine nmctcf(noma  , modele, ds_print, sderro, defico,&
                  resoco, valinc, mmcvfr  )
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
    character(len=8) :: noma
    character(len=24) :: modele
    character(len=24) :: defico, resoco
    character(len=24) :: sderro
    type(NL_DS_Print), intent(inout) :: ds_print
    character(len=19) :: valinc(*)
    aster_logical :: mmcvfr
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGO - BOUCLE CONTACT)
!
! SEUIL DE FROTTEMENT
!
! ----------------------------------------------------------------------
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  MODELE : NOM DU MODELE
! IO  ds_print         : datastructure for printing parameters
! IN  SDERRO : GESTION DES ERREURS
! IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
! IN  RESOCO : SD POUR LA RESOLUTION DE CONTACT
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! OUT MMCVCA : INDICATEUR DE CONVERGENCE POUR BOUCLE DU
!              FROTTEMENT
!               .TRUE. SI LA BOUCLE A CONVERGE
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
    character(len=8) :: nomo
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
    nomo   = modele(1:8)
    mmcvfr = .false.
    deplam = resoco(1:14)//'.DEPF'
    lerrof = .false.
!
! --- DECOMPACTION DES VARIABLES CHAPEAUX
!
    call nmchex(valinc, 'VALINC', 'DEPMOI', depmoi)
    call nmchex(valinc, 'VALINC', 'DEPPLU', depplu)
!
! --- INFOS BOUCLE FROTTEMENT
!
    call mmbouc(resoco, 'FROT', 'READ', mmitfr)
    maxfro = cfdisi(defico,'ITER_FROT_MAXI')
    epsfro = cfdisr(defico,'RESI_FROT' )
!
! --- TYPE DE CONTACT
!
    lctcc = cfdisl(defico,'FORMUL_CONTINUE')
    lxfcm = cfdisl(defico,'FORMUL_XFEM')
    ltfcm = cfdisl(defico,'CONT_XFEM_GG')
!
! --- MISE A JOUR DES SEUILS
!
    if (lxfcm) then
        if (.not.ltfcm) then
            call xreacl(noma, nomo, valinc, resoco)
        endif
    else if (lctcc) then
        call mmreas(noma, defico, resoco, valinc)
    else
        ASSERT(.false.)
    endif
!
! --- CONVERGENCE SEUIL FROTTEMENT
!
    call mmmcri('FROT', noma, depmoi, deplam, depplu,&
                resoco, epsfro, cvgnoe, cvgval, mmcvfr)
!
    if ((.not.mmcvfr) .and. (mmitfr.eq.maxfro)) then
        lerrof = .true.
    endif
!
! --- CONVERGENCE ET ERREUR
!
    call nmcrel(sderro, 'ERRE_CTCF', lerrof)
    if (mmcvfr) then
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
    if (.not.mmcvfr) then
        call copisd('CHAMP_GD', 'V', depplu, deplam)
    endif
!
end subroutine
