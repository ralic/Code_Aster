subroutine nmctcc(mesh  , modele    , mate  , sddyna, sderro,&
                  sdstat, ds_contact, valinc, solalg, mmcvca,&
                  instan)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfdisl.h"
#include "asterfort/infdbg.h"
#include "asterfort/mmbouc.h"
#include "asterfort/mm_cycl_flip.h"
#include "asterfort/mmmbca.h"
#include "asterfort/nmcrel.h"
#include "asterfort/nmimck.h"
#include "asterfort/utmess.h"
#include "asterfort/xmmbca.h"
#include "asterfort/xmtbca.h"
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
    character(len=24), intent(in) :: modele
    character(len=24), intent(in) :: mate
    type(NL_DS_Contact), intent(inout) :: ds_contact
    character(len=19), intent(in) :: sddyna
    character(len=24), intent(in) :: sderro
    character(len=24), intent(in) :: sdstat
    character(len=19), intent(in) :: valinc(*)
    character(len=19), intent(in) :: solalg(*)
    real(kind=8), intent(in) :: instan
    aster_logical, intent(out) :: mmcvca
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGO - BOUCLE CONTACT)
!
! ALGO. DES CONTRAINTES ACTIVES
!
! ----------------------------------------------------------------------
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  MODELE : NOM DU MODELE
! IN  MATE   : SD MATERIAU
! IN  SDDYNA : SD POUR DYNAMIQUE
! IN  SDERRO : GESTION DES ERREURS
! IO  ds_contact       : datastructure for contact management
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! OUT MMCVCA : INDICATEUR DE CONVERGENCE POUR BOUCLE DES
!              CONTRAINTES ACTIVES
!               .TRUE. SI LA BOUCLE DES CONTRAINTES ACTIVES A CONVERGE
!
! ----------------------------------------------------------------------
!
    integer :: ifm, niv
    aster_logical :: ltfcm, lctcc, lxfcm, lfrot, lerroc
    integer :: ntpc, itemul, maxcon, ctcsta
    integer :: mmitca
    character(len=8) :: nomo
    integer :: iterat
    aster_logical :: cycl_flip
!
! ----------------------------------------------------------------------
!
    call infdbg('MECANONLINE', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ALGORITHME DES CONTRAINTES ACTIVES'
    endif
!
! --- INITIALISATIONS
!
    nomo = modele(1:8)
    ntpc = cfdisi(ds_contact%sdcont_defi,'NTPC')
    mmcvca = .false.
    lerroc = .false.
    iterat = -1
!
! --- INFOS BOUCLE CONTACT
!
    call mmbouc(ds_contact, 'Cont', 'READ', mmitca)
    itemul = cfdisi(ds_contact%sdcont_defi,'ITER_CONT_MULT')
    if (itemul .eq. -1) then
        maxcon = cfdisi(ds_contact%sdcont_defi,'ITER_CONT_MAXI')
    else
        maxcon = itemul*ntpc
    endif
!
! --- TYPE DE CONTACT
!
    lctcc = cfdisl(ds_contact%sdcont_defi,'FORMUL_CONTINUE')
    lxfcm = cfdisl(ds_contact%sdcont_defi,'FORMUL_XFEM')
    lfrot = cfdisl(ds_contact%sdcont_defi,'FROTTEMENT')
    ltfcm = cfdisl(ds_contact%sdcont_defi,'CONT_XFEM_GG')
!
! --- APPEL ALGO DES CONT. ACTIVES
!
    if (lxfcm) then
        if (ltfcm) then
            call xmtbca(mesh, ds_contact, valinc, mmcvca)
        else
            call xmmbca(mesh, nomo, mate, ds_contact, valinc,&
                        mmcvca)
        endif
    else if (lctcc) then
        call mmmbca(mesh  , sddyna, iterat, ds_contact, sdstat,&
                    valinc, solalg, instan, ctcsta    , mmcvca)
        call mm_cycl_flip(ds_contact, cycl_flip)
! ----- FLIP-FLOP: ON FORCE LA CONVERGENCE
        if (cycl_flip) mmcvca = .true.
    else
        ASSERT(.false.)
    endif
!
! --- CONVERGENCE CONTRAINTES ACTIVES
!
    if ((.not.mmcvca) .and. (mmitca.eq.maxcon)) then
        if (lfrot .and. lxfcm) then
! ------- CONVERGENCE FORCEE
            call utmess('A', 'CONTACT3_86')
            mmcvca = .true.
        else
            lerroc = .true.
            mmcvca = .false.
        endif
    endif
!
! --- CONVERGENCE ET ERREUR
!
    call nmcrel(sderro, 'ERRE_CTCC', lerroc)
    if (mmcvca) then
        call nmcrel(sderro, 'DIVE_FIXC', .false._1)
    else
        call nmcrel(sderro, 'DIVE_FIXC', .true._1)
    endif
!
end subroutine
