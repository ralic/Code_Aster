subroutine cfconv(mesh  , sdstat, ds_print, sderro, ds_contact,&
                  solalg)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8vide.h"
#include "asterfort/cfcgeo.h"
#include "asterfort/cfdisl.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmcrel.h"
#include "asterfort/nmimci.h"
#include "asterfort/nmimck.h"
#include "asterfort/nmimcr.h"
#include "asterfort/nmlecv.h"
#include "asterfort/nmrvai.h"
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
    type(NL_DS_Contact), intent(inout) :: ds_contact
    character(len=24), intent(in) :: sderro
    character(len=24), intent(in) :: sdstat
    character(len=19), intent(in) :: solalg(*)
    type(NL_DS_Print), intent(inout) :: ds_print
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES)
!
! CONVERGENCE LIEE AU CONTACT DISCRET
!
! ----------------------------------------------------------------------
!
! IN  NOMA   : NOM DU MAILLAGE
! IO  ds_print         : datastructure for printing parameters
! IN  SDSTAT : SD STATISTIQUES
! IN  SDERRO : GESTION DES ERREURS
! IO  ds_contact       : datastructure for contact management
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
!
! ----------------------------------------------------------------------
!
    character(len=24) :: clreac=' '
    character(len=16) :: geonoe=' ', k16bla=' '
    integer :: jclrea, ctcite
    real(kind=8) :: geoval
    aster_logical :: ctderg, cvresi
    aster_logical :: dvpfix, dvfixg
    aster_logical :: lallv, lreag
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    dvpfix = .false.
    dvfixg = .false.
    ctderg = .false.
    lreag = .false.
    k16bla = ' '
    geonoe = ' '
    geoval = r8vide()
    call nmrvai(sdstat, 'CTCD_ALGO_ITER', 'N', ctcite)
!
! - Values in convergence table: not affected
!
    call nmimck(ds_print, 'BOUC_NOEU', ' ' , .false._1)
    call nmimcr(ds_print, 'BOUC_VALE', 0.d0, .false._1)
!
! --- CONVERGENCE DES RESIDUS D'EQUILIBRE ?
!
    call nmlecv(sderro, 'RESI', cvresi)
!
! --- ACCES SD CONTACT
!
    clreac = ds_contact%sdcont_solv(1:14)//'.REAL'
!
! --- FONCTIONNALISTES ACTIVEES
!
    lallv = cfdisl(ds_contact%sdcont_defi,'ALL_VERIF')
!
! --- CONVERGENCE
!
    if (.not.lallv) then
!
        call jeveuo(clreac, 'E', jclrea)
!
! ----- ATTENTE POINT FIXE ?
!
        dvpfix = zl(jclrea+2-1)
        lreag = .false.
!
! ----- EVALUATION DE LA REACUALISATION GEOMETRIQUE POUR CONTACT DISCRET
!
        if (cvresi) then
            if (.not.dvpfix) then
                call cfcgeo(mesh  , ds_contact, solalg, dvfixg,&
                            ctderg, geonoe, geoval)
                zl(jclrea+1-1) = dvfixg
                zl(jclrea+4-1) = ctderg
                lreag = .true.
            endif
        endif
    endif
!
! - Set values in convergence table for contact geoemtry informations
!
    if (lreag) then
        call nmimck(ds_print, 'BOUC_NOEU', geonoe, .true._1)
        call nmimcr(ds_print, 'BOUC_VALE', geoval, .true._1)
    endif
    call nmimci(ds_print, 'CTCD_NBIT', ctcite, .true._1)
!
! --- ENREGISTREMENT DES EVENEMENTS - DIVERGENCES
!
    call nmcrel(sderro, 'DIVE_PFIX', dvpfix)
!
    call jedema()
end subroutine
