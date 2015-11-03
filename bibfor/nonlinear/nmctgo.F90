subroutine nmctgo(mesh  , ds_print, sderro, ds_contact, hval_incr,&
                  mmcvgo)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfdisl.h"
#include "asterfort/cfdisr.h"
#include "asterfort/cfverl.h"
#include "asterfort/copisd.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mmbouc.h"
#include "asterfort/mmmcri.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmcrel.h"
#include "asterfort/nmimck.h"
#include "asterfort/nmimcr.h"
#include "asterfort/utmess.h"
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
    type(NL_DS_Print), intent(inout) :: ds_print
    character(len=19), intent(in) :: hval_incr(*)
    aster_logical, intent(out) :: mmcvgo
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGO - BOUCLE CONTACT)
!
! SEUIL DE GEOMETRIE
!
! ----------------------------------------------------------------------
!
! In  mesh             : name of mesh
! IO  ds_print         : datastructure for printing parameters
! IN  SDERRO : GESTION DES ERREURS
! IO  ds_contact       : datastructure for contact management
! In  hval_incr        : hat-variable for incremental values fields
! OUT MMCVCA : INDICATEUR DE CONVERGENCE POUR BOUCLE DE GEOMETRIE
!               .TRUE. SI LA BOUCLE A CONVERGE
!
! ----------------------------------------------------------------------
!
    integer :: ifm, niv
    aster_logical :: lctcc, lctcd, lxfcm
    aster_logical :: lsans, lmanu, lauto
    integer :: nbreag, maxgeo
    integer :: mmitgo
    character(len=19) :: depplu, depgeo, depmoi
    character(len=16) :: cvgnoe
    real(kind=8) :: cvgval, epsgeo
    character(len=24) :: clreac
    integer :: jclrea
    aster_logical :: ctcgeo, lerrog
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECANONLINE', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> MISE A JOUR DU SEUIL DE GEOMETRIE'
    endif
!
! --- INITIALISATIONS
!
    cvgnoe = ' '
    cvgval = r8vide()
    mmcvgo = .false.
    depgeo = ds_contact%sdcont_solv(1:14)//'.DEPG'
    lerrog = .false.
!
! --- DECOMPACTION DES VARIABLES CHAPEAUX
!
    call nmchex(hval_incr, 'VALINC', 'DEPMOI', depmoi)
    call nmchex(hval_incr, 'VALINC', 'DEPPLU', depplu)
!
! --- INFOS BOUCLE GEOMETRIQUE
!
    call mmbouc(ds_contact, 'Geom', 'READ', mmitgo)
    maxgeo = cfdisi(ds_contact%sdcont_defi,'ITER_GEOM_MAXI')
    nbreag = cfdisi(ds_contact%sdcont_defi,'NB_ITER_GEOM' )
    epsgeo = cfdisr(ds_contact%sdcont_defi,'RESI_GEOM' )
!
! --- TYPE DE CONTACT
!
    lctcc = cfdisl(ds_contact%sdcont_defi,'FORMUL_CONTINUE')
    lctcd = cfdisl(ds_contact%sdcont_defi,'FORMUL_DISCRETE')
    lxfcm = cfdisl(ds_contact%sdcont_defi,'FORMUL_XFEM')
!
    lmanu = cfdisl(ds_contact%sdcont_defi,'REAC_GEOM_MANU')
    lsans = cfdisl(ds_contact%sdcont_defi,'REAC_GEOM_SANS')
    lauto = cfdisl(ds_contact%sdcont_defi,'REAC_GEOM_AUTO')
!
! --- MISE A JOUR DES SEUILS
!
    if (lctcc .or. lxfcm) then
!
! ----- CALCUL DU CRITERE
!
        call mmmcri('GEOM', mesh, depmoi, depgeo, depplu,&
                    ds_contact, epsgeo, cvgnoe, cvgval, mmcvgo)
!
! ----- CAS MANUEL
!
        if (lmanu) then
            if (mmitgo .eq. nbreag) then
                if ((.not.mmcvgo) .and. (nbreag.gt.1)) then
                    call utmess('A', 'CONTACT3_96')
                endif
                mmcvgo = .true.
            else
                mmcvgo = .false.
            endif
        endif
!
! ----- CAS SANS
!
        if (lsans) then
            mmcvgo = .true.
        endif
!
! ----- CAS AUTO
!
        if (lauto) then
            if ((.not.mmcvgo) .and. (mmitgo.eq.maxgeo)) then
!           LA VERIFICATION DE LA FACETTISATION N'A PAS DE SENS EN X-FEM
                if (.not.lxfcm) then
                    call cfverl(ds_contact)
                endif
                lerrog = .true.
            endif
        endif
!
        if (.not.mmcvgo) then
            call copisd('CHAMP_GD', 'V', depplu, depgeo)
        endif
    else if (lctcd) then
!
        clreac = ds_contact%sdcont_solv(1:14)//'.REAL'
        call jeveuo(clreac, 'L', jclrea)
!
! ----- CTCGEO : TRUE. SI BOUCLE GEOMETRIQUE CONVERGEE
!
        ctcgeo = zl(jclrea+1-1)
        lerrog = zl(jclrea+4-1)
!
! ----- IMPRESSIONS
!
        if (ctcgeo) then
            mmcvgo = .false.
        else
            mmcvgo = .true.
        endif
    else
        ASSERT(.false.)
    endif
!
! --- SAUVEGARDE DES EVENEMENTS
!
    call nmcrel(sderro, 'ERRE_CTCG', lerrog)
    if (mmcvgo) then
        call nmcrel(sderro, 'DIVE_FIXG', .false._1)
    else
        call nmcrel(sderro, 'DIVE_FIXG', .true._1)
    endif
!
! - Set values in convergence table for contact geoemtry informations
!
    if (lctcc .or. lxfcm) then
        call nmimck(ds_print, 'BOUC_NOEU', cvgnoe, .true._1)
        call nmimcr(ds_print, 'BOUC_VALE', cvgval, .true._1)
    endif
!
    call jedema()
end subroutine
