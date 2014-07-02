subroutine nmctgo(noma, sdimpr, sderro, defico, resoco,&
                  valinc, mmcvgo)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
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
    character(len=8) :: noma
    character(len=24) :: defico, resoco
    character(len=24) :: sdimpr, sderro
    character(len=19) :: valinc(*)
    aster_logical :: mmcvgo
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGO - BOUCLE CONTACT)
!
! SEUIL DE GEOMETRIE
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  SDIMPR : SD AFFICHAGE
! IN  SDERRO : GESTION DES ERREURS
! IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
! IN  RESOCO : SD POUR LA RESOLUTION DE CONTACT
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! OUT MMCVCA : INDICATEUR DE CONVERGENCE POUR BOUCLE DE
!              GEOMETRIE
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
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> MISE A JOUR DU SEUIL DE GEOMETRIE'
    endif
!
! --- INITIALISATIONS
!
    cvgnoe = ' '
    cvgval = r8vide()
    mmcvgo = .false.
    depgeo = resoco(1:14)//'.DEPG'
    lerrog = .false.
!
! --- DECOMPACTION DES VARIABLES CHAPEAUX
!
    call nmchex(valinc, 'VALINC', 'DEPMOI', depmoi)
    call nmchex(valinc, 'VALINC', 'DEPPLU', depplu)
!
! --- INFOS BOUCLE GEOMETRIQUE
!
    call mmbouc(resoco, 'GEOM', 'READ', mmitgo)
    maxgeo = cfdisi(defico,'ITER_GEOM_MAXI')
    nbreag = cfdisi(defico,'NB_ITER_GEOM' )
    epsgeo = cfdisr(defico,'RESI_GEOM' )
!
! --- TYPE DE CONTACT
!
    lctcc = cfdisl(defico,'FORMUL_CONTINUE')
    lctcd = cfdisl(defico,'FORMUL_DISCRETE')
    lxfcm = cfdisl(defico,'FORMUL_XFEM')
!
    lmanu = cfdisl(defico,'REAC_GEOM_MANU')
    lsans = cfdisl(defico,'REAC_GEOM_SANS')
    lauto = cfdisl(defico,'REAC_GEOM_AUTO')
!
! --- MISE A JOUR DES SEUILS
!
    if (lctcc .or. lxfcm) then
!
! ----- CALCUL DU CRITERE
!
        call mmmcri('GEOM', noma, depmoi, depgeo, depplu,&
                    resoco, epsgeo, cvgnoe, cvgval, mmcvgo)
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
                    call cfverl(defico, resoco)
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
        clreac = resoco(1:14)//'.REAL'
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
! --- VALEUR ET ENDROIT OU SE REALISE L'EVALUATION DE LA BOUCLE
!
    if (lctcc .or. lxfcm) then
        call nmimck(sdimpr, 'BOUC_NOEU', cvgnoe, .true._1)
        call nmimcr(sdimpr, 'BOUC_VALE', cvgval, .true._1)
    endif
!
    call jedema()
end subroutine
