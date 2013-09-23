subroutine nmctcc(noma, modele, mate, sddyna, sdimpr,&
                  sderro, defico, resoco, valinc, solalg,&
                  mmcvca, instan)
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
#include "asterfort/assert.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfdisl.h"
#include "asterfort/infdbg.h"
#include "asterfort/mmbouc.h"
#include "asterfort/mmcycl.h"
#include "asterfort/mmmbca.h"
#include "asterfort/nmcrel.h"
#include "asterfort/nmimck.h"
#include "asterfort/utmess.h"
#include "asterfort/xmmbca.h"
#include "asterfort/xmtbca.h"
    character(len=8) :: noma
    character(len=24) :: modele
    character(len=24) :: defico, resoco
    character(len=19) :: sddyna
    character(len=24) :: sdimpr, sderro
    character(len=19) :: valinc(*), solalg(*)
    logical :: mmcvca
    real(kind=8) :: instan
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGO - BOUCLE CONTACT)
!
! ALGO. DES CONTRAINTES ACTIVES
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  MODELE : NOM DU MODELE
! IN  MATE   : SD MATERIAU
! IN  SDDYNA : SD POUR DYNAMIQUE
! IN  SDIMPR : SD AFFICHAGE
! IN  SDERRO : GESTION DES ERREURS
! IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
! IN  RESOCO : SD POUR LA RESOLUTION DE CONTACT
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! OUT MMCVCA : INDICATEUR DE CONVERGENCE POUR BOUCLE DES
!              CONTRAINTES ACTIVES
!               .TRUE. SI LA BOUCLE DES CONTRAINTES ACTIVES A CONVERGE
!
! ----------------------------------------------------------------------
!
    integer :: ifm, niv
    logical :: ltfcm, lctcc, lxfcm, lfrot, lerroc
    integer :: ntpc, itemul, maxcon, ctcsta
    integer :: mmitca
    character(len=8) :: nomo
    integer :: iterat
    character(len=16) :: typcyc, liecyc
    character(len=24) :: mate
!
! ----------------------------------------------------------------------
!
    call infdbg('MECANONLINE', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ALGORITHME DES CONTRAINTES ACTIVES'
    endif
!
! --- INITIALISATIONS
!
    nomo = modele(1:8)
    ntpc = cfdisi(defico,'NTPC')
    typcyc = ' '
    mmcvca = .false.
    lerroc = .false.
    iterat = -1
!
! --- INFOS BOUCLE CONTACT
!
    call mmbouc(resoco, 'CONT', 'READ', mmitca)
    itemul = cfdisi(defico,'ITER_CONT_MULT')
    if (itemul .eq. -1) then
        maxcon = cfdisi(defico,'ITER_CONT_MAXI')
    else
        maxcon = itemul*ntpc
    endif
!
! --- TYPE DE CONTACT
!
    lctcc = cfdisl(defico,'FORMUL_CONTINUE')
    lxfcm = cfdisl(defico,'FORMUL_XFEM')
    lfrot = cfdisl(defico,'FROTTEMENT')
    ltfcm = cfdisl(defico,'CONT_XFEM_GG')
!
! --- APPEL ALGO DES CONT. ACTIVES
!
    if (lxfcm) then
        if (ltfcm) then
            call xmtbca(noma, defico, resoco, valinc, mmcvca)
        else
            call xmmbca(noma, nomo, mate, resoco, valinc,&
                        mmcvca)
        endif
    else if (lctcc) then
        call mmmbca(noma, sddyna, iterat, defico, resoco,&
                    valinc, solalg, ctcsta, mmcvca, instan)
        call mmcycl(defico, resoco, typcyc, liecyc)
! ----- FLIP-FLOP: ON FORCE LA CONVERGENCE
        if (typcyc .eq. 'FLIP FLOP') then
            mmcvca = .true.
            call nmimck(sdimpr, 'CTCC_CYCL', typcyc, .true.)
        endif
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
        call nmcrel(sderro, 'DIVE_FIXC', .false.)
    else
        call nmcrel(sderro, 'DIVE_FIXC', .true.)
    endif
!
!
end subroutine
