subroutine cgleco(resu, modele, mate, iord0, compor,&
                  incr)
implicit none
!
#include "asterf_types.h"
#include "asterc/getfac.h"
#include "asterfort/assert.h"
#include "asterfort/cgvein.h"
#include "asterfort/cgvtem.h"
#include "asterfort/comp_init.h"
#include "asterfort/comp_meca_elas.h"
#include "asterfort/dismoi.h"
#include "asterfort/gverlc.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/nmdocc.h"
#include "asterfort/rsexch.h"
#include "asterfort/utmess.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: samuel.geniaut at edf.fr
!
    integer, intent(in) :: iord0
    character(len=8), intent(in) :: resu
    character(len=8), intent(in) :: modele
    character(len=8), intent(in) :: mate
    character(len=19), intent(out) :: compor
    aster_logical, intent(out) :: incr
!
! --------------------------------------------------------------------------------------------------
!
! CALC_G
!
! Comportment selection
!
! --------------------------------------------------------------------------------------------------
!
! In  resu   : name of result
! In  model  : name of model
! In  mate   : name of material field
! In  iord0  : first NUME_ORDRE in result
! Out compor : name of COMPOR <CARTE>
! Out incr   : if incrental comportment
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nbcomp, iret, nb_cmp, nbetin
    character(len=16) :: keywordfact
    character(len=24) :: repk
    character(len=8) :: mesh
    aster_logical :: limpel, l_etat_init, l_temp, l_implex
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    l_implex = .false.
    limpel = .false.
    incr = .false.
    nbcomp = 0
    keywordfact = 'COMPORTEMENT'
    compor      = '&&CGLECO.COMPOR'
    call dismoi('NOM_MAILLA', modele, 'MODELE', repk=mesh)
    l_etat_init = .false.
    call getfac('ETAT_INIT', nbetin)
    if (nbetin .ne. 0) l_etat_init=.true.
!
! - How many COMPORTEMENT in CALC_G ?
!
    call getfac(keywordfact, nbcomp)
!
! - Get or create COMPOR <CARTE>
!
    if (nbcomp .eq. 0) then
!
! ----- No COMPORTEMENT: get from RESULT
!
        call rsexch(' ', resu, 'COMPORTEMENT', iord0, compor, iret)
!
! ----- No COMPOR <CARTE> in RESULT: create ELAS COMPOR <CARTE>
!
        if (iret .ne. 0) then
            limpel = .true.
            call comp_init(mesh, compor, 'V', nb_cmp)
            call comp_meca_elas(compor, nb_cmp, l_etat_init)
        endif
    else
!
! ----- Get COMPORTEMENT from command file
!
        call nmdocc(modele, mate, l_etat_init, l_implex, compor)
!
    endif
!
! - Incremental comportement or not ?
!
    if (limpel) then
        incr = .false.
    else
        call dismoi('ELAS_INCR', compor, 'CARTE_COMPOR', repk=repk)
        if (repk .eq. 'ELAS') then
            incr = .false.
        else if (repk.eq.'INCR'.or.repk.eq.'MIXTE') then
            incr = .true.
        else
            ASSERT(.false.)
        endif
    endif
!
!  -Si Comportement dans CALC_G(alors RELATION est renseigne) ---> emission d'un message d'alarme !
!    normalement le comportement est recupere dans Meca_stat ou stat_non_line.
!
    if (nbcomp .gt. 0) then
        call utmess('A', 'RUPTURE1_41')
        end if
!
! - Check is CALG_G COMPOR <CARTE> is coherent with result COMPOR <CARTE>
!
        call gverlc(resu, compor, iord0)
!
! - Check if TEMP is present or not in result cham_mater
!
        l_temp = cgvtem(resu, iord0)
!
! - Check COMPORTEMENT / RELATION in result for incremental comportement 
!
        if (incr) call cgvein(resu, compor, iord0, l_temp)
!
        call jedema()
!
    end subroutine
