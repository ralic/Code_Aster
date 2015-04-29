subroutine nmfonc(crit_para  , algo_para     , algo_meth, solver , model ,&
                  sdcont_defi, list_load     , l_cont   , l_unil , sdnume,&
                  sddyna     , sdcriq        , mate     , compor_, result,&
                  comp_para  , list_func_acti)
!
implicit none
!
#include "asterf_types.h"
#include "asterc/gcucon.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfdisl.h"
#include "asterfort/dismoi.h"
#include "asterfort/exfonc.h"
#include "asterfort/exixfe.h"
#include "asterfort/getvtx.h"
#include "asterfort/infniv.h"
#include "asterfort/ischar.h"
#include "asterfort/isdiri.h"
#include "asterfort/isfonc.h"
#include "asterfort/jeexin.h"
#include "asterfort/jeveuo.h"
#include "asterfort/ndynlo.h"
#include "asterfort/nmcpqu.h"
#include "asterfort/nmlssv.h"
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
    real(kind=8), intent(in) :: crit_para(*)
    real(kind=8), intent(in) :: algo_para(*)
    character(len=16), intent(in) :: algo_meth(*)
    character(len=19), intent(in) :: solver
    character(len=24), intent(in) :: model
    character(len=24), intent(in) :: sdcont_defi
    character(len=19), intent(in) :: list_load
    aster_logical, intent(in) :: l_cont
    aster_logical, intent(in) :: l_unil
    character(len=19), intent(in) :: sdnume
    character(len=19), intent(in) :: sddyna
    character(len=24), intent(in) :: sdcriq
    character(len=24), intent(in) :: mate
    character(len=*), intent(in) :: compor_
    character(len=8), intent(in) :: result
    character(len=24), intent(in) :: comp_para
    integer, intent(inout) :: list_func_acti(*)
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Init
!
! Prepare active functionnalities information
!
! --------------------------------------------------------------------------------------------------
!
! NB: to ask list_func_acti, use ISFONC.F90 subroutine !
!
! In  crit_para        : parameters for convergence criteria
! In  algo_para        : parameters for algorithm criteria
! In  algo_meth        : parameters for algorithm methods
! In  solver           : datastructure for solver parameters 
! In  model            : name of the model
! In  sdcont_defi      : name of contact definition datastructure (from DEFI_CONTACT)
! In  list_load        : name of datastructure for list of loads
! In  l_cont           : .true. if contact
! In  l_unil           : .true. if unilateral condition
! In  sdnume           : datastructure for dof positions
! In  sddyna           : dynamic parameters datastructure
! In  sdcriq           : datastructure for quality indicators
! In  mate             : name of material characteristics (field)
! In  compor           : name of comportment definition (field)
! In  result           : name of results datastructure
! In  comp_para        : parameters for comportment
! IO  list_func_acti   : list of active functionnalities
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nocc, iret, nb_subs_stat, nb_load_subs
    integer :: i_cont_form
    aster_logical :: l_deborst, l_frot, l_elem_choc, l_all_verif
    aster_logical :: l_loop_geom, l_loop_frot, l_loop_cont
    integer :: ixfem, ichar, i_buckl, i_vibr_mode, i_stab
    aster_logical :: l_load_undead, l_load_laplace, l_load_elim, l_load_didi
    character(len=8) :: k8bid, repk
    character(len=16) :: command, k16bid, matdis
    character(len=19) :: compor
    character(len=24) :: solv_type, solv_precond, sdcriq_errt
    aster_logical :: l_stat, l_dyna, l_stop_no
    aster_logical :: l_newt_cont, l_newt_frot, l_newt_geom
    aster_logical :: l_dyna_expl
    integer :: ifm, niv
    integer :: nb_dof_stab
    character(len=24), pointer :: slvk(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    compor = compor_
!
! - Print
!    
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... CREATION VECTEUR FONCTIONNALITES ACTIVEES: '
    endif
!
! - Command
!
    call getres(k8bid, k16bid, command)
    l_stat      = command(1:4).eq.'STAT'
    l_dyna      = command(1:4).eq.'DYNA'
    l_dyna_expl = ndynlo(sddyna,'EXPLICITE')

!
! - Large rotations
!
    call jeexin(sdnume(1:19)//'.NDRO', iret)
    if (iret.gt.0) list_func_acti(15) = 1
!
! - Damaged nodes
!
    call jeexin(sdnume(1:19)//'.ENDO', iret)
    if (iret.gt.0) list_func_acti(40) = 1
!
! - Line search
!
    call getfac('RECH_LINEAIRE', nocc)
    if (nocc.gt.0) list_func_acti(1) = 1
!
! - Continuation methods (PILOTAGE)
!
    if (l_stat) then
        call getfac('PILOTAGE', nocc)
        if (nocc .ne. 0) list_func_acti(2) = 1
    endif
!
! - Unilateral condition
!
    if (l_unil) list_func_acti(12) = 1
!
! - Energy computation
!
    call getfac('ENERGIE', nocc)
    if (nocc .ne. 0) list_func_acti(50) = 1
!
! - Modal projection for dynamic
!
    if (l_dyna) then
        call getfac('PROJ_MODAL', nocc)
        if (nocc .ne. 0) list_func_acti(51) = 1
    endif
!
! - Distributed matrix (parallel computaing)
!
    matdis='NON'
    call getvtx('SOLVEUR', 'MATR_DISTRIBUEE', iocc=1, scal=matdis)
    if (matdis .eq. 'OUI') list_func_acti(52) = 1
!
! - Deborst algorithm
!
    call nmcpqu(compor, 'C_PLAN', 'DEBORST', l_deborst)
    if (l_deborst) list_func_acti(7) = 1
!
! - Reference criterion
!
    if (crit_para(6) .ne. r8vide()) list_func_acti(8) = 1
!
! - By components criterion
!
    if (crit_para(7) .ne. r8vide()) list_func_acti(35) = 1
!
! - X-FEM
!
    call exixfe(model, ixfem)
    if (ixfem .ne. 0) list_func_acti(6) = 1
!
! - Contact_friction
!
    if (l_cont) then
        i_cont_form = cfdisi(sdcont_defi,'FORMULATION')
        if (i_cont_form .eq. 2) then
            list_func_acti(5)  = 1
            list_func_acti(17) = cfdisi(sdcont_defi,'ALL_INTERPENETRE')
            list_func_acti(26) = 1
            l_frot = cfdisl(sdcont_defi,'FROTTEMENT')
            if (l_frot) then
                list_func_acti(10) = 1
                list_func_acti(27) = 1
            endif
        else if (i_cont_form.eq.3) then
            list_func_acti(9) = 1
            list_func_acti(26) = 1
            l_frot = cfdisl(sdcont_defi,'FROTTEMENT')
            if (l_frot) then
                list_func_acti(25) = 1
                list_func_acti(27) = 1
            endif
            list_func_acti(27) = 1
        else if (i_cont_form.eq.1) then
            list_func_acti(4) = 1
            l_frot = cfdisl(sdcont_defi,'FROTTEMENT')
            if (l_frot) then
                list_func_acti(3) = 1
            endif
        else
            ASSERT(.false.)
        endif
    endif
!
! - Contact: no computation
!
    if (l_cont) then
        l_all_verif = cfdisl(sdcont_defi,'ALL_VERIF')
        if (l_all_verif) list_func_acti(38) = 1
    endif
!
! - Contact: fixed loops
!
    if (l_cont) then
        l_loop_geom = .false.
        l_loop_frot = .false.
        l_loop_cont = .false.
        l_loop_geom = cfdisl(sdcont_defi,'GEOM_BOUCLE')
        if (l_frot) l_loop_frot = cfdisl(sdcont_defi,'FROT_BOUCLE')
        l_loop_cont = cfdisl(sdcont_defi,'CONT_BOUCLE')
        if (l_all_verif) then
            l_loop_cont = .false.
            l_loop_geom = .false.
            l_loop_frot = .false.
        endif
        if (i_cont_form .eq. 1) then
            l_loop_cont = .false.
            l_loop_frot = .false.
        endif
        if (l_loop_geom) list_func_acti(31) = 1
        if (l_loop_frot) list_func_acti(32) = 1
        if (l_loop_cont) list_func_acti(33) = 1
        if (l_loop_geom .or. l_loop_frot .or. l_loop_cont) list_func_acti(34) = 1
    endif
!
! - Generalized Newton
!
    if (l_cont) then
        if (i_cont_form .eq. 2) then
            l_newt_geom = cfdisl(sdcont_defi,'GEOM_NEWTON')
            l_newt_frot = cfdisl(sdcont_defi,'FROT_NEWTON')
            l_newt_cont = cfdisl(sdcont_defi,'CONT_NEWTON')
            if (l_newt_frot) list_func_acti(47) = 1
            if (l_newt_cont) list_func_acti(53) = 1
            if (l_newt_geom) list_func_acti(55) = 1
        endif
    endif
!
! - At least, one undead load ?
!
    ichar = 0
    l_load_undead = ischar(list_load, 'NEUM', 'SUIV', ichar)
    if (l_load_undead) list_func_acti(13) = 1
!
! - At least, one "DIDI" load ?
!
    ichar = 0
    l_load_didi = ischar(list_load,'DIRI','DIDI', ichar)
    if (l_load_didi) list_func_acti(22) = 1
!
! - At least, one AFFE_CHAR_CINE load ?
!
    l_load_elim = isdiri(list_load,'ELIM')
    if (l_load_elim) list_func_acti(36) = 1
!
! - At least, one Laplace load ?
!
    l_load_laplace = ischar(list_load,'NEUM','LAPL',ichar )
    if (l_load_laplace) list_func_acti(20) = 1
!
! - Static substructuring-
!
    call dismoi('NB_SS_ACTI', model, 'MODELE', repi=nb_subs_stat)
    if (nb_subs_stat .gt. 0) list_func_acti(14) = 1
!
! - Substructuring loads
!
    call nmlssv('LECT', list_load, nb_load_subs)
    if (nb_load_subs .gt. 0) list_func_acti(24) = 1
!
! - Buckling
!
    call getfac('CRIT_STAB', i_buckl)
    if (i_buckl .gt. 0) list_func_acti(18) = 1
!
! - Stability
!
    i_stab = 0
    call getvtx('CRIT_STAB', 'DDL_STAB', iocc=1, nbval=0, nbret=nb_dof_stab)
    i_stab = -nb_dof_stab
    if (i_stab .gt. 0) list_func_acti(49) = 1
!
! - Vibration modes
!
    if (l_dyna) then
        call getfac('MODE_VIBR', i_vibr_mode)
        if (i_vibr_mode .gt. 0) list_func_acti(19) = 1
    endif
!
! - THM time error
!
    if (l_stat) then
        sdcriq_errt = sdcriq(1:19)//'.ERRT'
        call jeexin(sdcriq_errt, iret)
        if (iret .ne. 0) list_func_acti(21) = 1
    endif
!
! - IMPLEX algorithm
!
    if (l_stat) then
        if (algo_meth(1) .eq. 'IMPLEX') list_func_acti(28) = 1
    endif
!
! - NEWTON_KRYLOV algorithm
!
    if (algo_meth(1) .eq. 'NEWTON_KRYLOV') list_func_acti(48) = 1
!
! - DIS_CHOC elements ?
!
    call nmcpqu(compor, 'RELCOM', 'DIS_CHOC', l_elem_choc)
    if (l_elem_choc) list_func_acti(29) = 1
!
! - Command variables
!
    call dismoi('EXI_VARC', mate, 'CHAM_MATER', repk=repk)
    if (repk .eq. 'OUI') list_func_acti(30) = 1
!
! - THM ?
!
    call dismoi('EXI_THM', model, 'MODELE', repk=repk)
    if (repk .eq. 'OUI') list_func_acti(37) = 1
!
! - Elemesnt with STRX field (multifibers for instantce)
!
    call dismoi('EXI_STRX', model, 'MODELE', repk=repk)
    if (repk .eq. 'OUI') list_func_acti(56) = 1
!
! - REUSE ?
!
    call gcucon(result, 'EVOL_NOLI', iret)
    if (iret .gt. 0) list_func_acti(39) = 1
!
! - Does ETAT_INIT (initial state) exist ?
!
    call getfac('ETAT_INIT', nocc)
    if (nocc.gt.0) list_func_acti(59) = 1
!
! - Solvers
!
    call jeveuo(solver//'.SLVK', 'L', vk24=slvk)
    solv_type=slvk(1)
    if (solv_type .eq. 'LDLT') list_func_acti(41) = 1
    if (solv_type .eq. 'MULT_FRONT') list_func_acti(42) = 1
    if (solv_type .eq. 'GCPC') list_func_acti(43) = 1
    if (solv_type .eq. 'MUMPS') list_func_acti(44) = 1
    if (solv_type .eq. 'PETSC') list_func_acti(45) = 1
    if (solv_type .eq. 'PETSC' .or. solv_type .eq. 'GCPC') then
        solv_precond=slvk(2)
        if (solv_precond .eq. 'LDLT_SP') list_func_acti(46) = 1
    endif
!
! - ARRET=NON
!
    l_stop_no = (nint(crit_para(4)).eq.1)
    if (l_stop_no) then
        call utmess('A', 'MECANONLINE5_37')
    endif
!
! - Explicit dynamics
!
    if (l_dyna_expl) list_func_acti(54) = 1
!
! - Do elastic properties are functions ?
!
    call dismoi('ELAS_FO', mate, 'CHAM_MATER', repk=repk)
    if (repk .eq. 'OUI') list_func_acti(57) = 1
!
! - Post-treatment on comportment laws ?
!
    call dismoi('POST_INCR', comp_para, 'CARTE_CARCRI', repk=repk)
    if (repk .eq. 'OUI') list_func_acti(58) = 1
!
! - Print
!
    if (niv .ge. 2) then
!
! ----- Solving methods
!
        if (isfonc(list_func_acti,'IMPLEX')) then
            write (ifm,*) '<MECANONLINE> ...... METHODE IMPLEX'  
        endif
        if (isfonc(list_func_acti,'EXPLICITE')) then
            write (ifm,*) '<MECANONLINE> ...... METHODE EXPLICITE'
        endif
        if (isfonc(list_func_acti,'NEWTON_KRYLOV')) then
            write (ifm,*) '<MECANONLINE> ...... METHODE NEWTON_KRYLOV'
        endif
        if (isfonc(list_func_acti,'RECH_LINE')) then
            write (ifm,*) '<MECANONLINE> ...... RECHERCHE LINEAIRE'  
        endif
        if (isfonc(list_func_acti,'PILOTAGE')) then
            write (ifm,*) '<MECANONLINE> ...... PILOTAGE'  
        endif
        if (isfonc(list_func_acti,'DEBORST')) then
            write (ifm,*) '<MECANONLINE> ...... METHODE DEBORST' 
        endif
        if (isfonc(list_func_acti,'SOUS_STRUC')) then
            write (ifm,*) '<MECANONLINE> ...... CALCUL PAR SOUS-STRUCTURATION'
        endif
        if (isfonc(list_func_acti,'PROJ_MODAL')) then
            write (ifm,*) '<MECANONLINE> ...... CALCUL PAR PROJECTION MODALE'    
        endif
!
! ----- Contact
!
        if (isfonc(list_func_acti,'CONTACT')) then
            write (ifm,*) '<MECANONLINE> ...... CONTACT' 
        endif
        if (isfonc(list_func_acti,'CONT_DISCRET')) then
            write (ifm,*) '<MECANONLINE> ...... CONTACT DISCRET'  
        endif
        if (isfonc(list_func_acti,'CONT_CONTINU')) then
            write (ifm,*) '<MECANONLINE> ...... CONTACT CONTINU' 
        endif
        if (isfonc(list_func_acti,'CONT_XFEM')) then
            write (ifm,*) '<MECANONLINE> ...... CONTACT XFEM'
        endif
        if (isfonc(list_func_acti,'BOUCLE_EXT_GEOM')) then
            write (ifm,*) '<MECANONLINE> ...... CONTACT BOUCLE GEOM'  
        endif
        if (isfonc(list_func_acti,'BOUCLE_EXT_CONT')) then
            write (ifm,*) '<MECANONLINE> ...... CONTACT BOUCLE CONTACT'
        endif
        if (isfonc(list_func_acti,'BOUCLE_EXT_FROT')) then
            write (ifm,*) '<MECANONLINE> ...... CONTACT BOUCLE FROT' 
        endif
        if (isfonc(list_func_acti,'BOUCLE_EXTERNE')) then
            write (ifm,*) '<MECANONLINE> ...... BOUCLE EXTERNE'
        endif
        if (isfonc(list_func_acti,'GEOM_NEWTON')) then
            write (ifm,*) '<MECANONLINE> ...... GEOMETRIE AVEC NEWTON GENERALISE'
        endif
        if (isfonc(list_func_acti,'FROT_NEWTON')) then
            write (ifm,*) '<MECANONLINE> ...... FROTTEMENT AVEC NEWTON GENERALISE'
        endif
        if (isfonc(list_func_acti,'CONT_NEWTON')) then
            write (ifm,*) '<MECANONLINE> ...... CONTACT AVEC NEWTON GENERALISE'
        endif
        if (isfonc(list_func_acti,'CONT_ALL_VERIF')) then
            write (ifm,*) '<MECANONLINE> ...... CONTACT SANS CALCUL SUR TOUTES LES ZONES'
        endif
        if (isfonc(list_func_acti,'CONTACT_INIT')) then
            write (ifm,*) '<MECANONLINE> ...... CONTACT INITIAL'   
        endif
        if (isfonc(list_func_acti,'LIAISON_UNILATER')) then
            write (ifm,*) '<MECANONLINE> ...... LIAISON UNILATERALE'
        endif
        if (isfonc(list_func_acti,'FROT_DISCRET')) then
            write (ifm,*) '<MECANONLINE> ...... FROTTEMENT DISCRET'
        endif
        if (isfonc(list_func_acti,'FROT_CONTINU')) then
            write (ifm,*) '<MECANONLINE> ...... FROTTEMENT CONTINU'
        endif
        if (isfonc(list_func_acti,'FROT_XFEM')) then
            write (ifm,*) '<MECANONLINE> ...... FROTTEMENT XFEM'  
        endif
!
! ----- Finite elements
!
        if (isfonc(list_func_acti,'ELT_CONTACT')) then
            write (ifm,*) '<MECANONLINE> ...... ELEMENTS DE CONTACT'    
        endif
        if (isfonc(list_func_acti,'ELT_FROTTEMENT')) then
            write (ifm,*) '<MECANONLINE> ...... ELEMENTS DE FROTTEMENT'
        endif
        if (isfonc(list_func_acti,'DIS_CHOC')) then
            write (ifm,*) '<MECANONLINE> ...... ELEMENTS DIS_CHOC '   
        endif
        if (isfonc(list_func_acti,'GD_ROTA')) then
            write (ifm,*) '<MECANONLINE> ...... ELEMENTS DE STRUCTURES EN GRANDES ROTATIONS'
        endif
        if (isfonc(list_func_acti,'XFEM')) then
            write (ifm,*) '<MECANONLINE> ...... ELEMENTS XFEM' 
        endif
        if (isfonc(list_func_acti,'EXI_STRX')) then
            write (ifm,*) '<MECANONLINE> ...... ELEMENTS DE STRUCTURES DE TYPE PMF'
        endif
!
! ----- CONVERGENCE
!
        if (isfonc(list_func_acti,'RESI_REFE')) then
            write (ifm,*) '<MECANONLINE> ...... CONVERGENCE PAR RESI_REFE' 
        endif
        if (isfonc(list_func_acti,'RESI_COMP')) then
            write (ifm,*) '<MECANONLINE> ...... CONVERGENCE PAR RESI_COMP'   
        endif
!
! ----- Loads
!
        if (isfonc(list_func_acti,'FORCE_SUIVEUSE')) then
            write (ifm,*) '<MECANONLINE> ...... CHARGEMENTS SUIVEURS'   
        endif
        if (isfonc(list_func_acti,'DIDI')) then
            write (ifm,*) '<MECANONLINE> ...... CHARGEMENTS DE DIRICHLET DIFFERENTIEL' 
        endif
        if (isfonc(list_func_acti,'DIRI_CINE')) then
            write (ifm,*) '<MECANONLINE> ...... CHARGEMENTS CINEMATIQUES PAR ELIMINATION'
        endif
        if (isfonc(list_func_acti,'LAPLACE')) then
            write (ifm,*) '<MECANONLINE> ...... CHARGEMENTS DE LAPLACE'
        endif
!
! ----- MODELISATION
!
        if (isfonc(list_func_acti,'MACR_ELEM_STAT')) then
            write (ifm,*) '<MECANONLINE> ...... MACRO-ELEMENTS STATIQUES'
        endif
        if (isfonc(list_func_acti,'THM')) then
            write (ifm,*) '<MECANONLINE> ...... MODELISATION THM' 
        endif
        if (isfonc(list_func_acti,'ENDO_NO')) then
            write (ifm,*) '<MECANONLINE> ...... MODELISATION GVNO' 
        endif
!
! ----- Post-treatments
!
        if (isfonc(list_func_acti,'CRIT_STAB')) then
            write (ifm,*) '<MECANONLINE> ...... CALCUL CRITERE FLAMBEMENT'  
        endif
        if (isfonc(list_func_acti,'DDL_STAB')) then
            write (ifm,*) '<MECANONLINE> ...... CALCUL CRITERE STABILITE' 
        endif
        if (isfonc(list_func_acti,'MODE_VIBR')) then
            write (ifm,*) '<MECANONLINE> ...... CALCUL MODES VIBRATOIRES'
        endif
        if (isfonc(list_func_acti,'ENERGIE')) then
            write (ifm,*) '<MECANONLINE> ...... CALCUL DES ENERGIES'   
        endif
        if (isfonc(list_func_acti,'ERRE_TEMPS_THM')) then
            write (ifm,*) '<MECANONLINE> ...... CALCUL ERREUR TEMPS EN THM'  
        endif
        if (isfonc(list_func_acti,'POST_INCR')) then
            write (ifm,*) '<MECANONLINE> ...... CALCUL POST_INCR'
        endif
        if (isfonc(list_func_acti,'EXI_VARC')) then
            write (ifm,*) '<MECANONLINE> ...... VARIABLES DE COMMANDE'
        endif
        if (isfonc(list_func_acti,'ELAS_FO')) then
            write (ifm,*) '<MECANONLINE> ...... Elasticite fonction'
        endif
!
        if (isfonc(list_func_acti,'REUSE')) then
            write (ifm,*) '<MECANONLINE> ...... CONCEPT RE-ENTRANT'    
        endif
        if (isfonc(list_func_acti,'ETAT_INIT')) then
            write (ifm,*) '<MECANONLINE> ...... Etat initial present'    
        endif
!
! ----- Solver options
!
        if (isfonc(list_func_acti,'LDLT')) then
            write (ifm,*) '<MECANONLINE> ...... SOLVEUR LDLT'
        endif
        if (isfonc(list_func_acti,'MULT_FRONT')) then
            write (ifm,*) '<MECANONLINE> ...... SOLVEUR MULT_FRONT'  
        endif
        if (isfonc(list_func_acti,'GCPC')) then
            write (ifm,*) '<MECANONLINE> ...... SOLVEUR GCPC'    
        endif
        if (isfonc(list_func_acti,'MUMPS')) then
            write (ifm,*) '<MECANONLINE> ...... SOLVEUR MUMPS'
        endif
        if (isfonc(list_func_acti,'PETSC')) then
            write (ifm,*) '<MECANONLINE> ...... SOLVEUR PETSC'
        endif
        if (isfonc(list_func_acti,'LDLT_SP')) then
            write (ifm,*) '<MECANONLINE> ...... PRECONDITIONNEUR LDLT_SP' 
        endif
        if (isfonc(list_func_acti,'MATR_DISTRIBUEE')) then
            write (ifm,*) '<MECANONLINE> ...... MATRICE GLOBALE DISTRIBUEE'
        endif
    endif
!
! - Check compatibility of some functionnalities
!
    call exfonc(list_func_acti, algo_para, algo_meth, solver, sdcont_defi,&
                sddyna        , mate)
!
end subroutine
