subroutine nmfonc(parcri, parmet, method, solveu, modele,&
                  defico, lischa, lcont, lunil, sdnume,&
                  sddyna, sdcriq, mate, compoz, result,&
                  carcri, fonact)
!
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
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
#include "asterfort/infdbg.h"
#include "asterfort/ischar.h"
#include "asterfort/isdiri.h"
#include "asterfort/isfonc.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
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
    real(kind=8), intent(in) :: parcri(*)
    real(kind=8), intent(in) :: parmet(*)
    character(len=16), intent(in) :: method(*)
    character(len=19), intent(in) :: solveu
    character(len=24), intent(in) :: modele
    character(len=24), intent(in) :: defico
    character(len=19), intent(in) :: lischa
    aster_logical, intent(in) :: lcont
    aster_logical, intent(in) :: lunil
    character(len=19), intent(in) :: sdnume
    character(len=19), intent(in) :: sddyna
    character(len=24), intent(in) :: sdcriq
    character(len=24), intent(in) :: mate
    character(len=*), intent(in) :: compoz
    character(len=8), intent(in) :: result
    character(len=24), intent(in) :: carcri
    integer, intent(inout) :: fonact(*)
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Init
!
! Prepare active functionnalities information
!
! --------------------------------------------------------------------------------------------------
!
!
! IN  MODELE : MODELE MECANIQUE
! IN  DEFICO : SD DE DEFINITION DU CONTACT
! IN  SDNUME : NOM DE LA SD NUMEROTATION
! IN  LCONT  : .TRUE. S'IL Y A DU CONTACT
! IN  LUNIL  : .TRUE. S'IL Y A LIAISON_UNILATER
! IN  SOLVEU : NOM DU SOLVEUR DE NEWTON
! IN  SDDYNA : SD DYNAMIQUE
! IN  SDCRIQ : SD CRITERE QUALITE
! IN  MATE   : NOM DU CHAMP DE MATERIAU
! IN  PARMET : PARAMETRES DES METHODES DE RESOLUTION
! IN  PARCRI : RESI_CONT_RELA VAUT R8VIDE SI NON ACTIF
! IN  METHOD : DESCRIPTION DE LA METHODE DE RESOLUTION
! IN  ZFON   : LONGUEUR DU VECTEUR FONACT
! IN  LISCHA : SD DE DEFINITION DES CHARGES
! IN  COMPOR : CARTE DE COMPORTEMENT
! IN  RESULT : STRUCTURE DONNEE RESULTAT
! In  carcri : name of <CARTE> CARCRI
! OUT FONACT : FONCTIONNALITES SPECIFIQUES ACTIVEES (VOIR ISFONC)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nocc, iret, nbss, nbsst
    integer :: nbfonc, iform
    aster_logical :: lbors, lfrot, lchoc, lallv
    aster_logical :: lboucg, lboucf, lboucc
    integer :: ixfem, ichar, iflamb, imvibr, istab, nmatdi
    aster_logical :: lsuiv, llapl, lcine, ldidi
    character(len=8) :: k8bid, repk
    character(len=16) :: nomcmd, k16bid, matdis
    character(len=19) :: compor
    character(len=24) :: metres, precon, errthm
    aster_logical :: lstat, ldyna, larrno
    aster_logical :: lnewtc, lnewtf, lnewtg
    aster_logical :: lexpl
    integer :: ifm, niv
    integer :: nsta
    character(len=24), pointer :: slvk(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- INITIALISATIONS
!
    nbfonc = 0
    compor = compoz
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... CREATION VECTEUR '//&
     &                'FONCTIONNALITES ACTIVEES: '
    endif
!
! --- NOM DE LA COMMANDE: STAT_NON_LINE, DYNA_NON_LINE
!
    call getres(k8bid, k16bid, nomcmd)
    lstat = nomcmd(1:4).eq.'STAT'
    ldyna = nomcmd(1:4).eq.'DYNA'
    lexpl = ndynlo(sddyna,'EXPLICITE')
!
    call jeveuo(solveu//'.SLVK', 'L', vk24=slvk)
    metres=slvk(1)
!
! --- ELEMENTS EN GRANDES ROTATIONS
!
    call jeexin(sdnume(1:19)//'.NDRO', iret)
    if (iret .ne. 0) fonact(15) = 1
!
! --- ELEMENTS AVEC ENDO AUX NOEUDS
!
    call jeexin(sdnume(1:19)//'.ENDO', iret)
    if (iret .ne. 0) fonact(40) = 1
!
! --- RECHERCHE LINEAIRE
!
    call getfac('RECH_LINEAIRE', nocc)
    if (nocc .ne. 0) fonact(1) = 1
!
! --- PILOTAGE
!
    if (lstat) then
        nocc = 0
        call getfac('PILOTAGE', nocc)
        if (nocc .ne. 0) fonact(2) = 1
    endif
!
! --- LIAISON UNILATERALE
!
    if (lunil) fonact(12) = 1
!
! --- ENERGIE
!
    call getfac('ENERGIE', nocc)
    if (nocc .ne. 0) fonact(50) = 1
!
! --- PROJ_MODAL
!
    if (ldyna) then
        call getfac('PROJ_MODAL', nocc)
        if (nocc .ne. 0) fonact(51) = 1
    endif
!
! --- MATR_DISTRIBUEE
!
    matdis='NON'
    call getvtx('SOLVEUR', 'MATR_DISTRIBUEE', iocc=1, scal=matdis, nbret=nmatdi)
    if (matdis .eq. 'OUI') fonact(52) = 1
!
! --- DEBORST ?
!
    call nmcpqu(compor, 'C_PLAN', 'DEBORST', lbors)
    if (lbors) fonact(7) = 1
!
! --- CONVERGENCE SUR CRITERE EN CONTRAINTE GENERALISEE
!
    if (parcri(6) .ne. r8vide()) fonact(8) = 1
!
! --- CONVERGENCE SUR CRITERE NORME PAR FORC CMP
!
    if (parcri(7) .ne. r8vide()) fonact(35) = 1
!
! --- X-FEM
!
    call exixfe(modele, ixfem)
    if (ixfem .ne. 0) fonact(6) = 1
!
! --- CONTACT / FROTTEMENT
!
    if (lcont) then
        iform = cfdisi(defico,'FORMULATION')
        if (iform .eq. 2) then
            fonact(5) = 1
            fonact(17) = cfdisi(defico,'ALL_INTERPENETRE')
            fonact(26) = 1
            lfrot = cfdisl(defico,'FROTTEMENT')
            if (lfrot) then
                fonact(10) = 1
                fonact(27) = 1
            endif
        else if (iform.eq.3) then
            fonact(9) = 1
            fonact(26) = 1
            lfrot = cfdisl(defico,'FROTTEMENT')
            if (lfrot) then
                fonact(25) = 1
                fonact(27) = 1
            endif
! --- GLUTE TANT QUE XFEM NE DISTINGUE PAS
! --- CONTACT/FROTTEMENT
            fonact(27) = 1
        else if (iform.eq.1) then
            fonact(4) = 1
            lfrot = cfdisl(defico,'FROTTEMENT')
            if (lfrot) then
                fonact(3) = 1
            endif
        else
            ASSERT(.false.)
        endif
    endif
!
! --- MODE ALL VERIF
!
    if (lcont) then
        lallv = cfdisl(defico,'ALL_VERIF')
        if (lallv) fonact(38) = 1
    endif
!
! --- BOUCLES EXTERNES CONTACT / FROTTEMENT
!
    if (lcont) then
!
        lboucg = .false.
        lboucf = .false.
        lboucc = .false.
!
! ----- BOUCLE SUR GEOMETRIE
!
        lboucg = cfdisl(defico,'GEOM_BOUCLE')
!
! ----- BOUCLE SUR FROTTEMENT
!
        if (lfrot) lboucf = cfdisl(defico,'FROT_BOUCLE')
!
! ----- BOUCLE SUR CONTACT
!
        lboucc = cfdisl(defico,'CONT_BOUCLE')
!
! ----- TOUTES LES ZONES EN VERIF -> PAS DE BOUCLES
!
        lallv = cfdisl(defico,'ALL_VERIF')
        if (lallv) then
            lboucc = .false.
            lboucg = .false.
            lboucf = .false.
        endif
!
! ----- CONTACT DISCRET -> PAS DE BOUCLES CONT/FROT
!
        if (iform .eq. 1) then
            lboucc = .false.
            lboucf = .false.
        endif
!
! ----- BOUCLES EXTERNES
!
        if (lboucg) fonact(31) = 1
        if (lboucf) fonact(32) = 1
        if (lboucc) fonact(33) = 1
!
        if (lboucg .or. lboucf .or. lboucc) fonact(34) = 1
    endif
!
! --- NEWTON GENERALISE
!
    if (lcont) then
        if (iform .eq. 2) then
            lnewtg = cfdisl(defico,'GEOM_NEWTON')
            lnewtf = cfdisl(defico,'FROT_NEWTON')
            lnewtc = cfdisl(defico,'CONT_NEWTON')
            if (lnewtf) fonact(47) = 1
            if (lnewtc) fonact(53) = 1
            if (lnewtg) fonact(55) = 1
        endif
    endif
!
! --- AU MOINS UNE CHARGE SUIVEUSE
!
    ichar = 0
    lsuiv = ischar(lischa,'NEUM','SUIV',ichar )
    if (lsuiv) fonact(13) = 1
!
! --- AU MOINS UNE CHARGE DE TYPE DIDI
!
    ichar = 0
    ldidi = ischar(lischa,'DIRI','DIDI',ichar )
    if (ldidi) fonact(22) = 1
!
! --- AU MOINS UNE CHARGE DE TYPE DIRICHLET PAR
! --- ELIMINATION (AFFE_CHAR_CINE)
!
    lcine = isdiri(lischa,'ELIM')
    if (lcine) fonact(36) = 1
!
! --- AU MOINS UNE CHARGE DE TYPE FORCE DE LAPLACE
!
    llapl = ischar(lischa,'NEUM','LAPL',ichar )
    if (llapl) fonact(20) = 1
!
! --- SOUS STRUCTURES STATIQUES
!
    call dismoi('NB_SS_ACTI', modele, 'MODELE', repi=nbss)
    if (nbss .gt. 0) fonact(14) = 1
!
! --- CALCUL PAR SOUS-STRUCTURATION
!
    call nmlssv('LECT', lischa, nbsst)
    if (nbsst .gt. 0) fonact(24) = 1
!
! --- CALCUL DE FLAMBEMENT
!
    iflamb = 0
    call getfac('CRIT_STAB', iflamb)
    if (iflamb .gt. 0) fonact(18) = 1
!
! --- CALCUL DE STABILITE
!
    istab = 0
    call getvtx('CRIT_STAB', 'DDL_STAB', iocc=1, nbval=0, nbret=nsta)
    istab = -nsta
    if (istab .gt. 0) fonact(49) = 1
!
! --- CALCUL DE MODES VIBRATOIRES
!
    imvibr = 0
    if (ldyna) then
        call getfac('MODE_VIBR', imvibr)
        if (imvibr .gt. 0) fonact(19) = 1
    endif
!
! --- ERREUR EN TEMPS
!
    if (lstat) then
        errthm = sdcriq(1:19)//'.ERRT'
        call jeexin(errthm, iret)
        if (iret .ne. 0) fonact(21) = 1
    endif
!
! --- ALGORITHME IMPLEX
!
    if (lstat) then
        if (method(1) .eq. 'IMPLEX') fonact(28) = 1
    endif
!
! --- ALGORITHME NEWTON_KRYLOV
!
    if (method(1) .eq. 'NEWTON_KRYLOV') fonact(48) = 1
!
! --- ELEMENTS DIS_CHOC ?
!
    call nmcpqu(compor, 'RELCOM', 'DIS_CHOC', lchoc)
    if (lchoc) fonact(29) = 1
!
! --- PRESENCE DE VARIABLES DE COMMANDE
!
    call dismoi('EXI_VARC', mate, 'CHAM_MATER', repk=repk)
    if (repk .eq. 'OUI') fonact(30) = 1
!
! --- MODELISATION THM ?
!
    call dismoi('EXI_THM', modele, 'MODELE', repk=repk)
    if (repk .eq. 'OUI') fonact(37) = 1
!
! --- PRESENCE D'ELEMENTS UTILISANT STRX (PMF)
!
    call dismoi('EXI_STRX', modele, 'MODELE', repk=repk)
    if (repk .eq. 'OUI') fonact(56) = 1
!
! --- CONCEPT REENTRANT ?
!
    call gcucon(result, 'EVOL_NOLI', iret)
    if (iret .gt. 0) fonact(39) = 1
!
! --- SOLVEUR LDLT?
!
    if (metres .eq. 'LDLT') fonact(41) = 1
!
! --- SOLVEUR MULT_FRONT?
!
    if (metres .eq. 'MULT_FRONT') fonact(42) = 1
!
! --- SOLVEUR GCPC?
!
    if (metres .eq. 'GCPC') fonact(43) = 1
!
! --- SOLVEUR MUMPS?
!
    if (metres .eq. 'MUMPS') fonact(44) = 1
!
! --- SOLVEUR PETSC?
!
    if (metres .eq. 'PETSC') fonact(45) = 1
!
! --- PRECONDITIONNEUR LDLT_SP?
!
    if (metres .eq. 'PETSC' .or. metres .eq. 'GCPC') then
        precon=slvk(2)
        if (precon .eq. 'LDLT_SP') fonact(46) = 1
    endif
!
! --- BLINDAGE ARRET=NON
!
    larrno = (nint(parcri(4)).eq.1)
    if (larrno) then
        call utmess('A', 'MECANONLINE5_37')
    endif
!
! --- CALCUL DYNAMIQUE EXPLICITE
!
    if (lexpl) fonact(54) = 1
!
! - Do elastic properties are functions ?
!
    call dismoi('ELAS_FO', mate, 'CHAM_MATER', repk=repk)
    if (repk .eq. 'OUI') then
        fonact(57) = 1
    endif
!
! - Post-treatment on comportment laws ?
!
    call dismoi('POST_INCR', carcri, 'CARTE_CARCRI', repk=repk)
    if (repk .eq. 'OUI') then
        fonact(58) = 1
    endif

!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
!
! ----- METHODES DE RESOLUTION
!
        if (isfonc(fonact,'IMPLEX')) then
            write (ifm,*) '<MECANONLINE> ...... METHODE IMPLEX'
            nbfonc = nbfonc + 1
        endif
        if (isfonc(fonact,'EXPLICITE')) then
            write (ifm,*) '<MECANONLINE> ...... METHODE EXPLICITE'
            nbfonc = nbfonc + 1
        endif
        if (isfonc(fonact,'NEWTON_KRYLOV')) then
            write (ifm,*) '<MECANONLINE> ...... METHODE NEWTON_KRYLOV'
            nbfonc = nbfonc + 1
        endif
        if (isfonc(fonact,'RECH_LINE')) then
            write (ifm,*) '<MECANONLINE> ...... RECHERCHE LINEAIRE'
            nbfonc = nbfonc + 1
        endif
        if (isfonc(fonact,'PILOTAGE')) then
            write (ifm,*) '<MECANONLINE> ...... PILOTAGE'
            nbfonc = nbfonc + 1
        endif
        if (isfonc(fonact,'DEBORST')) then
            write (ifm,*) '<MECANONLINE> ...... METHODE DEBORST'
            nbfonc = nbfonc + 1
        endif
        if (isfonc(fonact,'SOUS_STRUC')) then
            write (ifm,*) '<MECANONLINE> ...... CALCUL PAR SOUS-'//&
            'STRUCTURATION'
            nbfonc = nbfonc + 1
        endif
        if (isfonc(fonact,'PROJ_MODAL')) then
            write (ifm,*) '<MECANONLINE> ...... CALCUL PAR PROJECTION'//&
     &                    ' MODALE'
            nbfonc = nbfonc + 1
        endif
!
! ----- CONTACT
!
        if (isfonc(fonact,'CONTACT')) then
            write (ifm,*) '<MECANONLINE> ...... CONTACT'
            nbfonc = nbfonc + 1
        endif
        if (isfonc(fonact,'CONT_DISCRET')) then
            write (ifm,*) '<MECANONLINE> ...... CONTACT DISCRET'
            nbfonc = nbfonc + 1
        endif
        if (isfonc(fonact,'CONT_CONTINU')) then
            write (ifm,*) '<MECANONLINE> ...... CONTACT CONTINU'
            nbfonc = nbfonc + 1
        endif
        if (isfonc(fonact,'CONT_XFEM')) then
            write (ifm,*) '<MECANONLINE> ...... CONTACT XFEM'
            nbfonc = nbfonc + 1
        endif
        if (isfonc(fonact,'BOUCLE_EXT_GEOM')) then
            write (ifm,*) '<MECANONLINE> ...... CONTACT BOUCLE GEOM'
            nbfonc = nbfonc + 1
        endif
        if (isfonc(fonact,'BOUCLE_EXT_CONT')) then
            write (ifm,*) '<MECANONLINE> ...... CONTACT BOUCLE CONTACT'
            nbfonc = nbfonc + 1
        endif
        if (isfonc(fonact,'BOUCLE_EXT_FROT')) then
            write (ifm,*) '<MECANONLINE> ...... CONTACT BOUCLE FROT'
            nbfonc = nbfonc + 1
        endif
        if (isfonc(fonact,'BOUCLE_EXTERNE')) then
            write (ifm,*) '<MECANONLINE> ...... BOUCLE EXTERNE'
            nbfonc = nbfonc + 1
        endif
        if (isfonc(fonact,'GEOM_NEWTON')) then
            write (ifm,*) '<MECANONLINE> ...... GEOMETRIE AVEC '//&
            'NEWTON GENERALISE'
            nbfonc = nbfonc + 1
        endif
        if (isfonc(fonact,'FROT_NEWTON')) then
            write (ifm,*) '<MECANONLINE> ...... FROTTEMENT AVEC '//&
            'NEWTON GENERALISE'
            nbfonc = nbfonc + 1
        endif
        if (isfonc(fonact,'CONT_NEWTON')) then
            write (ifm,*) '<MECANONLINE> ...... CONTACT AVEC '//&
            'NEWTON GENERALISE'
            nbfonc = nbfonc + 1
        endif
        if (isfonc(fonact,'CONT_ALL_VERIF')) then
            write (ifm,*) '<MECANONLINE> ...... CONTACT SANS '//&
            'CALCUL SUR TOUTES LES ZONES'
            nbfonc = nbfonc + 1
        endif
        if (isfonc(fonact,'CONTACT_INIT')) then
            write (ifm,*) '<MECANONLINE> ...... CONTACT INITIAL '
            nbfonc = nbfonc + 1
        endif
        if (isfonc(fonact,'LIAISON_UNILATER')) then
            write (ifm,*) '<MECANONLINE> ...... LIAISON UNILATERALE'
            nbfonc = nbfonc + 1
        endif
        if (isfonc(fonact,'FROT_DISCRET')) then
            write (ifm,*) '<MECANONLINE> ...... FROTTEMENT DISCRET'
            nbfonc = nbfonc + 1
        endif
        if (isfonc(fonact,'FROT_CONTINU')) then
            write (ifm,*) '<MECANONLINE> ...... FROTTEMENT CONTINU'
            nbfonc = nbfonc + 1
        endif
        if (isfonc(fonact,'FROT_XFEM')) then
            write (ifm,*) '<MECANONLINE> ...... FROTTEMENT XFEM'
            nbfonc = nbfonc + 1
        endif
!
! ----- ELEMENTS FINIS
!
        if (isfonc(fonact,'ELT_CONTACT')) then
            write (ifm,*) '<MECANONLINE> ...... ELEMENTS DE CONTACT'
            nbfonc = nbfonc + 1
        endif
        if (isfonc(fonact,'ELT_FROTTEMENT')) then
            write (ifm,*) '<MECANONLINE> ...... ELEMENTS DE FROTTEMENT'
            nbfonc = nbfonc + 1
        endif
        if (isfonc(fonact,'DIS_CHOC')) then
            write (ifm,*) '<MECANONLINE> ...... ELEMENTS DIS_CHOC '
            nbfonc = nbfonc + 1
        endif
        if (isfonc(fonact,'GD_ROTA')) then
            write (ifm,*) '<MECANONLINE> ...... ELEMENTS DE STRUCTURES'//&
     &                  ' EN GRANDES ROTATIONS'
            nbfonc = nbfonc + 1
        endif
        if (isfonc(fonact,'XFEM')) then
            write (ifm,*) '<MECANONLINE> ...... ELEMENTS XFEM'
            nbfonc = nbfonc + 1
        endif
        if (isfonc(fonact,'EXI_STRX')) then
            write (ifm,*) '<MECANONLINE> ...... ELEMENTS DE STRUCTURES'//&
     &                  ' DE TYPE PMF'
            nbfonc = nbfonc + 1
        endif
!
! ----- CONVERGENCE
!
        if (isfonc(fonact,'RESI_REFE')) then
            write (ifm,*) '<MECANONLINE> ...... CONVERGENCE PAR RESI_REFE'
            nbfonc = nbfonc + 1
        endif
        if (isfonc(fonact,'RESI_COMP')) then
            write (ifm,*) '<MECANONLINE> ...... CONVERGENCE PAR RESI_COMP'
            nbfonc = nbfonc + 1
        endif
!
! ----- CHARGEMENTS
!
        if (isfonc(fonact,'FORCE_SUIVEUSE')) then
            write (ifm,*) '<MECANONLINE> ...... CHARGEMENTS SUIVEURS'
            nbfonc = nbfonc + 1
        endif
        if (isfonc(fonact,'DIDI')) then
            write (ifm,*) '<MECANONLINE> ...... CHARGEMENTS DE '//&
            'DIRICHLET DIFFERENTIEL'
            nbfonc = nbfonc + 1
        endif
        if (isfonc(fonact,'DIRI_CINE')) then
            write (ifm,*) '<MECANONLINE> ...... CHARGEMENTS '//&
            'CINEMATIQUES PAR ELIMINATION'
            nbfonc = nbfonc + 1
        endif
        if (isfonc(fonact,'LAPLACE')) then
            write (ifm,*) '<MECANONLINE> ...... CHARGEMENTS '//&
            'DE LAPLACE'
            nbfonc = nbfonc + 1
        endif
!
! ----- MODELISATION
!
        if (isfonc(fonact,'MACR_ELEM_STAT')) then
            write (ifm,*) '<MECANONLINE> ...... MACRO-ELEMENTS STATIQUES'
            nbfonc = nbfonc + 1
        endif
        if (isfonc(fonact,'THM')) then
            write (ifm,*) '<MECANONLINE> ...... MODELISATION THM'
            nbfonc = nbfonc + 1
        endif
        if (isfonc(fonact,'ENDO_NO')) then
            write (ifm,*) '<MECANONLINE> ...... MODELISATION GVNO'
            nbfonc = nbfonc + 1
        endif
!
! ----- POST-TRAITEMENTS
!
        if (isfonc(fonact,'CRIT_STAB')) then
            write (ifm,*) '<MECANONLINE> ...... CALCUL CRITERE FLAMBEMENT'
            nbfonc = nbfonc + 1
        endif
        if (isfonc(fonact,'DDL_STAB')) then
            write (ifm,*) '<MECANONLINE> ...... CALCUL CRITERE STABILITE'
            nbfonc = nbfonc + 1
        endif
        if (isfonc(fonact,'MODE_VIBR')) then
            write (ifm,*) '<MECANONLINE> ...... CALCUL MODES VIBRATOIRES'
            nbfonc = nbfonc + 1
        endif
        if (isfonc(fonact,'ENERGIE')) then
            write (ifm,*) '<MECANONLINE> ...... CALCUL DES ENERGIES'
            nbfonc = nbfonc + 1
        endif
        if (isfonc(fonact,'ERRE_TEMPS_THM')) then
            write (ifm,*) '<MECANONLINE> ...... CALCUL ERREUR TEMPS EN THM'
            nbfonc = nbfonc + 1
        endif
        if (isfonc(fonact,'POST_INCR')) then
            write (ifm,*) '<MECANONLINE> ...... CALCUL POST_INCR'
            nbfonc = nbfonc + 1
        endif
        if (isfonc(fonact,'EXI_VARC')) then
            write (ifm,*) '<MECANONLINE> ...... VARIABLES DE COMMANDE'
            nbfonc = nbfonc + 1
        endif
        if (isfonc(fonact,'ELAS_FO')) then
            write (ifm,*) '<MECANONLINE> ...... Elasticite fonction'
            nbfonc = nbfonc + 1
        endif
!
        if (isfonc(fonact,'REUSE')) then
            write (ifm,*) '<MECANONLINE> ...... CONCEPT RE-ENTRANT'
            nbfonc = nbfonc + 1
        endif
!
        if (isfonc(fonact,'LDLT')) then
            write (ifm,*) '<MECANONLINE> ...... SOLVEUR LDLT'
            nbfonc = nbfonc + 1
        endif
        if (isfonc(fonact,'MULT_FRONT')) then
            write (ifm,*) '<MECANONLINE> ...... SOLVEUR MULT_FRONT'
            nbfonc = nbfonc + 1
        endif
        if (isfonc(fonact,'GCPC')) then
            write (ifm,*) '<MECANONLINE> ...... SOLVEUR GCPC'
            nbfonc = nbfonc + 1
        endif
        if (isfonc(fonact,'MUMPS')) then
            write (ifm,*) '<MECANONLINE> ...... SOLVEUR MUMPS'
            nbfonc = nbfonc + 1
        endif
        if (isfonc(fonact,'PETSC')) then
            write (ifm,*) '<MECANONLINE> ...... SOLVEUR PETSC'
            nbfonc = nbfonc + 1
        endif
        if (isfonc(fonact,'LDLT_SP')) then
            write (ifm,*) '<MECANONLINE> ...... PRECONDITIONNEUR LDLT_SP'
            nbfonc = nbfonc + 1
        endif
        if (isfonc(fonact,'MATR_DISTRIBUEE')) then
            write (ifm,*) '<MECANONLINE> ...... MATRICE GLOBALE '//&
            'DISTRIBUEE'
            nbfonc = nbfonc + 1
        endif
!
        if (nbfonc .eq. 0) then
            write (ifm,*) '<MECANONLINE> ...... <AUCUNE>'
        endif
    endif
!
! --- FONCTIONNALITES INCOMPATIBLES
!
    call exfonc(fonact, parmet, method, solveu, defico,&
                sddyna, mate)
!
    call jedema()
end subroutine
