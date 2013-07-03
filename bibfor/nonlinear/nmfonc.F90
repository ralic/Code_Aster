subroutine nmfonc(parcri, parmet, method, solveu, modele,&
                  defico, lischa, lcont, lunil, sdnume,&
                  sddyna, sdcriq, mate, compoz, result,&
                  fonact)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! aslint: disable=W1501
    implicit      none
#include "jeveux.h"
#include "asterc/gcucon.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterc/getvtx.h"
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfdisl.h"
#include "asterfort/dismoi.h"
#include "asterfort/exfonc.h"
#include "asterfort/exixfe.h"
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
#include "asterfort/u2mess.h"
    integer :: fonact(*)
    logical :: lunil, lcont
    character(len=16) :: method(*)
    real(kind=8) :: parcri(*), parmet(*)
    character(len=19) :: solveu, lischa, sddyna, sdnume
    character(len=24) :: sdcriq
    character(len=24) :: mate, modele
    character(len=24) :: defico
    character(len=8) :: result
    character(len=*) :: compoz
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (INITIALISATION)
!
! FONCTIONNALITES ACTIVEES
!
! ----------------------------------------------------------------------
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
! OUT FONACT : FONCTIONNALITES SPECIFIQUES ACTIVEES (VOIR ISFONC)
!
! ----------------------------------------------------------------------
!
    integer :: nocc, iret, nbss, nbsst, ibid
    integer :: nbfonc, iform, jslvk
    logical :: lbors, lfrot, lchoc, lallv
    logical :: lboucg, lboucf, lboucc
    integer :: jsolve, ixfem, ichar, iflamb, imvibr, istab, nmatdi
    logical :: lsuiv, llapl, lcine, ldidi
    character(len=8) :: k8bid, repk
    character(len=16) :: nomcmd, k16bid, matdis
    character(len=19) :: compor
    character(len=24) :: metres, precon, errthm
    logical :: lstat, ldyna, lgrot, lendo, larrno
    logical :: lnewtc, lnewtf, lnewtg
    logical :: lexpl
    integer :: ifm, niv
    integer :: iarg, nsta
!
! ----------------------------------------------------------------------
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
! --- ELEMENTS EN GRANDES ROTATIONS
!
    call jeexin(sdnume(1:19)//'.NDRO', iret)
    lgrot = iret.ne.0
!
! --- ELEMENTS AVEC ENDO AUX NOEUDS
!
    call jeexin(sdnume(1:19)//'.ENDO', iret)
    lendo = iret.ne.0
!
! --- RECHERCHE LINEAIRE
!
    call getfac('RECH_LINEAIRE', nocc)
    if (nocc .ne. 0) then
        fonact(1) = 1
    endif
!
! --- PILOTAGE
!
    if (lstat) then
        nocc = 0
        call getfac('PILOTAGE', nocc)
        if (nocc .ne. 0) then
            fonact(2) = 1
        endif
    endif
!
! --- LIAISON UNILATERALE
!
    if (lunil) then
        fonact(12) = 1
    endif
!
! --- ENERGIE
!
    call getfac('ENERGIE', nocc)
    if (nocc .ne. 0) then
        fonact(50) = 1
    endif
!
! --- PROJ_MODAL
!
    if (ldyna) then
        call getfac('PROJ_MODAL', nocc)
        if (nocc .ne. 0) then
            fonact(51) = 1
        endif
    endif
!
! --- MATR_DISTRIBUEE
!
    matdis='NON'
    call getvtx('SOLVEUR', 'MATR_DISTRIBUEE', 1, iarg, 1,&
                matdis, nmatdi)
    if (matdis .eq. 'OUI') then
        fonact(52) = 1
    endif
!
! --- DEBORST ?
!
    call nmcpqu(compor, 'C_PLAN', 'DEBORST', lbors)
    if (lbors) then
        fonact(7) = 1
    endif
!
! --- CONVERGENCE SUR CRITERE EN CONTRAINTE GENERALISEE
!
    if (parcri(6) .ne. r8vide()) then
        fonact(8) = 1
    endif
!
! --- CONVERGENCE SUR CRITERE NORME PAR FORC CMP
!
    if (parcri(7) .ne. r8vide()) then
        fonact(35) = 1
        if (ldyna) call u2mess('F', 'MECANONLINE5_53')
    endif
!
! --- X-FEM
!
    call exixfe(modele, ixfem)
    if (ixfem .ne. 0) then
        fonact(6) = 1
    endif
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
            call assert(.false.)
        endif
    endif
!
! --- MODE ALL VERIF
!
    if (lcont) then
        lallv = cfdisl(defico,'ALL_VERIF')
        if (lallv) then
            fonact(38) = 1
        endif
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
        if (lboucg) then
            fonact(31) = 1
        endif
        if (lboucf) then
            fonact(32) = 1
        endif
        if (lboucc) then
            fonact(33) = 1
        endif
!
        if (lboucg .or. lboucf .or. lboucc) then
            fonact(34) = 1
        endif
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
! --- FETI
!
    call jeveuo(solveu//'.SLVK', 'L', jsolve)
    if (zk24(jsolve)(1:4) .eq. 'FETI') then
        fonact(11) = 1
    else
        fonact(11) = 0
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
    call dismoi('F', 'NB_SS_ACTI', modele, 'MODELE', nbss,&
                k8bid, iret)
    if (nbss .gt. 0) then
        fonact(14) = 1
    endif
!
! --- CALCUL PAR SOUS-STRUCTURATION
!
    call nmlssv('LECT', lischa, nbsst)
    if (nbsst .gt. 0) then
        fonact(24) = 1
    endif
!
! --- ELEMENTS EN GRANDES ROTATIONS
!
    if (lgrot) then
        fonact(15) = 1
    endif
!
! --- ELEMENTS AVEC ENDO AUX NOEUDS
!
    if (lendo) then
        fonact(40) = 1
    endif
!
! --- CALCUL DE FLAMBEMENT
!
    iflamb = 0
    call getfac('CRIT_STAB', iflamb)
    if (iflamb .gt. 0) then
        fonact(18) = 1
    endif
!
! --- CALCUL DE STABILITE
!
    istab = 0
    call getvtx('CRIT_STAB', 'DDL_STAB', 1, iarg, 0,&
                k16bid, nsta)
    istab = -nsta
    if (istab .gt. 0) then
        fonact(49) = 1
    endif
!
! --- CALCUL DE MODES VIBRATOIRES
!
    imvibr = 0
    if (ldyna) then
        call getfac('MODE_VIBR', imvibr)
        if (imvibr .gt. 0) then
            fonact(19) = 1
        endif
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
        if (method(1) .eq. 'IMPLEX') then
            fonact(28) = 1
        else
            fonact(28) = 0
        endif
    endif
!
! --- ALGORITHME NEWTON_KRYLOV
!
    if (method(1) .eq. 'NEWTON_KRYLOV') then
        fonact(48) = 1
    else
        fonact(48) = 0
    endif
!
! --- ELEMENTS DIS_CHOC ?
!
    call nmcpqu(compor, 'RELCOM', 'DIS_CHOC', lchoc)
    if (lchoc) then
        fonact(29) = 1
    endif
!
! --- PRESENCE DE VARIABLES DE COMMANDE
!
    call dismoi('F', 'EXI_VARC', mate, 'CHAM_MATER', ibid,&
                repk, iret)
    if (repk .eq. 'OUI') then
        fonact(30) = 1
    endif
!
! --- MODELISATION THM ?
!
    call dismoi('F', 'EXI_THM', modele, 'MODELE', ibid,&
                repk, iret)
    if (repk(1:3) .eq. 'OUI') then
        fonact(37) = 1
    endif
!
! --- PRESENCE D'ELEMENTS UTILISANT STRX (PMF)
!
    call dismoi('F', 'EXI_STRX', modele, 'MODELE', ibid,&
                repk, iret)
    if (repk .eq. 'OUI') then
        fonact(56) = 1
    endif
!
! --- CONCEPT REENTRANT ?
!
    call gcucon(result, 'EVOL_NOLI', iret)
    if (iret .gt. 0) then
        fonact(39) = 1
    endif
!
! --- SOLVEUR LDLT?
!
    call jeveuo(solveu//'.SLVK', 'L', jslvk)
    metres=zk24(jslvk)
    if (metres .eq. 'LDLT') then
        fonact(41) = 1
    endif
!
! --- SOLVEUR MULT_FRONT?
!
    call jeveuo(solveu//'.SLVK', 'L', jslvk)
    metres=zk24(jslvk)
    if (metres .eq. 'MULT_FRONT') then
        fonact(42) = 1
    endif
!
! --- SOLVEUR GCPC?
!
    call jeveuo(solveu//'.SLVK', 'L', jslvk)
    metres=zk24(jslvk)
    if (metres .eq. 'GCPC') then
        fonact(43) = 1
    endif
!
! --- SOLVEUR MUMPS?
!
    call jeveuo(solveu//'.SLVK', 'L', jslvk)
    metres=zk24(jslvk)
    if (metres .eq. 'MUMPS') then
        fonact(44) = 1
    endif
!
! --- SOLVEUR PETSC?
!
    call jeveuo(solveu//'.SLVK', 'L', jslvk)
    metres=zk24(jslvk)
    if (metres .eq. 'PETSC') then
        fonact(45) = 1
    endif
!
! --- PRECONDITIONNEUR LDLT_SP?
!
    call jeveuo(solveu//'.SLVK', 'L', jslvk)
    metres=zk24(jslvk)
    if (metres .eq. 'PETSC' .or. metres .eq. 'GCPC') then
        precon=zk24(jslvk-1+2)
        if (precon .eq. 'LDLT_SP') then
            fonact(46) = 1
        endif
    endif
!
! --- BLINDAGE ARRET=NON
!
    larrno = (nint(parcri(4)).eq.1)
    if (larrno) call u2mess('A', 'MECANONLINE5_37')
!
! --- CALCUL DYNAMIQUE EXPLICITE
!
    if (lexpl) fonact(54) = 1
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
            write (ifm,*) '<MECANONLINE> ...... CALCUL ERREUR TEMPS EN'//&
     &                  ' THM'
            nbfonc = nbfonc + 1
        endif
        if (isfonc(fonact,'EXI_VARC')) then
            write (ifm,*) '<MECANONLINE> ...... VARIABLES DE COMMANDE'
            nbfonc = nbfonc + 1
        endif
!
        if (isfonc(fonact,'REUSE')) then
            write (ifm,*) '<MECANONLINE> ...... CONCEPT RE-ENTRANT'
            nbfonc = nbfonc + 1
        endif
!
        if (isfonc(fonact,'FETI')) then
            write (ifm,*) '<MECANONLINE> ...... SOLVEUR FETI'
            nbfonc = nbfonc + 1
        endif
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
                sddyna)
!
    call jedema()
end subroutine
