subroutine exfonc(fonact, parmet, method, solveu, defico,&
                  sddyna)
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
    implicit none
#include "jeveux.h"
#include "asterc/getvtx.h"
#include "asterfort/cfdisl.h"
#include "asterfort/infdbg.h"
#include "asterfort/isfonc.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/ndynlo.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
    integer :: fonact(*)
    character(len=19) :: solveu, sddyna
    character(len=24) :: defico
    real(kind=8) :: parmet(*)
    character(len=16) :: method(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (INITIALISATION)
!
! FONCTIONNALITES INCOMPATIBLES
!
! ----------------------------------------------------------------------
!
!
! IN  DEFICO : SD DE DEFINITION DU CONTACT
! IN  SOLVEU : NOM DU SOLVEUR DE NEWTON
! IN  METHOD : DESCRIPTION DE LA METHODE DE RESOLUTION
! IN  SDDYNA : SD DYNAMIQUE
! IN  PARMET : PARAMETRES DES METHODES DE RESOLUTION
! IN  FONACT : FONCTIONNALITES SPECIFIQUES ACTIVEES
!
! ---------------------------------------------------------------------
!
    integer :: reincr
    integer :: jsolve, n1
    logical :: lcont, lallv, lctcc, lctcd, lpena, leltc
    logical :: lfeti, lpilo, lreli, lmacr, lunil
    logical :: lmvib, lflam, lexpl, lxfem, lmodim
    logical :: lrcmk, lgcpc, lpetsc, lamg, lsyme, limpex
    logical :: londe, ldyna, lgrot, ltheta, lnkry
    logical :: lener, lproj, lmatdi, lldsp, lctgcp
    integer :: ifm, niv
    character(len=24) :: typilo, typrel
    integer :: iarg
!
! ---------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- FONCTIONNALITES ACTIVEES
!
    lfeti = isfonc(fonact,'FETI')
    lxfem = isfonc(fonact,'XFEM')
    lctcc = isfonc(fonact,'CONT_CONTINU')
    lctcd = isfonc(fonact,'CONT_DISCRET')
    lcont = isfonc(fonact,'CONTACT')
    lunil = isfonc(fonact,'LIAISON_UNILATER')
    lpilo = isfonc(fonact,'PILOTAGE')
    lreli = isfonc(fonact,'RECH_LINE')
    lmacr = isfonc(fonact,'MACR_ELEM_STAT')
    lmvib = isfonc(fonact,'MODE_VIBR')
    lflam = isfonc(fonact,'CRIT_STAB')
    londe = ndynlo(sddyna,'ONDE_PLANE')
    ldyna = ndynlo(sddyna,'DYNAMIQUE')
    lexpl = isfonc(fonact,'EXPLICITE')
    lgrot = isfonc(fonact,'GD_ROTA')
    ltheta = ndynlo(sddyna,'THETA_METHODE')
    limpex = isfonc(fonact,'IMPLEX')
    lnkry = isfonc(fonact,'NEWTON_KRYLOV')
    lener = isfonc(fonact,'ENERGIE')
    lproj = isfonc(fonact,'PROJ_MODAL')
    lmatdi = isfonc(fonact,'MATR_DISTRIBUEE')
    leltc = isfonc(fonact,'ELT_CONTACT')
!
! --- INITIALISATIONS
!
    reincr = nint(parmet(1))
!
! --- TYPE DE SOLVEUR
!
    call jeveuo(solveu//'.SLVK', 'E', jsolve)
    lrcmk = .false.
    lgcpc = .false.
    lpetsc = .false.
    lamg = .false.
    lldsp = .false.
    if (zk24(jsolve-1+4) .eq. 'RCMK') then
        lrcmk = .true.
    endif
    if (zk24(jsolve)(1:4) .eq. 'GCPC') then
        lgcpc = .true.
    endif
    if (zk24(jsolve)(1:5) .eq. 'PETSC') then
        lpetsc = .true.
    endif
    if ((zk24(jsolve-1+2).eq.'ML') .or. (zk24(jsolve-1+2).eq.'BOOMER')) then
        lamg = .true.
    endif
    if (zk24(jsolve-1+2) .eq. 'LDLT_SP') then
        lldsp = .true.
    endif
    lsyme = zk24(jsolve-1+5).eq.'OUI'
!
! --- FETI
!
    if (lfeti) then
        if (lmacr) then
            call u2mess('F', 'MECANONLINE3_70')
        endif
        if (londe) then
            call u2mess('F', 'MECANONLINE3_71')
        endif
        if (ldyna) then
            call u2mess('F', 'MECANONLINE3_73')
        endif
        if (lctcd) then
            call u2mess('F', 'MECANONLINE3_78')
        endif
        if (lctcc) then
            call u2mess('F', 'MECANONLINE3_79')
        endif
    endif
!
! --- CONTACT DISCRET
!
    if (lctcd) then
        lmodim = cfdisl(defico,'MODI_MATR_GLOB')
        lallv = cfdisl(defico,'ALL_VERIF')
        lpena = cfdisl(defico,'CONT_PENA')
        lctgcp = cfdisl(defico,'CONT_GCP')
        if (lpilo) then
            call u2mess('F', 'MECANONLINE_43')
        endif
        if (lreli .and. (.not.lallv)) then
            call u2mess('A', 'MECANONLINE3_89')
        endif
        if (lgcpc .or. lpetsc) then
            if (.not.(lallv.or.lpena.or.lctgcp)) then
                call u2mesk('F', 'MECANONLINE3_90', 1, zk24(jsolve))
            endif
            if (lctgcp .and. .not.lldsp) then
                call u2mess('F', 'MECANONLINE3_88')
            endif
        endif
        if (reincr .eq. 0) then
            if (lmodim) then
                call u2mess('F', 'CONTACT_88')
            endif
        endif
!       ON FORCE SYME='OUI' AVEC LE CONTACT DISCRET
        if (.not.(lsyme.or.lallv)) then
            zk24(jsolve+4) = 'OUI'
            call u2mess('A', 'CONTACT_1')
        endif
        if ((lmvib.or.lflam) .and. lmodim) then
            call u2mess('F', 'MECANONLINE5_14')
        endif
    endif
!
! --- CONTACT CONTINU
!
    if (lctcc) then
        if (lpilo .and. (.not.lxfem)) then
!         LEVEE D INTERDICTION TEMPORAIRE POUR X-FEM
            call u2mess('F', 'MECANONLINE3_92')
        endif
        if (lreli) then
            call u2mess('F', 'MECANONLINE3_91')
        endif
        if (lrcmk) then
            call u2mesk('F', 'MECANONLINE3_93', 1, zk24(jsolve))
        endif
        if (lamg) then
            call u2mesk('F', 'MECANONLINE3_97', 1, zk24(jsolve-1+2))
        endif
        if (lpetsc .and. lmatdi) then
            call u2mess('F', 'MECANONLINE3_98')
        endif
    endif
!
! --- LIAISON UNILATERALE
!
    if (lunil) then
        if (lpilo) then
            call u2mess('F', 'MECANONLINE3_94')
        endif
        if (lreli) then
            call u2mess('A', 'MECANONLINE3_95')
        endif
        if (lgcpc .or. lpetsc) then
            call u2mesk('F', 'MECANONLINE3_96', 1, zk24(jsolve))
        endif
!       ON FORCE SYME='OUI' AVEC LIAISON_UNILATER
        if (.not.lsyme) then
            zk24(jsolve+4)='OUI'
            call u2mess('A', 'UNILATER_1')
        endif
    endif
!
! --- CALCUL DE MODES/FLAMBEMENT: PAS GCPC/PETSC
!
    if (lmvib .or. lflam) then
        if (lgcpc .or. lpetsc) then
            call u2mesk('F', 'FACTOR_52', 1, zk24(jsolve))
        endif
        if (leltc) then
            call u2mess('F', 'MECANONLINE5_3')
        endif
    endif
!
! --- EXPLICITE
!
    if (lexpl) then
        if (lcont) then
            call u2mess('F', 'MECANONLINE5_22')
        endif
        if (lunil) then
            call u2mess('F', 'MECANONLINE5_23')
        endif
        if (lgrot) then
            call u2mess('A', 'MECANONLINE5_24')
        endif
    endif
!
! --- DYNAMIQUE
!
    if (ldyna) then
        if (lpilo) then
            call u2mess('F', 'MECANONLINE5_25')
        endif
        if (ltheta) then
            if (lgrot) then
                call u2mess('F', 'MECANONLINE5_27')
            endif
        endif
        if (lxfem) then
            call u2mess('F', 'MECANONLINE5_28')
        endif
        if (limpex) then
            call u2mess('F', 'MECANONLINE5_33')
        endif
    endif
!
! --- PILOTAGE
!
    if (lpilo) then
        call getvtx('PILOTAGE', 'TYPE', 1, iarg, 1,&
                    typilo, n1)
        if (lreli) then
            if (typilo .eq. 'DDL_IMPO') then
                call u2mess('F', 'MECANONLINE5_34')
            endif
        endif
        if ((method(5).eq.'DEPL_CALCULE') .or. (method(5) .eq.'EXTRAPOLE')) then
            call u2mess('F', 'MECANONLINE5_36')
        endif
    endif
    if (lreli) then
        call getvtx('RECH_LINEAIRE', 'METHODE', 1, iarg, 1,&
                    typrel, n1)
        if ((typrel.eq.'PILOTAGE') .and. (.not.lpilo)) then
            call u2mess('F', 'MECANONLINE5_35')
        endif
    endif
!
! --- NEWTON_KRYLOV
!
    if (lnkry) then
        if (lpilo) call u2mess('F', 'MECANONLINE5_48')
        if ((.not.lgcpc) .and. (.not.lpetsc)) then
            call u2mess('F', 'MECANONLINE5_51')
        endif
    endif
!
! --- ENERGIES
!
    if (lener) then
        if (lfeti) call u2mess('F', 'MECANONLINE5_2')
        if (lproj) call u2mess('F', 'MECANONLINE5_6')
        if (lmatdi) call u2mess('F', 'MECANONLINE5_8')
    endif
!
    call jedema()
end subroutine
