subroutine exfonc(fonact, parmet, method, solveu, defico,&
                  sddyna, mate)
!
    implicit none
!
#include "jeveux.h"
#include "asterfort/cfdisl.h"
#include "asterfort/getvtx.h"
#include "asterfort/infdbg.h"
#include "asterfort/isfonc.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/ndynlo.h"
#include "asterfort/utmess.h"
#include "asterfort/dismoi.h"
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
    integer, intent(in) :: fonact(*)
    character(len=19), intent(in) :: solveu
    character(len=19), intent(in) :: sddyna
    character(len=24), intent(in) :: defico
    real(kind=8), intent(in) :: parmet(*)
    character(len=16), intent(in) :: method(*)
    character(len=24), intent(in) :: mate
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Init
!
! No compatible functionnalities
!
! --------------------------------------------------------------------------------------------------
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
    integer :: jslvk, n1
    logical :: lcont, lallv, lctcc, lctcd, lpena, leltc
    logical :: lpilo, lreli, lmacr, lunil
    logical :: lmvib, lflam, lexpl, lxfem, lmodim
    logical :: lrcmk, lgcpc, lpetsc, lamg, lsyme, limpex
    logical :: londe, ldyna, lgrot, ltheta, lnkry
    logical :: lener, lproj, lmatdi, lldsp, lctgcp, lcomp
    integer :: ifm, niv
    character(len=24) :: typilo, typrel, metres
    character(len=3)  :: mfdet
!
! ---------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- FONCTIONNALITES ACTIVEES
!
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
    lcomp = isfonc(fonact,'RESI_COMP')
    lgcpc = isfonc(fonact,'GCPC')
    lpetsc = isfonc(fonact,'PETSC')
    lldsp = isfonc(fonact,'LDLT_SP')
!
    call jeveuo(solveu//'.SLVK', 'E', jslvk)
    metres = zk24(jslvk)
!
! --- INITIALISATIONS
!
    reincr = nint(parmet(1))
!
! --- TYPE DE SOLVEUR
!
    lrcmk = zk24(jslvk-1+4) .eq. 'RCMK'
    lamg = ((zk24(jslvk-1+2).eq.'ML') .or. (zk24(jslvk-1+2).eq.'BOOMER'))
    lsyme = zk24(jslvk-1+5).eq.'OUI'
!
! --- CONTACT DISCRET
!
    if (lctcd) then
        lmodim = cfdisl(defico,'MODI_MATR_GLOB')
        lallv = cfdisl(defico,'ALL_VERIF')
        lpena = cfdisl(defico,'CONT_PENA')
        lctgcp = cfdisl(defico,'CONT_GCP')
        if (lpilo) then
            call utmess('F', 'MECANONLINE_43')
        endif
        if (lreli .and. (.not.lallv)) then
            call utmess('A', 'MECANONLINE3_89')
        endif
        if (lgcpc .or. lpetsc) then
            if (.not.(lallv.or.lpena.or.lctgcp)) then
                call utmess('F', 'MECANONLINE3_90', sk=metres)
            endif
            if (lctgcp .and. .not.lldsp) then
                call utmess('F', 'MECANONLINE3_88')
            endif
        endif
        if (reincr .eq. 0) then
            if (lmodim) then
                call utmess('F', 'CONTACT_88')
            endif
        endif
!       ON FORCE SYME='OUI' AVEC LE CONTACT DISCRET
        if (.not.(lsyme.or.lallv)) then
            zk24(jslvk+4) = 'OUI'
            call utmess('A', 'CONTACT_1')
        endif
        if ((lmvib.or.lflam) .and. lmodim) then
            call utmess('F', 'MECANONLINE5_14')
        endif
    endif
!
! --- CONTACT CONTINU
!
    if (lctcc) then
        if (lpilo .and. (.not.lxfem)) then
!         LEVEE D INTERDICTION TEMPORAIRE POUR X-FEM
            call utmess('F', 'MECANONLINE3_92')
        endif
        if (lreli) then
            call utmess('F', 'MECANONLINE3_91')
        endif
        if (lrcmk) then
            call utmess('F', 'MECANONLINE3_93', sk=zk24(jslvk))
        endif
        if (lamg) then
            call utmess('F', 'MECANONLINE3_97', sk=zk24(jslvk-1+2))
        endif
        if (lpetsc .and. lmatdi) then
            call utmess('F', 'MECANONLINE3_98')
        endif
    endif
!
! --- LIAISON UNILATERALE
!
    if (lunil) then
        if (lpilo) then
            call utmess('F', 'MECANONLINE3_94')
        endif
        if (lreli) then
            call utmess('A', 'MECANONLINE3_95')
        endif
        if (lgcpc .or. lpetsc) then
            call utmess('F', 'MECANONLINE3_96', sk=zk24(jslvk))
        endif
!       ON FORCE SYME='OUI' AVEC LIAISON_UNILATER
        if (.not.lsyme) then
            zk24(jslvk+4)='OUI'
            call utmess('A', 'UNILATER_1')
        endif
    endif
!
! --- CALCUL DE MODES/FLAMBEMENT: PAS GCPC/PETSC
!
    if (lmvib .or. lflam) then
        if (lgcpc .or. lpetsc) then
            call utmess('F', 'FACTOR_52', sk=zk24(jslvk))
        endif
        if (leltc) then
            call utmess('F', 'MECANONLINE5_3')
        endif
    endif
!
! --- EXPLICITE
!
    if (lexpl) then
        if (lcont) then
            call utmess('F', 'MECANONLINE5_22')
        endif
        if (lunil) then
            call utmess('F', 'MECANONLINE5_23')
        endif
        if (lgrot) then
            call utmess('A', 'MECANONLINE5_24')
        endif
    endif
!
! --- DYNAMIQUE
!
    if (ldyna) then
        if (lcomp) then
            call utmess('F', 'MECANONLINE5_53')
        endif
        if (lpilo) then
            call utmess('F', 'MECANONLINE5_25')
        endif
        if (ltheta) then
            if (lgrot) then
                call utmess('F', 'MECANONLINE5_27')
            endif
        endif
        if (lxfem) then
            call utmess('F', 'MECANONLINE5_28')
        endif
        if (limpex) then
            call utmess('F', 'MECANONLINE5_33')
        endif
    endif
!
! --- PILOTAGE
!
    if (lpilo) then
        call getvtx('PILOTAGE', 'TYPE', iocc=1, scal=typilo, nbret=n1)
        if (lreli) then
            if (typilo .eq. 'DDL_IMPO') then
                call utmess('F', 'MECANONLINE5_34')
            endif
        endif
        if ((method(5).eq.'DEPL_CALCULE') .or. (method(5) .eq.'EXTRAPOLE')) then
            call utmess('F', 'MECANONLINE5_36')
        endif
!
!       --- VERIFICATION QUE LES VARIABLES DE COMMANDE NE DEPENDENT PAS DU TEMPS
        call dismoi('VARC_F_INST', mate, 'CHAM_MATER', repk=mfdet)
        if (mfdet.eq.'OUI') then
           call utmess('F', 'CALCULEL2_58', nk=1, valk=mate(1:8))
        endif

    endif
    if (lreli) then
        call getvtx('RECH_LINEAIRE', 'METHODE', iocc=1, scal=typrel, nbret=n1)
        if ((typrel.eq.'PILOTAGE') .and. (.not.lpilo)) then
            call utmess('F', 'MECANONLINE5_35')
        endif
    endif
!
! --- NEWTON_KRYLOV
!
    if (lnkry) then
        if (lpilo) then
            call utmess('F', 'MECANONLINE5_48')
        endif
        if ((.not.lgcpc) .and. (.not.lpetsc)) then
            call utmess('F', 'MECANONLINE5_51')
        endif
    endif
!
! --- ENERGIES
!
    if (lener) then
        if (lproj) then
            call utmess('F', 'MECANONLINE5_6')
        endif
        if (lmatdi) then
            call utmess('F', 'MECANONLINE5_8')
        endif
    endif
!
    call jedema()
end subroutine
