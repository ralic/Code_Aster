subroutine nmiclb(fami, kpg, ksp, option, compor,&
                  imate, xlong0, a, tmoins, tplus,&
                  dlong0, effnom, vim, effnop, vip,&
                  klv, fono, epsm, crildc, codret)
! ----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
! ------------------------------------------------------------------
    implicit none
#include "asterf_types.h"
#include "asterfort/lcimpl.h"
#include "asterfort/nm1dci.h"
#include "asterfort/nm1dco.h"
#include "asterfort/nm1dis.h"
#include "asterfort/r8inir.h"
#include "asterfort/rcvalb.h"
#include "asterfort/utmess.h"
#include "asterfort/verift.h"
    integer :: imate, neq, nbt, kpg, ksp, codret
    parameter (neq=6,nbt=21)
!
    character(len=16) :: compor(*), option
    character(len=*) :: fami
    real(kind=8) :: xlong0, a
    real(kind=8) :: em, ep
    real(kind=8) :: dlong0, crildc(3)
    real(kind=8) :: effnom, vim(*)
    real(kind=8) :: effnop, vip(*), fono(neq), klv(nbt)
    integer :: codres(1)
    real(kind=8) :: dsde, epsm
! -------------------------------------------------------------------
!
!    TRAITEMENT DE LA RELATION DE COMPORTEMENT -ELASTOPLASTICITE-
!    ECROUISSAGE ISOTROPE ET CINEMATIQUE- LINEAIRE - VON MISES-
!    POUR UN MODELE BARRE ELEMENT MECA_BARRE
!
! -------------------------------------------------------------------
! IN  : IMATE : POINTEUR MATERIAU CODE
!       COMPOR : LOI DE COMPORTEMENT
!       XLONG0 : LONGUEUR DE L'ELEMENT DE BARRE AU REPOS
!       A      : SECTION DE LA BARRE
!       TMOINS : INSTANT PRECEDENT
!       TPLUS  : INSTANT COURANT
!       DLONG0 : INCREMENT D'ALLONGEMENT DE L'ELEMENT
!       EFFNOM : EFFORT NORMAL PRECEDENT
!       TREF   : TEMPERATURE DE REFERENCE
!       TEMPM  : TEMPERATURE IMPOSEE A L'INSTANT PRECEDENT
!       TEMPP  : TEMPERATURE IMPOSEE A L'INSTANT COURANT
!       OPTION : OPTION DEMANDEE (R_M_T,FULL OU RAPH_MECA)
! OUT : EFFNOP : CONTRAINTE A L'INSTANT ACTUEL
!       VIP    : VARIABLE INTERNE A L'INSTANT ACTUEL
!       FONO   : FORCES NODALES COURANTES
!       KLV    : MATRICE TANGENTE
!
!----------VARIABLES LOCALES
!
    real(kind=8) :: sigm, deps, depsth, depsm, tmoins, tplus
    real(kind=8) :: sigp, xrig, val(1)
    aster_logical :: isot, cine, elas, corr, impl, isotli
!
!
!----------INITIALISATIONS
!
    elas = .false.
    isot = .false.
    cine = .false.
    corr = .false.
    impl = .false.
    isotli = .false.
    if (compor(1) .eq. 'ELAS') then
        elas = .true.
    else if ((compor(1).eq.'VMIS_ISOT_LINE') .or. (compor(1).eq.'VMIS_ISOT_TRAC')) then
        isot = .true.
        if (compor(1) .eq. 'VMIS_ISOT_LINE') then
            isotli = .true.
        endif
        if (crildc(2) .eq. 9) then
            impl = .true.
        endif
        if (impl .and. (.not.isotli)) then
            call utmess('F', 'ELEMENTS5_50')
        endif
    else if (compor(1).eq.'VMIS_CINE_LINE') then
        cine = .true.
    else if (compor(1).eq.'CORR_ACIER') then
        corr = .true.
    endif
!
    call r8inir(nbt, 0.d0, klv, 1)
    call r8inir(neq, 0.d0, fono, 1)
!
!----------RECUPERATION DES CARACTERISTIQUES
!
    deps = dlong0/xlong0
    sigm = effnom/a
!
! --- CARACTERISTIQUES ELASTIQUES A TMOINS
!
    call rcvalb(fami, kpg, ksp, '-', imate,&
                ' ', 'ELAS', 0, ' ', [0.d0],&
                1, 'E', val, codres, 1)
    em=val(1)            
!
! --- CARACTERISTIQUES ELASTIQUES A TPLUS
!
    call rcvalb(fami, kpg, ksp, '+', imate,&
                ' ', 'ELAS', 0, ' ', [0.d0],&
                1, 'E', val, codres, 1)
    ep=val(1) 
!
!
    if (isot .and. (.not.impl)) then
        call verift(fami, kpg, ksp, 'T', imate,&
                    epsth=depsth)
        depsm=deps-depsth
        call nm1dis(fami, kpg, ksp, imate, em,&
                    ep, sigm, depsm, vim, option,&
                    compor, ' ', sigp, vip, dsde)
    else if (cine) then
        call verift(fami, kpg, ksp, 'T', imate,&
                    epsth=depsth)
        depsm = deps-depsth
        call nm1dci(fami, kpg, ksp, imate, em,&
                    ep, sigm, depsm, vim, option,&
                    ' ', sigp, vip, dsde)
    else if (elas) then
        dsde = ep
        vip(1) = 0.d0
        call verift(fami, kpg, ksp, 'T', imate,&
                    epsth=depsth)
        sigp = ep* (sigm/em+deps-depsth)
    else if (corr) then
        call nm1dco(fami, kpg, ksp, option, imate,&
                    ' ', ep, sigm, epsm, deps,&
                    vim, sigp, vip, dsde, crildc,&
                    codret)
    else if (impl) then
        call lcimpl(fami, kpg, ksp, imate, em,&
                    ep, sigm, tmoins, tplus, deps,&
                    vim, option, sigp, vip, dsde)
    else
        call utmess('F', 'ALGORITH6_87')
    endif
!
! --- CALCUL DU COEFFICIENT NON NUL DE LA MATRICE TANGENTE
!
    if (option(1:10) .eq. 'RIGI_MECA_' .or. option(1:9) .eq. 'FULL_MECA') then
!
        xrig = dsde*a/xlong0
        klv(1) = xrig
        klv(7) = -xrig
        klv(10) = xrig
    endif
!
! --- CALCUL DES FORCES NODALES
!
    if (option(1:14) .eq. 'RAPH_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
        effnop = sigp*a
        fono(1) = -effnop
        fono(4) = effnop
    endif
!      EFFNOP = SIGP*A
!
    if (option(1:16) .eq. 'RIGI_MECA_IMPLEX') then
        if ((.not.impl) .and. (.not.elas)) then
            call utmess('F', 'ELEMENTS5_49')
        endif
        effnop = sigp*a
        fono(1) = -effnop
        fono(4) = effnop
    endif
!
! -------------------------------------------------------------
!
end subroutine
