subroutine lc0059(fami, kpg, ksp, imate,&
                  compor, carcri, instam, instap, neps, epsm,&
                  deps, nsig, sigm, nvi, vim, option, angmas,&
                  sigp, vip, &
                  typmod, icomp, dsidep, codret)
!
implicit none
!
#include "asterfort/plasti.h"
#include "asterfort/srcomp.h"
#include "asterfort/utlcal.h"
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
! aslint: disable=W1504
!
    character(len=*), intent(in) :: fami
    integer, intent(in) :: kpg
    integer, intent(in) :: ksp
    integer, intent(in) :: imate
    character(len=16), intent(in) :: compor(*)
    real(kind=8), intent(in) :: carcri(*)
    real(kind=8), intent(in) :: instam
    real(kind=8), intent(in) :: instap
    integer, intent(in) :: neps
    integer, intent(in) :: nsig
    real(kind=8), intent(in) :: epsm(neps)
    real(kind=8), intent(in) :: deps(neps)
    real(kind=8), intent(in) :: sigm(nsig)
    integer, intent(in) :: nvi
    real(kind=8), intent(in) :: vim(nvi)
    character(len=16), intent(in) :: option
    real(kind=8), intent(in) :: angmas(3)
    real(kind=8), intent(out) :: sigp(nsig)
    real(kind=8), intent(out) :: vip(nvi)
    character(len=8), intent(in) :: typmod(*)
    integer, intent(in) :: icomp
    real(kind=8), intent(out) :: dsidep(6, 6)
    integer, intent(out) :: codret
!
! --------------------------------------------------------------------------------------------------
!
! Behaviour
!
! lkr
!
! --------------------------------------------------------------------------------------------------
!
! VARIABLES INTERNES DU MODELE :
!         1.  RXIP      : VARIABLE D ECROUISSAGE MECA. PLASTIQUE
!         2.  RGAMMAP   : DISTORSION PLASTIQUE
!         3.  RXIVP     : VARIABLE D ECROUISSAGE DU MECANISME 
!                         VISCOPLASTIQUE
!         4.  RGAMMAVP  : DISTORSION VISCOPLASTIQUE
!         5.  RINDICDIL : INDICATEUR DE DILATANCE : 1 SI DIL    0 SINON
!         6.  INDIVISC  : INDICATEUR DE VISCO.    : 1 SI VISCO. 0 SINON
!         7.  INDIPLAS  : INDICATEUR DE PLAST.    : 1 SI PLAST. 0 SINON
!         8.  RDVME     : DEF. VOL. ELAS. MECA.
!         9.  RDVTE     : DEF. VOL. ELAS. THER.
!         10. RDVPL     : DEF. VOL. PLAS.
!         11. RDVVP     : DEF. VOL. VISCOPLAS.
!         12. DOMAINES  : DOMAINE EN FONCTION DES VALEURS DE XIP
!                          ELAS                      ---> DOMAINE = 0
!                          PLAS PRE-PIC              ---> DOMAINE = 1
!                          PLAS POST-PIC AVT CLIVAGE ---> DOMAINE = 2
!                          PLAS POST-PIC AP CLIVAGE  ---> DOMAINE = 3
!
! --------------------------------------------------------------------------------------------------
!
    character(len=16) :: algo_inte
!
! --------------------------------------------------------------------------------------------------
! 
    call utlcal('VALE_NOM', algo_inte, carcri(6))
!
    if ((algo_inte(1:10).eq.'SPECIFIQUE') .or. (option(1:14).eq.'RIGI_MECA_TANG')) then
        call srcomp(typmod, imate, instam, instap, deps, sigm, vim,&
                    option, sigp, vip, dsidep, codret, nvi)
    else
        
        call plasti(fami, kpg, ksp, typmod, imate,&
                    compor, carcri, instam, instap, &
                    epsm, deps, sigm,&
                    vim, option, angmas, sigp, vip,&
                    dsidep, icomp, nvi, codret)
    endif

end subroutine
