subroutine lc1058(fami, kpg, ksp, ndim, typmod,&
                  imate, compor, crit, instam, instap,&
                  neps, epsm, deps, nsig, sigm,&
                  nvi, vim, option, angmas,&
                  icomp, stress, statev, dsidep,&
                  codret)
use calcul_module, only : ca_iactif_
!
implicit none
!
#include "jeveux.h"
#include "asterc/mfront_behaviour.h"
#include "asterc/mfront_get_external_state_variable.h"
#include "asterc/r8nnem.h"
#include "asterfort/assert.h"
#include "asterfort/get_elas_para.h"
#include "asterfort/infniv.h"
#include "asterfort/lceqvn.h"
#include "asterfort/lcicma.h"
#include "asterfort/lcsmelas.h"
#include "asterfort/matrot.h"
#include "asterfort/pmat.h"
#include "asterfort/tecael.h"
#include "asterfort/mfront_get_mater_value.h"
#include "asterfort/mfront_varc.h"
#include "asterfort/lcdetf.h"
#include "asterfort/utmess.h"
#include "blas/daxpy.h"
#include "blas/dcopy.h"
#include "blas/dscal.h"
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

! ======================================================================
!     BUT: INTERFACE POUR ROUTINE D'INTEGRATION LOI DE COMPORTEMENT MFRONT
!       IN   FAMI    FAMILLE DE POINT DE GAUSS (RIGI,MASS,...)
!            KPG,KSP NUMERO DU (SOUS)POINT DE GAUSS
!            NDIM    DIMENSION DE L ESPACE (3D=3,2D=2,1D=1)
!            IMATE    ADRESSE DU MATERIAU CODE
!            COMPOR    COMPORTEMENT DE L ELEMENT
!                COMPOR(1) = RELATION DE COMPORTEMENT (MFRONT)
!                COMPOR(2) = NB DE VARIABLES INTERNES
!                COMPOR(3) = TYPE DE DEFORMATION(PETIT,GDEF_LOG)
!            CRIT    CRITERES  LOCAUX, INUTILISES PAR MFRONT
!            INSTAM   INSTANT T
!            INSTAP   INSTANT T+DT
!            EPSM   DEFORMATION TOTALE A T EVENTUELLEMENT TOURNEE
!                   DANS LE REPERE COROTATIONNEL SI GDEF_LOG
!            DEPS   INCREMENT DE DEFORMATION EVENTUELLEMENT TOURNEE
!                   DANS LE REPERE COROTATIONNEL SI GDEF_LOG
!            SIGM   CONTRAINTE A T EVENTUELLEMENT TOURNEE...
!            VIM    VARIABLES INTERNES A T + INDICATEUR ETAT T
! ATTENTION : SI MODELE CINEMATIQUE ET GDEF, MODIFIER AUSSI VICIN0.F
!            OPTION     OPTION DE CALCUL A FAIRE
!                          'RIGI_MECA_TANG'> DSIDEP(T)
!                          'FULL_MECA'     > DSIDEP(T+DT) , SIG(T+DT)
!                          'RAPH_MECA'     > SIG(T+DT)
!            ANGMAS  ANGLES DE ROTATION DU REPERE LOCAL, CF. MASSIF
!       OUT  STRESS    CONTRAINTE A T+DT
! !!!!        ATTENTION : ZONE MEMOIRE NON DEFINIE SI RIGI_MECA_TANG
!       OUT  STATEV  VARIABLES INTERNES A T+DT
! !!!!        ATTENTION : ZONE MEMOIRE NON DEFINIE SI RIGI_MECA_TANG
!            TYPMOD  TYPE DE MODELISATION (3D, AXIS, D_PLAN)
!            ICOMP   NUMERO DU SOUS-PAS DE TEMPS (CF. REDECE.F)
!            NVI     NOMBRE TOTAL DE VARIABLES INTERNES (+9 SI GDEF_HYP)
!       OUT  DSIDEP  MATRICE DE COMPORTEMENT TANGENT A T+DT OU T
!       OUT  CODRET  CODE-RETOUR = 0 SI OK, =1 SINON
! ======================================================================


!
    integer ::      imate, ndim, kpg, ksp, codret, icomp, nvi, nprops, czm, nbvarc
    integer ::      ntens, ndi, nshr, i, nstatv, npt, nume_elem, layer
    integer ::      kspt, kstep, kinc, j, ifm, niv, elas_id
    integer ::      pfcmfr
    integer ::      nummod
    integer :: idbg = 1
    integer, parameter :: npropmax = 197
    integer, parameter :: npred = 8
    integer ::      neps, nsig, iadzi, iazk24
    real(kind=8) :: angmas(*), crit(*)
    real(kind=8) :: instam, instap, drot(3, 3), dstran(9), props(npropmax)
    real(kind=8) :: epsm(6), deps(6), young, nu
    real(kind=8) :: sigm(6), stress(6), sse, spd, scd, time(2)
    real(kind=8) :: vim(*), statev(nvi)
    real(kind=8) :: predef(npred), dpred(npred)
    real(kind=8) :: ddsdde(54), dfgrd0(3, 3), dfgrd1(3, 3)
    real(kind=8) :: ddsddt(6), drplde(6), stran(9), dsidep(6, 6)
    real(kind=8) :: dtime, temp, dtemp, coords(3), rpl, pnewdt, drpldt
    real(kind=8) :: depsth(6), epsth(6), rac2, usrac2, drott(3, 3),detf
    character(len=16) :: compor(*), option
    character(len=8) :: typmod(*), lvarc(npred)
    character(len=*) :: fami
    common/tdim/  ntens  , ndi
!
!     NTENS  :  NB TOTAL DE COMPOSANTES TENSEURS
!     NDI    :  NB DE COMPOSANTES DIRECTES  TENSEURS
! ======================================================================

!
    call infniv(ifm, niv)

!
! - Initializations
!
    ntens       = 2*ndim
    ndi         = 3
    nshr        = ntens-ndi
    codret      = 0
    rac2        = sqrt(2.d0)
    usrac2      = rac2*0.5d0
    nprops      = npropmax
    czm         = 0
    stran(:)    = 0.d0
    dstran(:)   = 0.d0
    dfgrd0(:,:) = 0.d0
    dfgrd1(:,:) = 0.d0
    coords(:)   = r8nnem()
!
! - Get current index of cell
!
    nume_elem = 0
    if (ca_iactif_ .ne. 2) then
        call tecael(iadzi, iazk24, noms=0)
        nume_elem=zi(iadzi)
    endif
!
! - Get material properties
!
    call mfront_get_mater_value(fami, kpg, ksp, imate, ifm, &
                                niv, idbg, compor(1), nprops, props)
!
! - Get type of modelization
!
    if ( typmod(1)(1:4).eq.'AXIS' ) then
        nummod = 4
    else if ( typmod(1)(1:6).eq.'C_PLAN' ) then
        nummod = 5
    else if ( typmod(1)(1:6).eq.'D_PLAN' ) then
        nummod = 6
    else if ( typmod(1)(1:2).eq.'3D' ) then
        nummod = 3
    else
        ASSERT(.false.)
    endif
!
! - Prepare external state variables
!
    call mfront_get_external_state_variable(int(crit(14)), int(crit(15)), lvarc, nbvarc)
    ASSERT(nbvarc.le.npred)
    call mfront_varc(fami, kpg, ksp, imate, ifm, niv, idbg, lvarc, nbvarc, &
                     temp, dtemp, predef, dpred, neps, epsth, depsth )
!
! - Prepare strains
!
    ASSERT(neps .eq. 9)
    call dcopy(neps, epsm, 1, dfgrd0, 1)
    if (option(1:9) .eq. 'RAPH_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
        call pmat(3, deps, dfgrd0, dfgrd1)
    else
        call dcopy(neps, dfgrd0, 1, dfgrd1, 1)
    endif
    call dcopy(neps, dfgrd0, 1, stran, 1)
    call dcopy(neps, dfgrd1, 1, dstran, 1)
!
! - Real number of internal variable: SIMO_MIEHE
!
    nstatv=nvi-6
!
! - Time parameters
!
    time(1) = instap-instam
    time(2) = instam
    dtime   = instap-instam
!
! - Anistropic case
!
    call matrot(angmas, drott)
    do i = 1,3
        do j = 1,3
            drot(j,i) = drott(i,j)
        end do
    end do
!
    npt=kpg
    layer=1
    kspt=ksp
    kstep=icomp
    kinc=1
!
! - Unused arguments for MFront
!
    sse       = 0.d0
    spd       = 0.d0
    scd       = 0.d0
    rpl       = 0.d0
    ddsddt(:) = 0.d0
    drplde(:) = 0.d0
    drpldt    = 0.d0
!
    if (option(1:9) .eq. 'RAPH_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
        if ((niv.ge.2) .and. (idbg.eq.1)) then
            write(ifm,*)' '
            write(ifm,*)'AVANT APPEL MFRONT, INSTANT=',time(2)+dtime
            write(ifm,*)'NUMERO ELEMENT=',nume_elem
            write(ifm,*)'DEFORMATIONS INSTANT PRECEDENT STRAN='
            write(ifm,'(6(1X,E11.4))') (stran(i),i=1,ntens)
            write(ifm,*)'ACCROISSEMENT DE DEFORMATIONS DSTRAN='
            write(ifm,'(6(1X,E11.4))') (dstran(i),i=1,ntens)
            write(ifm,*)'CONTRAINTES INSTANT PRECEDENT STRESS='
            write(ifm,'(6(1X,E11.4))') (sigm(i),i=1,ntens)
            write(ifm,*)'NVI=',nstatv,' VARIABLES INTERNES STATEV='
            write(ifm,'(10(1X,E11.4))') (vim(i),i=1,nstatv)
        endif
    endif
!
    pnewdt=1.d0
!
!   pour MFRONT ddsdde(1)= type de matrice tangente
!   ddsdde(1) <0 : matrice de prédiction
!   -1, matrice elastique initiale (sans endommagement)
!   -2, matrice secante (avec endommagement)
!   -3, matrice tangente.
!   ddsdde(1) >0 : matrice tangente (FULL_MECA, FULL_MECA_ELAS)
!    1 matrice elastique initiale (sans endommagement)
!    2 matrice secante (avec endommagement)
!    3 matrice tangente
!    4 matrice tangente cohérente
!
    ddsdde=1.d0
    if (option .eq. 'RIGI_MECA_TANG') then
!        ddsdde(1)=-3.d0 disponible a partir de la version 2.584 de mfront
        ddsdde(1)=4.d0
    else if (option .eq. 'RIGI_MECA_ELAS') then
!        ddsdde(1)=-2.d0
        ddsdde(1)=1.d0
    else if (option .eq. 'FULL_MECA_ELAS') then
        ddsdde(1)= 2.d0
    else if (option .eq. 'FULL_MECA') then
        ddsdde(1)= 4.d0
    else if (option .eq. 'RAPH_MECA') then
        ddsdde(1)= 0.d0
    endif
!
!   Adresse de la fonction mfront a appeler
    pfcmfr = int(crit(16))
    if (option(1:9) .eq. 'RAPH_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
!
        call dcopy(nsig, sigm, 1, stress, 1)
        call dscal(3, usrac2, stress(4), 1)
!
        call lceqvn(nstatv, vim, statev)
!
        call mfront_behaviour(pfcmfr, stress, statev, ddsdde,&
                              stran, dstran, dtime, temp, dtemp,&
                              predef, dpred, ntens, nstatv, props,&
                              nprops, drot, pnewdt, nummod)
!
    else if (option(1:9).eq. 'RIGI_MECA') then
!        call mfront_behaviour(pfcmfr, sigm, vim, ddsdde, stran,&
!                              dstran, dtime, temp, dtemp, predef,&
!                              dpred, ntens, nstatv, props, nprops,&
!                              drot, pnewdt, nummod)
        call get_elas_para(fami     , imate, '-', kpg, ksp, elas_id,&
                               e = young, nu = nu)
        call lcsmelas(stran, dstran , ddsdde,&
                      nmat = 0, young_ = young, nu_ = nu)
    endif
!
    if (option(1:9) .eq. 'RAPH_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
        if ((niv.ge.2) .and. (idbg.eq.1)) then
            write(ifm,*)' '
            write(ifm,*)'APRES APPEL MFRONT, STRESS='
            write(ifm,'(6(1X,E11.4))') (stress(i),i=1,ntens)
            write(ifm,*)'APRES APPEL MFRONT, STATEV='
            write(ifm,'(10(1X,E11.4))')(statev(i),i=1,nstatv)
        endif
    endif
!
    if (option(1:9) .eq. 'RAPH_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
        call dscal(3, rac2, stress(4), 1)
! transformation cauchy kirchhoff
        call lcdetf(3, dfgrd1, detf)
        call dscal(3, detf, stress, 1)
    endif
!
    if (option(1:9) .eq. 'RIGI_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
        call dcopy(54, ddsdde, 1, dsidep, 1)  
    endif
!
    if (pnewdt .lt. 0.0d0) then
        if (pnewdt .lt. -0.99d0 .and. pnewdt .gt. -1.01d0) then
            codret=1
        else if (pnewdt .lt. -1.99d0 .and. pnewdt .gt. -2.01d0) then
            call utmess('F', 'MFRONT_1')
        else if (pnewdt .lt. -2.99d0 .and. pnewdt .gt. -3.01d0) then
            call utmess('F', 'MFRONT_2')
        else if (pnewdt .lt. -3.99d0 .and. pnewdt .gt. -4.01d0) then
            codret=1
        else
            call utmess('F', 'MFRONT_3')
        endif
    endif
    idbg=0
!
end subroutine
