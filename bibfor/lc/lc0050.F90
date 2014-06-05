subroutine lc0050(fami, kpg, ksp, ndim, typmod,&
                  imate, compor, crit, instam, instap,&
                  neps, epsm, deps, nsig, sigm,&
                  nvi, vim, option, angmas, nwkin,&
                  wkin, icomp, stress, statev, ndsde,&
                  dsidep, nwkout, wkout, codret)
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
! ======================================================================
!     BUT: INTERFACE POUR ROUTINE D'INTEGRATION LOI DE COMPORTEMENT UMAT
!       IN   FAMI    FAMILLE DE POINT DE GAUSS (RIGI,MASS,...)
!            KPG,KSP NUMERO DU (SOUS)POINT DE GAUSS
!            NDIM    DIMENSION DE L ESPACE (3D=3,2D=2,1D=1)
!            IMATE    ADRESSE DU MATERIAU CODE
!            COMPOR    COMPORTEMENT DE L ELEMENT
!                COMPOR(1) = RELATION DE COMPORTEMENT (UMAT)
!                COMPOR(2) = NB DE VARIABLES INTERNES
!                COMPOR(3) = TYPE DE DEFORMATION(PETIT,GDEF_LOG)
!            CRIT    CRITERES  LOCAUX, INUTILISES PAR UMAT
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
!        IN  WKIN  TABLEAUX DES ELEMENTS GEOMETRIQUES SPECIFIQUES
!            TYPMOD  TYPE DE MODELISATION (3D, AXIS, D_PLAN)
!            ICOMP   NUMERO DU SOUS-PAS DE TEMPS (CF. REDECE.F)
!            NVI     NOMBRE TOTAL DE VARIABLES INTERNES (+9 SI GDEF_HYP)
!       OUT  DSIDEP  MATRICE DE COMPORTEMENT TANGENT A T+DT OU T
!       OUT  CODRET  CODE-RETOUR = 0 SI OK, =1 SINON
! ======================================================================
! aslint: disable=W1504,W0104
    implicit none
#include "jeveux.h"
#include "asterc/r8nnem.h"
#include "asterc/umatwp.h"
#include "asterfort/assert.h"
#include "asterfort/infniv.h"
#include "asterfort/lceqvn.h"
#include "asterfort/lcicma.h"
#include "asterfort/matrot.h"
#include "asterfort/pmat.h"
#include "asterfort/r8inir.h"
#include "asterfort/tecael.h"
#include "asterfort/matumat.h"
#include "asterfort/varcumat.h"
#include "blas/daxpy.h"
#include "blas/dcopy.h"
#include "blas/dscal.h"
!
    integer ::      imate, ndim, kpg, ksp, codret, icomp, nvi, nprops
    integer ::      npropmax, ntens, ndi, nshr, i, nstatv, npt, noel, layer, npred
    integer ::      kspt, kstep, kinc, idbg, j, ifm, niv, nwkin, nwkout, ndsde
    parameter     ( npropmax = 197)
    parameter     ( npred = 8)
    integer ::      neps, nsig, iadzi, iazk24
    real(kind=8) :: angmas(*), crit(*)
    real(kind=8) :: instam, instap, drot(3, 3), dstran(9), props(npropmax)
    real(kind=8) :: epsm(6), deps(6), wkin(nwkin), wkout(nwkout)
    real(kind=8) :: sigm(6), stress(6), sse, spd, scd, time(2)
    real(kind=8) :: vim(*), statev(nvi)
    real(kind=8) :: predef(npred), dpred(npred)
    real(kind=8) :: ddsdde(36), dfgrd0(3, 3), dfgrd1(3, 3)
    real(kind=8) :: ddsddt(6), drplde(6), celent, stran(9), dsidep(6, 6)
    real(kind=8) :: dtime, temp, dtemp, coords(3), rpl, pnewdt, drpldt
    real(kind=8) :: depsth(6), epsth(6), rac2, usrac2, drott(3, 3),detf
    character(len=16) :: compor(*), option
    character(len=8) :: typmod(*)
    character(len=*) :: fami
    character(len=80) :: cmname
    common/tdim/  ntens  , ndi
!     POUR TECAEL
    character(len=128) :: nomlib
    character(len=16) :: nomsub
    integer :: ii, dimaki
!     DIMAKI = DIMENSION MAX DE LA LISTE DES RELATIONS KIT
    parameter (dimaki=9)
    data idbg/1/
!
!     NTENS  :  NB TOTAL DE COMPOSANTES TENSEURS
!     NDI    :  NB DE COMPOSANTES DIRECTES  TENSEURS
! ======================================================================
!
!     NUMERO D'ELEMENT SEULEMENT SI ON N'EST PAS DANS CALC_POINT_MAT
    noel=0
    if (compor(17) .ne. 'POINT') then
!        NUMERO D'ELEMENT
        call tecael(iadzi, iazk24)
        noel=zi(iadzi)
    endif
!
    ntens=2*ndim
    ndi=3
    nshr=ntens-ndi
    codret=0
    rac2=sqrt(2.d0)
    usrac2=rac2*0.5d0
    nprops = npropmax
!
!     IMPRESSIONS EVENTUELLES EN DEBUG
    call infniv(ifm, niv)
!
!     PARAMETRES UMAT STOCKES DANS 'KIT1-KIT9'
    do 10 ii = 1, dimaki-1
        nomlib(16*(ii-1)+1:16*ii) = compor(7+ii)
 10 continue
    nomsub = compor(7+dimaki)

!   LECTURE DES PROPRIETES MATERIAU (MOT-CLE UMAT DE DEFI_MATERIAU)   
    call matumat(fami, kpg, ksp, imate, ifm, niv, idbg, nprops, props)

!   LECTURE DES VARIABLES DE COMMANDE  ET DEFORMATIONS ASSOCIEES
    call varcumat(fami, kpg, ksp, imate, ifm, niv, idbg,  temp, dtemp, &
   &              predef, dpred, neps, epsth, depsth )
!
! CAS DES GRANDES DEFORMATIONS : ON VEUT F- ET F+
!
    if (neps .eq. 9) then
        call dcopy(neps, epsm, 1, dfgrd0, 1)
        if (option(1:9) .eq. 'RAPH_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
            call pmat(3, deps, dfgrd0, dfgrd1)
        else
            call dcopy(neps, dfgrd0, 1, dfgrd1, 1)
        endif
        call dcopy(neps, dfgrd0, 1, stran, 1)
        call dcopy(neps, dfgrd1, 1, dstran, 1)
!
    else if (neps.eq.6) then
!
! PETITES DEFORMATIONS : DEFORMATION - DEFORMATION THERMIQUE
        if (option(1:9) .eq. 'RAPH_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
            call dcopy(neps, deps, 1, dstran, 1)
            call daxpy(neps, -1.d0, depsth, 1, dstran,1)
! TRAITEMENT DES COMPOSANTES 4,5,6 : DANS UMAT, GAMMAXY,XZ,YZ
            call dscal(3, rac2, dstran(4), 1)
        else
            call r8inir(neps, 0.d0, dstran, 1)
        endif
!
        call dcopy(neps, epsm, 1, stran, 1)
        call daxpy(neps, -1.d0, epsth, 1, stran, 1)
        call dscal(3, rac2, stran(4), 1)
        call r8inir(9, 0.d0, dfgrd0, 1)
        call r8inir(9, 0.d0, dfgrd1, 1)
    else
        ASSERT(.false.)
    endif
!
    if (compor(3) .eq. 'GDEF_LOG') then
        nstatv=nvi-6
    elseif (compor(3) .eq. 'SIMO_MIEHE') then
        nstatv=nvi-6
    else
        nstatv=nvi
    endif
!
    time(1)=instap-instam
    time(2)=instam
    dtime=instap-instam
    cmname=compor(1)
!
    call r8inir(3, r8nnem(), coords, 1)
    call matrot(angmas, drott)
!
    do 100,i = 1,3
    do 90,j = 1,3
        drot(j,i) = drott(i,j)
 90 continue
100 continue
!
    celent=wkin(1)
    npt=kpg
    layer=1
    kspt=ksp
    kstep=icomp
    kinc=1
!     initialisations des arguments inutilises
    sse=0.d0
    spd=0.d0
    scd=0.d0
    rpl=0.d0
    call r8inir(6, 0.d0, ddsddt, 1)
    call r8inir(6, 0.d0, drplde, 1)
    drpldt=0.d0
!
    if (option(1:9) .eq. 'RAPH_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
        if ((niv.ge.2) .and. (idbg.eq.1)) then
            write(ifm,*)' '
            write(ifm,*)'AVANT APPEL UMAT, INSTANT=',time(2)+dtime
            write(ifm,*)'     NOM LIBRAIRIE : '//nomlib
            write(ifm,*)'       NOM ROUTINE : '//nomsub
            write(ifm,*)'NUMERO ELEMENT=',noel
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
        ddsdde(1)=-2.d0
    else if (option .eq. 'FULL_MECA_ELAS') then
        ddsdde(1)= 2.d0
    else if (option .eq. 'FULL_MECA') then
        ddsdde(1)= 4.d0
    else if (option .eq. 'RAPH_MECA') then
        ddsdde(1)= 0.d0
    endif
!
    if (option(1:9) .eq. 'RAPH_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
!
        call dcopy(nsig, sigm, 1, stress, 1)
        call dscal(3, usrac2, stress(4), 1)
!
        call lceqvn(nstatv, vim, statev)
!
        call umatwp(nomlib, nomsub, stress, statev, ddsdde,&
                    sse, spd, scd, rpl, ddsddt,&
                    drplde, drpldt, stran, dstran, time,&
                    dtime, temp, dtemp, predef, dpred,&
                    cmname, ndi, nshr, ntens, nstatv,&
                    props, nprops, coords, drot, pnewdt,&
                    celent, dfgrd0, dfgrd1, noel, npt,&
                    layer, kspt, kstep, kinc)
!
    else if (option(1:9).eq. 'RIGI_MECA') then
        call r8inir(6, 0.d0, dstran, 1)
        call umatwp(nomlib, nomsub, sigm, vim, ddsdde,&
                    sse, spd, scd, rpl, ddsddt,&
                    drplde, drpldt, stran, dstran, time,&
                    dtime, temp, dtemp, predef, dpred,&
                    cmname, ndi, nshr, ntens, nstatv,&
                    props, nprops, coords, drot, pnewdt,&
                    celent, dfgrd0, dfgrd1, noel, npt,&
                    layer, kspt, kstep, kinc)
    endif
!
    if (option(1:9) .eq. 'RAPH_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
        if ((niv.ge.2) .and. (idbg.eq.1)) then
            write(ifm,*)' '
            write(ifm,*)'APRES APPEL UMAT, STRESS='
            write(ifm,'(6(1X,E11.4))') (stress(i),i=1,ntens)
            write(ifm,*)'APRES APPEL UMAT, STATEV='
            write(ifm,'(10(1X,E11.4))')(statev(i),i=1,nstatv)
        endif
    endif
!
    if (option(1:9) .eq. 'RAPH_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
        call dscal(3, rac2, stress(4), 1)
        if (compor(3) .eq. 'SIMO_MIEHE') then
! transformation cauchy kirchhoff
           call lcdetf(3, dfgrd1, detf)  
           call dscal(3, detf, stress, 1)
        endif
    endif
!
    if (option(1:9) .eq. 'RIGI_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
        call r8inir(36, 0.d0, dsidep, 1)
        call lcicma(ddsdde, ntens, ntens, ntens, ntens,  1, 1, dsidep, 6, 6, 1, 1)
        do 40 i = 1, 6
            do 40 j = 4, 6
                dsidep(i,j) = dsidep(i,j)*rac2
 40         continue
        do 50 i = 4, 6
            do 50 j = 1, 6
                dsidep(i,j) = dsidep(i,j)*rac2
 50         continue
        if ((niv.ge.2) .and. (idbg.eq.1)) then
            write(ifm,*)'APRES APPEL UMAT,OPERATEUR TANGENT DSIDEP='
            do 60 i = 1, 6
                write(ifm,'(6(1X,E11.4))') (dsidep(i,j),j=1,6)
 60         continue
        endif
    endif
!
    if (pnewdt .lt. 0.99d0) codret=1
    idbg=0
!
end subroutine
