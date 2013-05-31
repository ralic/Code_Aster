subroutine lcmmjp(mod, nmat, mater, timed, timef,&
                  comp, nbcomm, cpmono, pgl, nfs,&
                  nsg, toutms, hsr, nr, nvi,&
                  itmax, toler, vinf, vind, dsde,&
                  drdy, option, iret)
! aslint: disable=W1306,W1504
    implicit none
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: jean-michel.proix at edf.fr
!     ----------------------------------------------------------------
!     COMPORTEMENT MONOCRISTALLIN
!                :  MATRICE SYMETRIQUE DE COMPORTEMENT TANGENT
!                   COHERENT A T+DT? en hpp et gdef
!     ----------------------------------------------------------------
!     IN  MOD    :  TYPE DE MODELISATION
!         NMAT   :  DIMENSION MATER
!         MATER  :  COEFFICIENTS MATERIAU
!         TIMED  :  ISTANT PRECEDENT
!         TIMEF  :  INSTANT ACTUEL
!         COMP   :  NOM COMPORTEMENT
!         NBCOMM :  INCIDES DES COEF MATERIAU
!         CPMONO :  NOM DES COMPORTEMENTS
!         PGL    :  MATRICE DE PASSAGE
!         TOUTMS :  TENSEURS D'ORIENTATION
!         HSR    :  MATRICE D'INTERACTION
!         NVI    :  NOMBRE DE VARIABLES INTERNES
!         NR     :  DIMENSION DU SYSTEME A RESOUDRE
!         ITMAX  :  ITER_INTE_MAXI
!         TOLER  :  RESI_INTE_RELA
!         VIND   :  VARIABLES INTERNES A L'INSTANT PRECEDENT T
!         VINF   :  VARIABLES INTERNES A T+DT
!         DRDY   :  MATRICE JACOBIENNE
!         OPTION :  OPTION DE CALCUL MATRICE TANGENTE
!     OUT DSDE   :  MATRICE DE COMPORTEMENT TANGENT = DSIG/DEPS
!                   DSDE = INVERSE(Y0-Y1*INVERSE(Y3)*Y2)
!         IRET   :  CODE RETOUR
!     ----------------------------------------------------------------
    include 'asterfort/lcmmja.h'
    include 'asterfort/lcmmkg.h'
    include 'asterfort/mgauss.h'
    include 'asterfort/promat.h'
    include 'asterfort/r8inir.h'
    include 'blas/dcopy.h'
    integer :: ndt, ndi, nmat, nvi, itmax, nfs, nsg
    integer :: k, j, nr, iret, ns, nbcomm(nmat, 3)
! DIMENSIONNEMENT DYNAMIQUE
    real(kind=8) :: drdy(nr, nr), dsde(6, *), kyl(6, 6), det, i6(6, 6)
    real(kind=8) :: zinv(6, 6)
    real(kind=8) :: toler, mater(*), yf(nr), dy(nr), un, zero, timed, timef
    real(kind=8) :: pgl(3, 3)
    real(kind=8) :: z0(6, 6), z1(6, (nr-ndt))
    real(kind=8) :: z2((nr-ndt), 6), z3((nr-ndt), (nr-ndt))
    real(kind=8) :: toutms(nfs, nsg, 6), hsr(nsg, nsg)
    real(kind=8) :: vind(*), vinf(*), df(9), yd(nr)
    character(len=8) :: mod
    character(len=16) :: comp(*), option
    character(len=24) :: cpmono(5*nmat+1)
    parameter       ( un   =  1.d0   )
    parameter       ( zero =  0.d0   )
    common /tdim/ ndt,ndi
    integer :: irr, decirr, nbsyst, decal, gdef
    common/polycr/irr,decirr,nbsyst,decal,gdef
    data  i6        /un     , zero  , zero  , zero  ,zero  ,zero,&
     &                 zero   , un    , zero  , zero  ,zero  ,zero,&
     &                 zero   , zero  , un    , zero  ,zero  ,zero,&
     &                 zero   , zero  , zero  , un    ,zero  ,zero,&
     &                 zero   , zero  , zero  , zero  ,un    ,zero,&
     &                 zero   , zero  , zero  , zero  ,zero  ,un/
!
! -  INITIALISATION
!
    ns=nr-ndt
    iret=0
!
!     RECALCUL DE LA DERNIERE MATRICE JACOBIENNE
    if (option .eq. 'RIGI_MECA_TANG') then
        call r8inir(nr, 0.d0, dy, 1)
        call r8inir(9, 0.d0, df, 1)
        call r8inir(nr, 0.d0, yf, 1)
        call r8inir(nr, 0.d0, yd, 1)
        call r8inir(nvi, 0.d0, vind, 1)
        call lcmmja(comp, mod, nmat, mater, timed,&
                    timef, itmax, toler, nbcomm, cpmono,&
                    pgl, nfs, nsg, toutms, hsr,&
                    nr, nvi, vind, df, yf,&
                    yd, dy, drdy, iret)
        if (iret .gt. 0) goto 9999
    endif
!
! - RECUPERER LES SOUS-MATRICES BLOC
!
    do 101 k = 1, 6
        do 101 j = 1, 6
            z0(k,j)=drdy(k,j)
101      continue
    do 201 k = 1, 6
        do 201 j = 1, ns
            z1(k,j)=drdy(k,ndt+j)
201      continue
!
    do 301 k = 1, ns
        do 301 j = 1, 6
            z2(k,j)=drdy(ndt+k,j)
301      continue
    do 401 k = 1, ns
        do 401 j = 1, ns
            z3(k,j)=drdy(ndt+k,ndt+j)
401      continue
!     Z2=INVERSE(Z3)*Z2
!     CALL MGAUSS ('NCSP',Z3, Z2, NS, NS, 6, DET, IRET )
    call mgauss('NCWP', z3, z2, ns, ns,&
                6, det, iret)
    if (iret .gt. 0) goto 9999
!
!     KYL=Z1*INVERSE(Z3)*Z2
    call promat(z1, 6, 6, ns, z2,&
                ns, ns, 6, kyl)
!
!     Z0=Z0+Z1*INVERSE(Z3)*Z2
    do 501 k = 1, 6
        do 501 j = 1, 6
            z0(k,j)=z0(k,j)-kyl(k,j)
501      continue
!
    call dcopy(36, i6, 1, zinv, 1)
!     CALL MGAUSS ('NCSP',Z0, ZINV, 6, 6, 6, DET, IRET )
    call mgauss('NCWP', z0, zinv, 6, 6,&
                6, det, iret)
    if (iret .gt. 0) goto 9999
!
    if (gdef .eq. 0) then
!
!        DSDE = INVERSE(Z0-Z1*INVERSE(Z3)*Z2)
!
        call dcopy(36, zinv, 1, dsde, 1)
!
    else
!
        call lcmmkg(zinv, nvi, vind, vinf, nmat,&
                    mater, mod, nr, dsde)
!
    endif
9999  continue
end subroutine
