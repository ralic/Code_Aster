subroutine lcjplc(loi, mod, angmas, imat, nmat,&
                  mater, timed, timef, comp, nbcomm,&
                  cpmono, pgl, nfs, nsg, toutms,&
                  hsr, nr, nvi, epsd, deps,&
                  itmax, toler, sigf, vinf, sigd,&
                  vind, dsde, drdy, option, iret)
! aslint: disable=W1504
    implicit none
! ----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!       ----------------------------------------------------------------
!       MATRICE SYMETRIQUE DE COMPORTEMENT TANGENT ELASTO-PLASTIQUE OU
!       VISCO-PLASTIQUE COHERENT A T+DT OU T
!       COHERENT A T+DT OU T
!       IN  FAMI   :  FAMILLE DES POINTS DE GAUSS
!           KPG    :  NUMERO DU POINT DE GAUSS
!           KSP    :  NUMERO DU SOUS POINT DE GAUSS
!           LOI    :  MODELE DE COMPORTEMENT
!           MOD    :  TYPE DE MODELISATION
!           NMAT   :  DIMENSION MATER
!           MATER  :  COEFFICIENTS MATERIAU
!       OUT DSDE   :  MATRICE DE COMPORTEMENT TANGENT = DSIG/DEPS
!       ----------------------------------------------------------------
    include 'asterfort/burjpl.h'
    include 'asterfort/cvmjpl.h'
    include 'asterfort/hujopt.h'
    include 'asterfort/lcmmjp.h'
    include 'asterfort/lcoptg.h'
    include 'asterfort/lkijpl.h'
    integer :: imat, nmat, nr, nvi, itmax, iret, nfs, nsg, ndt, ndi, n2
    real(kind=8) :: dsde(6, 6), epsd(*), deps(*), toler, angmas(3)
    real(kind=8) :: mater(nmat, 2)
    real(kind=8) :: toutms(nfs, nsg, 6), hsr(nsg, nsg)
    character(len=8) :: mod
    character(len=16) :: loi, option
    common /tdim/   ndt  , ndi
!
    integer :: nbcomm(nmat, 3)
    real(kind=8) :: sigf(*), sigd(*), vind(*), vinf(*), timed, timef, pgl(3, 3)
    real(kind=8) :: drdy(nr, nr)
    character(len=16) :: comp(*)
    character(len=24) :: cpmono(5*nmat+1)
!       ----------------------------------------------------------------
    iret=0
    if (loi(1:9) .eq. 'VISCOCHAB') then
        call cvmjpl(mod, nmat, mater, timed, timef,&
                    epsd, deps, sigf, vinf, sigd,&
                    vind, nvi, nr, dsde)
    else if (loi(1:8) .eq. 'MONOCRIS') then
        call lcmmjp(mod, nmat, mater, timed, timef,&
                    comp, nbcomm, cpmono, pgl, nfs,&
                    nsg, toutms, hsr, nr, nvi,&
                    itmax, toler, vinf, vind, dsde,&
                    drdy, option, iret)
    else if (loi(1:15) .eq. 'BETON_BURGER_FP') then
        call burjpl(nmat, mater, nr, drdy, dsde)
    else if (loi(1:4) .eq. 'LETK') then
        call lkijpl(nmat, mater, sigf, nr, drdy,&
                    dsde)
    else if (loi.eq.'HAYHURST') then
        n2=nr-ndt
        call lcoptg(nmat, mater, nr, n2, drdy,&
                    0, dsde, iret)
    else if (loi(1:6) .eq. 'HUJEUX') then
        call hujopt(mod, angmas, imat, nmat, mater,&
                    nvi, vinf, nr, drdy, sigf,&
                    dsde, iret)
    else
        n2=nr-ndt
        call lcoptg(nmat, mater, nr, n2, drdy,&
                    1, dsde, iret)
    endif
!
end subroutine
