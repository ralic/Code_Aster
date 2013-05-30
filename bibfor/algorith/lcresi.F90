subroutine lcresi(fami, kpg, ksp, loi, typmod,&
                  imat, nmat, materd, materf, comp,&
                  nbcomm, cpmono, pgl, nfs, nsg,&
                  toutms, hsr, nr, nvi, vind,&
                  vinf, itmax, toler, timed, timef,&
                  yd, yf, deps, epsd, dy,&
                  r, iret, crit, indi)
    implicit   none
! TOLE CRP_21
!       ================================================================
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
!       CALCUL DES TERMES DU SYSTEME NL A RESOUDRE = R(DY)
!       IN  FAMI   :  FAMILLE DU POINT DE GAUSS
!           KPG    :  POINT DE GAUSS
!           KSP    :  SOUS-POINT DE GAUSS
!           LOI    :  MODELE DE COMPORTEMENT
!           TYPMOD    :  TYPE DE MODELISATION
!           IMAT   :  NOM DU MATERIAU
!           NMAT   :  DIMENSION MATER
!           MATERD :  COEFFICIENTS MATERIAU A T
!           MATERF :  COEFFICIENTS MATERIAU A T+DT
!           TIMED  :  INSTANT  T
!           TIMEF  :  INSTANT  T+DT
!           DEPS   :  INCREMENT DE DEFORMATION
!           EPSD   :  DEFORMATION A T
!           VIND   :  VARIABLES INTERNES A T
!           VINF   :  VARIABLES INTERNES A T+DT
!           YD     :  VARIABLES A T      =    ( SIGD  VIND  (EPSD3)  )
!           YF     :  VARIABLES A T + DT =    ( SIGF  VINF  (EPS3F)  )
!           DY     :  SOLUTION           =    ( DSIG  DVIN  (DEPS3)  )
!           INDI   :  INDICATEURS DES MECANISMES POT.ACTIFS (HUJEUX)
!       OUT R      :  SYSTEME NL A T + DT
!       ----------------------------------------------------------------
!
    include 'asterfort/burres.h'
    include 'asterfort/cvmres.h'
    include 'asterfort/hayres.h'
    include 'asterfort/huresi.h'
    include 'asterfort/irrres.h'
    include 'asterfort/lcmmre.h'
    include 'asterfort/lcresa.h'
    include 'asterfort/lkresi.h'
    integer :: imat, nmat, nr, nvi, kpg, ksp, itmax, iret
    integer :: nfs, nsg, indi(7)
    real(kind=8) :: deps(6), epsd(6), vind(*), toler
    real(kind=8) :: r(*), yd(*), yf(*), dy(*), vinf(*)
    real(kind=8) :: materd(nmat, 2), materf(nmat, 2)
    real(kind=8) :: timed, timef, crit(*)
    character(len=8) :: typmod
    character(len=16) :: loi
    real(kind=8) :: toutms(nfs, nsg, 6), hsr(nsg, nsg)
    character(len=*) :: fami
!
    integer :: nbcomm(nmat, 3)
    real(kind=8) :: pgl(3, 3)
    character(len=16) :: comp(*)
    character(len=24) :: cpmono(5*nmat+1)
!
!       ----------------------------------------------------------------
!
    iret=0
    if (loi(1:9) .eq. 'VISCOCHAB') then
        call cvmres(typmod, nmat, materd, materf, timed,&
                    timef, yd, yf, epsd, deps,&
                    dy, r)
!
    else if (loi(1:8) .eq. 'MONOCRIS') then
        call lcmmre(typmod, nmat, materd, materf, comp,&
                    nbcomm, cpmono, pgl, nfs, nsg,&
                    toutms, hsr, nr, nvi, vind,&
                    itmax, toler, timed, timef, yd,&
                    yf, deps, dy, r, iret)
    else if (loi(1:7) .eq. 'IRRAD3M') then
        call irrres(fami, kpg, ksp, typmod, nmat,&
                    materd, materf, yd, yf, deps,&
                    dy, r)
    else if (loi(1:15) .eq. 'BETON_BURGER_FP') then
        call burres(typmod, nmat, materd, materf, timed,&
                    timef, nvi, vind, yd, yf,&
                    deps, dy, nr, r)
    else if (loi(1:4) .eq. 'LETK') then
        call lkresi(typmod, nmat, materf, timed, timef,&
                    nvi, vind, vinf, yd, yf,&
                    deps, nr, r)
    else if (loi .eq. 'HAYHURST') then
        call hayres(typmod, nmat, materd, materf, timed,&
                    timef, yd, yf, deps, dy,&
                    r, crit, iret)
    else if (loi(1:6) .eq. 'HUJEUX') then
        call huresi(typmod, nmat, materf, indi, deps,&
                    nr, yd, yf, nvi, vind,&
                    r, iret)
    else
        call lcresa(fami, kpg, ksp, typmod, imat,&
                    nmat, materd, materf, comp, nr,&
                    nvi, timed, timef, deps, epsd,&
                    yf, dy, r, iret, yd,&
                    crit)
!
    endif
!
end subroutine
