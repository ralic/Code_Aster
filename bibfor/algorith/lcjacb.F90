subroutine lcjacb(fami, kpg, ksp, loi, mod,&
                  nmat, materd, materf, timed, timef,&
                  yf, deps, itmax, toler, nbcomm,&
                  cpmono, pgl, nfs, nsg, toutms,&
                  hsr, nr, comp, nvi, vind,&
                  vinf, epsd, yd, dy, ye,&
                  crit, indi, vind1, bnews, mtrac,&
                  drdy, iret)
! aslint: disable=W1504
    implicit   none
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
!       CALCUL DU JACOBIEN DU SYSTEME NL A RESOUDRE = DRDY(DY)
!       IN  FAMI   :  FAMILLE DES POINTS DE GAUSS
!           KPG    :  NUMERO DU POINT DE GAUSS
!           KSP    :  NUMERO DU SOUS POINT DE GAUSS
!           LOI    :  MODELE DE COMPORTEMENT
!           MOD    :  TYPE DE MODELISATION
!           IMAT   :  ADRESSE DU MATERIAU CODE
!           NMAT   :  DIMENSION MATER
!           MATERD :  COEFFICIENTS MATERIAU A T
!           MATERF :  COEFFICIENTS MATERIAU A T+DT
!           YF     :  VARIABLES A T + DT =    ( SIGF  VINF  (EPS3F)  )
!           TOLER  :  TOLERANCE DE CONVERGENCE LOCALE
!           DEPS   :  INCREMENT DE DEFORMATION
!           ITMAX  :  NOMBRE MAXI D'ITERATIONS LOCALES
!           TIMED  :  INSTANT  T
!           TIMEF  :  INSTANT  T+DT
!           NBCOMM :  INCIDES DES COEF MATERIAU monocristal
!           CPMONO :  NOM DES COMPORTEMENTS monocristal
!           PGL    :  MATRICE DE PASSAGE
!           TOUTMS :  TENSEURS D'ORIENTATION monocristal
!           HSR    :  MATRICE D'INTERACTION monocristal
!           NR     :  DIMENSION DECLAREE DRDY
!           COMP   :  COMPOR - LOI ET TYPE DE DEFORMATION
!           NVI    :  NOMBRE DE VARIABLES INTERNES
!           VIND   :  VARIABLE INTERNES A T
!           VINF   :  VARIABLE INTERNES A T+DT
!           EPSD   :  DEFORMATION A T
!           YD     :  VARIABLES A T   = ( SIGD  VARD  ) A T
!           DY     :  SOLUTION           =    ( DSIG  DVIN  (DEPS3)  )
!           CRIT   :  CRITERES LOCAUX
!           INDI   :  MECANISMES POTENTIEL ACTIFS (HUJEUX)
!           VIND1  :  VARIABLES INTERNES D'ORIGINE (HUJEUX)
!           YE     :  VECTEUR SOLUTION APRES LCINIT
!           BNEWS  :  INDICATEURS LIES A LA TRACTION (HUJEUX)
!           MTRAC  :  INDICATEUR LIE A LA TRACTION (HUJEUX - BIS)
!       OUT DRDY   :  JACOBIEN DU SYSTEME NON LINEAIRE
!           IRET   :  CODE RETOUR
!       ----------------------------------------------------------------
!
#include "asterfort/burjac.h"
#include "asterfort/cvmjac.h"
#include "asterfort/hayjac.h"
#include "asterfort/hujjac.h"
#include "asterfort/irrjac.h"
#include "asterfort/lcmmja.h"
#include "asterfort/lkijac.h"
    integer :: nr, nmat, kpg, ksp, itmax, iret, nvi, nfs, nsg
    integer :: indi(7)
    real(kind=8) :: deps(*), epsd(*), toler, crit(*)
    real(kind=8) :: drdy(nr, nr), yf(nr), dy(nr), yd(nr), vind1(nvi)
    real(kind=8) :: ye(nr)
!
    real(kind=8) :: materd(nmat, 2), materf(nmat, 2)
    real(kind=8) :: timed, timef, vind(*), vinf(*)
    real(kind=8) :: toutms(nfs, nsg, 6), hsr(nsg, nsg)
!
    character(len=*) :: fami
    character(len=8) :: mod
    character(len=16) :: loi
!
    integer :: nbcomm(nmat, 3)
    real(kind=8) :: pgl(3, 3)
    character(len=16) :: comp(*)
    character(len=24) :: cpmono(5*nmat+1)
!
    logical :: prox(4), proxc(4), bnews(3), mtrac
!       ----------------------------------------------------------------
!
    iret=0
    if (loi(1:9) .eq. 'VISCOCHAB') then
        call cvmjac(mod, nmat, materf, timed, timef,&
                    yf, dy, nr, epsd, deps,&
                    drdy)
!
    else if (loi(1:8) .eq. 'MONOCRIS') then
        call lcmmja(comp, mod, nmat, materf, timed,&
                    timef, itmax, toler, nbcomm, cpmono,&
                    pgl, nfs, nsg, toutms, hsr,&
                    nr, nvi, vind, deps, yf,&
                    yd, dy, drdy, iret)
!
    else if (loi(1:7) .eq. 'IRRAD3M') then
        call irrjac(fami, kpg, ksp, mod, nmat,&
                    materf, yf, dy, nr, drdy)
!
    else if (loi(1:15) .eq. 'BETON_BURGER_FP') then
        call burjac(mod, nmat, materd, materf, nvi,&
                    vind, timed, timef, yd, yf,&
                    dy, nr, drdy)
!
    else if (loi(1:4) .eq. 'LETK') then
        call lkijac(mod, nmat, materf, timed, timef,&
                    yf, deps, nr, nvi, vind,&
                    vinf, yd, dy, drdy, iret)
!
    else if (loi .eq. 'HAYHURST') then
        call hayjac(mod, nmat, materf(1, 1), materf(1, 2), timed,&
                    timef, yf, deps, nr, nvi,&
                    vind, vinf, yd, dy, crit,&
                    drdy, iret)
!
    else if (loi(1:6) .eq. 'HUJEUX') then
        call hujjac(mod, nmat, materf, indi, deps,&
                    nr, yd, yf, ye, nvi,&
                    vind, vind1, vinf, drdy, bnews,&
                    mtrac, iret)
!
    endif
!
end subroutine
