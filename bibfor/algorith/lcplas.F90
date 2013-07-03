subroutine lcplas(fami, kpg, ksp, loi, toler,&
                  itmax, mod, imat, nmat, materd,&
                  materf, nr, nvi, timed, timef,&
                  deps, epsd, sigd, vind, sigf,&
                  vinf, comp, nbcomm, cpmono, pgl,&
                  nfs, nsg, toutms, hsr, icomp,&
                  codret, theta, vp, vecp, seuil,&
                  devg, devgii, drdy, tampon, crit)
! aslint: disable=W1504
    implicit   none
!     ================================================================
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ----------------------------------------------------------------
!     INTEGRATION IMPLICITE DES COMPORTEMENTS. CALCUL DE SIGF,VINF,DSDE
!     ----------------------------------------------------------------
!     ARGUMENTS
!
!     IN FAMI    FAMILLE DE POINT DE GAUSS (RIGI,MASS,...)
!        KPG,KSP NUMERO DU (SOUS)POINT DE GAUSS
!        LOI    :  MODELE DE COMPORTEMENT
!        TOLER  :  TOLERANCE DE CONVERGENCE LOCALE
!        ITMAX  :  NOMBRE MAXI D'ITERATIONS LOCALES
!        MOD    :  TYPE DE MODELISATION
!        IMAT   :  ADRESSE DU MATERIAU CODE
!        NMAT   :  DIMENSION MATER
!        MATERD :  COEFFICIENTS MATERIAU A T
!        MATERF :  COEFFICIENTS MATERIAU A T+DT
!        NR     :  NB EQUATION DU SYSTEME R(DY)
!        NVI    :  NB VARIABLES INTERNES
!        TIMED  :  INSTANT  T
!        TIMEF  :  INSTANT T+DT
!        DEPS   :  INCREMENT DE DEFORMATION
!        EPSD   :  DEFORMATION A T
!        SIGD   :  CONTRAINTE A T
!        VIND   :  VARIABLES INTERNES A T
!        COMP   :  COMPOR - LOI ET TYPE DE DEFORMATION
!        NBCOMM :  INCIDES DES COEF MATERIAU monocristal
!        CPMONO :  NOM DES COMPORTEMENTS monocristal
!        PGL    :  MATRICE DE PASSAGE
!        TOUTMS :  TENSEURS D'ORIENTATION monocristal
!        HSR    :  MATRICE D'INTERACTION monocristal
!        ICOMP  :  COMPTEUR POUR LE REDECOUPAGE DU PAS DE TEMPS
!        THETA  :  PARAMETRE DE LA THETA-METHODE
!        VP     :  VALEURS PROPRES DU DEVIATEUR ELASTIQUE(HOEK-BROWN)
!        VECP   :  VECTEURS PROPRES DU DEVIATEUR ELASTIQUE(HOEK-BROWN)
!        TAMPON : TABLEAU DE TRAVAIL EN ENTREE(SUIVANT MODELISATION)
!        CRIT   : CRITERES DE CONVERGENCE LOCAUX
!
!    OUT SIGF   :  CONTRAINTE A T+DT
!        VINF   :  VARIABLES INTERNES A T+DT
!        CODRET :  CODE RETOUR. 0=OK, 1=ECHEC
!        DRDY   :  MATRICE JACOBIENNE
!       ----------------------------------------------------------------
#include "asterfort/lchobr.h"
#include "asterfort/lcpllg.h"
#include "asterfort/lcplnl.h"
#include "asterfort/lcrous.h"
    integer :: itmax, icomp, codret, irtet, kpg, ksp
    integer :: imat, nmat, nvi, nr
!
!
    real(kind=8) :: timed, timef, deltat, crit(*)
    real(kind=8) :: toler, theta
    real(kind=8) :: epsd(6), deps(6)
    real(kind=8) :: sigd(6), sigf(6)
    real(kind=8) :: vind(*), vinf(*), tampon(*)
    real(kind=8) :: materf(nmat, 2), materd(nmat, 2)
    real(kind=8) :: seuil, devg(*), devgii
    real(kind=8) :: vp(3), vecp(3, 3), drdy(nr, nr)
!
    character(len=8) :: mod
    character(len=16) :: loi
!
    integer :: nbcomm(nmat, 3), nfs, nsg
    real(kind=8) :: pgl(3, 3)
    real(kind=8) :: toutms(nfs, nsg, 6), hsr(nsg, nsg)
    character(len=16) :: comp(*)
    character(len=24) :: cpmono(5*nmat+1)
    character(len=*) :: fami
!       ----------------------------------------------------------------
!
    codret = 0
    deltat = timef - timed
!
!       ----------------------------------------------------------------
!       CAS PARTICULIERS
!       ----------------------------------------------------------------
!
    if (loi(1:8) .eq. 'ROUSS_PR' .or. loi(1:10) .eq. 'ROUSS_VISC') then
        call lcrous(fami, kpg, ksp, toler, itmax,&
                    imat, nmat, materd, materf, nvi,&
                    deps, sigd, vind, theta, loi,&
                    deltat, sigf, vinf, irtet)
        if (irtet .gt. 0) goto 1
!
        elseif (( loi(1:10) .eq. 'HOEK_BROWN' ).or. ( loi(1:14) .eq.&
    'HOEK_BROWN_EFF' ))then
        call lchobr(toler, itmax, mod, nmat, materf,&
                    nr, nvi, deps, sigd, vind,&
                    seuil, vp, vecp, icomp, sigf,&
                    vinf, irtet)
        if (irtet .gt. 0) goto 1
!
    else if (loi(1:6) .eq. 'LAIGLE') then
        call lcpllg(toler, itmax, mod, nmat, materf,&
                    nr, nvi, deps, sigd, vind,&
                    seuil, icomp, sigf, vinf, devg,&
                    devgii, irtet)
        if (irtet .gt. 0) goto 1
!
!       ----------------------------------------------------------------
!       CAS GENERAL : RESOLUTION PAR NEWTON
!       ----------------------------------------------------------------
    else
        call lcplnl(fami, kpg, ksp, loi, toler,&
                    itmax, mod, imat, nmat, materd,&
                    materf, nr, nvi, timed, timef,&
                    deps, epsd, sigd, vind, comp,&
                    nbcomm, cpmono, pgl, nfs, nsg,&
                    toutms, hsr, sigf, vinf, icomp,&
                    irtet, drdy, tampon, crit)
        if (irtet .gt. 0) goto (1,2), irtet
!
    endif
!
!     CONVERGENCE OK
!
    codret = 0
    goto 9999
!
 1  continue
!
!     PB INTEGRATION ou ITMAX ATTEINT : redecoupage local puis global
    codret = 1
    goto 9999
!
 2  continue
!     ITMAX ATTEINT : redecoupage du pas de temps global
    codret = 2
!
9999  continue
end subroutine
