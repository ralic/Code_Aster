subroutine lcplnl(fami, kpg, ksp, loi, toler,&
                  itmax, mod, imat, nmat, materd,&
                  materf, nr, nvi, timed, timef,&
                  deps, epsd, sigd, vind, comp,&
                  nbcomm, cpmono, pgl, nfs, nsg,&
                  toutms, hsr, sigf, vinf, icomp,&
                  codret, drdy, tampon, crit)
! aslint: disable=W1306,W1504
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
! person_in_charge: jean-michel.proix at edf.fr
!
!     INTEGRATION ELASTO-PLASTIQUE ET VISCO-PLASTICITE
!           SUR DT DE Y = ( SIG , VIN )
!     LE SYSTEME  A RESOUDRE EN DY ETANT NON  LINEAIRE
!
!     ON RESOUD DONC                  R(DY) = 0
!     PAR UNE METHODE DE NEWTON       DRDY(DYI) DDYI = - R(DYI)
!                                     DYI+1 = DYI + DDYI  (DYO DEBUT)
!     ET ON REACTUALISE               YF = YD + DY
!
!     ATTENTION :     ON REACTUALISE ICI DEPS DE FACON A CE QUE
!                     DEPS(3) = DY(NR) EN C_PLAN
!
!
!     IN  FAMI   :  FAMILLE DE POINT DE GAUSS
!         KPG    :  NUMERO DU POINT DE GAUSS
!         KSP    :  NUMERO DU SOUS-POINT DE GAUSS
!         LOI    :  MODELE DE COMPORTEMENT
!         TOLER  :  TOLERANCE DE CONVERGENCE LOCALE
!         ITMAX  :  NOMBRE MAXI D'ITERATIONS LOCALES
!         MOD    :  TYPE DE MODELISATION
!         IMAT   :  ADRESSE DU MATERIAU CODE
!         NMAT   :  DIMENSION MATER
!         MATERD :  COEFFICIENTS MATERIAU A T
!         MATERF :  COEFFICIENTS MATERIAU A T+DT
!         NR     :  NB EQUATION DU SYSTEME R(DY)
!         NVI    :  NB VARIABLES INTERNES
!         TIMED  :  INSTANT  T
!         TIMEF  :  INSTANT T+DT
!         EPSD   :  DEFORMATION A T
!         SIGD   :  CONTRAINTE A T
!         VIND   :  VARIABLES INTERNES A T
!         COMP   :  COMPOR - LOI ET TYPE DE DEFORMATION
!         NBCOMM :  INCIDES DES COEF MATERIAU monocristal
!         CPMONO :  NOM DES COMPORTEMENTS monocristal
!         PGL    :  MATRICE DE PASSAGE
!         TOUTMS :  TENSEURS D'ORIENTATION monocristal
!         HSR    :  MATRICE D'INTERACTION monocristal
!         ICOMP  :  COMPTEUR POUR LE REDECOUPAGE DU PAS DE TEMPS
!     VAR DEPS   :  INCREMENT DE DEFORMATION
!     OUT SIGF   :  CONTRAINTE A T+DT
!         VINF   :  VARIABLES INTERNES A T+DT
!         CODRET :  CONTROLE DU REDECOUPAGE DU PAS DE TEMPS
!         DRDY   :  JACOBIEN
!         TAMPON :  DONNES GEOM SUIVANT LE TE APPELANT
!         CRIT   :  CRITERES DE CONVERGENCE LOCAUX
! VARIABLES LOCALES
!         R      :  VECTEUR RESIDU
!         DY     :  INCREMENT DES VARIABLES = ( DSIG  DVIN  (DEPS3)  )
!         DDY    :  CORRECTION SUR L'INCREMENT DES VARIABLES
!                                           = ( DDSIG DDVIN (DDEPS3) )
!         YD     :  VARIABLES A T   = ( SIGD  VARD  )
!         YF     :  VARIABLES A T+DT= ( SIGF  VARF  )
!         TYPESS :  TYPE DE SOLUTION D ESSAI POUR NEWTON
!         ESSAI  :  VALEUR  SOLUTION D ESSAI POUR NEWTON
!         INTG   :  COMPTEUR DU NOMBRE DE TENTATIVES D'INTEGRATIONS
!         INDI   :  INDICATEURS DES MECANISMES POT.ACTIFS (HUJEUX)
!         YE     : VECTEUR SOLUTIONS APRES LCINIT
!
#include "asterfort/lcafyd.h"
#include "asterfort/lccaga.h"
#include "asterfort/lcconv.h"
#include "asterfort/lceqmn.h"
#include "asterfort/lceqvn.h"
#include "asterfort/lcinit.h"
#include "asterfort/lcjacb.h"
#include "asterfort/lcjacp.h"
#include "asterfort/lcplnf.h"
#include "asterfort/lcreli.h"
#include "asterfort/lcresi.h"
#include "asterfort/lcsovn.h"
#include "asterfort/mgauss.h"
#include "asterfort/r8inir.h"
#include "asterfort/utlcal.h"
    integer :: imat, nmat, icomp
    integer :: typess, itmax, iret, kpg, ksp
    integer :: nr, ndt, ndi, nvi, iter
!
    real(kind=8) :: toler, essai, rbid, crit(*)
    real(kind=8) :: epsd(*), deps(*)
    real(kind=8) :: sigd(6), sigf(6)
    real(kind=8) :: vind(*), vinf(*)
!     DIMENSIONNEMENT DYNAMIQUE (MERCI F90)
    real(kind=8) :: r(nr), drdy(nr, nr), rini(nr)
    real(kind=8) :: drdy1(nr, nr)
    real(kind=8) :: ddy(ndt+nvi), dy(ndt+nvi), yd(ndt+nvi), yf(ndt+nvi)
    real(kind=8) :: materd(nmat, 2), materf(nmat, 2), dt
    real(kind=8) :: timed, timef, tampon(*), drdyb(nr, nr)
    logical :: lreli
!
    character(len=8) :: mod
    character(len=16) :: loi, comp(*)
    character(len=*) :: fami
!
    common /tdim/   ndt  , ndi
!
    integer :: intg, codret
!
    integer :: nbcomm(nmat, 3), verjac, nfs, nsg
    real(kind=8) :: pgl(3, 3), epstr(6)
    real(kind=8) :: toutms(nfs, nsg, 6), hsr(nsg, nsg)
    character(len=4) :: cargau
    character(len=16) :: algo
    character(len=24) :: cpmono(5*nmat+1)
!
    logical :: bnews(3), mtrac
    integer :: indi(7), nr1
    real(kind=8) :: vind0(nvi), vind1(nvi), ye(nr)
!
!     ACTIVATION OU PAS DE LA RECHERCHE LINEAIRE
    lreli = .false.
    loi=comp(1)
    call utlcal('VALE_NOM', algo, crit(6))
    if (algo .eq. 'NEWTON_RELI') lreli = .true.
!
!     VERIFICATION DE LA MATRICE JACOBIENNE
!     VERJAC=0 : PAS DE VERIFICATION
!     VERJAC=1 : CONSTRUCTION DE LA JACOBIENNE PAR PERTURBATION (LCJACP)
!                COMPARAISON A LA MATRICE JACOBIENNE ISSU DE LCJACB
!     VERJAC=2 : UTILISATION DE LA JACOBIENNE PAR PERTURBATION (LCJACP)
!                COMME MATRICE JACOBIENNE A LA PLACE DE LCJACB
!
    verjac = 0
!
    if (algo .eq. 'NEWTON_PERT') then
        verjac=2
    endif
!
    essai = 1.d-5
    dt=timef-timed
!
    call r8inir(ndt+nvi, 0.d0, yd, 1)
!
    codret = 0
!
! --- SAUVEGARDE DE VIND INITIAL - VIND0 (POUR HUJEUX)
!     + VARIABLES POUR GESTION TRACTION AVEC HUJEUX
    call lceqvn(nvi, vind, vind0)
    nr1 = nr
    iret = 0
!
!
!     CHOIX DES VALEURS DE VIND A AFFECTER A YD
    call lcafyd(comp, materd, materf, nbcomm, cpmono,&
                nmat, mod, nvi, vind, vinf,&
                sigd, nr1, yd, bnews, mtrac)
!
! --- SAUVEGARDE DE VIND INITIAL - VIND1 (POUR HUJEUX)
    call lceqvn(nvi, vind, vind1)
!
!     CHOIX DES PARAMETRES DE LANCEMENT DE MGAUSS
    call lccaga(loi, cargau)
!
    if (mod(1:6) .eq. 'C_PLAN') yd (nr) = epsd(3)
!
!     RESOLUTION ITERATIVE PAR NEWTON DE R(DY) = 0
!     SOIT  DRDY(DYI) DDYI = -R(DYI)  ET DYI+1 = DYI + DDYI
!                                                         -
! -   INITIALISATION DU TYPE DE SOLUTION D ESSAI (-1)
    typess = -1
    intg = 0
!
  2 continue
!
    call r8inir(nr, 0.d0, r, 1)
    call r8inir(ndt+nvi, 0.d0, ddy, 1)
    call r8inir(ndt+nvi, 0.d0, dy, 1)
    call r8inir(ndt+nvi, 0.d0, yf, 1)
!
!     CALCUL DE LA SOLUTION D ESSAI INITIALE DU SYSTEME NL EN DY
    call lcinit(fami, kpg, ksp, loi, typess,&
                essai, mod, nmat, materd, materf,&
                timed, timef, intg, nr1, nvi,&
                yd, epsd, deps, dy, comp,&
                nbcomm, cpmono, pgl, nfs, nsg,&
                toutms, vind, sigd, sigf, epstr,&
                bnews, mtrac, indi, iret)
!
    if (iret .ne. 0) then
        goto 3
    endif
!
! --- ENREGISTREMENT DE LA SOLUTION D'ESSAI
    call lcsovn(nr, yd, dy, ye)
!
    iter = 0
!
  1 continue
!
!     ITERATIONS DE NEWTON
    iter = iter + 1
!
!     PAR SOUCIS DE PERFORMANCES, ON NE REFAIT PAS DES OPERATIONS
!     QUI ONT DEJA ETE FAITE A L'ITERATION PRECEDENTE DANS LE CAS
!     DE LA RECHERCHE LINEAIRE
    if (.not.lreli .or. iter .eq. 1) then
!        INCREMENTATION DE  YF = YD + DY
        call lcsovn(nr, yd, dy, yf)
!
!        CALCUL DES TERMES DU SYSTEME A T+DT = -R(DY)
        call lcresi(fami, kpg, ksp, loi, mod,&
                    imat, nmat, materd, materf, comp,&
                    nbcomm, cpmono, pgl, nfs, nsg,&
                    toutms, hsr, nr, nvi, vind,&
                    vinf, itmax, toler, timed, timef,&
                    yd, yf, deps, epsd, dy,&
                    r, iret, crit, indi)
!
        if (iret .ne. 0) then
            goto 3
        endif
!
    endif
!     SAUVEGARDE DE R(DY0) POUR TEST DE CONVERGENCE
    if (iter .eq. 1) call lceqvn(nr, r, rini)
!
!
    if (verjac .ne. 2) then
!         CALCUL DU JACOBIEN DU SYSTEME A T+DT = DRDY(DY)
        call lcjacb(fami, kpg, ksp, loi, mod,&
                    nmat, materd, materf, timed, timef,&
                    yf, deps, itmax, toler, nbcomm,&
                    cpmono, pgl, nfs, nsg, toutms,&
                    hsr, nr, comp, nvi, vind,&
                    vinf, epsd, yd, dy, ye,&
                    crit, indi, vind1, bnews, mtrac,&
                    drdy, iret)
        if (iret .ne. 0) then
            goto 3
        endif
    endif
!
    if (verjac .ge. 1) then
        call lcjacp(fami, kpg, ksp, loi, toler,&
                    itmax, mod, imat, nmat, materd,&
                    materf, nr, nvi, timed, timef,&
                    deps, epsd, vind, vinf, yd,&
                    comp, nbcomm, cpmono, pgl, nfs,&
                    nsg, toutms, hsr, dy, r,&
                    drdy, verjac, drdyb, iret, crit,&
                    indi)
        if (iret .ne. 0) goto 3
    endif
!
!     RESOLUTION DU SYSTEME LINEAIRE DRDY(DY).DDY = -R(DY)
    call lceqmn(nr, drdy, drdy1)
    call lceqvn(nr, r, ddy)
    call mgauss(cargau, drdy1, ddy, nr, nr1,&
                1, rbid, iret)
!
    if (iret .ne. 0) then
        goto 3
    endif
!
!     ACTUALISATION DE LA SOLUTION
    if (.not.lreli) then
!        REACTUALISATION DE DY = DY + DDY
        call lcsovn(nr, ddy, dy, dy)
    else if (lreli) then
!        RECHERCHE LINEAIRE : RENVOIE DY, YF ET R RE-ACTUALISES
        call lcreli(fami, kpg, ksp, loi, mod,&
                    imat, nmat, materd, materf, comp,&
                    nbcomm, cpmono, pgl, nfs, nsg,&
                    toutms, hsr, nr, nvi, vind,&
                    vinf, itmax, toler, timed, timef,&
                    yd, yf, deps, epsd, dy,&
                    r, ddy, iret, crit, indi)
        if (iret .ne. 0) goto 3
    endif
    if (mod(1:6) .eq. 'C_PLAN') deps(3) = dy(nr)
!
!     VERIFICATION DE LA CONVERGENCE EN DY  ET RE-INTEGRATION ?
    call lcconv(loi, yd, dy, ddy, ye,&
                nr, itmax, toler, iter, intg,&
                nmat, materf, r, rini, epstr,&
                typess, essai, icomp, nvi, vind,&
                vinf, vind1, indi, bnews, mtrac,&
                lreli, iret)
!     IRET = 0 CONVERGENCE
!          = 1 ITERATION SUIVANTE
!          = 2 RE-INTEGRATION
!          = 3 REDECOUPAGE DU PAS DE TEMPS
!
    if (iret .eq. 1) then
        goto 1
    else if (iret .eq. 2) then
        goto 2
    else if (iret .eq. 3) then
        goto 3
    endif
!
!     CONVERGENCE > INCREMENTATION DE  YF = YD + DY
    call lcsovn(ndt+nvi, yd, dy, yf)
!
!     MISE A JOUR DE SIGF , VINF
    call lceqvn(ndt, yf(1), sigf)
!
!     POST-TRAITEMENTS POUR DES LOIS PARTICULIERES
    call lcplnf(loi, vind, nbcomm, nmat, cpmono,&
                materd, materf, iter, nvi, itmax,&
                toler, pgl, nfs, nsg, toutms,&
                hsr, dt, dy, yd, yf,&
                vinf, tampon, comp, sigd, sigf,&
                deps, nr1, mod, timed, timef,&
                indi, vind0, iret)
!
    if (iret .ne. 0) then
        if (iret .eq. 2) goto 2
        goto 3
    endif
!
!     CONVERGENCE
    codret = 0
    goto 999
!
  3 continue
!     NON CV, OU PB => REDECOUPAGE (LOCAL OU GLOBAL) DU PAS DE TEMPS
    codret = 1
!
999 continue
!
end subroutine
