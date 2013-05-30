subroutine lcreli(fami, kpg, ksp, loi, mod,&
                  imat, nmat, materd, materf, comp,&
                  nbcomm, cpmono, pgl, nfs, nsg,&
                  toutms, hsr, nr, nvi, vind,&
                  vinf, itmax, toler, timed, timef,&
                  yd, yf, deps, epsd, dy,&
                  r, ddy, iret, crit, indi)
    implicit none
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
! person_in_charge: samuel.geniaut at edf.fr
!
    include 'asterc/r8prem.h'
    include 'asterfort/assert.h'
    include 'asterfort/lcpsvn.h'
    include 'asterfort/lcresi.h'
    include 'asterfort/lcsovn.h'
    include 'blas/ddot.h'
    real(kind=8) :: ddy(*)
!
!     VARIABLES EN ARGUMENT DE LCRESI
    integer :: imat, nmat, nr, nvi, kpg, ksp, itmax, nfs, nsg
    real(kind=8) :: deps(6), epsd(6), vind(*), toler, vinf(*)
    real(kind=8) :: r(*), yd(*), yf(*), dy(*)
    real(kind=8) :: materd(nmat, 2), materf(nmat, 2)
    real(kind=8) :: timed, timef
    character(len=8) :: mod
    character(len=16) :: loi
    real(kind=8) :: toutms(nfs, nsg, 6), hsr(nsg, nsg), crit(*)
    character(len=*) :: fami
!
    integer :: nbcomm(nmat, 3), indi(7)
    real(kind=8) :: pgl(3, 3)
    character(len=16) :: comp(*)
    character(len=24) :: cpmono(5*nmat+1)
!
!
!
!     ----------------------------------------------------------------
!     RECHERCHE LINEAIRE POUR PLASTI
!
!     ON CREE UNE FONCTIONNELLE F(X) = 1/2 || R(X) ||^2
!     ET ON CHERCHE RHO = ARGMIN (        F(DY+RHO.DDY)       )
!     CAD           RHO = ARGMIN ( 1/2 || R(DY+RHO.DDY) ||^2  )
!     ET ON MET A JOUR YF, R ET DY
!
!     ON UTILISE L'ALGORITHME AVEC REBROUSSEMENT ET LA REGLE D'ARMIJO
!     (W EST LA PARAMETRE DE LA REGLE D'ARMIJO)
!     AVEC RABATEMMENT SUR [RHOMIN,RHOMAX]
!     ON COMMENCE PAR UN ESSAI AVEC UN RHO = 1
!     PUIS UN ESSAI AVEC UNE INTERPOLATION QUADRATIQUE
!     PUIS PLUSIEURS ESSAIS (NB ESSAIS = IMXRHO) D'INTERPOLATION CUBIQUE
!
!     IN  TOUS LES ARGUMENTS DE LCRESI.F
!     IN  DDY    :  CORRECTION DE L'INCREMENT = DIRECTION DE DESCENTE
!     OUT R      :  VECTEUR RESIDU
!     OUT DY     :  INCREMENT DES VARIABLES
!     OUT YF     :  VARIABLES A T+DT
!     IN /OUT IRET : CODE RETOUR D'ERREUR (DIFFERENT DE 0 SI PB)
!
! TOLE CRP_21 CRS_1404
!     ----------------------------------------------------------------
    integer :: i, iret, itrho, imxrho
    real(kind=8) :: f, df, w, rhomin, rhomax
    real(kind=8) :: rhoddy(nr), dyp(nr), rp(nr), yfp(nr)
    real(kind=8) :: rho0, fp0, rho1, fp1, fp2, rho2, rho05, fsup
    real(kind=8) :: m(2, 2), s(2), a, b
    parameter (w = 1.d-4)
    parameter (rhomin = 0.1d0, rhomax=0.5d0)
    parameter (imxrho = 2)
!     ----------------------------------------------------------------
!
!     REMARQUE : ON POURRAIT METTRE DANS UNE ROUTINE UTILITAIRE LES 8
!     LIGNES CORRESPONDANTES AU CALCUL DU R ACTUALISE, MAIS CA VAUT PAS
!     VRAIMENT LE COUP
!
!
!     FONCTIONNELLE EN "MOINS" : F = 1/2 || R(DY) ||^2
    f = 0.5d0 * ddot(nr,r,1,r,1)
!
!     DERIVEE DE LA FONCTIONNELLE EN "MOINS" : DF=<GRAD(F).DDY>=<R.DDY>
    df = -ddot(nr,r,1,r,1)
!
!     ------------------------------------
!     ESSAI AVEC LE PAS DE NEWTON RHO0 = 1
!     ------------------------------------
!
    rho0 = 1
!     CALCUL DE DY "PLUS" : DYP
    call lcpsvn(nr, rho0, ddy, rhoddy)
    call lcsovn(nr, rhoddy, dy, dyp)
    call lcsovn(nr, yd, dyp, yfp)
!     CALCUL DE R "PLUS" : RP
    call lcresi(fami, kpg, ksp, loi, mod,&
                imat, nmat, materd, materf, comp,&
                nbcomm, cpmono, pgl, nfs, nsg,&
                toutms, hsr, nr, nvi, vind,&
                vinf, itmax, toler, timed, timef,&
                yd, yfp, deps, epsd, dyp,&
                rp, iret, crit, indi)
!
    if (iret .ne. 0) goto 9999
!
!     TEST DE LA REGLE D'ARMIJO : SI TEST REUSSI, ON SORT
    fp0 = 0.5d0 * ddot(nr,rp,1,rp,1)
    if (fp0 .lt. r8prem()) goto 8888
    if (fp0 .le. f+w*rho0*df) goto 8888
!
!     ------------------------------------
!     TEST SUPPLEMENTAIRE AVEC RHO = 0.5
!     ------------------------------------
!
    rho05 = 0.5d0
    call lcpsvn(nr, rho05, ddy, rhoddy)
    call lcsovn(nr, rhoddy, dy, dyp)
    call lcsovn(nr, yd, dyp, yfp)
    call lcresi(fami, kpg, ksp, loi, mod,&
                imat, nmat, materd, materf, comp,&
                nbcomm, cpmono, pgl, nfs, nsg,&
                toutms, hsr, nr, nvi, vind,&
                vinf, itmax, toler, timed, timef,&
                yd, yfp, deps, epsd, dyp,&
                rp, iret, crit, indi)
    if (iret .ne. 0) goto 9999
!
!     TEST DE LA REGLE D'ARMIJO : SI TEST REUSSI, ON SORT
    fsup = 0.5d0*ddot(nr,rp,1,rp,1)
    if (fsup .lt. r8prem()) goto 8888
    if (fsup .le. f+w*0.5d0*df) goto 8888
!
!     ----------------------------------------
!     INTERPOLATION QUADRATIQUE (ENTRE 0 ET 1)
!     ----------------------------------------
!
    call assert(abs(fp0-f-df).gt.r8prem())
    rho1 = -0.5d0 * df /( fp0- f - df)
!
!     PROJECTION SUR L'INTERVALLE [RHOMIN,RHOMAX]
    if (rho1 .lt. rhomin*rho0) rho1 = rhomin*rho0
    if (rho1 .gt. rhomax*rho0) rho1 = rhomax*rho0
!
    call lcpsvn(nr, rho1, ddy, rhoddy)
    call lcsovn(nr, rhoddy, dy, dyp)
    call lcsovn(nr, yd, dyp, yfp)
    call lcresi(fami, kpg, ksp, loi, mod,&
                imat, nmat, materd, materf, comp,&
                nbcomm, cpmono, pgl, nfs, nsg,&
                toutms, hsr, nr, nvi, vind,&
                vinf, itmax, toler, timed, timef,&
                yd, yfp, deps, epsd, dyp,&
                rp, iret, crit, indi)
    if (iret .ne. 0) goto 9999
!
!     TEST DE LA REGLE D'ARMIJO : SI TEST REUSSI, ON SORT
    fp1 = 0.5d0 * ddot(nr,rp,1,rp,1)
    if (fp1 .lt. r8prem()) goto 8888
    if (fp1 .le. f+w*rho1*df) goto 8888
!
!     ------------------------------------
!     INTERPOLATIONS CUBIQUES
!     ------------------------------------
!
    do 100 itrho = 1, imxrho
        m(1,1) = 1.d0/(rho0**2)
        m(1,2) = -1.d0/(rho1**2)
        m(2,1) = -rho1/(rho0**2)
        m(2,2) = rho0/(rho1**2)
        s(1) = fp0 - f - df * rho0
        s(2) = fp1 - f - df * rho1
        call assert(abs(rho0-rho1).gt.r8prem())
        a = 1.d0/(rho0 - rho1) * ( m(1,1)*s(1) + m(1,2)*s(2) )
        b = 1.d0/(rho0 - rho1) * ( m(2,1)*s(2) + m(2,2)*s(2) )
        if (abs(3.d0*a) .le. r8prem()) goto 8888
        rho2 = (-b + sqrt(b**2-3.d0*a*df)) / (3.d0 * a)
!
!       PROJECTION SUR L'INTERVALLE [RHOMIN,RHOMAX]
        if (rho2 .lt. rhomin*rho1) rho2 = rhomin*rho1
        if (rho2 .gt. rhomax*rho1) rho2 = rhomax*rho1
!
        call lcpsvn(nr, rho2, ddy, rhoddy)
        call lcsovn(nr, rhoddy, dy, dyp)
        call lcsovn(nr, yd, dyp, yfp)
        call lcresi(fami, kpg, ksp, loi, mod,&
                    imat, nmat, materd, materf, comp,&
                    nbcomm, cpmono, pgl, nfs, nsg,&
                    toutms, hsr, nr, nvi, vind,&
                    vinf, itmax, toler, timed, timef,&
                    yd, yfp, deps, epsd, dyp,&
                    rp, iret, crit, indi)
        if (iret .ne. 0) goto 9999
!
!       TEST DE LA REGLE D'ARMIJO : SI TEST REUSSI, ON SORT
        fp2 = 0.5d0 * ddot(nr,rp,1,rp,1)
        if (fp2 .lt. r8prem()) goto 8888
        if (fp2 .le. f+w*rho2*df) goto 8888
!
!       NOUVELLE INTERPOLATION CUBIQUE AVEC LES DEUX DERNIERS RHO
        rho0 = rho1
        rho1 = rho2
        fp0 = fp1
        fp1 = fp2
100  end do
!
!     ON A FAIT TOUTES LES INTERATIONS D'INTERPOLATIONS CUBIQUES
!
8888  continue
!
!     EN ECRASE LES ENTREES AVEC LES VARIABLES RE-ACTUALISEES
    do 500 i = 1, nr
        r(i) = rp(i)
        yf(i) = yfp(i)
        dy(i) = dyp(i)
500  end do
!
9999  continue
end subroutine
