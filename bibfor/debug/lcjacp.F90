subroutine lcjacp(fami, kpg, ksp, loi, toler,&
                  itmax, mod, imat, nmat, materd,&
                  materf, nr, nvi, timed, timef,&
                  deps, epsd, vind, vinf, yd,&
                  comp, nbcomm, cpmono, pgl, nfs,&
                  nsg, toutms, hsr, dy, r,&
                  drdy, verjac, drdyb, iret, crit,&
                  indi)
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
!     CONSTRUCTION DE LA MATRICE JACOBIENNE PAR PERTURBATION
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
!     VAR DEPS   :  INCREMENT DE DEFORMATION
!     IN  EPSD   :  DEFORMATION A T
!         SIGD   :  CONTRAINTE A T
!         VIND   :  VARIABLES INTERNES A T
!         VINF   :  VARIABLES INTERNES A T+DT
!         YD     :  VARIABLES A T   = ( SIGD  VIND  (EPSD3)   )
!         COMP   :  COMPORTEMENT
!         DY     :  INCREMENT DES VARIABLES = ( DSIG  DVIN  (DEPS3)  )
!         R      :  VECTEUR RESIDU
!         DRDY   :  JACOBIEN
!
!         VERJAC : =0 : PAS DE VERIFICATION
!         =1 : CONSTRUCTION DE LA JACOBIENNE PAR PERTURBATION (LCJACP)
!                COMPARAISON A LA MATRICE JACOBIENNE ISSU DE LCJACB
!         =2 : UTILISATION DE LA JACOBIENNE PAR PERTURBATION (LCJACP)
!                COMME MATRICE JACOBIENNE A LA PLACE DE LCJACB
!     OUT DRDYB  : MATRICE JACOBIENNE PAR PERTURBATION
! ----------------------------------------------------------------------
! aslint: disable=W1306,W1504
    implicit none
!
    include 'asterc/r8miem.h'
    include 'asterfort/lceqvn.h'
    include 'asterfort/lcresi.h'
    include 'asterfort/lcsovn.h'
    include 'asterfort/u2mesg.h'
    integer :: nmat, nbcomm(nmat, 3), nr, impr, vali(2), nfs, nsg
    integer :: imat, i, j, itmax, iret, kpg, ksp, nvi, verjac, indi(7)
!
    real(kind=8) :: toler, epsd(6), deps(6), vind(nvi), vinf(nvi), timed, timef
    real(kind=8) :: err
!
!     DIMENSIONNEMENT DYNAMIQUE (MERCI F90)
    real(kind=8) :: dy(nr), r(nr), drdyb(nr, nr), rini(nr), dyini(nr), rp(nr)
    real(kind=8) :: rm(nr)
    real(kind=8) :: drdy(nr, nr), yd(nr), dym(nr), dyp(nr), yfp(nr), yfm(nr)
!
    real(kind=8) :: materd(nmat, 2), materf(nmat, 2), pgl(3, 3), eps1, eps2
    real(kind=8) :: eps0
    real(kind=8) :: toutms(nfs, nsg, 6), hsr(nsg, nsg), crit(*)
    real(kind=8) :: valr(4), maxtgt, normd1, normd2, maxerr
!
    character(len=8) :: mod
    character(len=16) :: loi, comp(*)
    character(len=24) :: cpmono(5*nmat+1)
    character(len=*) :: fami
    data impr/0/
! ----------------------------------------------------------------------
    call lceqvn(nr, dy, dyini)
    call lceqvn(nr, r, rini)
    maxtgt=0.d0
    normd1=0.d0
    normd2=0.d0
!
    do 1002 i = 1, 6
        normd1=normd1+dyini(i)*dyini(i)
1002  end do
!
    do 1003 i = 7, nr
        normd2=normd2+dyini(i)*dyini(i)
1003  end do
!
    if (normd1 .lt. r8miem()) then
        do 1007 i = 1, 6
            normd1=normd1+yd(i)*yd(i)
1007      continue
    endif
    if (normd2 .lt. r8miem()) then
        do 1008 i = 7, nr
            normd2=normd2+yd(i)*yd(i)
1008      continue
    endif
!
    eps0=1.d-7
    eps1=eps0
    eps2=eps0
    if (normd1 .gt. r8miem()) then
        eps1=eps1*sqrt(normd1)
    endif
    if (normd2 .gt. r8miem()) then
        eps2=eps2*sqrt(normd2)
    endif
!
    do 1004 i = 1, nr
        call lceqvn(nr, dyini, dyp)
        if (i .le. 6) then
            dyp(i)=dyp(i)+eps1
        else
            dyp(i)=dyp(i)+eps2
        endif
        call lcsovn(nr, yd, dyp, yfp)
        call lcresi(fami, kpg, ksp, loi, mod,&
                    imat, nmat, materd, materf, comp,&
                    nbcomm, cpmono, pgl, nfs, nsg,&
                    toutms, hsr, nr, nvi, vind,&
                    vinf, itmax, toler, timed, timef,&
                    yd, yfp, deps, epsd, dyp,&
                    rp, iret, crit, indi)
        if (iret .gt. 0) then
            goto 9999
        endif
        call lceqvn(nr, dyini, dym)
        if (i .le. 6) then
            dym(i)=dym(i)-eps1
        else
            dym(i)=dym(i)-eps2
        endif
        call lcsovn(nr, yd, dym, yfm)
        call lcresi(fami, kpg, ksp, loi, mod,&
                    imat, nmat, materd, materf, comp,&
                    nbcomm, cpmono, pgl, nfs, nsg,&
                    toutms, hsr, nr, nvi, vind,&
                    vinf, itmax, toler, timed, timef,&
                    yd, yfm, deps, epsd, dym,&
                    rm, iret, crit, indi)
        if (iret .gt. 0) then
            goto 9999
        endif
!        SIGNE - CAR LCRESI CALCULE -R
        do 1005 j = 1, nr
            if (i .le. 6) then
                drdyb(j,i)=-(rp(j)-rm(j))/2.d0/eps1
            else
                drdyb(j,i)=-(rp(j)-rm(j))/2.d0/eps2
            endif
1005      continue
1004  end do
!
! COMPARAISON DRDY ET DRDYB
!
    maxerr=0.d0
    err=0.d0
    if ((verjac.eq.1) .and. (impr.eq.0)) then
        do 1001 i = 1, nr
            do 1001 j = 1, nr
                if (abs(drdy(i,j)) .gt. maxtgt) then
                    maxtgt=abs(drdy(i,j))
                endif
1001          continue
        do 1006 i = 1, nr
            do 1006 j = 1, nr
                if (abs(drdy(i,j)) .gt. (1.d-9*maxtgt)) then
                    if (abs(drdyb(i,j)) .gt. (1.d-9*maxtgt)) then
                        err=abs(drdy(i,j)-drdyb(i,j))/drdyb(i,j)
                        if (err .gt. 1.d-3) then
                            vali(1) = i
                            vali(2) = j
!
                            valr(1) = timef
                            valr(2) = err
                            valr(3) = drdyb(i,j)
                            valr(4) = drdy(i,j)
                            call u2mesg('I', 'DEBUG_1', 0, ' ', 2,&
                                        vali, 4, valr)
                            maxerr=max(maxerr,abs(err))
                            impr=1
                        endif
                    endif
                endif
1006          continue
    endif
!
!     UTILISATION DE DRDYB COMME MATRICE JACOBIENNE
    if (verjac .eq. 2) then
        call lceqvn(nr*nr, drdyb, drdy)
    endif
!
9999  continue
end subroutine
