subroutine calsta(proj, gamma, dh, def, nno,&
                  kpg, sig, tmp, kk, kkd,&
                  matuu, dsidep, jac)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
!-----------------------------------------------------------------------
!     CALCUL DES TERMES DE STABILISATION POUR LE QUAD4 SOUS INTEGRE
!     STABILISE PAR LA METHODE ASSUMED STRAIN => QUAS4
!-----------------------------------------------------------------------
!
    implicit none
!
    integer :: kpg, kk, kkd, n, i, m, j, kl, nno, j1, ifiltr, proj
    real(kind=8) :: dsidep(6, 6), f(3, 3)
    real(kind=8) :: tmp, sig(6)
    real(kind=8) :: gamma(4), dh(8)
    real(kind=8) :: kron(3, 3), kron2(2, 3), kron3(2, 3)
    real(kind=8) :: filtr1(2, 3), filtr2(2, 3), jac
    real(kind=8) :: matuu(*), defst1(4, 4, 2)
    real(kind=8) :: defst2(4, 4, 2), def(4, 4, 2)
    real(kind=8) :: rac2
!
    data kron/1.d0,0.d0,0.d0,0.d0,1.d0,0.d0,0.d0,0.d0,1.d0/
    data kron2/1.d0,0.d0,1.d0,0.d0,0.d0,1.d0/
    data kron3/1.d0,0.d0,0.d0,1.d0,1.d0,0.d0/
!
!
!    PROJ : INDICATEUR DE LA PROJECTION
!           0 AUCUNE
!           1 OPTIMAL BENDING
!           2 INCOMPRESSIBLE
!
    rac2 = sqrt(2.d0)
!
    do 20 i = 1, 3
        do 10 j = 1, 3
            f(i,j) = kron(i,j)
10      continue
20  end do
!
!
!
    do 40 i = 1, 2
        do 30 j = 1, 3
            filtr1(i,j) = kron2(i,j)
            filtr2(i,j) = kron3(i,j)
30      continue
40  end do
!
    do 120 ifiltr = 1, 3
        do 60 n = 1, nno
            do 50 i = 1, 2
!
!         QUAS4 SANS PROJECTION
!         ---------------------
                if (proj .eq. 0) then
!
                    defst1(1,n,i) = f(i,1)*gamma(n)*dh(2*kpg-1)* filtr1(1,ifiltr) + def(1,n,i)* f&
                                    &iltr1(2,ifiltr)
!
                    defst1(2,n,i) = f(i,2)*gamma(n)*dh(2*kpg)* filtr1(1,ifiltr) + def(2,n,i)* fil&
                                    &tr1(2,ifiltr)
!
                    defst1(3,n,i) = 0.d0
!
                    defst1(4,n,i) = (&
                                    filtr1(1, ifiltr)* (f(i, 1)*gamma( n)*dh(2*kpg)+f(i, 2)*gamma&
                                    &(n)*dh(2*kpg-1))/rac2) + def(4,&
                                    n, i)*filtr1(2, ifiltr&
                                    )
!
!
!
                    defst2(1,n,i) = f(i,1)*gamma(n)*dh(2*kpg-1)* filtr2(1,ifiltr) + def(1,n,i)* f&
                                    &iltr2(2,ifiltr)
!
                    defst2(2,n,i) = f(i,2)*gamma(n)*dh(2*kpg)* filtr2(1,ifiltr) + def(2,n,i)* fil&
                                    &tr2(2,ifiltr)
!
                    defst2(3,n,i) = 0.d0
!
!
                    defst2(4,n,i) = (&
                                    filtr2(1, ifiltr)* (f(i, 1)*gamma( n)*dh(2*kpg)+f(i, 2)*gamma&
                                    &(n)*dh(2*kpg-1))/rac2) + def(4,&
                                    n, i)*filtr2(2, ifiltr&
                                    )
!
!         OPTIMAL BENDING
!         ---------------
                else if (proj.eq.1) then
!
                    defst1(1,n,i) = f(i,1)*gamma(n)*dh(2*kpg-1)* filtr1(1,ifiltr) + def(1,n,i)* f&
                                    &iltr1(2,ifiltr)
!
                    defst1(2,n,i) = f(i,2)*gamma(n)*dh(2*kpg)* filtr1(1,ifiltr) + def(2,n,i)* fil&
                                    &tr1(2,ifiltr)
!
                    defst1(3,n,i) = 0.d0
!
                    defst1(4,n,i) = def(4,n,i)*filtr1(2,ifiltr)
!
!
!
                    defst2(1,n,i) = f(i,1)*gamma(n)*dh(2*kpg-1)* filtr2(1,ifiltr) + def(1,n,i)* f&
                                    &iltr2(2,ifiltr)
!
                    defst2(2,n,i) = f(i,2)*gamma(n)*dh(2*kpg)* filtr2(1,ifiltr) + def(2,n,i)* fil&
                                    &tr2(2,ifiltr)
!
                    defst2(3,n,i) = 0.d0
!
!
                    defst2(4,n,i) = def(4,n,i)*filtr2(2,ifiltr)
!
!
!         INCOMPRESSIBLE
!         --------------
                else if (proj.eq.2) then
!
                    defst1(1,n,i) = f(i,1)*gamma(n)*dh(2*kpg-1)*0.5d0* filtr1(1,ifiltr) + def(1,n&
                                    &,i)* filtr1(2,ifiltr) + f(i,2)* (-0.5d0)* dh(2*kpg)*gamma(n)
!
                    defst1(2,n,i) = f(i,2)*gamma(n)*dh(2*kpg)*0.5d0* filtr1(1,ifiltr) + def(2,n,i&
                                    &)* filtr1(2,ifiltr) + f(i,1)* (-0.5d0)* dh(2*kpg-1)*gamma(n)
!
                    defst1(3,n,i) = 0.d0
!
                    defst1(4,n,i) = def(4,n,i)*filtr1(2,ifiltr)
!
!
!
                    defst2(1,n,i) = f(i,1)*gamma(n)*dh(2*kpg-1)*0.5d0* filtr2(1,ifiltr) + def(1,n&
                                    &,i)* filtr2(2,ifiltr) + f(i,2)*gamma(n)* dh(2*kpg)* (-0.5d0)
!
                    defst2(2,n,i) = f(i,2)*gamma(n)*dh(2*kpg)*0.5d0* filtr2(1,ifiltr) + def(2,n,i&
                                    &)* filtr2(2,ifiltr) + f(i,1)*gamma(n)* dh(2*kpg-1)* (-0.5d0)
!
                    defst2(3,n,i) = 0.d0
!
                    defst2(4,n,i) = def(4,n,i)*filtr2(2,ifiltr)
!
                endif
50          continue
60      continue
!
!
! - CALCUL DE LA MATRICE DE RIGIDITE
!
!
        do 110 n = 1, nno
            do 100 i = 1, 2
                do 70,kl = 1,4
                sig(kl) = 0.d0
                sig(kl) = sig(kl) + defst1(1,n,i)*dsidep(1,kl)
                sig(kl) = sig(kl) + defst1(2,n,i)*dsidep(2,kl)
                sig(kl) = sig(kl) + defst1(3,n,i)*dsidep(3,kl)
                sig(kl) = sig(kl) + defst1(4,n,i)*dsidep(4,kl)
70              continue
!
                do 90 j = 1, 2
                    do 80 m = 1, n
                        if (m .eq. n) then
                            j1 = i
                        else
                            j1 = 2
                        endif
!
!                 RIGIDITE ELASTIQUE
                        tmp = 0.d0
                        tmp = tmp + sig(1)*defst2(1,m,j)
                        tmp = tmp + sig(2)*defst2(2,m,j)
                        tmp = tmp + sig(3)*defst2(3,m,j)
                        tmp = tmp + sig(4)*defst2(4,m,j)
!
!                 STOCKAGE EN TENANT COMPTE DE LA SYMETRIE
                        if (j .le. j1) then
                            kkd = (2* (n-1)+i-1)* (2* (n-1)+i)/2
                            kk = kkd + 2* (m-1) + j
                            matuu(kk) = matuu(kk) + tmp*jac
                        endif
!
80                  continue
90              continue
100          continue
110      continue
120  end do
!
end subroutine
