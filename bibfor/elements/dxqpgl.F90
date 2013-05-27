subroutine dxqpgl(xyzg, pgl, kstop, iret)
    implicit none
    include 'jeveux.h'
    include 'asterc/r8miem.h'
    include 'asterfort/assert.h'
    include 'asterfort/tecael.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mesr.h'
    real(kind=8) :: xyzg(3, *), pgl(3, 3)
    character(len=1) :: kstop
    integer :: iret
!     -----------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!
!     IN  XYZG  R  12  COORDONNEES  X1 Y1 Z1 X2 Y2 ...
!     IN  KSTOP K1     COMPORTEMENT EN CAS D'ERREUR ('C' OU 'S')
!     OUT PGL   R 3,3  MATRICE DE PASSAGE GLOBAL INTRINSEQUE
!     OUT IRET  I      CODE RETOUR
!                      (0 : OK, 1 : MODELISA10_5, 2 : ELEMENTS4_80)
!     -----------------------------------------------------------------
!     CONSTRUCTION DE LA MATRICE DE PASSAGE GLOBAL --> INTRINSEQUE
!     POUR UNE MAILLE TRIANGLE DKQ OU DSQ
!
!            I MILIEU DE 4 1                        3
!            J MILIEU DE 2 3                        *
!            K MILIEU DE 1 2                     L *  *
!            L MILIEU DE 3 4                      *     *
!                                                *        *
!        I : VECTEUR UNITAIRE PORTE PAR IJ    4 *           * J
!                                                *            *
!        K : PERPENDICULAIRE A IJ ET A KL        I*             *
!                                                  *              *
!        J : PRODUIT VECTORIEL K I                  *****************
!                                                  1        K        2
!
!     CALCUL DES MATRICE T1VE ET T2VE DE PASSAGE D'UNE MATRICE,
!     RESPECTIVEMENT (3,3) ET (2,2), DU REPERE DE LA VARIETE AU REPERE
!     DE L'ELEMENT ET T2VE, INVERSE DE T2EV
!
!     VERIFICATION QUE L'ELEMENT EST REELLEMENT PLAN
!
!     ------------------------------------------------------------------
    real(kind=8) :: vx, vy, vz, xi, yi, zzi, xj, yj, zzj, xk, yk, zzk, xl, yl
    real(kind=8) :: zzl
    real(kind=8) :: norm
    real(kind=8) :: x12, y12, z12, x13, y13, z13, x14, y14, z14
    real(kind=8) :: ux, uy, uz, pscal, normu, norm4, dist
    integer :: iadzi, iazk24
!     ------------------------------------------------------------------
    real(kind=8) :: valr
    iret = 0
    xi = (xyzg(1,1) + xyzg(1,4))/2.d0
    yi = (xyzg(2,1) + xyzg(2,4))/2.d0
    zzi = (xyzg(3,1) + xyzg(3,4))/2.d0
    xj = (xyzg(1,3) + xyzg(1,2))/2.d0
    yj = (xyzg(2,3) + xyzg(2,2))/2.d0
    zzj = (xyzg(3,3) + xyzg(3,2))/2.d0
    xk = (xyzg(1,2) + xyzg(1,1))/2.d0
    yk = (xyzg(2,2) + xyzg(2,1))/2.d0
    zzk = (xyzg(3,2) + xyzg(3,1))/2.d0
    xl = (xyzg(1,4) + xyzg(1,3))/2.d0
    yl = (xyzg(2,4) + xyzg(2,3))/2.d0
    zzl = (xyzg(3,4) + xyzg(3,3))/2.d0
!
    norm = sqrt((xj-xi)*(xj-xi)+(yj-yi)*(yj-yi)+(zzj-zzi)*(zzj-zzi))
    pgl(1,1) = (xj-xi)/norm
    pgl(1,2) = (yj-yi)/norm
    pgl(1,3) = (zzj-zzi)/norm
!
    vx = (yj-yi)*(zzl-zzk) - (zzj-zzi)*(yl-yk)
    vy = - (xj-xi)*(zzl-zzk) + (zzj-zzi)*(xl-xk)
    vz = (xj-xi)*(yl-yk) - (yj-yi)*(xl-xk)
!
    norm = sqrt(vx*vx + vy*vy + vz*vz)
    pgl(3,1) = vx / norm
    pgl(3,2) = vy / norm
    pgl(3,3) = vz / norm
!
    pgl(2,1) = pgl(3,2)*pgl(1,3) - pgl(3,3)*pgl(1,2)
    pgl(2,2) = - pgl(3,1)*pgl(1,3) + pgl(3,3)*pgl(1,1)
    pgl(2,3) = pgl(3,1)*pgl(1,2) - pgl(3,2)*pgl(1,1)
!
    norm = sqrt ( pgl(2,1) * pgl(2,1) + pgl(2,2) * pgl(2,2) + pgl(2,3) * pgl(2,3))
    pgl(2,1) = pgl(2,1) / norm
    pgl(2,2) = pgl(2,2) / norm
    pgl(2,3) = pgl(2,3) / norm
!
!
!          VERIFICATION DE LA PLANEITE :
!     CALCUL DE : T14 P_SCAL (T12 P_VECT T13)
!
!     DEFINITION DU VECTEUR T12 (VECTEUR DE DIR 1 A 2)
!
    x12 = xyzg(1,2) - xyzg(1,1)
    y12 = xyzg(2,2) - xyzg(2,1)
    z12 = xyzg(3,2) - xyzg(3,1)
!
!     DEFINITION DU VECTEUR T13 (VECTEUR DE DIR 1 A 3)
    x13 = xyzg(1,3) - xyzg(1,1)
    y13 = xyzg(2,3) - xyzg(2,1)
    z13 = xyzg(3,3) - xyzg(3,1)
!
!     DEFINITION DU VECTEUR T14 (VECTEUR DE DIR 1 A 4)
    x14 = xyzg(1,4) - xyzg(1,1)
    y14 = xyzg(2,4) - xyzg(2,1)
    z14 = xyzg(3,4) - xyzg(3,1)
!
!     U = (VECTEUR) T12 P_VECT T13
    ux = (y12*z13) - (y13*z12)
    uy = (z12*x13) - (z13*x12)
    uz = (x12*y13) - (x13*y12)
!
!     PSCAL = (SCALAIRE) T14 P_SCAL U
    pscal = (ux*x14) + (uy*y14) + (uz*z14)
!
!     DISTANCE DU POINT 4 AU PLAN (123)
    normu = sqrt( (ux*ux) + (uy*uy) + (uz*uz) )
    if (normu .lt. r8miem()) then
        if (kstop .eq. 'S') then
            call tecael(iadzi, iazk24)
            call u2mesk('F', 'MODELISA10_5', 1, zk24(iazk24+2))
        else if (kstop.eq.'C') then
            iret = 1
        else
            call assert(.false.)
        endif
    endif
    norm4 = sqrt( (x14*x14) + (y14*y14) + (z14*z14) )
    dist = pscal / normu
    pscal = dist / norm4
!
!     TESTE SI PSCAL > EPS (1D-4 EN DUR DANS LE FORTRAN)
!
    if (abs(pscal) .gt. 1.d-4) then
        if (kstop .eq. 'S') then
            call tecael(iadzi, iazk24)
            valr = abs(dist)
            call u2mesk('A+', 'ELEMENTS4_80', 1, zk24(iazk24+2))
            call u2mesr('A', 'ELEMENTS4_82', 1, valr)
        else if (kstop.eq.'C') then
            iret = 2
        else
            call assert(.false.)
        endif
    endif
!
end subroutine
