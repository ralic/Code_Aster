subroutine cq3d2d(nno, coor3d, coteta, siteta, coor2d)
    implicit none
    include 'asterfort/trigom.h'
    integer :: nno
    real(kind=8) :: coor3d(*), coteta, siteta, coor2d(*)
! ......................................................................
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
! .  - FONCTION REALISEE:  CALCUL DES COORDONNEES 2-D D'UN TRIANGLE    .
! .     OU D'UN QUADRANGLE                                             .
! .     A PARTIR DE SES COORDONNEES 3-D. PASSAGE DANS LE REPERE DU PLAN.
! .     DU TRIANGLE OU DU QUADRANGLE                                   .
! .     AVEC TETA=ANGLE ENTRE L'AXE X ET LE COTE A1A2.                 .
! .                                                                    .
! .  - ARGUMENTS:                                                      .
! .                                                                    .
! .       DONNEES :        NNO     -->  NOMBRE DE NOEUDS               .
! .                      COOR3D    -->  COORD. 3-D DES NOEUDS          .
! .                      COTETA    -->  COSINUS ANGLE TETA             .
! .                      SITETA    -->  SINUS ANGLE TETA               .
! .                                                                    .
! .       RESULTATS :    COOR2D    <--  COORD. 2-D DES NOEUDS          .
! ......................................................................
!
    real(kind=8) :: na1a2, na1a3, na1a4, pscal, ppscal, qpscal, sigama, sidlta
    real(kind=8) :: va1a3(3), va1a2(3), va1a4(3)
    real(kind=8) :: pvec1, pvec2, pvec3, qvec1, qvec2, qvec3, norme, pnorme
    real(kind=8) :: qnorme
    real(kind=8) :: gamma, delta, alpha, alpha1, alpha2
!
!-----------------------------------------------------------------------
    integer :: i
    real(kind=8) :: teta
!-----------------------------------------------------------------------
    if ((nno.eq.3) .or. (nno.eq.6) .or. (nno.eq.7)) then
!
!     CAS TRIANGLES A 3, 6, OU 7 NOEUDS
!
        na1a2 = 0.d0
        na1a3 = 0.d0
!
        do 10 i = 1, 3
            va1a2(i) = coor3d(3+i) - coor3d(i)
            va1a3(i) = coor3d(6+i) - coor3d(i)
            na1a2 = na1a2 + va1a2(i)**2
            na1a3 = na1a3 + va1a3(i)**2
10      end do
        na1a2 = sqrt(na1a2)
        na1a3 = sqrt(na1a3)
        pvec1 = va1a2(2)*va1a3(3) - va1a2(3)*va1a3(2)
        pvec2 = va1a2(3)*va1a3(1) - va1a2(1)*va1a3(3)
        pvec3 = va1a2(1)*va1a3(2) - va1a2(2)*va1a3(1)
        norme = sqrt(pvec1**2+pvec2**2+pvec3**2)
!
        coor2d(1) = 0.d0
        coor2d(2) = 0.d0
        coor2d(3) = na1a2*coteta
        coor2d(4) = -na1a2*siteta
!
        pscal = 0.d0
        do 20 i = 1, 3
            pscal = pscal + va1a2(i)*va1a3(i)
20      end do
!
        sigama = norme/ (na1a2*na1a3)
        if (sigama .gt. 1.d0) sigama = 1.d0
        gamma = trigom('ASIN',sigama)
        if (pscal .lt. 0.d0) gamma = 4.d0*atan2(1.d0,1.d0) - gamma
        teta = trigom('ASIN',siteta)
        if (coteta .lt. 0.d0) teta = 4.d0*atan2(1.d0,1.d0) - teta
        alpha = gamma - teta
!
        coor2d(5) = na1a3*cos(alpha)
        coor2d(6) = na1a3*sin(alpha)
!
        if ((nno.eq.6) .or. (nno.eq.7)) then
            do 30 i = 1, 2
                coor2d(i+6) = (coor2d(i+2)+coor2d(i))/2.d0
                coor2d(i+8) = (coor2d(i+4)+coor2d(i+2))/2.d0
                coor2d(i+10) = (coor2d(i)+coor2d(i+4))/2.d0
30          continue
        endif
!
        if (nno .eq. 7) then
            coor2d(13) = (coor2d(1)+coor2d(3)+coor2d(5))/3.d0
            coor2d(14) = (coor2d(2)+coor2d(4)+coor2d(6))/3.d0
        endif
!
    else if ((nno.eq.4).or.(nno.eq.8).or.(nno.eq.9)) then
!
!     CAS QUADRANGLES A 4, 8 OU 9 NOEUDS
!
        na1a2 = 0.d0
        na1a3 = 0.d0
        na1a4 = 0.d0
!
        do 50 i = 1, 3
            va1a2(i) = coor3d(3+i) - coor3d(i)
            va1a3(i) = coor3d(6+i) - coor3d(i)
            va1a4(i) = coor3d(9+i) - coor3d(i)
!
            na1a2 = na1a2 + va1a2(i)**2
            na1a3 = na1a3 + va1a3(i)**2
            na1a4 = na1a4 + va1a4(i)**2
50      end do
        na1a2 = sqrt(na1a2)
        na1a3 = sqrt(na1a3)
        na1a4 = sqrt(na1a4)
!
        pvec1 = va1a2(2)*va1a3(3) - va1a2(3)*va1a3(2)
        pvec2 = va1a2(3)*va1a3(1) - va1a2(1)*va1a3(3)
        pvec3 = va1a2(1)*va1a3(2) - va1a2(2)*va1a3(1)
!
        qvec1 = va1a2(2)*va1a4(3) - va1a2(3)*va1a4(2)
        qvec2 = va1a2(3)*va1a4(1) - va1a2(1)*va1a4(3)
        qvec3 = va1a2(1)*va1a4(2) - va1a2(2)*va1a4(1)
!
        pnorme = sqrt(pvec1**2+pvec2**2+pvec3**2)
!
        qnorme = sqrt(qvec1**2+qvec2**2+qvec3**2)
!
        coor2d(1) = 0.d0
        coor2d(2) = 0.d0
        coor2d(3) = na1a2*coteta
        coor2d(4) = -na1a2*siteta
!
        ppscal = 0.d0
        qpscal = 0.d0
        do 60 i = 1, 3
            ppscal = ppscal + va1a2(i)*va1a3(i)
            qpscal = qpscal + va1a2(i)*va1a4(i)
60      end do
!
        sigama = pnorme/ (na1a2*na1a3)
        if (sigama .gt. 1.d0) sigama = 1.d0
        gamma = trigom('ASIN',sigama)
        if (ppscal .lt. 0.d0) gamma = 4.d0*atan2(1.d0,1.d0) - gamma
!
        sidlta = qnorme/ (na1a2*na1a4)
        if (sidlta .gt. 1.d0) sidlta = 1.d0
        delta = trigom('ASIN',sidlta)
        if (qpscal .lt. 0.d0) delta = 4.d0*atan2(1.d0,1.d0) - delta
!
        teta = trigom('ASIN',siteta)
        if (coteta .lt. 0.d0) teta = 4.d0*atan2(1.d0,1.d0) - teta
!
        alpha1 = gamma - teta
        alpha2 = delta - teta
!
        coor2d(5) = na1a3*cos(alpha1)
        coor2d(6) = na1a3*sin(alpha1)
!
        coor2d(7) = na1a4*cos(alpha2)
        coor2d(8) = na1a4*sin(alpha2)
!
        if ((nno.eq.8) .or. (nno.eq.9)) then
            do 70 i = 1, 2
                coor2d(i+8) = (coor2d(i+2)+coor2d(i))/2.d0
                coor2d(i+10) = (coor2d(i+4)+coor2d(i+2))/2.d0
                coor2d(i+12) = (coor2d(i+6)+coor2d(i+4))/2.d0
                coor2d(i+14) = (coor2d(i)+coor2d(i+6))/2.d0
70          continue
        endif
!
        if (nno .eq. 9) then
            coor2d(17) = (coor2d(1)+coor2d(3)+coor2d(5)+coor2d(7))/ 4.d0
            coor2d(18) = (coor2d(2)+coor2d(4)+coor2d(6)+coor2d(8))/ 4.d0
        endif
!
    endif
!
end subroutine
