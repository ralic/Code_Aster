subroutine angco4(coor, zk1, izk, icoude, zk2,&
                  rayon, theta, angl1, angl2, angl3,&
                  angl4, pgl1, pgl2, pgl3, pgl4,&
                  omega, dn1n2, epsi, crit)
    implicit none
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "asterc/r8prem.h"
#include "asterfort/angcou.h"
#include "asterfort/assert.h"
    real(kind=8) :: coor(*), rayon, theta, epsi
    real(kind=8) :: coor3(12)
    real(kind=8) :: pgl1(3, 3), pgl2(3, 3), pgl3(3, 3), valtes
!     ON POURRAIT SE PASSER DE PGLI
    real(kind=8) :: angl1(3), angl2(3), angl3(3), rayon1, omega1
    real(kind=8) :: rayon2, theta1, theta2, angl4(3), pgl4(3, 3), test
    real(kind=8) :: zk1(3), zk2(3), zkini(3), zk4(3), zk3(3), epsi2
    real(kind=8) :: omega, dn1n2, omega2, coo1(3), coo2(3), dn1n4, dn3n2
    character(len=8) :: crit
! ......................................................................
!
!    - FONCTION REALISEE:  CALCUL DE LA GEOMETRIE COUDE
!                          TUYAU
!    - ARGUMENTS
!        DONNEES:      COOR       -->  CORDONNEES DE SNOEUDS
!         SORTIE:      ICOUDE      =0 DROIT =1 COUDE
!         SORTIE:      ANGL1,2,3
! ......................................................................
!
    integer :: icoude, i, izk, icoud1, icoud2
!
!     POUR VERIFICATIONS (PAS TRES EXIGEANTES) SUR LA GEOMETRIE
!     SINON, IL FAUDRAIT INTRODUIRE UN AUTRE MOT CLE PRECISON2
!     DIFFERENT DE PRECISION
!
    epsi2 = 1.d-4
!
!     POINTS 1 4 3
!
    do 1 i = 1, 3
        coor3(i)=coor(i)
        coor3(3+i)=coor(9+i)
        coor3(6+i)=coor(6+i)
        zkini(i)=zk1(i)
 1  end do
!
!
    call angcou(coor3, zkini, izk, icoud1, zk4,&
                rayon1, theta1, angl1, angl4, angl3,&
                pgl1, pgl4, pgl3, omega1, dn1n4,&
                epsi, crit, zk3)
!
!     POINTS 3 2 4
!
    do 2 i = 1, 3
        coor3(i) =coor(6+i)
        coor3(3+i)=coor(3+i)
        coor3(6+i)=coor(9+i)
        zkini(i)=zk3(i)
 2  end do
!
    call angcou(coor3, zkini, izk, icoud2, zk2,&
                rayon2, theta2, angl3, angl2, angl4,&
                pgl3, pgl2, pgl4, omega2, dn3n2,&
                epsi, crit, zk4)
!
    do 3 i = 1, 3
        coo1(i) =coor(i)
        coo2(i) = coor(3+i)
 3  end do
    dn1n2 = sqrt( (coo1(1)-coo2(1) )**2 + ( coo1(2)-coo2(2) )**2 + ( coo1(3)-coo2(3) )**2 )
!
    if (icoud2 .ne. icoud1) then
        ASSERT(.false.)
    else
        icoude=icoud2
        rayon=rayon2
    endif
!
    valtes = abs(rayon1)
    if (crit .eq. 'RELATIF') then
        if (valtes .lt. r8prem()) then
            test = r8prem()
        else
            test = epsi2*valtes
        endif
    else if (crit.eq.'ABSOLU') then
        test = epsi2
    endif
!
    if (abs(rayon2-rayon1) .gt. test) then
        ASSERT(.false.)
    else
        rayon=rayon2
    endif
!
    valtes = abs(theta1)
    if (crit .eq. 'RELATIF') then
        if (valtes .lt. r8prem()) then
            test = r8prem()
        else
            test = epsi2*valtes
        endif
    else if (crit.eq.'ABSOLU') then
        test = epsi2
    endif
!
    if (abs(theta2-theta1) .gt. test) then
        ASSERT(.false.)
    else
        theta=1.5d0*theta1
    endif
!
    valtes = abs(omega1)
    if (crit .eq. 'RELATIF') then
        if (valtes .lt. r8prem()) then
            test = r8prem()
        else
            test = epsi2*valtes
        endif
    else if (crit.eq.'ABSOLU') then
        test = epsi2
    endif
!
    if (abs(omega2-omega1) .gt. test) then
        ASSERT(.false.)
    else
        omega=omega1
    endif
end subroutine
