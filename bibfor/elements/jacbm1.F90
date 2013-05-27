subroutine jacbm1(epais, vectg, vectt, matj, jm1,&
                  detj)
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
    implicit none
    real(kind=8) :: epais, vectg(2, 3), vectt(3, 3)
    real(kind=8) :: matj(3, 3), jm1(3, 3), detj
!
!     CONSTRUCTION DU JACOBIEN J (3,3) AUX X PTS D'INTEGRATION
!                                          X=REDUIT OU NORMAL
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    matj(1,1)=vectg(1,1)
    matj(1,2)=vectg(1,2)
    matj(1,3)=vectg(1,3)
!
    matj(2,1)=vectg(2,1)
    matj(2,2)=vectg(2,2)
    matj(2,3)=vectg(2,3)
!
    matj(3,1)=vectt(3,1)*epais/2.d0
    matj(3,2)=vectt(3,2)*epais/2.d0
    matj(3,3)=vectt(3,3)*epais/2.d0
!
!     CONSTRUCTION DE J-1 AUX X PTS D'INTEGRATION
!
!     CALCUL DU DETERMINANT
!
    detj= matj(1,1)*(matj(2,2)*matj(3,3)-matj(3,2)*matj(2,3))&
     &     -matj(1,2)*(matj(2,1)*matj(3,3)-matj(3,1)*matj(2,3))&
     &     +matj(1,3)*(matj(2,1)*matj(3,2)-matj(3,1)*matj(2,2))
!
    jm1(1,1)= (matj(2,2)*matj(3,3)-matj(2,3)*matj(3,2))/detj
    jm1(1,2)=-(matj(1,2)*matj(3,3)-matj(1,3)*matj(3,2))/detj
    jm1(1,3)= (matj(1,2)*matj(2,3)-matj(1,3)*matj(2,2))/detj
!
    jm1(2,1)=-(matj(2,1)*matj(3,3)-matj(2,3)*matj(3,1))/detj
    jm1(2,2)= (matj(1,1)*matj(3,3)-matj(1,3)*matj(3,1))/detj
    jm1(2,3)=-(matj(1,1)*matj(2,3)-matj(1,3)*matj(2,1))/detj
!
    jm1(3,1)= (matj(2,1)*matj(3,2)-matj(2,2)*matj(3,1))/detj
    jm1(3,2)=-(matj(1,1)*matj(3,2)-matj(1,2)*matj(3,1))/detj
    jm1(3,3)= (matj(1,1)*matj(2,2)-matj(1,2)*matj(2,1))/detj
!
end subroutine
