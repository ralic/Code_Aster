subroutine lcdvmi(sigma, y, f, dfds, d2fds,&
                  seq)
!-----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
!-----------------------------------------------------------------------
    implicit none
    real(kind=8) :: y, sigma(6), f, seq, dfds(6), d2fds(6, 6)
!-----------------------------------------------------------------------
!  ROUTINE D EVALUATION DU CRITERE DE VON-MISES ISOTROPE
!  ET DE SES DERIVEES PARTIELLES PAR RAPPORT A SIGMA
!
!        !!! DONNEES ET RESULTATS PAR PG !!!
!-----------------------------------------------------------------------
!  ENTREES
!    SIGMA : VECTEUR CONTRAINTE
!    Y     : VALEUR COURANTE DU CRITERE
!
!  SORTIES
!    F     : CRITERE (NUL A LA CONVERGENCE)
!    DFDS  : DERIVEE DE F PAR RAPPORT AUX CONTRAINTES
!    D2FDS : DERIVEE SECONDE DE F PAR RAPPORT AUX CONTRAINTES
!    SEQ   : CONTRAINTE EQUIVALENTE (= Y A LA CONVERGENCE)
!-----------------------------------------------------------------------
    integer :: i, j
    real(kind=8) :: s23, s31, s12, t12, t23, t31
!-----------------------------------------------------------------------
!  CALCUL DE F
!  -----------
!
    s23 = (sigma(2)-sigma(3))*(sigma(2)-sigma(3))
    s31 = (sigma(3)-sigma(1))*(sigma(3)-sigma(1))
    s12 = (sigma(1)-sigma(2))*(sigma(1)-sigma(2))
    t12 = sigma(4)*sigma(4)
    t23 = sigma(5)*sigma(5)
    t31 = sigma(6)*sigma(6)
!
    seq = sqrt((s23+s31+s12)/2.d0+1.5d0*(t12+t23+t31))
!
    f = seq - y
!
    if (seq .lt. 1.d-9) goto 20
!
!  CALCUL DE DF/DSIG (DFDS(6))
!  ---------------------------
!
    dfds(1) = (2.d0*sigma(1)-sigma(2)-sigma(3))/(2.d0*seq)
    dfds(2) = (2.d0*sigma(2)-sigma(3)-sigma(1))/(2.d0*seq)
    dfds(3) = (2.d0*sigma(3)-sigma(1)-sigma(2))/(2.d0*seq)
    dfds(4) = 3.d0*sigma(4)/seq/2.d0
    dfds(5) = 3.d0*sigma(5)/seq/2.d0
    dfds(6) = 3.d0*sigma(6)/seq/2.d0
!
    d2fds(1,1) = ( 1.0d0-dfds(1)*dfds(1) )/seq
    d2fds(1,2) = (-0.5d0-dfds(1)*dfds(2) )/seq
    d2fds(1,3) = (-0.5d0-dfds(1)*dfds(3) )/seq
    d2fds(1,4) = ( -dfds(1)*dfds(4) )/seq
    d2fds(1,5) = ( -dfds(1)*dfds(5) )/seq
    d2fds(1,6) = ( -dfds(1)*dfds(6) )/seq
!
    d2fds(2,2) = ( 1.0d0-dfds(2)*dfds(2) )/seq
    d2fds(2,3) = (-0.5d0-dfds(2)*dfds(3) )/seq
    d2fds(2,4) = ( -dfds(2)*dfds(4) )/seq
    d2fds(2,5) = ( -dfds(2)*dfds(5) )/seq
    d2fds(2,6) = ( -dfds(2)*dfds(6) )/seq
!
    d2fds(3,3) = ( 1.0d0-dfds(3)*dfds(3) )/seq
    d2fds(3,4) = ( -dfds(3)*dfds(4) )/seq
    d2fds(3,5) = ( -dfds(3)*dfds(5) )/seq
    d2fds(3,6) = ( -dfds(3)*dfds(6) )/seq
!
    d2fds(4,4) = ( 3.d0-dfds(4)*dfds(4) )/seq
    d2fds(4,5) = ( -dfds(4)*dfds(5) )/seq
    d2fds(4,6) = ( -dfds(4)*dfds(6) )/seq
!
    d2fds(5,5) = ( 3.d0-dfds(5)*dfds(5) )/seq
    d2fds(5,6) = ( -dfds(5)*dfds(6) )/seq
!
    d2fds(6,6) = ( 3.d0-dfds(6)*dfds(6) )/seq
!
    do 10 i = 1, 6
        do 10 j = i, 6
            d2fds(j,i) = d2fds(i,j)
10      continue
!
20  continue
!
end subroutine
