subroutine q4gniw(qsi, eta, caraq4, dba, wsq)
    implicit  none
    real(kind=8) :: qsi, eta, caraq4(*), dba(2, 12), wsq(12)
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     ------------------------------------------------------------------
!     INTERPOLATION DE LA FLECHE AU POINTS D'INTEGRATION POUR LE Q4G
!     ------------------------------------------------------------------
    integer :: j
    real(kind=8) :: pqsi, mqsi, peta, meta, qsic, etac
    real(kind=8) :: x5, x6, x7, x8, y5, y6, y7, y8
    real(kind=8) :: n(12)
!     ------------------------------------------------------------------
    x5 = caraq4(1)
    x6 = caraq4(2)
    x7 = caraq4(3)
    x8 = caraq4(4)
    y5 = caraq4(5)
    y6 = caraq4(6)
    y7 = caraq4(7)
    y8 = caraq4(8)
!
    peta = 1.d0 + eta
    meta = 1.d0 - eta
    pqsi = 1.d0 + qsi
    mqsi = 1.d0 - qsi
    etac = 1.d0 - eta * eta
    qsic = 1.d0 - qsi * qsi
!
!     --- FONCTIONS D'INTERPOLATION DU DKQ DANS LE REPERE REDUIT -------
    n(1) = mqsi * meta / 8.d0 * (qsic + etac - qsi - eta)
    n(2) = mqsi * meta / 8.d0 * qsic
    n(3) = mqsi * meta / 8.d0 * etac
    n(4) = pqsi * meta / 8.d0 * (qsic + etac + qsi - eta)
    n(5) = - pqsi * meta / 8.d0 * qsic
    n(6) = pqsi * meta / 8.d0 * etac
    n(7) = pqsi * peta / 8.d0 * (qsic + etac + qsi + eta)
    n(8) = - pqsi * peta / 8.d0 * qsic
    n(9) = - pqsi * peta / 8.d0 * etac
    n(10) = mqsi * peta / 8.d0 * (qsic + etac - qsi + eta)
    n(11) = mqsi * peta / 8.d0 * qsic
    n(12) = - mqsi * peta / 8.d0 * etac
!
!     --- RAPPEL DE LA DISTORSION ( DBA ) ---------------
!
!     --- FONCTIONS D'INTERPOLATION DU DSQ DANS LE REPERE LOCAL --------
    do 140 j = 1, 12
        wsq(j) = (&
                 - dba(1,j)*x5 + dba(2,j)*x8) * n(2) + (- dba(1,j)* y5 + dba(2,j)*y8) * n(2) + (-&
                 & dba(1,j)*x5 - dba(2,j)*x6) * n( 5) + (- dba(1,j)*y5 - dba(2,j)*y6) * n(5) + ( &
                 &dba(1,j)*x7 - dba(2,j)*x6) * n(8) + ( dba(1,j)*y7 - dba(2,j)*y6) * n(8) + ( dba&
                 &(1,j)*x7 + dba(2,j)*x8) * n(11) + ( dba(1,j)*y7 + dba( 2,j)*y8) * n(11&
                 )
140  end do
!
! ----- FONCTIONS D'INTERPOLATION DANS LE REPERE LOCAL -------------
    wsq(1) = wsq(1) + n(1)
    wsq(2) = wsq(2) + (- x5*n(2) + x8*n(3) ) / 2.d0
    wsq(3) = wsq(3) + (- y5*n(2) + y8*n(3) ) / 2.d0
    wsq(4) = wsq(4) + n(4)
    wsq(5) = wsq(5) + (- x5*n(5) - x6*n(6) ) / 2.d0
    wsq(6) = wsq(6) + (- y5*n(5) - y6*n(6) ) / 2.d0
    wsq(7) = wsq(7) + n(7)
    wsq(8) = wsq(8) + ( x7*n(8) - x6*n(9) ) / 2.d0
    wsq(9) = wsq(9) + ( y7*n(8) - y6*n(9) ) / 2.d0
    wsq(10) = wsq(10) + n(10)
    wsq(11) = wsq(11) + ( x7*n(11) + x8*n(12)) / 2.d0
    wsq(12) = wsq(12) + ( y7*n(11) + y8*n(12)) / 2.d0
end subroutine
