subroutine rc32my(nbabsc, absc, vale, momen0, momen1)
    implicit   none
    integer :: nbabsc
    real(kind=8) :: absc(*), vale(*), momen0, momen1
!     ------------------------------------------------------------------
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ------------------------------------------------------------------
!     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3200
!
!     CALCUL DES MOYENNES
!     EXTRAIT DE LA ROUTINE RVPSTM
!
!     ------------------------------------------------------------------
!
    integer :: nbsgt, isgt
    real(kind=8) :: l, s1, s2, s12, t1, t2, t12, smil
! DEB ------------------------------------------------------------------
!
! --- LONGUEUR DU SEGMENT
!
    l = 1.0d0/ ( absc(nbabsc) - absc(1) )
    nbsgt = nbabsc - 1
!
    momen0 = 0.0d0
    momen1 = 0.0d0
!
    do 10, isgt = 1, nbsgt, 1
!
    s1 = absc(isgt) - absc(1)
    s2 = absc(isgt+1) - absc(1)
    s12 = s2 - s1
!
    t1 = vale(isgt)
    t2 = vale(isgt+1)
    t12 = (t1+t2)/2.0d0
!
    smil = (s1+s2)/2.0d0
    momen0 = momen0 + s12*(t1 + t2)
    momen1 = momen1 + s12/3.0d0 * (t1*s1 + 4.0d0*t12*smil + t2*s2)
!
    10 end do
!
    momen0 = momen0*l
    momen1 = momen1*l*l
!
    momen0 = 0.5d0*momen0
    momen1 = 6.0d0*(momen1 - momen0)
!
end subroutine
