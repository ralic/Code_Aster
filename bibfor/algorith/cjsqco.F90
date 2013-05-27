subroutine cjsqco(gamma, sig, x, pref, epssig,&
                  i1, s, sii, siirel, cos3ts,&
                  hts, dets, q, qii, qiirel,&
                  cos3tq, htq, detq)
    implicit none
! ======================================================================
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
!     ------------------------------------------------------------------
!     CALCUL DE GRANDEURS UTILES
!     ------------------------------------------------------------------
!     IN
!          GAMMA    :  PARAMETRE MATERIAU
!          SIG      :  CONTRAINTES
!          X        :  VARIABLES ECROUI CINE
!          PREF     :  PRESS REF POUR NORMALISATION
!          EPSSIG   :  EPSILON POUR NULLITE DEVIATEUR
!          I1       :  TRACE DU TENSEUR DES CONTRAINTES
!     OUT
!          S        : DEV(SIG)
!          SII      : SQRT(S:S)
!          SIIREL   : SII/PREF
!          COS3TS   : LODE(SIG)
!          HTS      : FONCTION H(TETHA_S)
!          DETS     : DETERMINANT DE S
!
!          Q        : Q(SIG-X)
!          QII      : SQRT(Q:Q)
!          QIIREL   : QII/PREF
!          COS3TQ
!          HTQ      : FONCTION H(TETHA_Q)
!          DETQ     : DETERMINANT DE Q
! ======================================================================
    include 'asterfort/cjsqij.h'
    include 'asterfort/cos3t.h'
    include 'asterfort/hlode.h'
    include 'asterfort/lcdete.h'
    include 'asterfort/lcdevi.h'
    include 'asterfort/lcprsc.h'
    integer :: ndt, ndi
    real(kind=8) :: gamma, pref, epssig
    real(kind=8) :: sig(6), x(6), s(6), q(6)
    real(kind=8) :: i1, sii, siirel, cos3ts, qii, qiirel, cos3tq
    real(kind=8) :: hts, dets, htq, detq
! ======================================================================
    common /tdim/   ndt, ndi
! ======================================================================
! --- CALCUL DES ANGLES DE LODE POUR S ET Q ----------------------------
! ======================================================================
    call lcdevi(sig, s)
    call lcdete(s, dets)
    call lcprsc(s, s, sii)
    sii = sqrt(sii)
    siirel = sii / pref
    cos3ts = cos3t(s, pref, epssig)
!
    call cjsqij(s, i1, x, q)
    call lcdete(q, detq)
    call lcprsc(q, q, qii)
    qii = sqrt(qii)
    qiirel = qii / pref
    cos3tq = cos3t(q, pref, epssig)
! ======================================================================
! --- CALCUL DE HT POUR LES ANGLES DE LODE DE S ET Q -------------------
! ======================================================================
    hts = hlode(gamma,cos3ts)
    htq = hlode(gamma,cos3tq)
! ======================================================================
end subroutine
