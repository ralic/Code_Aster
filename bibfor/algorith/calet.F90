subroutine calet(ndim, fm, fma, fmp, edpn1,&
                 fmam, fta, etdpn1, jm, jp)
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
    implicit none
    include 'asterfort/matinv.h'
    include 'asterfort/pmat.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/utbtab.h'
    integer :: ndim
    real(kind=8) :: fma(3, 3), fmp(3, 3)
    real(kind=8) :: edpn1(3, 3)
    real(kind=8) :: fm(3, 3), work(9), det
!
! ------------------------------------------------------------------
!   NDIM    : DIMENSION DE L'ESPACE
!   FM      : TENSEUR DE DEFORMATION ENTRE CONFIGURATION INITIALE ET
!             CONFIGURATION A T-
!   FMA     : TENSEUR DE DEFORMATION ENTRE CONFIGURATION T- ET
!             CONFIGURATION A T_(N+ALPHA)
!   FMP     : TENSEUR DE DEFORMATION ENTRE CONFIGURATION T- ET
!             CONFIGURATION A T+
!   EDPN1   : TENSEUR
!
!   SORTIE:
!   FMAM    : INVERSE DU FMA
!   FTA     : TENSEUR PRODUIT DE FMP ET FMAM
!   ETDPN1  :
!   JM      : DETERMINANT DE FM
!   JP      : DETERMINANT DE FMP
!
!---------------------------------------------------------------
!
!       CALCUL DE ETDPN1
!
!---------------------------------------------------------------
    real(kind=8) :: fmam(3, 3), fta(3, 3)
    real(kind=8) :: etdpn1(3, 3), jm, jp
!
! ---------------------------INITIALISATION---------------------
    call r8inir(9, 0.d0, fmam, 1)
    call r8inir(9, 0.d0, fta, 1)
    call r8inir(9, 0.d0, etdpn1, 1)
    jm=0.d0
    jp=0.d0
!
!--------------------------------CALCUL DE e~_(n+alpha)---------
!      CALCUL DE f~_(n+alpha)
!
!      CALCUL DE L'INVERSE DE FMA = F_(N+ALPHA) NOTE FMAM
    call matinv('S', 3, fma, fmam, det)
!
!      CALCUL DE F_(n+1)* FMAM= f~_(n+alpha) = FTA
    call pmat(3, fmp, fmam, fta)
!
!      CALCUL DE e~_(n+alpha) = ETDPN1
    call utbtab('ZERO', 3, 3, edpn1, fta,&
                work, etdpn1)
!
!      DETERMINANT DES MATRICE F A L INSTANT T-(JM) ET T+(JP)
    jm = fm(1,1)*(fm(2,2)*fm(3,3)-fm(2,3)*fm(3,2)) - fm(2,1)*(fm(1,2)*fm(3,3)-fm(1,3)*fm(3,2)) + &
         &fm(3,1)*(fm(1,2)*fm(2,3)-fm(1,3)*fm(2,2))
    jp = fmp(1,1)*(fmp(2,2)*fmp(3,3)-fmp(2,3)*fmp(3,2)) - fmp(2,1)*(fmp(1,2)*fmp(3,3)-fmp(1,3)*fm&
         &p(3,2)) + fmp(3,1)*(fmp(1,2)*fmp(2,3)-fmp(1,3)*fmp(2,2))
!
end subroutine
