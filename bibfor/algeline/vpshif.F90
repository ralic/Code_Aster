subroutine vpshif(lmatk, valshi, lmatm, lmatsh)
    implicit none
    include 'jeveux.h'
    include 'asterfort/mtcmbl.h'
    real(kind=8) :: valshi
    integer :: lmatk, lmatm, lmatsh
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ------------------------------------------------------------------
!     EFFECTUE LE DECCALAGE SPECTRALE :
!                  MSH =  K - W * M  (W ETANT LE SHIFT)
!     ------------------------------------------------------------------
! IN  LMATK  : IS : ADRESSE ATTRIBUT MATRICE K
! IN  VALSHI : R8 : VALEUR DU DECALAGE
! IN  LMATM  : IS : ADRESSE ATTRIBUT MATRICE M
! IN  LMATSH : IS : ADRESSE ATTRIBUT MATRICE SHIFTEE
!     ------------------------------------------------------------------
!
!
    real(kind=8) :: coef(2)
    character(len=1) :: typcst(2)
    character(len=8) :: nomddl
    character(len=24) :: nmat(2), nmatsh
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: nbcmb
!-----------------------------------------------------------------------
    data typcst/'R', 'R'/
    data nomddl /'        '/
!     ------------------------------------------------------------------
!
!     --- DECALAGE SPECTRAL  K - W * M    (W ETANT LE SHIFT) ---
    coef(1) = 1.d0
    coef(2) = -valshi
    nmat (1) = zk24(zi(lmatk+1))
    nmat (2) = zk24(zi(lmatm+1))
    nmatsh=zk24(zi(lmatsh+1))
    nbcmb = 2
    call mtcmbl(nbcmb, typcst, coef, nmat, nmatsh,&
                nomddl, ' ', 'ELIM=')
end subroutine
