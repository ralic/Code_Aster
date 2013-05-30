subroutine xdelt2(elp, nno, ndim, ksi, ptint,&
                  tabco, jtabls, ipp, ip, delta)
    implicit none
!
    include 'jeveux.h'
    include 'asterc/r8prem.h'
    include 'asterfort/assert.h'
    include 'asterfort/elrfdf.h'
    include 'asterfort/elrfvf.h'
    integer :: ndim, nno, jtabls, ipp, ip
    real(kind=8) :: ksi(ndim), delta(ndim), ptint(*), tabco(*)
    character(len=8) :: elp
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! TOLE CRS_1404
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
!                 CALCUL DE LA QUANTITE A MINIMISER POUR LE CALCUL
!                    DES COORDONNEES DU PT MILIEU DE LA FISSURE
!
!     ENTREE
!       NDIM    : DIMENSION TOPOLOGIQUE DU MAILLAGE
!       KSI     : COORDONNEES DE REFERENCE DU POINT
!       PINTER  : COORDONNÉES DES POINTS D'INTERSECTION
!       TABCO   : COORDONNEES DES NOEUDS DE L'ELEMENT
!       JTABLS  : VALEUR DES LSN DES NOEUDS DE L'ELEMENT
!
!     SORTIE
!       DELTA   : QUANTITE A MINIMISER
!     ----------------------------------------------------------------
!
    integer :: nbnomx
    parameter     (nbnomx = 27)
!
    real(kind=8) :: ff(nno), dff(3, nbnomx)
    integer :: i, nderiv
    real(kind=8) :: c, d, delta3, pint1(ndim), pint2(ndim)
    real(kind=8) :: fctf, fctg
    real(kind=8) :: d1fctf, d1fctg
    real(kind=8) :: d2fctf, d2fctg
!
!......................................................................
!
! --- CALCUL DES FONCTIONS DE FORME ET DE LEUR DERIVEES EN UN POINT
! --- DANS LA MAILLE
!
!
    fctf = 0.d0
    fctg = 0.d0
    d1fctf = 0.d0
    d2fctf = 0.d0
    d1fctg = 0.d0
    d2fctg = 0.d0
    do 10 i = 1, ndim
        pint1(i)=ptint(ndim*(ipp-1)+i)
        pint2(i)=ptint(ndim*(ip-1)+i)
10  end do
!
!     CALCUL DES FONCTIONS DE FORME DE L'ELEMENT EN KSI
    call elrfvf(elp, ksi, nbnomx, ff, nno)
!
!     CALCUL DES DERIVEES FONCTIONS DE FORME DE L'ELEMENT EN KSI
    call elrfdf(elp, ksi, ndim*nbnomx, dff, nno,&
                nderiv)
!
! --- CALCUL DE FCTF EN KSI
! ---           FCTF : MEDIATRICE DU SEGMENT PASSANT PAR LES 2 PTS INTER
!               SPECIFIQUE NDIM=2
!     SI SEGMENT HORIZONTAL --> MEDIATRICE VERTICALE
    if (pint1(2) .eq. pint2(2)) then
!        CHAQUE POINT DE LA MEDIATRICE VERIFIE X = C (SOIT X-C=0)
!                   C = (XI+XII)/2
!                   X = SUM(FF*TABCO(1))
        c = (pint1(1)+pint2(1))/2
        do 101 i = 1, nno
            fctf = fctf + ff(i)*tabco(ndim*(i-1)+1)
101      continue
        fctf = fctf - c
!
!        CALCUL DES DERIVEES PREMIERES DE F EN KSI
        do 102 i = 1, nno
            d1fctf = d1fctf + dff(1,i)*tabco(ndim*(i-1)+1)
            d2fctf = d2fctf + dff(2,i)*tabco(ndim*(i-1)+1)
102      continue
!
!     SI SEGMENT NON HORIZONTAL --> MEDIATRICE INCLINEE
    else
!     CHAQUE POINT DE LA MEDIATRICE VERIFIE Y = C*X+D (SOIT Y-C*X-D=0)
!                   C = -(XII-XI)/YII-YI
!                   D = Y-C*X = (YII+YI)/2 - C*(XII+XI)/2
!                   X = SUM(FF*TABCO(1))
!                   Y = SUM(FF*TABCO(2))
!
        c = pint1(1)-pint2(1)
        c = c/(pint2(2)-pint1(2))
!
        d = (pint2(2)+pint1(2))/2
        d = d-c*(pint1(1)+pint2(1))/2
!
        do 103 i = 1, nno
            fctf = fctf + ff(i)*(tabco(ndim*(i-1)+2)- c*tabco(ndim*(i- 1)+1))
103      continue
!
        fctf = fctf - d
!
!        CALCUL DES DERIVEES PREMIERES DE F EN KSI
        do 104 i = 1, nno
            d1fctf = d1fctf + dff(1,i)*(tabco(ndim*(i-1)+2)- c*tabco( ndim*(i-1)+1))
            d2fctf = d2fctf + dff(2,i)*(tabco(ndim*(i-1)+2)- c*tabco( ndim*(i-1)+1))
104      continue
!
    endif
!
! --- CALCUL DE FCTG,D1FCTG,D2FCTG EN KSI
! ---           FCTG : LEVEL SET NORMALE
    do 105 i = 1, nno
        fctg = fctg + ff(i)*zr(jtabls-1+i)
        d1fctg = d1fctg + dff(1,i)*zr(jtabls-1+i)
        d2fctg = d2fctg + dff(2,i)*zr(jtabls-1+i)
105  continue
!
! --- CALCUL DES QUANTITES A MINIMISER
!     CALCUL DE DELTAS
    delta3 = d1fctf*d2fctg - d2fctf*d1fctg
    call assert(abs(delta3).gt.r8prem())
    delta(1) = (fctf*d2fctg - fctg*d2fctf) / delta3
    delta(2) = (fctg*d1fctf - fctf*d1fctg) / delta3
!
end subroutine
