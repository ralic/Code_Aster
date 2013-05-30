subroutine calcn(s, b, vecn)
!
    implicit      none
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'blas/ddot.h'
    real(kind=8) :: b, s(6), vecn(6)
! ======================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
!
! ======================================================================
! ======================================================================
! --- BUT : CALCUL DE N ------------------------------------------------
! ======================================================================
! IN  : NDT    : NOMBRE TOTAL DE COMPOSANTES DU TENSEUR ----------------
! --- : NDI    : NOMBRE DE COMPOSANTES DIAGONALES DU TENSEUR -----------
! --- : S      : DEVIATEUR DES CONTRAINTES -----------------------------
! --- : B      : PARAMETRE DU CALCUL DE LA NORMALE ---------------------
! OUT : VECN   : N = (B*S/SII+I)/SQRT(B**2+3) --------------------------
! ======================================================================
    integer :: ii, ndt, ndi
    real(kind=8) :: sii, racine, un, trois
! ======================================================================
! --- INITIALISATION DE PARAMETRE --------------------------------------
! ======================================================================
    parameter       ( un      =   1.0d0  )
    parameter       ( trois   =   3.0d0  )
! ======================================================================
    common /tdim/   ndt , ndi
! ======================================================================
    call jemarq()
! ======================================================================
! --- INITIALISATION ---------------------------------------------------
! ======================================================================
    sii=ddot(ndt,s,1,s,1)
    sii = sqrt(sii)
! ======================================================================
! --- CALCUL DE N ------------------------------------------------------
! ======================================================================
    racine = sqrt(b*b + trois)
    do 10 ii = 1, ndt
        vecn(ii) = b*s(ii)/(sii*racine)
10  end do
    do 20 ii = 1, ndi
        vecn(ii) = vecn(ii) + un / racine
20  end do
! ======================================================================
    call jedema()
! ======================================================================
end subroutine
