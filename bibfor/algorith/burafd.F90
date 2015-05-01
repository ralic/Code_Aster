subroutine burafd(materd, materf, nmat, afd, bfd,&
                  cfd)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: alexandre.foucault at edf.fr
!=====================================================================
!
! SOUS PROGRAMME QUI CALCUL LES MATRICE DE DEFORMATION DE FLUAGE
!   DE DESSICCATION D APRES LE MODELE DE BAZANT
!
!     - MODELE DE BAZANT : => EQUATION (3.3-2)
!
!   DFDES(N+1) = AFD + BFD * SIGMA(N) + CFD * SIGMA(N+1)
!
!    => EQUATION (3.3-1)
!     ----------------------------------------------------------------
!     IN
!          MATERD :  COEFFICIENTS MATERIAU A T
!          MATERF :  COEFFICIENTS MATERIAU A T+DT
!          NMAT   :  DIMENSION TABLEAU MATER
!     OUT
!          AFD     :  VECTEUR LIE AU FLUAGE DE DESSICCATION
!          BFD     :  TENSEUR LIE AU FLUAGE DE DESSICCATION
!          CFD     :  TENSEUR LIE AU FLUAGE DE DESSICCATION
!     ----------------------------------------------------------------
!=====================================================================
    implicit none
!     ----------------------------------------------------------------
    common /tdim/   ndt ,ndi
!     ----------------------------------------------------------------
    integer :: nmat, ndt, ndi
    integer :: i, j
    real(kind=8) :: materd(nmat, 2), materf(nmat, 2)
    real(kind=8) :: afd(6), bfd(6, 6), cfd(6, 6)
    real(kind=8) :: b, c, zero
    real(kind=8) :: etafd, dh
    data      zero/0.d0/
!
! === =================================================================
! RECUPERATION DES VALEURS DES PARAMETRES MATERIAU
! LE MODELE DE BAZANT NE COMPREND QU'UN PARAMETRE
! === =================================================================
    etafd = materd(8,2)
! === =================================================================
! --- INITIALISATION DES VECTEURS ET MATRICES
! === =================================================================
    do 1 i = 1, ndt
        afd(i) = zero
        do 2 j = 1, ndt
            bfd(i,j) = zero
            cfd(i,j) = zero
 2      continue
 1  end do
!
! === =================================================================
! INITIALISATION DES VARIABLES
! === =================================================================
!
    dh = abs(materf(6,1)-materd(6,1))
!
! === =================================================================
! AIGUILLAGE SUIVANT LA VALEUR DE ETAFD -> MODELE DE BAZANT
! === =================================================================
    if (etafd .gt. zero) then
        b = dh / (2.d0*etafd)
        c = b
! === =================================================================
! CONSTRUCTION DE LA MATRICE DE FLUAGE DE DESSICCATION
! === =================================================================
        do 10 i = 1, ndt
            bfd(i,i) = b
            cfd(i,i) = c
10      continue
    endif
!
end subroutine
