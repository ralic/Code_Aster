subroutine sigtmc(fami, nno, ndim, nbsig, npg,&
                  ni, xyz, instan, mater, repere,&
                  option, sigma)
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
!.======================================================================
    implicit none
!
!      SIGTMC   -- CALCUL DES  CONTRAINTES THERMIQUES/HYDRIQUE OU DE
!                  SECHAGE AUX POINTS D'INTEGRATION
!                  POUR LES ELEMENTS ISOPARAMETRIQUES
!
!   ARGUMENT        E/S  TYPE         ROLE
!    NNO            IN     I        NOMBRE DE NOEUDS DE L'ELEMENT
!    NDIM           IN     I        DIMENSION DE L'ELEMENT (2 OU 3)
!    NBSIG          IN     I        NOMBRE DE CONTRAINTES ASSOCIE
!                                   A L'ELEMENT
!    NPG            IN     I        NOMBRE DE POINTS D'INTEGRATION
!                                   DE L'ELEMENT
!    NI(1)          IN     R        FONCTIONS DE FORME
!    XYZ(1)         IN     R        COORDONNEES DES CONNECTIVITES
!    INSTAN         IN     R        INSTANT DE CALCUL (0 PAR DEFAUT)
!    MATER          IN     I        MATERIAU
!    REPERE(7)      IN     R        VALEURS DEFINISSANT LE REPERE
!                                   D'ORTHOTROPIE
!    OPTION         IN     K16      OPTION DE CALCUL
!    SIGMA(1)       OUT    R        CONTRAINTES THERMIQUES
!                                   AUX POINTS D'INTEGRATION
!
!.========================= DEBUT DES DECLARATIONS ====================
! -----  ARGUMENTS
#include "asterc/r8miem.h"
#include "asterfort/dmatmc.h"
#include "asterfort/epstmc.h"
#include "asterfort/lteatt.h"
    character(len=16) :: option
    real(kind=8) :: ni(1), xyz(1), repere(7), sigma(1)
    real(kind=8) :: instan
    character(len=*) :: fami
! -----  VARIABLES LOCALES
    real(kind=8) :: d(36), xyzgau(3), epsth(6)
    integer :: iepsv
    character(len=2) :: k2bid
!.========================= DEBUT DU CODE EXECUTABLE ==================
!
! --- INITIALISATIONS :
!     -----------------
!-----------------------------------------------------------------------
    integer :: i, idim, igau, j, mater, nbsig, ndim
    integer :: ndim2, nno, npg
    real(kind=8) :: zero
!-----------------------------------------------------------------------
    k2bid = '  '
    zero = 0.0d0
    ndim2 = ndim
    if (lteatt('FOURIER','OUI')) then
        ndim2 = 2
    endif
!
    sigma(1:nbsig*npg) = zero

!
! --- CALCUL DES CONTRAINTES D'ORIGINE THERMIQUE/HYDRIQUE/SECHAGE
! ---  BOUCLE SUR LES POINTS D'INTEGRATION
!      -----------------------------------
    do igau = 1, npg
!
!  --      COORDONNEES ET TEMPERATURE AU POINT D'INTEGRATION
!  --      COURANT
!          -------
        xyzgau(1) = zero
        xyzgau(2) = zero
        xyzgau(3) = zero
!
!       REM : HYDRATATION ET SECHAGE ACTIVES POUR CALCUL DU SECOND
!        MEMBRE CHAR_MECA_* OU CALCUL DES CONTRAINTES VRAIES (SIGVMC.F)
!
        do i = 1, nno
            do idim = 1, ndim2
                xyzgau(idim) = xyzgau(idim) + ni(i+nno*(igau-1))*xyz( idim+ndim2*(i-1))
            end do
        end do
!
!  --      CALCUL DES DEFORMATIONS THERMIQUES/HYDRIQUE/DE SECHAGE
!  --      AU POINT D'INTEGRATION COURANT
!          ------------------------------
        call epstmc(fami, ndim, instan, '+', igau,&
                    1, xyzgau, repere, mater, option,&
                    epsth)
!
! TEST DE LA NULLITE DES DEFORMATIONS DUES AUX VARIABLES DE COMMANDE
        iepsv=0
        do i = 1, 6
            if (abs(epsth(i)) .gt. r8miem()) iepsv=1
        end do
! TOUTES DES COMPOSANTES SONT NULLES. ON EVITE LE CALCUL DE D ET SIGMA
        if (iepsv .ne. 0) then
!
!         PASSAGE DES COMPOSANTES DE CISAILLEMENTS EN CONFORMITE
!         ( DMATMC RETOURNE UNE MATRICE DE HOOKE EN SUPPOSANT
!           UN DEUX SUR LES DEFORMATIONS DE CISAILLEMENT )
!
            do i = 4, 2*ndim
                epsth(i)=2.d0*epsth(i)
            end do
!  --      CALCUL DE LA MATRICE DE HOOKE (LE MATERIAU POUVANT
!  --      ETRE ISOTROPE, ISOTROPE-TRANSVERSE OU ORTHOTROPE)
!          -------------------------------------------------
            call dmatmc(fami, mater, instan, '+',igau, &
                        1, repere, xyzgau, nbsig,d)
!
!  --      CONTRAINTES THERMIQUES/HYDRIQUE/DE SECHAGE AU POINT
!  --      D'INTEGRATION COURANT
!          ------------------------------------------------------
            do i = 1, nbsig
                do j = 1, nbsig
                    sigma(i+nbsig*(igau-1)) = sigma( i+nbsig*(igau-1)) + d(j+(i-1)*nbsig)*epsth(j )
                end do
            end do
        endif
    end do
!
!.============================ FIN DE LA ROUTINE ======================
end subroutine
