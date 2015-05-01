subroutine ethdst(fami, nno, ndim, nbsig, npg,&
                  ipoids, ivf, idfde, xyz, depl,&
                  instan, repere, mater, option, enthth)
    implicit none
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
!
!      ETHDST   -- CALCUL DU TERME EPSTHT*D*EPSTH RENTRANT
!                  DANS LE CALCUL DE L'ENERGIE POTENTIELLE
!                  (I.E.  1/2*UT*K*U - UT*FTH + 1/2*EPSTHT*D*EPSTH)
!                  POUR LES ELEMENTS ISOPARAMETRIQUES
!
!
!   ARGUMENT        E/S  TYPE         ROLE
!    NNO            IN     I        NOMBRE DE NOEUDS DE L'ELEMENT
!    NDIM           IN     I        DIMENSION DE L'ELEMENT (2 OU 3)
!    NBSIG          IN     I        NOMBRE DE CONTRAINTES ASSOCIE
!                                   A L'ELEMENT
!    NPG            IN     I        NOMBRE DE POINTS D'INTEGRATION
!                                   DE L'ELEMENT
!    IPOIDS         IN     I        POIDS D'INTEGRATION
!    IVF            IN     I        FONCTIONS DE FORME
!    IDFDE          IN     I        DERIVEES DES FONCTIONS DE FORME
!    XYZ(1)         IN     R        COORDONNEES DES CONNECTIVITES
!    DEPL(1)        IN     R        VECTEUR DES DEPLACEMENTS SUR
!                                   L'ELEMENT
!    INSTAN         IN     R        INSTANT DE CALCUL (0 PAR DEFAUT)
!    REPERE(7)      IN     R        VALEURS DEFINISSANT LE REPERE
!                                   D'ORTHOTROPIE
!    MATER          IN     I        MATERIAU
!    OPTION         IN     K16      OPTION DE CALCUL
!    ENTHTH         OUT    R        SOMME(EPSTH_T*D*EPSTH)
!
!.========================= DEBUT DES DECLARATIONS ====================
! -----  ARGUMENTS
#include "jeveux.h"
#include "asterfort/dfdm2d.h"
#include "asterfort/dfdm3d.h"
#include "asterfort/epthmc.h"
#include "asterfort/lteatt.h"
#include "asterfort/sigtmc.h"
!
    integer :: ipoids, ivf, idfde
    character(len=16) :: option
    character(len=*) :: fami
    real(kind=8) :: xyz(*), depl(*), repere(7)
    real(kind=8) :: instan, enthth
! -----  VARIABLES LOCALES
    integer :: i, mater, nbsig, ndim, nno, npg, k, igau
    character(len=16) :: k16bid
    real(kind=8) :: sigth(162), zero
    real(kind=8) :: rayon
    real(kind=8) :: epsith(162), enthpg, dfdx(27), dfdy(27), dfdz(27)
    real(kind=8) :: poidi
!.========================= DEBUT DU CODE EXECUTABLE ==================
!
! --- INITIALISATIONS :
!     -----------------
    zero = 0.0d0
    k16bid = ' '
    enthth = zero
!
! --- CALCUL DES CONTRAINTES MECANIQUES AUX POINTS D'INTEGRATION
!      ---------------------------------------------------------
    call epthmc(fami, nno, ndim, nbsig, npg,&
                zr(ivf), xyz, repere, instan, mater,&
                option, epsith)
!
! --- CALCUL DES CONTRAINTES THERMIQUES AUX POINTS D'INTEGRATION
!      ---------------------------------------------------------
    call sigtmc(fami, nno, ndim, nbsig, npg,&
                zr(ivf), xyz, instan, mater, repere,&
                k16bid, sigth)
!
! --- CALCUL DES CONTRAINTES TOTALES AUX POINTS D'INTEGRATION
!      ---------------------------------------------------------
    do 20 igau = 1, npg
        enthpg=0.d0
! ----  CALCUL DU JACOBIEN*POIDS - CAS MASSIF 3D
!
        if (lteatt('DIM_TOPO_MAILLE','3')) then
            call dfdm3d(nno, igau, ipoids, idfde, xyz,&
                        poidi, dfdx, dfdy, dfdz)
! ----  CALCUL DU JACOBIEN*POIDS - CAS MASSIF 2D
        else
            k=(igau-1)*nno
            call dfdm2d(nno, igau, ipoids, idfde, xyz,&
                        poidi, dfdx, dfdy)
            if (lteatt('AXIS','OUI')) then
                rayon = 0.d0
                do 41 i = 1, nno
                    rayon = rayon + zr(ivf+k-1+i)*xyz(2*(i-1)+1)
41              continue
                poidi=poidi*rayon
            endif
        endif
        do 30 i = 1, nbsig
            enthpg = enthpg+epsith(i+nbsig*(igau-1))* sigth(i+nbsig*( igau-1))
30      continue
        enthth = enthth+(enthpg*poidi)
20  end do
!
!.============================ FIN DE LA ROUTINE ======================
end subroutine
