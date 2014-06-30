subroutine epsvmc(fami, nno, ndim, nbsig, npg,&
                  ipoids, ivf, idfde, xyz, depl,&
                  instan, mater, repere, nharm, option,&
                  epsm)
    implicit none
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
!      EPSVMC   -- CALCUL DES  DEFORMATIONS MECANIQUES
!                  (I.E. EPS_TOTALES - EPS_THERMIQUES )
!                  AUX POINTS D'INTEGRATION POUR LES ELEMENTS
!                  ISOPARAMETRIQUES
!
!   ARGUMENT        E/S  TYPE         ROLE
!    FAMI           IN     K4       TYPE DE FAMILLE DE POINT DE GAUSS
!    NNO            IN     I        NOMBRE DE NOEUDS DE L'ELEMENT
!    NDIM           IN     I        DIMENSION DE L'ELEMENT (2 OU 3)
!    NBSIG          IN     I        NOMBRE DE CONTRAINTES ASSOCIE
!                                   A L'ELEMENT
!    NPG            IN     I        NOMBRE DE POINTS D'INTEGRATION
!                                   DE L'ELEMENT
!    IPOIDS         IN     I        POINTEUR POIDS D'INTEGRATION
!    IVF            IN     I        POINTEUR FONCTIONS DE FORME
!    IDFDE          IN     I        PT DERIVEES DES FONCTIONS DE FORME
!    XYZ(1)         IN     R        COORDONNEES DES CONNECTIVITES
!    DEPL(1)        IN     R        VECTEUR DES DEPLACEMENTS SUR
!                                   L'ELEMENT
!    INSTAN         IN     R        INSTANT DE CALCUL (0 PAR DEFAUT)
!    MATER          IN     I        MATERIAU
!    NHARM          IN     R        NUMERO D'HARMONIQUE
!    OPTION         IN     K16      OPTION DE CALCUL
!    EPSM(1)        OUT    R        DEFORMATIONS MECANIQUES AUX
!                                   POINTS D'INTEGRATION
!
!.========================= DEBUT DES DECLARATIONS ====================
! -----  ARGUMENTS
#include "jeveux.h"
#include "asterfort/dmatmc.h"
#include "asterfort/eps1mc.h"
#include "asterfort/eps2mc.h"
#include "asterfort/epthmc.h"
#include "asterfort/lteatt.h"
#include "asterfort/rccoma.h"
#include "asterfort/utmess.h"
    character(len=16) :: option
    character(len=4) :: fami
    real(kind=8) :: xyz(1), depl(1), epsm(1), repere(7)
    real(kind=8) :: instan, nharm
    integer :: idfde, ipoids, ivf, mater, nbsig, ndim, nno, npg
! -----  VARIABLES LOCALES
    character(len=8) :: phenom
    real(kind=8) :: epsth(162), eps2(162), xyzgau(3), d(4, 4)
    real(kind=8) :: zero, un, deux
    integer :: i, igau, icodre(1)
    logical(kind=1) :: l_modi_cp
!.========================= DEBUT DU CODE EXECUTABLE ==================
!
! --- INITIALISATIONS :
!     -----------------
    zero = 0.0d0
    un = 1.0d0
    deux = 2.0d0
!
    epsm(1:nbsig*npg)  = zero
    eps2(1:nbsig*npg)  = zero
    epsth(1:nbsig*npg) = zero

!
!
! --- CALCUL DES DEFORMATIONS DU PREMIER ORDRE
! --- AUX POINTS D'INTEGRATION :
!      -----------------------
    call eps1mc(nno, ndim, nbsig, npg, ipoids,&
                ivf, idfde, xyz, depl, nharm,&
                epsm)
!
! ---   CALCUL DES DEFORMATIONS DU SECOND ORDRE AUX POINTS
! ---   D'INTEGRATION POUR LES GRANDES TRANSFORMATIONS :
!       ----------------------------------------------
    if (option(4:4) .eq. 'G') then
        call eps2mc(nno, ndim, nbsig, npg, ipoids,&
                    ivf, idfde, xyz, depl, eps2)
    endif
!
! --- CALCUL DES DEFORMATIONS THERMIQUES AUX POINTS D'INTEGRATION
! --- (AJOUTEES AUX DEFORMATIONS DE RETRAIT ENDOGENE/DESSICCATION)
!      ----------------------------------------------------------
    call rccoma(mater, 'ELAS', 1, phenom, icodre(1))
    if (phenom(1:8) .ne. 'ELAS_MET') then
        call epthmc(fami, nno, ndim, nbsig, npg,&
                    zr(ivf), xyz, repere, instan, mater,&
                    option, epsth)
    else if (option(1:4).eq.'EPME'.or.option(1:4).eq.'EPMG') then
        call utmess('F', 'ELEMENTS_15', sk=phenom)
    endif
!
! --- CALCUL DES DEFORMATIONS MECANIQUES AUX POINTS D'INTEGRATION :
!      ----------------------------------------------------------
    do i = 1, nbsig*npg
        epsm(i) = epsm(i) + eps2(i)
    end do
!
    if (option(1:4) .eq. 'EPME' .or. option(1:4) .eq. 'EPMG') then
        do i = 1, nbsig*npg
            epsm(i) = epsm(i) - epsth(i)
        end do
    endif
!
! --- CAS DES CONTRAINTES PLANES, ON CALCULE EPSZZ A PARTIR
! --- DE SIGZZ = 0 :
!     ------------
    if (lteatt('C_PLAN','OUI')) then
!
! ---   BOUCLE SUR LES POINTS D'INTEGRATION :
!       -----------------------------------
        do igau = 1, npg
!
!  --      COORDONNEES AU POINT D'INTEGRATION
!  --      COURANT
!          -------
            xyzgau(1) = zero
            xyzgau(2) = zero
            xyzgau(3) = zero

!  --     il s'agit de calculer EPS33 : pour cela il faut donner la 
!  --     condition SIG33=0 dans l'expression complete de la loi de 
!  --     Hooke c'est à dire avec la loi 3D : 
!  --     Eps33= -1/D33 (D13.Eps11 +D12.Eps22), ce qui donne (en 
!  --     isotrope) l'expression classique : 
!  --     Eps33 = -Nu / (1-Nu) * (Eps11 + Eps22).
! ---     voir issue12540


            l_modi_cp = .true.
!
!  --      CALCUL DE LA MATRICE DE HOOKE (LE MATERIAU POUVANT
!  --      ETRE ISOTROPE, ISOTROPE-TRANSVERSE OU ORTHOTROPE)
!          -------------------------------------------------
            call dmatmc(fami, mater, instan, '+', igau, &
                        1, repere, xyzgau, nbsig, d,&
                        l_modi_cp)
!
            if (option(1:4) .eq. 'EPME' .or. option(1:4) .eq. 'EPMG') then
                epsm(nbsig*(igau-1)+3) = -un/d(3,3)* ( d(3,1)*epsm( nbsig*(igau-1)+1) + d(3,2)*ep&
                                         &sm(nbsig*(igau-1)+2) + d(3,4)*epsm(nbsig*(igau-1)+4)*de&
                                         &ux)
            else
                epsm(nbsig*(igau-1)+3) = -un/d(3,3)* (d(3,1)*(epsm( nbsig*(igau-1)+1)-epsth(nbsig&
                                         &*(igau-1)+1)) +d(3,2)*( epsm(nbsig*(igau-1)+2)-epsth(nb&
                                         &sig*(igau-1)+2)) +d(3,4)*(epsm(nbsig*(igau-1)+4) -epsth&
                                         &(nbsig*(igau-1)+ 4))*deux)+epsth(nbsig*(igau-1)+3)
            endif
        end do
!
! --- CAS DES DEFORMATIONS PLANES,  EPSZZ = 0 :
!     ---------------------------------------
    else if (lteatt('D_PLAN','OUI')) then
!
! ---   BOUCLE SUR LES POINTS D'INTEGRATION :
!       -----------------------------------
        do igau = 1, npg
            epsm(nbsig*(igau-1)+3) = zero
        end do
!
    endif
!
!.============================ FIN DE LA ROUTINE ======================
end subroutine
