subroutine utjac(l2d, geom, ipg, idfde, niv,&
                 ifm, nno, jacob)
!-----------------------------------------------------------------------
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
! person_in_charge: olivier.boiteau at edf.fr
!-----------------------------------------------------------------------
!    - FONCTION REALISEE:  CALCUL LE JACOBIEN D'UN ELEMENT FINI K
!                          POUR AERER TE0003
!
! IN L2D    : FLAG INDICATEUR DU 2D
! IN GEOM   : LA GEOMETRIE
! IN IDFDE/DK/DN  : ADRESSE JEVEUX DES DERIVEES DES FONCTIONS DE FORME
! IN NIV    : NIVEAU D'IMPRESSION
! IN IFM    : UNITE LOGIQUE D'IMPRESSION
! IN NNO    : NOMBRE DE NOEUDS
! OUT JACOB : SIGNE DU JACOBIEN
!   -------------------------------------------------------------------
!     FONCTIONS INTRINSEQUES:
!       SIGN.
!   -------------------------------------------------------------------
!     ASTER INFORMATIONS:
!       18/09/01 (OB): CREATION POUR SIMPLIFIER TE0003.F.
!----------------------------------------------------------------------
! CORPS DU PROGRAMME
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/matini.h"
#include "asterfort/tecael.h"
#include "asterfort/utmess.h"
    integer :: ipg, idfde, niv, ifm, nno, ia1, ia2
    real(kind=8) :: jacob, geom(*)
    aster_logical :: l2d
!
    real(kind=8) :: valr
!
! DECLARATION VARIABLES LOCALES
    integer :: i, i1, j, kp, idfdk, idfdn
    integer :: vali
    real(kind=8) :: dxde, dxdk, dyde, dydk, xp, yp, dfrde, dfrdk, dfrdn, g(3, 3)
    real(kind=8) :: j11, j21, j31
!
! INIT
    if (l2d) then
        idfdk = idfde + 1
    else
        idfdn = idfde + 1
        idfdk = idfdn + 1
    endif
!
    if (l2d) then
! CAS 2D
        kp = 2*(ipg-1)*nno
        dxde=0.d0
        dxdk=0.d0
        dyde=0.d0
        dydk=0.d0
        do 100 i = 1, nno
            i1 = 2*(i-1)+1
            xp = geom(i1)
            yp = geom(i1+1)
            dfrde = zr(idfde+kp+i1-1)
            dfrdk = zr(idfdk+kp+i1-1)
            dxde = dxde+xp*dfrde
            dxdk = dxdk+xp*dfrdk
            dyde = dyde+yp*dfrde
            dydk = dydk+yp*dfrdk
100     continue
        jacob=dxde*dydk-dxdk*dyde
!
    else
! CAS 3D
!
        kp = 3*(ipg-1)*nno
        call matini(3, 3, 0.d0, g)
        do 140 i = 1, nno
            i1 = 3*(i-1)
            dfrde = zr(idfde+kp+i1)
            dfrdk = zr(idfdk+kp+i1)
            dfrdn = zr(idfdn+kp+i1)
            do 130 j = 1, 3
                xp = geom(i1+j)
                g(1,j) = g(1,j) + xp * dfrde
                g(2,j) = g(2,j) + xp * dfrdn
                g(3,j) = g(3,j) + xp * dfrdk
130         continue
140     continue
        j11 = g(2,2) * g(3,3) - g(2,3) * g(3,2)
        j21 = g(3,1) * g(2,3) - g(2,1) * g(3,3)
        j31 = g(2,1) * g(3,2) - g(3,1) * g(2,2)
        jacob = g(1,1)*j11 + g(1,2)*j21 + g(1,3)*j31
!
    endif
!
! EN 2D ON NE TESTE PAS LE SIGNE DU JACOBIEN
    if (.not.l2d .and. (jacob.lt.0.d0)) then
        call utmess('A+', 'CALCULEL6_73')
        call tecael(ia1, ia2)
        vali = zi(ia1)
        valr = jacob
        call utmess('A', 'CALCULEL6_74', si=vali, sr=valr)
    endif
!
! CALCUL DU SIGNE DU JACOBIEN + AFFICHAGE SI NECESSAIRE
    jacob = sign(1.d0,jacob)
    if (niv .eq. 2) write(ifm,*)'ORIENTATION MAILLE ',jacob
!
end subroutine
