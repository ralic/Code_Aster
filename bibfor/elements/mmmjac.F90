subroutine mmmjac(alias, jgeom, ff, dff, laxis,&
                  nne, ndim, jacobi)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/subaco.h"
#include "asterfort/sumetr.h"
#include "asterfort/utmess.h"
    character(len=8) :: alias
    integer :: jgeom
    real(kind=8) :: ff(9), dff(2, 9)
    aster_logical :: laxis
    integer :: nne, ndim
    real(kind=8) :: jacobi
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
!
! CALCUL DU JACOBIEN D'UN ELEMENT
!
! ----------------------------------------------------------------------
!
!
! IN  ALIAS  : NOM DE L'ELEMENT
! IN  JGEOM  : ADRESSE DES COORDONNEES INITIALES DES NOEUDS DE L'ELEMENT
! IN  FF     : FONCTIONS DE FORMES EN XI,YI
! IN  DFF    : DERIVEES PREMIERES DES FONCTIONS DE FORME EN XI YI
! IN  LAXIS  : SI PROBLEME AXISYMETRIQUE
! IN  NNE    : NOMBRE DE NOEUDS ESCLAVES
! IN  NDIM   : DIMENSION DU PROBLEME
! OUT JACOBI : VALEUR DU JACOBIEN
!
!
! ----------------------------------------------------------------------
!
    integer :: inoe, idim
    real(kind=8) :: geom(3, 9)
    real(kind=8) :: dxdk, dydk, dzdk
    real(kind=8) :: r
    real(kind=8) :: cova(3, 3), metr(2, 2)
!
! ----------------------------------------------------------------------
!
    dxdk = 0.d0
    dydk = 0.d0
    dzdk = 0.d0
!
! --- COORDONNEES DES NOEUDS DANS LA CONFIGURATION DE REFERENCE
!
    do 100 inoe = 1, nne
        do 110 idim = 1, ndim
            geom(idim,inoe) = zr(jgeom-1+(inoe-1)*ndim+idim)
110     continue
100 end do
!
! --- ELEMENTS DE TYPE SEGMENT
! --- DISTINCTION 2D/3D/AXIS
!
    if (alias(1:2) .eq. 'SE') then
!       COMPOSANTES DU VECTEUR TANGENT
        do 90 inoe = 1, nne
            dxdk = dxdk + geom(1,inoe)*dff(1,inoe)
            dydk = dydk + geom(2,inoe)*dff(1,inoe)
            if (ndim .eq. 3) dzdk = dzdk + geom(3,inoe)*dff(1,inoe)
 90     continue
!       JACOBIEN 1D == DERIVEE DE L'ABSCISSE CURVILIGNE
        jacobi = sqrt(dxdk**2+dydk**2+dzdk**2)
!       TRAITEMENT AXISYMETRIE
        if (laxis) then
            r = 0.d0
            do 80 inoe = 1, nne
                r = r + geom(1,inoe)*ff(inoe)
 80         continue
            if (r .eq. 0.d0) then
                r=1.d-7
                call utmess('A', 'CONTACT2_14')
            endif
            jacobi = jacobi*abs(r)
        endif
!
! --- AUTRES ELEMENTS : DIMENSION 3
!
    else
        ASSERT(ndim.eq.3)
        call subaco(nne, dff, geom, cova)
        call sumetr(cova, metr, jacobi)
    endif
!
end subroutine
