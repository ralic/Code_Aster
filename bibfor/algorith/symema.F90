subroutine symema(geomi, perp, pt)
    implicit none
! ----------------------------------------------------------------------
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
!
!     BUT : SYMETRIE D'UN MAILLAGE PAR RAPPORT A UN PLAN
!
!     IN :
!            GEOMI  : CHAM_NO(GEOM_R) : CHAMP DE GEOMETRIE A SYMETRISER
!            PERP   : AXE PERPENDICULAIRE AU PLAN
!            PT     : UN POINT DU PLAN
!     OUT:
!            GEOMI  : CHAM_NO(GEOM_R) : CHAMP DE GEOMETRIE ACTUALISE
!
! ----------------------------------------------------------------------
!
!
#include "jeveux.h"
#include "asterc/matfpe.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
#include "blas/ddot.h"
#include "blas/dnrm2.h"
    character(len=24) :: coorjv
    character(len=19) :: geomi
    real(kind=8) :: norm, prec, xd, pti(3), pt(3), perp(3), dist
    integer :: i, iadcoo, n1
!
! ----------------------------------------------------------------------
!
    call matfpe(-1)
!
    call jemarq()
!     RECUPERATION DE L'ADRESSE DES COORDONNEES ET DU NOMBRE DE POINTS
    coorjv=geomi(1:19)//'.VALE'
    call jeveuo(coorjv, 'E', iadcoo)
    call jelira(coorjv, 'LONMAX', n1)
    iadcoo = iadcoo - 1
    n1=n1/3
!
!     NORMALISATION DE PERP
    prec=1.d-14
    norm = dnrm2(3,perp,1)
    if (norm .lt. prec) then
        call utmess('F', 'ALGORITH10_87')
    endif
    perp(1) = perp(1)/norm
    perp(2) = perp(2)/norm
    perp(3) = perp(3)/norm
!     LE PLAN PASSE PAR "PT"
    xd = -ddot(3,perp,1,pt,1)
!
!     BOUCLE SUR TOUS LES POINTS
    do 10 i = 1, n1
        pti(1) = zr(iadcoo+3*(i-1)+1)
        pti(2) = zr(iadcoo+3*(i-1)+2)
        pti(3) = zr(iadcoo+3*(i-1)+3)
        dist = ddot(3,perp,1,pti,1) + xd
        zr(iadcoo+3*(i-1)+1) = -2.0d0*dist*perp(1) + pti(1)
        zr(iadcoo+3*(i-1)+2) = -2.0d0*dist*perp(2) + pti(2)
        zr(iadcoo+3*(i-1)+3) = -2.0d0*dist*perp(3) + pti(3)
10  end do
!
    call jedema()
    call matfpe(1)
!
end subroutine
