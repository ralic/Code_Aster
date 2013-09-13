subroutine rotama(geomi, pt, d, angl, bidim)
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
!     BUT : ROTATION D'AXE QUELCONQUE D'UN MAILLAGE
!
!     IN :
!            GEOMI  : CHAM_NO(GEOM_R) : CHAMP DE GEOMETRIE A TOURNER
!            NBNO   : NOMBRE DE NOEUDS DE GEOMI
!            PT     : POINT DE L'AXE DE ROTATION
!            D    : DECTION DE L'AXE DE ROTATION
!            ANGL   : ANGLE DE ROTATION
!            BIDIM  : BOOLEEN VRAI SI GEOMETRIE 2D
!     OUT:
!            GEOMI  : CHAM_NO(GEOM_R) : CHAMP DE GEOMETRIE ACTUALISE
!
!
! ----------------------------------------------------------------------
!
!
#include "jeveux.h"
#include "asterc/matfpe.h"
#include "asterc/r8dgrd.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
#include "blas/dnrm2.h"
    integer :: n1, i, iadcoo
    logical :: bidim
    character(len=19) :: geomi
    character(len=24) :: coorjv
    real(kind=8) :: angl, pt(3), d(3), p1mx, p1my, p1mz, ca, sa, p1m, prec
! ----------------------------------------------------------------------
!
    call matfpe(-1)
!
    call jemarq()
    coorjv=geomi(1:19)//'.VALE'
    call jeveuo(coorjv, 'E', iadcoo)
    call jelira(coorjv, 'LONMAX', n1)
    prec=1.d-14
    n1=n1/3
    angl=angl*r8dgrd()
    ca=cos(angl)
    sa=sin(angl)
    iadcoo=iadcoo-1
!     -- ON TRAITE LE CAS 2D SEPAREMENT POUR OPTIMISER :
    if (bidim) then
        do 10 i = 1, n1
            p1mx=zr(iadcoo+3*(i-1)+1)-pt(1)
            p1my=zr(iadcoo+3*(i-1)+2)-pt(2)
            zr(iadcoo+3*(i-1)+1)=pt(1)+ca*p1mx-sa*p1my
            zr(iadcoo+3*(i-1)+2)=pt(2)+ca*p1my+sa*p1mx
10      continue
    else
        if (dnrm2(3,d,1) .lt. prec) then
            call utmess('F', 'ALGORITH10_48')
        else
            p1m=dnrm2(3,d,1)
            d(1)=d(1)/p1m
            d(2)=d(2)/p1m
            d(3)=d(3)/p1m
            do 20 i = 1, n1
                p1mx=zr(iadcoo+3*(i-1)+1)-pt(1)
                p1my=zr(iadcoo+3*(i-1)+2)-pt(2)
                p1mz=zr(iadcoo+3*(i-1)+3)-pt(3)
                p1m=p1mx*d(1)+p1my*d(2)+p1mz*d(3)
                zr(iadcoo+3*(i-1)+1)=pt(1)+ ca*p1mx+(1-ca)*p1m*d(1)+&
                sa*(d(2)*p1mz-d(3)*p1my)
                zr(iadcoo+3*(i-1)+2)=pt(2)+ ca*p1my+(1-ca)*p1m*d(2)+&
                sa*(d(3)*p1mx-d(1)*p1mz)
                zr(iadcoo+3*(i-1)+3)=pt(3)+ ca*p1mz+(1-ca)*p1m*d(3)+&
                sa*(d(1)*p1my-d(2)*p1mx)
20          continue
        endif
    endif
    call jedema()
    call matfpe(1)
!
end subroutine
