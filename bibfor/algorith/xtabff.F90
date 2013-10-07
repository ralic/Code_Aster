subroutine xtabff(nbfond, nfon, ndim, fiss)
!
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
! person_in_charge: samuel.geniaut at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/ltcrsd.h"
#include "asterfort/ltnotb.h"
#include "asterfort/tbajli.h"
#include "asterfort/tbajpa.h"
#include "asterfort/tbcrsd.h"
    integer :: ndim, nbfond, nfon
    character(len=8) :: fiss
!
!
! ----------------------------------------------------------------------
!
! ROUTINE XFEM (PREPARATION)
!
!     CONSTRUCTION DES 2 TABLES SUR LES FONDS DE FISSURE :
!             - TABLE DES COORDONNEES DES FONDS DE FISSURE
!             - TABLE DU NOMBRE DE FONDS DE FISSURE
!
! ----------------------------------------------------------------------
!
!
! I/O FISS   : NOM DE LA FISSURE
! IN  NBFOND : NOMBRE DE FONDS DE FISSURES DETECTES
! IN  NFON   : NOMBRE DE POINTS DE FOND DE FISSURE
! IN  NDIM   : DIMENSION DE L'ESPACE
!
!
!
!
    integer :: ifm, niv, i
    integer :: npara, nfonl, nfondl, vali(2)
    integer :: jmult, jfon
    real(kind=8) :: vale(4), r8bid
    character(len=1) :: typar2(3), typar3(6)
    character(len=8) :: k8bid
    character(len=12) :: nopar2(3), nopar3(6)
    character(len=19) :: tabcoo, tabnb
    complex(kind=8) :: c16b
    data nopar2 /'NUME_FOND','COOR_X','COOR_Y'/
    data typar2 /'I'        ,'R'     ,'R'/
    data nopar3 /'NUME_FOND','NUM_PT','ABSC_CURV',&
     &                                       'COOR_X','COOR_Y','COOR_Z'/
    data typar3 /'I'        ,'I'    ,'R'     ,'R',       'R'     ,'R'/
!
! ----------------------------------------------------------------------
!
    call jemarq()
    c16b=(0.d0,0.d0)
    r8bid=0.d0

    call infdbg('XFEM', ifm, niv)
!
!     S'IL N'Y A PAS DE FOND DE FISSURE ON SORT
    if (nbfond .eq. 0) goto 999
!
    call jeveuo(fiss//'.FONDMULT', 'L', jmult)
    call jeveuo(fiss//'.FONDFISS', 'L', jfon)
!
    call ltcrsd(fiss, 'G')
!
!     ------------------------------------------------------------------
!     CONSTRUCTION DE LA TABLE DES COORDONNEES DES FONDS DE FISSURE
!     ------------------------------------------------------------------
!
    call ltnotb(fiss, 'FOND_FISS', tabcoo)
    call tbcrsd(tabcoo, 'G')
!
    if (ndim .eq. 2) then
        npara = 3
        call tbajpa(tabcoo, npara, nopar2, typar2)
        do 100 i = 1, nbfond
            vali(1)=i
            vale(1)=zr(jfon-1+4*(i-1)+1)
            vale(2)=zr(jfon-1+4*(i-1)+2)
            call tbajli(tabcoo, npara, nopar2, vali, vale,&
                        [c16b], k8bid, 0)
100      continue
    else if (ndim.eq.3) then
        npara = 6
        call tbajpa(tabcoo, npara, nopar3, typar3)
        nfonl = 1
        nfondl = 0
        do 200 i = 1, nfon
            if (zi(jmult-1+2*nfondl+1) .eq. i) then
                nfondl = nfondl + 1
                nfonl = 1
            else
                nfonl = nfonl + 1
            endif
            vali(1)=nfondl
            vali(2)=nfonl
            vale(1)=zr(jfon-1+4*(i-1)+4)
            vale(2)=zr(jfon-1+4*(i-1)+1)
            vale(3)=zr(jfon-1+4*(i-1)+2)
            vale(4)=zr(jfon-1+4*(i-1)+3)
            call tbajli(tabcoo, npara, nopar3, vali, vale,&
                        [c16b], k8bid, 0)
200      continue
    else
        ASSERT(.false.)
    endif
!
!     ------------------------------------------------------------------
!     CONSTRUCTION DE LA TABLE DU NOMBRE DE FONDS DE FISSURE
!     ------------------------------------------------------------------
!
    call ltnotb(fiss, 'NB_FOND_FISS', tabnb)
    call tbcrsd(tabnb, 'G')
    call tbajpa(tabnb, 1, 'NOMBRE', 'I')
    call tbajli(tabnb, 1, 'NOMBRE', [nbfond], [r8bid],&
                [c16b], k8bid, 0)
!
999  continue
    call jedema()
end subroutine
