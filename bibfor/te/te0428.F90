subroutine te0428(option, nomte)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dkqrge.h"
#include "asterfort/dktrge.h"
#include "asterfort/dxqpgl.h"
#include "asterfort/dxtpgl.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/utpslg.h"
#include "asterfort/utpvgl.h"
    character(len=16) :: option, nomte

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
! ajout elements  
!
!    calcul de la matrice de rigidite geometrique des elements de plaque
!       => option rigi_meca_ge 
!
    integer :: ndim, nno, nnos, npg, ipoids, ivf, idfdx, jgano
    integer :: jgeom, jmatr, iret
    real(kind=8) :: pgl(3, 3), xyzl(3, 4)
!
!     ---> pour dkt/dktg  matelem = 3 * 6 ddl = 171 termes stockage syme
!     ---> pour dkq/dkqg  matelem = 4 * 6 ddl = 300 termes stockage syme
!
    real(kind=8) :: matloc(300)
!
! deb ------------------------------------------------------------------
!
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfdx, jgano)
!
    call jevech('PGEOMER', 'L', jgeom)
!
! --- calcul de la matrice de passage du repere global --> intrinseque
!
    if (nno .eq. 3) then
        call dxtpgl(zr(jgeom), pgl)
    else if (nno.eq.4) then
        call dxqpgl(zr(jgeom), pgl, 'S', iret)
    endif
!
    call utpvgl(nno, 3, pgl, zr(jgeom), xyzl)
!
    if (option .eq. 'RIGI_MECA_GE') then
!     --------------------------------------
!
        if ((nomte.eq.'MEDKTR3') .or. (nomte.eq.'MEDKTG3')) then
            call dktrge(nomte, xyzl, pgl, matloc)
        else if ((nomte.eq.'MEDKQU4').or.(nomte.eq.'MEDKQG4')) then
            call dkqrge(nomte, xyzl, pgl, matloc)
        else
! type d element invalide
            ASSERT(.false.)
        endif
!
! - stockage
!
        call jevech('PMATUUR', 'E', jmatr)
        call utpslg(nno, 6, pgl, matloc, zr(jmatr))
    else
!
! option de calcul invalide
!
        ASSERT(.false.)
    endif
!
end
