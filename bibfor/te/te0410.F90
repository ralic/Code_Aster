subroutine te0410(optioz, nomtz)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
#include "jeveux.h"
#include "asterfort/jevech.h"
#include "asterfort/jevete.h"
#include "asterfort/tecach.h"
#include "asterfort/utmess.h"
#include "asterfort/vdefro.h"
#include "asterfort/vdrepe.h"
#include "asterfort/vdsiro.h"
#include "asterfort/vdxedg.h"
#include "asterfort/vdxsig.h"
#include "asterfort/postcoq3d.h"
!
    character(len=*) :: optioz, nomtz
!
    character(len=16) :: option, nomte
!     ----------------------------------------------------------------
!     CALCUL DES OPTIONS DES ELEMENTS DE COQUE 3D
!                EPSI_ELGA
!                SIEF_ELGA
!                DEGE_ELGA
!                DEGE_ELNO
!

!-----------------------------------------------------------------------
    integer :: jcou
    integer :: nbcou
! DEB
!
    option=optioz
    nomte=nomtz
!
    call jevech('PNBSP_I', 'L', jcou)
    nbcou=zi(jcou)
    call postcoq3d(option, nomte, nbcou)
!
end subroutine
