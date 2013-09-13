subroutine op0020()
    implicit none
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     ECRITURE DES CATALOGUES D'ELEMENTS COMPILES
!     ET PRODUCTION DES IMPRESSIONS DE COMPLETUDE DES OPTIONS ET TYPELEM
!     ------------------------------------------------------------------
!
#include "asterc/getfac.h"
#include "asterfort/aidty2.h"
#include "asterfort/getvis.h"
#include "asterfort/ibcael.h"
#include "asterfort/ulexis.h"
#include "asterfort/ulopen.h"
    integer :: ouielt, impr, n1, nbocc
    character(len=24) :: fichie
!     ------------------------------------------------------------------
!
    impr = 0
    fichie = ' '
    call getvis(' ', 'UNITE', scal=impr, nbret=n1)
    if (.not. ulexis( impr )) then
        call ulopen(impr, ' ', fichie, 'NEW', 'O')
    endif
    call getfac('ELEMENT ', ouielt)
!
!     ---------- COMPILATION DES CATATLOGUES ---------------------------
    if (ouielt .ne. 0) then
        call ibcael('ECRIRE')
    endif
!
!     ---------- TRAITEMENT DE CE QUI EST RELATIF A LA COMPLETUDE ------
    call getfac('TYPE_ELEM', nbocc)
    if (nbocc .gt. 0) call aidty2(impr)
!     ------------------------------------------------------------------
!
end subroutine
