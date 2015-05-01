subroutine te0416(option, nomte)
    implicit none
#include "jeveux.h"
#include "asterfort/cosiro.h"
#include "asterfort/forngr.h"
#include "asterfort/fornpd.h"
#include "asterfort/jevech.h"
#include "asterfort/tecach.h"
#include "asterfort/utmess.h"
    character(len=16) :: option, nomte
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
!     CALCUL DES OPTIONS DES ELEMENTS DE COQUE : COQUE_3D
!     ----------------------------------------------------------------
!
!
    integer :: ibid, iret, icompo
!
!
! DEB
!
    if (option .eq. 'FORC_NODA') then
!        -- PASSAGE DES CONTRAINTES DANS LE REPERE INTRINSEQUE :
        call cosiro(nomte, 'PCONTMR', 'L', 'UI', 'G',&
                    ibid, 'S')
    endif
!
!
    call tecach('ONN', 'PCOMPOR', 'L', iret, iad=icompo)
    if (icompo .eq. 0) then
        call fornpd(option, nomte)
        goto 9999
    else
    
        call jevech('PCOMPOR', 'L', icompo)
        
        if (zk16 ( icompo + 2 ) .eq. 'GROT_GDEP') then
        
!           DEFORMATION DE GREEN

            call forngr(option, nomte)
!
            goto 9999
!
        else if (zk16(icompo + 2 ) (1:5) .eq. 'PETIT') then

            call fornpd(option, nomte)
        else

!----------- AUTRES MESURES DE DEFORMATIONS
!
            call utmess('F', 'ELEMENTS3_93', sk=zk16(icompo+2))
!
        endif
    endif
!
!
9999  continue
!
!
!
end subroutine
