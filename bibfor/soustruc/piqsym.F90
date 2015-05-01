subroutine piqsym(x, y, quar1, quar2, quar3,&
                  quar4)
    implicit none
#include "asterf_types.h"
    real(kind=8) :: x, y
    aster_logical :: quar1, quar2, quar3, quar4
!-----------------------------------------------------------------------
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
!     OPERATEUR: "DEFI_GROUP" , MOTCLE FACTEUR "EQUE_PIQUA"
!     AUTEUR Y. WADIER
!
!     EFFECTUE UNE SYMETRIE SUR LES COORDONNEES
!
!-----------------------------------------------------------------------
!
    if (quar1) then
        x = x
        y = y
    endif
!
    if (quar2) then
        x = -x
        y = y
    endif
!
    if (quar3) then
        x = -x
        y = -y
    endif
!
    if (quar4) then
        x = x
        y = -y
    endif
!
end subroutine
