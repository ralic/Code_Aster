function nopara(opt, te, statut, ipar)

use calcul_module, only : ca_iaopmo_, ca_iaopno_, ca_iaoptt_, ca_ilopmo_, ca_ilopno_, ca_lgco_

implicit none

! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.

! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.

! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr

#include "jeveux.h"
#include "asterfort/assert.h"

    integer :: opt, te, ipar
    character(len=3) :: statut
    character(len=8) :: nopara
!-----------------------------------------------------------------------
!     entrees:
!        opt    : option
!        te     : type_element
!        statut : in/out
!        ipar   : numero du champ parametre dans l'option
!
!     sorties:
!        nopara : nom du parametre dans l'option
!-----------------------------------------------------------------------
    integer :: nbin, optmod, optnom
    integer ::   jj
!-----------------------------------------------------------------------

!     jj = ioptte(opt,te)
    jj = zi(ca_iaoptt_-1+ (te-1)*ca_lgco_+opt)
    optmod = ca_iaopmo_ + zi(ca_ilopmo_-1+jj) - 1
    optnom = ca_iaopno_ + zi(ca_ilopno_-1+jj) - 1
    if (statut .eq. 'IN ') then
        nopara = zk8(optnom-1+ipar)
    else
        ASSERT(statut.eq.'OUT')
        nbin = zi(optmod-1+2)
        nopara = zk8(optnom-1+nbin+ipar)
    endif
end function
