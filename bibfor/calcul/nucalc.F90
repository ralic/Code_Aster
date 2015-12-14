function nucalc(opt, te, memoir)
use calcul_module, only : ca_iaopmo_, ca_iaoptt_, ca_ilopmo_, ca_lgco_
implicit none
    integer :: nucalc

! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"

    integer :: opt, te, memoir
!-----------------------------------------------------------------------
!     entrees:
!        opt    : option
!        te     : type_element
!        memoir : 1 : on met en memoire les objets catalogue necessaires
!                 0 : on ne fait pas de jeveux car les objets sont deja la
!                     (pour etre plus rapide dans calcul).
!
!     sorties:
!        nucalc : numero du calcul elementaire (te00ij)
!
!-----------------------------------------------------------------------
    integer :: optmod, jj
    integer, pointer :: nbligcol(:) => null()
!-------------------------------------------------------------------
    ASSERT(memoir.eq.0 .or. memoir.eq.1)
    if (memoir .eq. 1) then
        call jeveuo('&CATA.TE.OPTTE', 'L', ca_iaoptt_)
        call jeveuo('&CATA.TE.OPTMOD', 'L', ca_iaopmo_)
        call jeveuo(jexatr('&CATA.TE.OPTMOD', 'LONCUM'), 'L', ca_ilopmo_)
        call jeveuo('&CATA.TE.NBLIGCOL', 'L', vi=nbligcol)
        ca_lgco_ = nbligcol(1)
    endif

    jj = zi(ca_iaoptt_-1+ (te-1)*ca_lgco_+opt)
    if (jj .eq. 0) then
!        -- le type_element te n'est pas concerne par l'option opt:
        nucalc = 0
    else
        optmod = ca_iaopmo_ + zi(ca_ilopmo_-1+jj) - 1
        nucalc = zi(optmod-1+1)
    endif
end function
