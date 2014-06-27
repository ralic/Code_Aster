subroutine pjspco(moa1, moa2, corres, base, noca, &
                  method, isole )
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
!
! --------------------------------------------------------------------------------------------------
!
!   Commande :  PROJ_CHAMP /  METHOD = SOUS_POINT_MATER | SOUS_POINT_RIGI
!
!       Calculer la structure de donnee corresp_2_mailla
!
! --------------------------------------------------------------------------------------------------
!
    implicit none
#include "asterf_types.h"
!
    aster_logical :: isole
    character(len=1) :: base
    character(len=8) :: moa1, moa2, noca, masp
    character(len=16) :: corres
    character(len=19) :: method
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/pjefco.h"
#include "asterfort/pjmasp.h"
#include "asterfort/pjrisp.h"
#include "asterfort/utmess.h"
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    ASSERT(base.eq.'V')
!
!   Création du maillage "sous-point" (masp) et remplissage du .PJEF_SP dans la sd corres
!   qui est un tableau référencant, pour chaque noeud du maillage :
!       les numéros de maille des points de gauss
!       les numéros de maille des sous-points
!   auxquels il correspond dans moa2
!
! --------------------------------------------------------------------------------------------------
!
    if ( .not.isole .and. (method.eq.'SOUS_POINT_RIGI') ) then
        call utmess('F', 'CALCULEL5_28')
    endif
    masp='&&PJSPCO'
    if ( method.eq.'SOUS_POINT_MATER' ) then
        call pjmasp(moa2, masp, corres, noca)
    else if ( method.eq.'SOUS_POINT_RIGI' ) then
        call pjrisp(moa2, masp, corres, noca)
    else
        ASSERT( .false. )
    endif
!
!   Appel à la routine "usuelle" pjefco
    call pjefco(moa1, masp, corres, 'V')
    call jedema()
end subroutine
