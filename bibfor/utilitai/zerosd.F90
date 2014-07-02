function zerosd(typesd, sd)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/utmess.h"
#include "asterfort/zerobj.h"
    aster_logical :: zerosd
    character(len=*) :: sd, typesd
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
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
! ----------------------------------------------------------------------
!  BUT : DETERMINER SI UNE SD EST NULLE (OU PAS)
!  IN   TYPESD : TYPE DE  SD
!   LISTE DES POSSIBLES: 'RESUELEM', 'CARTE', 'CHAM_NO', 'CHAM_ELEM'
!       SD     : NOM DE LA SD
!
!     RESULTAT:
!       ZEROSD : .TRUE.    SI LES VALEURS DE LA SD SONT TOUTES NULLES
!                .FALSE.   SINON
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
    character(len=19) :: k19
    character(len=16) :: typ2sd
!
! -DEB------------------------------------------------------------------
!
    typ2sd=typesd
!
!
!
    if (typ2sd .eq. 'RESUELEM') then
!     --------------------------------
        k19=sd
        zerosd=zerobj(k19//'.RESL')
!
!
    else if (typ2sd.eq.'CHAM_NO') then
!     --------------------------------
        k19=sd
        zerosd=zerobj(k19//'.VALE')
!
!
    else if (typ2sd.eq.'CARTE') then
!     --------------------------------
        k19=sd
        zerosd=zerobj(k19//'.VALE')
!
!
    else if (typ2sd.eq.'CHAM_ELEM') then
!     --------------------------------
        k19=sd
        zerosd=zerobj(k19//'.CELV')
!
    else
        call utmess('F', 'UTILITAI_47', sk=typ2sd)
    endif
!
end function
