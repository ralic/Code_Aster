subroutine decod1(rec, irec, ifield, valatt, trouve)
    implicit  none
#include "asterfort/lxliis.h"
#include "asterfort/trfmot.h"
    character(len=*) :: rec(20)
    integer :: irec, ifield, valatt
    logical :: trouve
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!----------------------------------------------------------------------
!    VERIFICATION : L'ENTETE DU DATASET LU EST-IL CELUI RECHERCHE ?
!                   COMPARAISON ENTETE / SD FORMAT_IDEAS
!
! IN  : REC    : K80  : TABLEAU DE CARACTERES CONTENANT L'ENTETE DU
!                       DATASET
! IN  : IREC   : I    : NUMERO DE L'ENREGISTREMENT A TRAITER
! IN  : IFIELD : I    : NUMERO DU CHAMP A TRAITER
! IN  : VALATT : I    : VALEUR ATTENDUE
! OUT : TROUVE : L    : .TRUE.  ON A TROUVE LA VALEUR ATTENDUE
!                       .FALSE. ON N A PAS TROUVE LA VALEUR ATTENDUE
!
!----------------------------------------------------------------------
!
    integer :: ilu, ier
    character(len=80) :: field
!
!- RECHERCHE DU CHAMP A TRAITER
!
    call trfmot(rec(irec), field, ifield)
!
!- DECODAGE D'UN ENTIER
!
    call lxliis(field, ilu, ier)
!
!- LA VALEUR TROUVEE N'EST PAS UN ENTIER
!
    if (ier .eq. 1) trouve = .false.
!
!- LA VALEUR TROUVEE (ENTIERE) EST-ELLE CELLE ATTENDUE?
!
    if (ilu .eq. valatt) then
        trouve = .true.
    else
        trouve = .false.
    endif
!
end subroutine
