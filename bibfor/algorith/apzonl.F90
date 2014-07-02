subroutine apzonl(sdappa, izone, questz, vall)
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/apzoni.h"
#include "asterfort/assert.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
    character(len=19) :: sdappa
    integer :: izone
    aster_logical :: vall
    character(len=*) :: questz
!
! ----------------------------------------------------------------------
!
! ROUTINE APPARIEMENT (UTILITAIRE)
!
! INFO. DE TYPE LOGICAL SUR LA ZONE COURANTE
!
! ----------------------------------------------------------------------
!
!
! IN  SDAPPA : NOM DE LA SD APPARIEMENT
! IN  IZONE  : NUMERO DE LA ZONE
! IN  QUESTI : QUESTION
!               DIRE_APPA_FIXE DIRECTION FIXE D'APPARIEMENT ?
!               APPA_MAIT_ESCL APPARIEMENT MAITRE-ESCLAVE ?
!               CALC_NORM_ESCL CALCUL DE LA NORMALE SUR NOEUD ESCLAVE ?
!               CALC_NORM_MAIT CALCUL DE LA NORMALE SUR NOEUD MAITRE ?
! OUT VALL   : REPONSE A LA QUESTION
!
!
!
!
    integer :: ifm, niv
    character(len=24) :: questi
    integer :: iappa
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('APPARIEMENT', ifm, niv)
!
! --- INITIALISATIONS
!
    vall = .false.
    questi = questz
!
! --- REPONSE
!
    if (questi .eq. 'DIRE_APPA_FIXE') then
        call apzoni(sdappa, izone, 'DIRE_APPA', iappa)
        vall = iappa.eq.1
    else if (questi.eq.'APPA_MAIT_ESCL') then
        call apzoni(sdappa, izone, 'TYPE_APPA', iappa)
        vall = iappa.eq.1
    else if (questi.eq.'CALC_NORM_ESCL') then
        call apzoni(sdappa, izone, 'CALC_NORM_ESCL', iappa)
        vall = iappa.eq.1
    else if (questi.eq.'CALC_NORM_MAIT') then
        call apzoni(sdappa, izone, 'CALC_NORM_MAIT', iappa)
        vall = iappa.eq.1
    else
        ASSERT(.false.)
    endif
!
    call jedema()
!
end subroutine
