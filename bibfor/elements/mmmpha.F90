subroutine mmmpha(loptf, lcont, ladhe, ndexfr, lpenac,&
                  lpenaf, phasep)
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
    include 'asterfort/assert.h'
    logical :: lpenaf, lpenac
    logical :: loptf, lcont, ladhe
    integer :: ndexfr
    character(len=9) :: phasep
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - CALCUL)
!
! PREPARATION DES CALCULS - PHASE DE CALCUL
!
! ----------------------------------------------------------------------
!
!
! IN  LOPTF  : .TRUE. SI OPTION DE FROTTEMENT
! IN  LCONT  : .TRUE. SI CONTACT (SU=1)
! IN  LADHE  : .TRUE. SI ADHERENCE
! IN  LPENAC : .TRUE. SI CONTACT PENALISE
! IN  LPENAF : .TRUE. SI FROTTEMENT PENALISE
! IN  NDEXFR : ENTIER CODE POUR EXCLUSION DIRECTION DE FROTTEMENT
! OUT PHASEP : 'SANS' - PAS DE CONTACT
!              'CONT' - CONTACT
!              'ADHE' - CONTACT ADHERENT
!              'GLIS' - CONTACT GLISSANT
!              'SANS_PENA' - PENALISATION - PAS DE CONTACT
!              'CONT_PENA' - PENALISATION - CONTACT
!              'ADHE_PENA' - PENALISATION - CONTACT ADHERENT
!              'GLIS_PENA' - PENALISATION - CONTACT GLISSANT
!
! ----------------------------------------------------------------------
!
    character(len=4) :: phase
!
! ----------------------------------------------------------------------
!
    phase = ' '
    phasep = ' '
!
! --- PHASE PRINCIPALE
!
    if (loptf) then
        if (lcont) then
            if (ladhe) then
                phase = 'ADHE'
            else
                phase = 'GLIS'
            endif
        else
            phase = 'SANS'
        endif
    else
        ndexfr = 0
        if (lcont) then
            phase = 'CONT'
        else
            phase = 'SANS'
        endif
    endif
!
! --- PRISE EN COMPTE DE LA PENALISATION
!
    if (phase .eq. 'SANS') then
        if (lpenac .or. lpenaf) then
            phasep = phase(1:4)//'_PENA'
        else
            phasep = phase(1:4)
        endif
    else if (phase.eq.'CONT') then
        if (lpenac) then
            phasep = phase(1:4)//'_PENA'
        else
            phasep = phase(1:4)
        endif
    else if ((phase.eq.'ADHE').or.(phase.eq.'GLIS')) then
        if (lpenaf) then
            phasep = phase(1:4)//'_PENA'
        else
            phasep = phase(1:4)
        endif
    else
        call assert(.false.)
    endif
!
end subroutine
