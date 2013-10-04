subroutine nmrvai(sdstat, questz, phase, vali)
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
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=*) :: questz
    character(len=24) :: sdstat
    character(len=1) :: phase
    integer :: vali
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! MESURE DE STATISTIQUES - NOMBRE D'OCCURRENCES
!
! ----------------------------------------------------------------------
!
!
! IN  SDSTAT : SD STATISTIQUES
! IN  QUESTI : QUESTION DE STATISTIQUE
! IN  PHASE  : PHASE
!               'E' ECRITURE
!               'N' LECTURE SUR L'ITERATION DE NEWTON COURANTE
!               'P' LECTURE SUR LE PAS COURANT
!               'T' LECTURE SUR TOUT LE TRANSITOIRE
! I/O VALI   : VALEUR ENTIERE
!
!
!
!
    character(len=24) :: stvip, stvit, stvin
    integer :: jstvip, jstvit, jstvin
    character(len=24) :: questi
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    questi = questz
    if (phase .eq. 'E') then
        if (vali .le. 0) vali = 0
    else
        vali = 0
    endif
!
! --- ACCES SDSTAT
!
    stvip = sdstat(1:19)//'.VLIP'
    stvit = sdstat(1:19)//'.VLIT'
    stvin = sdstat(1:19)//'.VLIN'
    call jeveuo(stvip, 'E', jstvip)
    call jeveuo(stvit, 'E', jstvit)
    call jeveuo(stvin, 'E', jstvin)
!
    if (phase .eq. 'E') then
        if (questi .eq. 'PAS') then
            zi(jstvit-1+1) = zi(jstvit-1+1) + vali
            zi(jstvip-1+1) = zi(jstvip-1+1) + vali
            zi(jstvin-1+1) = zi(jstvin-1+1) + vali
        else if (questi.eq.'ITE') then
            zi(jstvit-1+2) = zi(jstvit-1+2) + vali
            zi(jstvip-1+2) = zi(jstvip-1+2) + vali
            zi(jstvin-1+2) = zi(jstvin-1+2) + vali
        else if (questi.eq.'INTEGRATION') then
            zi(jstvit-1+3) = zi(jstvit-1+3) + vali
            zi(jstvip-1+3) = zi(jstvip-1+3) + vali
            zi(jstvin-1+3) = zi(jstvin-1+3) + vali
        else if (questi.eq.'FACTOR') then
            zi(jstvit-1+4) = zi(jstvit-1+4) + vali
            zi(jstvip-1+4) = zi(jstvip-1+4) + vali
            zi(jstvin-1+4) = zi(jstvin-1+4) + vali
        else if (questi.eq.'SOLVE') then
            zi(jstvit-1+5) = zi(jstvit-1+5) + vali
            zi(jstvip-1+5) = zi(jstvip-1+5) + vali
            zi(jstvin-1+5) = zi(jstvin-1+5) + vali
        else if (questi.eq.'CONT_GEOM') then
            zi(jstvit-1+6) = zi(jstvit-1+6) + vali
            zi(jstvip-1+6) = zi(jstvip-1+6) + vali
            zi(jstvin-1+6) = zi(jstvin-1+6) + vali
        else if (questi.eq.'CTCD_ALGO_ITER') then
            zi(jstvit-1+7) = zi(jstvit-1+7) + vali
            zi(jstvip-1+7) = zi(jstvip-1+7) + vali
            zi(jstvin-1+7) = zi(jstvin-1+7) + vali
        else if (questi.eq.'CTCC_PREP') then
            zi(jstvit-1+8) = zi(jstvit-1+8) + vali
            zi(jstvip-1+8) = zi(jstvip-1+8) + vali
            zi(jstvin-1+8) = zi(jstvin-1+8) + vali
        else if (questi.eq.'CTCC_MATR') then
            zi(jstvit-1+9) = zi(jstvit-1+9) + vali
            zi(jstvip-1+9) = zi(jstvip-1+9) + vali
            zi(jstvin-1+9) = zi(jstvin-1+9) + vali
        else if (questi.eq.'CTCC_VECT') then
            zi(jstvit-1+10) = zi(jstvit-1+10) + vali
            zi(jstvip-1+10) = zi(jstvip-1+10) + vali
            zi(jstvin-1+10) = zi(jstvin-1+10) + vali
        else if (questi.eq.'CTCC_CONT') then
            zi(jstvit-1+11) = zi(jstvit-1+11) + vali
            zi(jstvip-1+11) = zi(jstvip-1+11) + vali
            zi(jstvin-1+11) = zi(jstvin-1+11) + vali
        else if (questi.eq.'CTCC_FROT') then
            zi(jstvit-1+12) = zi(jstvit-1+12) + vali
            zi(jstvip-1+12) = zi(jstvip-1+12) + vali
            zi(jstvin-1+12) = zi(jstvin-1+12) + vali
        else if (questi.eq.'CONT_NBLIAC') then
            zi(jstvit-1+13) = vali
            zi(jstvip-1+13) = vali
            zi(jstvin-1+13) = vali
        else if (questi.eq.'CONT_NBLIAF') then
            zi(jstvit-1+14) = vali
            zi(jstvip-1+14) = vali
            zi(jstvin-1+14) = vali
        else if (questi.eq.'RECH_LINE_ITER') then
            zi(jstvit-1+15) = zi(jstvit-1+15) + vali
            zi(jstvip-1+15) = zi(jstvip-1+15) + vali
            zi(jstvin-1+15) = zi(jstvin-1+15) + vali
        else if (questi.eq.'CTCC_CYCL_1') then
            zi(jstvit-1+17) = zi(jstvit-1+17) + vali
            zi(jstvip-1+17) = zi(jstvip-1+17) + vali
            zi(jstvin-1+17)  =zi(jstvin-1+17) + vali
        else if (questi.eq.'CTCC_CYCL_2') then
            zi(jstvit-1+18) = zi(jstvit-1+18) + vali
            zi(jstvip-1+18) = zi(jstvip-1+18) + vali
            zi(jstvin-1+18) = zi(jstvin-1+18) + vali
        else if (questi.eq.'CTCC_CYCL_3') then
            zi(jstvit-1+19) = zi(jstvit-1+19) + vali
            zi(jstvip-1+19) = zi(jstvip-1+19) + vali
            zi(jstvin-1+19) = zi(jstvin-1+19) + vali
        else if (questi.eq.'CTCC_CYCL_4') then
            zi(jstvit-1+20) = zi(jstvit-1+20) + vali
            zi(jstvip-1+20) = zi(jstvip-1+20) + vali
            zi(jstvin-1+20) = zi(jstvin-1+20) + vali
        else
            ASSERT(.false.)
        endif
!
        elseif ((phase.eq.'T').or. (phase.eq.'P').or. (phase.eq.'N'))&
    then
!
        if (questi .eq. 'PAS') then
            if (phase .eq. 'T') vali = zi(jstvit-1+1)
            if (phase .eq. 'P') vali = zi(jstvip-1+1)
            if (phase .eq. 'N') vali = zi(jstvin-1+1)
!
        else if (questi.eq.'ITE') then
            if (phase .eq. 'T') vali = zi(jstvit-1+2)
            if (phase .eq. 'P') vali = zi(jstvip-1+2)
            if (phase .eq. 'N') vali = zi(jstvin-1+2)
!
        else if (questi.eq.'INTEGRATION') then
            if (phase .eq. 'T') vali = zi(jstvit-1+3)
            if (phase .eq. 'P') vali = zi(jstvip-1+3)
            if (phase .eq. 'N') vali = zi(jstvin-1+3)
!
        else if (questi.eq.'FACTOR') then
            if (phase .eq. 'T') vali = zi(jstvit-1+4)
            if (phase .eq. 'P') vali = zi(jstvip-1+4)
            if (phase .eq. 'N') vali = zi(jstvin-1+4)
!
        else if (questi.eq.'SOLVE') then
            if (phase .eq. 'T') vali = zi(jstvit-1+5)
            if (phase .eq. 'P') vali = zi(jstvip-1+5)
            if (phase .eq. 'N') vali = zi(jstvin-1+5)
!
        else if (questi.eq.'CONT_GEOM') then
            if (phase .eq. 'T') vali = zi(jstvit-1+6)
            if (phase .eq. 'P') vali = zi(jstvip-1+6)
            if (phase .eq. 'N') vali = zi(jstvin-1+6)
!
        else if (questi.eq.'CTCD_ALGO_ITER') then
            if (phase .eq. 'T') vali = zi(jstvit-1+7)
            if (phase .eq. 'P') vali = zi(jstvip-1+7)
            if (phase .eq. 'N') vali = zi(jstvin-1+7)
!
        else if (questi.eq.'CTCC_PREP') then
            if (phase .eq. 'T') vali = zi(jstvit-1+8)
            if (phase .eq. 'P') vali = zi(jstvip-1+8)
            if (phase .eq. 'N') vali = zi(jstvin-1+8)
!
        else if (questi.eq.'CTCC_MATR') then
            if (phase .eq. 'T') vali = zi(jstvit-1+9)
            if (phase .eq. 'P') vali = zi(jstvip-1+9)
            if (phase .eq. 'N') vali = zi(jstvin-1+9)
!
        else if (questi.eq.'CTCC_VECT') then
            if (phase .eq. 'T') vali = zi(jstvit-1+10)
            if (phase .eq. 'P') vali = zi(jstvip-1+10)
            if (phase .eq. 'N') vali = zi(jstvin-1+10)
!
        else if (questi.eq.'CTCC_CONT') then
            if (phase .eq. 'T') vali = zi(jstvit-1+11)
            if (phase .eq. 'P') vali = zi(jstvip-1+11)
            if (phase .eq. 'N') vali = zi(jstvin-1+11)
!
        else if (questi.eq.'CTCC_FROT') then
            if (phase .eq. 'T') vali = zi(jstvit-1+12)
            if (phase .eq. 'P') vali = zi(jstvip-1+12)
            if (phase .eq. 'N') vali = zi(jstvin-1+12)
!
        else if (questi.eq.'CONT_NBLIAC') then
            if (phase .eq. 'T') vali = zi(jstvit-1+13)
            if (phase .eq. 'P') vali = zi(jstvip-1+13)
            if (phase .eq. 'N') vali = zi(jstvin-1+13)
!
        else if (questi.eq.'CONT_NBLIAF') then
            if (phase .eq. 'T') vali = zi(jstvit-1+14)
            if (phase .eq. 'P') vali = zi(jstvip-1+14)
            if (phase .eq. 'N') vali = zi(jstvin-1+14)
!
        else if (questi.eq.'RECH_LINE_ITER') then
            if (phase .eq. 'T') vali = zi(jstvit-1+15)
            if (phase .eq. 'P') vali = zi(jstvip-1+15)
            if (phase .eq. 'N') vali = zi(jstvin-1+15)
!
!
        else if (questi.eq.'CTCC_CYCL_1') then
            if (phase .eq. 'T') vali = zi(jstvit-1+17)
            if (phase .eq. 'P') vali = zi(jstvip-1+17)
            if (phase .eq. 'N') vali = zi(jstvin-1+17)
!
        else if (questi.eq.'CTCC_CYCL_2') then
            if (phase .eq. 'T') vali = zi(jstvit-1+18)
            if (phase .eq. 'P') vali = zi(jstvip-1+18)
            if (phase .eq. 'N') vali = zi(jstvin-1+18)
!
        else if (questi.eq.'CTCC_CYCL_3') then
            if (phase .eq. 'T') vali = zi(jstvit-1+19)
            if (phase .eq. 'P') vali = zi(jstvip-1+19)
            if (phase .eq. 'N') vali = zi(jstvin-1+19)
!
        else if (questi.eq.'CTCC_CYCL_4') then
            if (phase .eq. 'T') vali = zi(jstvit-1+20)
            if (phase .eq. 'P') vali = zi(jstvip-1+20)
            if (phase .eq. 'N') vali = zi(jstvin-1+20)
!
        else
            ASSERT(.false.)
        endif
!
    else
        ASSERT(.false.)
!
    endif
!
    call jedema()
end subroutine
