subroutine nmchoi(phase, sddyna, numins, fonact, metpre,&
                  metcor, reasma, lcamor, optrig, lcrigi,&
                  larigi, lcfint)
!
! person_in_charge: mickael.abbas at edf.fr
!
!
! COPYRIGHT (C) 1991 - 2015  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
    implicit none
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/isfonc.h"
#include "asterfort/ndynlo.h"
    character(len=16) :: metcor, metpre, optrig
    integer :: numins
    aster_logical :: reasma, lcamor, lcrigi, lcfint, larigi
    character(len=19) :: sddyna
    character(len=10) :: phase
    integer :: fonact(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (CALCUL)
!
! CALCUL DES MATR_ELEM DE RIGIDITE ET OPTION DE CALCUL POUR MERIMO
!
! ----------------------------------------------------------------------
!
!
! IN  PHASE  : PHASE DE CALCUL
!                'PREDICTION'
!                'CORRECTION'
! IN  SDDYNA : SD DYNAMIQUE
! IN  NUMINS : NUMERO D'INSTANT
! IN  FONACT : FONCTIONNALITES ACTIVEES
! IN  METCOR : TYPE DE MATRICE DE CORRECTION
! IN  METPRE : TYPE DE MATRICE DE PREDICTION
! IN  REASMA : REASSEMBLAGE MATRICE GLOBALE
! IN  LCAMOR : .TRUE. SI MATR_ELEM D'AMORTISSEMENT A CALCULER
! OUT OPTRIG : OPTION DE CALCUL DE MERIMO
! OUT LCRIGI : .TRUE. SI MATR_ELEM DE RIGIDITE A CALCULER
! OUT LARIGI : .TRUE. SI MATR_ELEM DE RIGIDITE A ASSEMBLER
! OUT LCFINT : .TRUE. SI VECT_ELEM DES FORCES INTERNES A CALCULER
!
! ----------------------------------------------------------------------
!
    aster_logical :: limpex, lshima, lprem
!
! ----------------------------------------------------------------------
!
!
!
! --- INITIALISATIONS
!
    optrig = ' '
    lcfint = .false.
    lcrigi = .false.
    larigi = .false.
!
! --- FONCTIONNALITES ACTIVEES
!
    limpex = isfonc(fonact,'IMPLEX')
    lshima = ndynlo(sddyna,'COEF_MASS_SHIFT')
!
! --- PREMIER PAS DE TEMPS ?
!
    lprem = numins.le.1
!
! --- OPTION DE CALCUL DE MERIMO
!
    if (phase .eq. 'CORRECTION') then
        if (reasma) then
            if (metcor .eq. 'TANGENTE') then
                optrig = 'FULL_MECA'
            else
                optrig = 'FULL_MECA_ELAS'
            endif
        else
            optrig = 'RAPH_MECA'
        endif
    else if (phase.eq.'PREDICTION') then
        if (metpre .eq. 'TANGENTE') then
            optrig = 'RIGI_MECA_TANG'
        else if (metpre.eq.'SECANTE') then
            optrig = 'RIGI_MECA_ELAS'
        else
            optrig = 'RIGI_MECA'
        endif
!
! --- METHODE IMPLEX
!
        if (limpex) then
            optrig = 'RIGI_MECA_IMPLEX'
        endif
    else
        ASSERT(.false.)
    endif
!
! --- MATR_ELEM DE RIGIDITE A CALCULER ?
!
    if (phase .eq. 'PREDICTION') then
        lcrigi = reasma
    endif
!
! --- SI ON DOIT RECALCULER L'AMORTISSEMENT DE RAYLEIGH
!
    if (lcamor) then
        lcrigi = .true.
    endif
!
! --- VECT_ELEM DES FORCES INTERNES A CALCULER ?
!
    if (phase .eq. 'PREDICTION') then
        if (optrig(1:9) .eq. 'FULL_MECA') then
            lcfint = .true.
        else if (optrig(1:10).eq.'RIGI_MECA ') then
            lcfint = .false.
        else if (optrig(1:10).eq.'RIGI_MECA_') then
            lcfint = .false.
        else if (optrig(1:9).eq.'RAPH_MECA') then
            lcfint = .true.
        else
            ASSERT(.false.)
        endif
    else if (phase.eq.'CORRECTION') then
        lcfint = lcrigi
    else
        ASSERT(.false.)
    endif
!
! --- DECALAGE COEF_MASS_SHIFT AU PREMIER PAS DE TEMPS -> ON A BESOIN
! --- DE LA MATRICE DE RIGIDITE
!
    if (lshima .and. lprem) then
        lcrigi = .true.
        larigi = .true.
    endif
!
! --- ASSEMBLAGE DE LA RIGIDITE ?
!
    if (reasma) then
        larigi = .true.
    endif
!
end subroutine
