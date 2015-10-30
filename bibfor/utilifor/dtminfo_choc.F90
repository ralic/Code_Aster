subroutine dtminfo_choc(nlcase, nbnoli)
    implicit none
!
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
! person_in_charge: hassan.berro at edf.fr
!
! dtminfo_choc : Print out information about the current choc state
!
#include "jeveux.h"
#include "asterfort/utmess.h"
#include "asterfort/codent.h"
!
!   -0.1- Input/output arguments
    integer          , intent(in)   :: nlcase
    integer          , intent(in)   :: nbnoli
!
!   -0.2- Local variables
    integer                      :: input, ind, base, digit, decal
    character(len=(13+4*nbnoli)) :: line, chaine

!
    do ind = 1, 13+4*nbnoli
        line(ind:ind) = '-'
    end do

    chaine = ' '
    do ind = 1, nbnoli
        decal = (ind-1)*4
        call codent(ind, 'D', chaine(decal+1:decal+2))
        chaine(decal+3:decal+4) = ' |'
    end do
    call utmess('I', 'DYNAMIQUE_91', nk=2, valk=[line, chaine])

    input = nlcase
    base = 2
    ind  = 1
    chaine = ' '
    chaine(1:1) = '|'
    do ind = 1, nbnoli
        decal = 1+(ind-1)*4
        digit = input - int(input/base)*base
        input = (input - digit)/base
        if (digit.eq.1) chaine(decal+1:decal+2) = ' x'
        chaine(decal+3:decal+4) = ' |'
    end do
    call utmess('I', 'DYNAMIQUE_92', nk=2, valk=[chaine, line])

end subroutine
