subroutine dtmcase_coder(input_, koutput)
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
! dtmcase_coder : Code, inside a string, a given non-linearity case given 
!                 as an integer. 63 ASCII characters are used for the encoding.
!                 Case-0  is '0'
!                 Case-63 is '.'
!
#include "jeveux.h"
!
!   -0.1- Input/output arguments
    integer          , intent(in)   :: input_
    character(len=*) , intent(out)  :: koutput
!
!   -0.2- Local variables
    integer          :: input, length, base, ind, digit
    character(len=1) :: charac(0:62)

    data        charac / '0','1','2','3','4','5','6','7','8','9',&
                         'A','B','C','D','E','F','G','H','I','J',&
                         'K','L','M','N','O','P','Q','R','S','T',&
                         'U','V','W','X','Y','Z','a','b','c','d',&
                         'e','f','g','h','i','j','k','l','m','n',&
                         'o','p','q','r','s','t','u','v','w','x',&
                         'y','z','.'/
    koutput = ' '
!
    input = input_
    if (input.lt.0) input = -input
    length = len(koutput)
!
    base = 63
    ind  = length
    do while (ind.gt.0)
        digit            = input - int(input/base)*base
        input            = (input - digit)/base
        koutput(ind:ind) = charac(digit)
        ind              = ind - 1
    end do

end subroutine
