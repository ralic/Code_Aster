function lteatt(typel, noattr, vattr)
    implicit none
    logical :: lteatt
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
! person_in_charge: jacques.pellet at edf.fr
#include "asterfort/teattr.h"
    character(len=*) :: typel, noattr, vattr
!---------------------------------------------------------------------
! but : Tester si l'attribut noattr existe et a la valeur vattr
!---------------------------------------------------------------------
!     arguments:
!     ----------
!     in typel  (k16) : nom du type_element a interroger
!                        (ou ' ' si l'on est "sous" la routine te0000)
!     in noattr (k16) : nom de l'attribut
!     in vattr  (k16) : valeur de l'attribut
!    out lteatt (l)   : .true. : l'attribut existe pour le type_element
!                                et sa valeur vaut vattr
!                       .false. : sinon
!-----------------------------------------------------------------------
!  cette routine est accessible partout dans le code. si elle est
!  appelee en dehors de te0000 (ou avec typel != ' '), elle necessite
!  des appels jeveux, elle devient donc un peu couteuse.
!-----------------------------------------------------------------------
!  VARIABLES LOCALES :
    character(len=16) :: vattr2
    integer :: iret
!
!----------------------------------------------------------------------
    call teattr(typel, 'C', noattr, vattr2, iret)
    if ((iret.eq.0) .and. (vattr.eq.vattr2)) then
        lteatt=.true.
    else
        lteatt=.false.
    endif
end function
