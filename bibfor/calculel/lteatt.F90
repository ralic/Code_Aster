function lteatt(noattr, vattr, typel)
    implicit none
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
#include "asterf_types.h"
#include "asterfort/teattr.h"
    aster_logical :: lteatt
    character(len=*), intent(in) :: noattr
    character(len=*), intent(in) :: vattr
    character(len=*), intent(in), optional :: typel
!---------------------------------------------------------------------
! but : Tester si l'attribut noattr existe et a la valeur vattr
!---------------------------------------------------------------------
!     arguments:
!     ----------
!    (o) in  noattr (k16) : nom de l'attribut
!    (o) in  vattr  (k16) : valeur de l'attribut
!    (f) in  typel  (k16) : Nom du type_element a interroger.
!                           Cet argument est inutile si la question concerne le
!                           type_element "courant".
!    (o) out lteatt (l)   : .true. : l'attribut existe pour le type_element
!                                    et sa valeur vaut vattr
!                           .false. : sinon
!
!-----------------------------------------------------------------------
!  Cette routine est utilisable partout dans le code.
!  Si elle est appelee en dehors de te0000 il faut fournir typel.
!  Sinon, typel est inutile.
!-----------------------------------------------------------------------
!  VARIABLES LOCALES :
    character(len=16) :: vattr2
    integer :: iret
!
!----------------------------------------------------------------------
    if (present(typel)) then
        call teattr('C', noattr, vattr2, iret, typel=typel)
    else
        call teattr('C', noattr, vattr2, iret)
    endif
    if ((iret.eq.0) .and. (vattr.eq.vattr2)) then
        lteatt=.true.
    else
        lteatt=.false.
    endif
end function
