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
    include 'asterfort/teattr.h'
    character(len=*) :: typel, noattr, vattr
!---------------------------------------------------------------------
! BUT : TESTER SI L'ATTRIBUT NOATTR EXISTE ET A LA VALEUR VATTR
!---------------------------------------------------------------------
!     ARGUMENTS:
!     ----------
!     IN TYPEL  (K16) : NOM DU TYPE_ELEMENT A INTERROGER
!                        (OU ' ' SI L'ON EST "SOUS" LA ROUTINE TE0000)
!     IN NOATTR (K16) : NOM DE L'ATTRIBUT
!     IN VATTR  (K16) : VALEUR DE L'ATTRIBUT
!    OUT LTEATT (L)   : .TRUE. : L'ATTRIBUT EXISTE POUR LE TYPE_ELEMENT
!                                ET SA VALEUR VAUT VATTR
!                       .FALSE. : SINON
!-----------------------------------------------------------------------
!  CETTE ROUTINE EST ACCESSIBLE PARTOUT DANS LE CODE. SI ELLE EST
!  APPELEE EN DEHORS DE TE0000 (OU AVEC TYPEL != ' '), ELLE NECESSITE
!  DES APPELS JEVEUX, ELLE DEVIENT DONC UN PEU COUTEUSE.
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
