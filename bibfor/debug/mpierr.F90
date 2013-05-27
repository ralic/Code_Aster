subroutine mpierr(iermpi)
!           CONFIGURATION MANAGEMENT OF EDF VERSION
! ==================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D              WWW.CODE-ASTER.ORG
!
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR
! MODIFY IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS
! PUBLISHED BY THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE
! LICENSE, OR (AT YOUR OPTION) ANY LATER VERSION.
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL,
! BUT WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO : EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ==================================================================
! person_in_charge: thomas.desoza at edf.fr
! CORPS DU PROGRAMME
    implicit none
! DECLARATION PARAMETRES D'APPELS
    include 'asterfort/u2mesk.h'
    integer(kind=4) :: iermpi
!
#ifdef _USE_MPI
!
    include 'mpif.h'
!
! VARIABLES LOCALES
    character(len=512) :: serr
    integer(kind=4) :: lenser, iermp2
    integer :: iaux1, iaux2
!
    iaux1 = iermpi
    iaux2 = MPI_SUCCESS
    if (iaux1 .ne. iaux2) then
        call MPI_ERROR_STRING(iermpi, serr, lenser, iermp2)
        iaux1 = lenser
        call u2mesk('F', 'APPELMPI_5', 1, serr(1:iaux1))
    endif
!
#endif
!
end subroutine
