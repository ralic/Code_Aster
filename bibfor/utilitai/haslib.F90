subroutine haslib(libraz, iret)
! person_in_charge: mathieu.courtois at edf.fr
!
! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
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
    include 'asterfort/assert.h'
    character(len=*) :: libraz
    integer :: iret
!--------------------------------------------------------------
! BUT : CONNAITRE LA DISPONIBILITE D'UNE LIBRAIRIE EXTERNE
!
! IN : LIBRAI :
!     /'MUMPS'  : POUR DEMANDER LA FACTORISATION DU PRECONDITIONNEUR
!     /'PETSC'  : POUR DEMANDER LA RESOLUTION ITERATIVE
!     /'HDF5'   : PRESENCE DE LIBHDF5
!     /'MED'    : PRESENCE DE MED
!
! OUT : IRET    : CODE_RETOUR :
!            1  : INSTALLE
!            0  : PAS INSTALLE
!---------------------------------------------------------------
    character(len=5) :: librai
!---------------------------------------------------------------
    librai = libraz
    iret=0
    if (librai .eq. 'MUMPS') then
#ifdef _HAVE_MUMPS
        iret=1
#endif
    else if (librai.eq.'PETSC') then
#ifdef _HAVE_PETSC
        iret=1
#endif
    else if (librai.eq.'HDF5') then
#ifndef _DISABLE_HDF5
        iret=1
#endif
    else if (librai.eq.'MED') then
#ifndef _DISABLE_MED
        iret=1
#endif
    else
        call assert(.false.)
    endif
end subroutine
