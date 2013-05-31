subroutine fclose(unit)
! aslint: disable=
    implicit   none
    include 'asterfort/u2mesk.h'
    integer :: unit
! ======================================================================
! COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
!     FERMETURE DE L'UNITE LOGIQUE fort.UNIT
!     UTILE POUR APPEL DEPUIS PYTHON UNE FOIS LES BASES JEVEUX FERMEES
!     CAR ULOPEN N EST ALORS PLUS UTILISABLE (CONFER E_JDC.py)
!
! IN  : UNIT   : NUMERO D'UNITE LOGIQUE
!     ------------------------------------------------------------------
    integer :: ierr
    character(len=4) :: k4b
!     ------------------------------------------------------------------
!
    close (unit=unit, iostat=ierr)
    if (ierr .gt. 0) then
        write(k4b,'(I3)') unit
        call u2mesk('F', 'UTILITAI_77', 1, k4b)
    endif
!
end subroutine
