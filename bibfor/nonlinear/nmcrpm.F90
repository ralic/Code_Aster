subroutine nmcrpm(list, nbinst, dtmin)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit     none
    real(kind=8) :: list(*), dtmin
    integer :: nbinst
!
! ----------------------------------------------------------------------
!
! ROUTINE *_NON_LINE (UTILITAIRE)
!
! CALCUL DU DELTA TEMPS MINIMUM
!
! ----------------------------------------------------------------------
!
!
! IN  LIST   : LISTE DES INSTANTS
! IN  NBINST : NOMBRE D'INSTANTS DANS LA LISTE
! OUT DTMIN  : INCREMENT DE TEMPS MINIMUM DANS LA LISTE
!
! ----------------------------------------------------------------------
!
    integer :: i
    real(kind=8) :: deltat
!
! ----------------------------------------------------------------------
!
!
!
! --- INITIALISATIONS
!
    dtmin = 0.d0
    if (nbinst .eq. 1) goto 99
!
! --- DELTA MINIMUM
!
    dtmin = list(2)-list(1)
    do 10 i = 2, nbinst-1
        deltat = list(i+1-1) - list(i-1)
        dtmin = min(deltat,dtmin )
10  end do
99  continue
!
end subroutine
