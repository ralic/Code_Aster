subroutine trabck(cmess, iexit)
! ----------------------------------------------------------------------
! IMPRIME LA REMONTEE DES APPELS
!
! IN  CMESS  : MESSAGE D'INFORMATION
! IN  IEXIT  : CONDITION DE SORTIE DE LA ROUTINE DE TRACEBACK
!              AVEC LE COMPILO INTEL <0 ON REDONNE LA MAIN
! ----------------------------------------------------------------------
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
! aslint: disable=W1304
#include "asterf.h"
#if _USE_INTEL_IFORT && HAVE_TRACEBACKQQ == 1
    use ifcore
#endif
    implicit none
    character(len=*) :: cmess
    integer(kind=4) :: iexit
#if HAVE_BACKTRACE != 1
    integer :: dummy
#endif
!   Dummy argument si HAVE_TRACEBACKQQ n'est pas defini
#if _USE_INTEL_IFORT && HAVE_TRACEBACKQQ == 1
    call TRACEBACKQQ(string=cmess, USER_EXIT_CODE=iexit)
#elif HAVE_BACKTRACE == 1
    call backtrace()
#else
!   ON NE PEUT PAS APPELER UTMESS (RECURSIVITE)
    write(6,*) 'Traceback is not provided by the compiler'
    dummy = len(cmess) + iexit
#endif
!
end subroutine
