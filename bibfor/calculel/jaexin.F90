subroutine jaexin(nomlu, iret)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
    character(len=*) :: nomlu
    integer :: iret
! ----------------------------------------------------------------------
! BUT : TESTE L'EXISTENCE REELLE D'UN OBJET JEVEUX
!
! IN  NOMLU  : NOM DE L'OBJET JEVEUX (EVENTUELLEMENT JEXNUM(NOMCO,IOBJ))
! OUT IRET   : =0 L'OBJET N'EXISTE PAS
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
    character(len=32) :: noml32
    integer :: iexi, iadm, iadd
! DEB ------------------------------------------------------------------
    noml32 = nomlu
!
    call jeexin(noml32, iexi)
    if (iexi .eq. 0) goto 9998
!
    call jelira(noml32, 'IADD', iadd)
    call jelira(noml32, 'IADM', iadm)
    if (iadm .eq. 0 .and. iadd .eq. 0) goto 9998
!
    iret=1
    goto 9999
!
9998  continue
    iret=0
    goto 9999
!
9999  continue
end subroutine
