function rgcmpg(icode, irgcmp)
    implicit none
    integer :: rgcmpg
#include "asterfort/exisdg.h"
    integer :: icode, irgcmp
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
!
! --- ------------------------------------------------------------------
!
!        RANG DE LA VALEUR DANS LA CARTE EN DECODANT L'ENTIER CODE
!
! --- ------------------------------------------------------------------
!
! IN
!     ICODE  : ENTIER CODE DE LA ZONE DE LA CARTE
!     IRGCMP : RANG DE LA COMPOSANTE DANS LA CARTE
!
! OUT
!     RGCMPG : RANG DE LA VALEUR DANS L'ENTIER CODE
! --- ------------------------------------------------------------------
    integer :: icmp, irgval
! --- ------------------------------------------------------------------
!
    irgval = 0
    do icmp = 1, irgcmp
        if (exisdg([icode],icmp)) irgval = irgval + 1
    end do
!     SORTIE
    rgcmpg = irgval
!
end function
