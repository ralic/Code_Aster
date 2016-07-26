function ltequa(elref, enr)
    implicit none
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "asterf_types.h"
#include "asterfort/iselli.h"
!---------------------------------------------------------------------
! but : Tester si l'enrichissement quadratique et autorisé pour la lecture/ecriture de ppmilto
!---------------------------------------------------------------------
!     arguments:
!     ----------
!    (o) in  elref (k8) : nom de l'element parent
!    (o) in  enr   (k8) : type d'enrichissement
!-----------------------------------------------------------------------
    character(len=8), intent(in) :: elref
    character(len=8), intent(in) :: enr
!-----------------------------------------------------------------------
!
    aster_logical :: ltequa
!
!----------------------------------------------------------------------
    ltequa=.false.
    if ((enr(1:2).eq.'XH' .or. enr(1:3).eq.'XHT' .or. enr(1:2).eq.'XT' .or.&
         enr(1:3).eq.'XHC'.or. enr(1:3).eq.'XTC'.or. enr(1:4).eq.'XTHC') .and.&
         .not.iselli(elref)) ltequa=.true.
end function
