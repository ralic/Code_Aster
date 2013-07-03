subroutine tuesch(nssch)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
#include "jeveux.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
    character(len=19) :: nssch
!
!**********************************************************************
!
!  OPERATION REALISEE :
!  ------------------
!
!     DESTRUCTION DE LA SD D' UN SOUS_CHAMP_GD
!
!**********************************************************************
!
!   -------------------------
!
!
    integer :: iret
!
!====================== CORPS DE LA ROUTINE ========================
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    call jedetr(nssch//'.VALE')
    call jedetr(nssch//'.PADR')
    call jedetr(nssch//'.NOMA')
    call jedetr(nssch//'.NUGD')
    call jedetr(nssch//'.ERRE')
    call jedetr(nssch//'.PCMP')
!
    call jeexin(nssch//'.PNBN', iret)
!
    if (iret .ne. 0) then
!
        call jedetr(nssch//'.PNBN')
        call jedetr(nssch//'.PNCO')
        call jedetr(nssch//'.PNSP')
!
!
    endif
!
end subroutine
