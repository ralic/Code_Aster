subroutine xmdeco(ds_contact)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterfort/copisd.h"
!
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
    type(NL_DS_Contact), intent(in) :: ds_contact
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONSTINUE - POST-TRAITEMENT)
!
! GESTION DE LA DECOUPE
!
! ----------------------------------------------------------------------
!
! In  ds_contact       : datastructure for contact management
!
    character(len=19) :: xindco, xmemco, xindcp, xmemcp
    character(len=19) :: xseuco, xseucp, xcohes, xcohep
!
! ----------------------------------------------------------------------
!
    xindco = ds_contact%sdcont_solv(1:14)//'.XFIN'
    xmemco = ds_contact%sdcont_solv(1:14)//'.XMEM'
    xindcp = ds_contact%sdcont_solv(1:14)//'.XFIP'
    xmemcp = ds_contact%sdcont_solv(1:14)//'.XMEP'
    xseuco = ds_contact%sdcont_solv(1:14)//'.XFSE'
    xseucp = ds_contact%sdcont_solv(1:14)//'.XFSP'
    xcohes = ds_contact%sdcont_solv(1:14)//'.XCOH'
    xcohep = ds_contact%sdcont_solv(1:14)//'.XCOP'
    call copisd('CHAMP_GD', 'V', xindco, xindcp)
    call copisd('CHAMP_GD', 'V', xmemco, xmemcp)
    call copisd('CHAMP_GD', 'V', xseuco, xseucp)
    call copisd('CHAMP_GD', 'V', xcohes, xcohep)
!
end subroutine
