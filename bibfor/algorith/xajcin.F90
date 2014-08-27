subroutine xajcin(model, option, mxchin, lchin, lpain,&
                  nchin)
!
implicit none
!
#include "asterfort/assert.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
    integer, intent(in) :: mxchin
    character(len=*), intent(in) :: model
    character(len=*), intent(in) :: option
    character(len=*), intent(inout) :: lpain(mxchin)
    character(len=*), intent(inout) :: lchin(mxchin)
    integer, intent(inout) :: nchin
!
! --------------------------------------------------------------------------------------------------
!
! Add XFEM fields for input fields
!
!  -> OPTIONS : - CHAR_MECA_TEMP_R
!               - CHAR_THER_PARO_F
!               - CHAR_THER_PARO_R
!               - FULL_MECA
!               - RIGI_MECA_*
!               - RAPH_MECA
!               - CHAR_MECA_NEUM
!
! --------------------------------------------------------------------------------------------------
!
! In  model  : name of model
! In  option : option to select input fields
! In  mxchin : maximum number of input fields
! IO  lpain  : list of parameters
! IO  lchin  : list of fields
! IO  nbin   : number of input fields
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nbadd
!
! --------------------------------------------------------------------------------------------------
!
    if ((option .eq. 'CHAR_MECA_TEMP_R').or. &
        (option(1:9) .eq. 'FULL_MECA') .or. &
        (option(1:9) .eq. 'RAPH_MECA') .or. &
        (option(1:9) .eq. 'RIGI_MECA')) then
!
        nbadd = 10
        ASSERT(nchin+nbadd .le. mxchin)
        lpain(nchin + 1 ) = 'PPINTTO'
        lchin(nchin + 1 ) = model(1:8)//'.TOPOSE.PIN'
        lpain(nchin + 2 ) = 'PCNSETO'
        lchin(nchin + 2 ) = model(1:8)//'.TOPOSE.CNS'
        lpain(nchin + 3 ) = 'PHEAVTO'
        lchin(nchin + 3 ) = model(1:8)//'.TOPOSE.HEA'
        lpain(nchin + 4 ) = 'PLONCHA'
        lchin(nchin + 4 ) = model(1:8)//'.TOPOSE.LON'
        lpain(nchin + 5 ) = 'PBASLOR'
        lchin(nchin + 5 ) = model(1:8)//'.BASLOC'
        lpain(nchin + 6 ) = 'PLSN'
        lchin(nchin + 6 ) = model(1:8)//'.LNNO'
        lpain(nchin + 7 ) = 'PLST'
        lchin(nchin + 7 ) = model(1:8)//'.LTNO'
        lpain(nchin + 8 ) = 'PSTANO'
        lchin(nchin + 8 ) = model(1:8)//'.STNO'
        lpain(nchin + 9 ) = 'PPMILTO'
        lchin(nchin + 9 ) = model(1:8)//'.TOPOSE.PMI'
        lpain(nchin + 10) = 'PFISNO'
        lchin(nchin + 10) = model(1:8)//'.FISSNO'
        nchin = nchin+nbadd
!
    elseif (option.eq.'CHAR_MECA_NEUM') then
!
        nbadd = 14
        ASSERT(nchin+nbadd .le. mxchin)
        lpain(nchin + 1 ) = 'PPINTTO'
        lchin(nchin + 1 ) = model(1:8)//'.TOPOSE.PIN'
        lpain(nchin + 2 ) = 'PCNSETO'
        lchin(nchin + 2 ) = model(1:8)//'.TOPOSE.CNS'
        lpain(nchin + 3 ) = 'PHEAVTO'
        lchin(nchin + 3 ) = model(1:8)//'.TOPOSE.HEA'
        lpain(nchin + 4 ) = 'PLONCHA'
        lchin(nchin + 4 ) = model(1:8)//'.TOPOSE.LON'
        lpain(nchin + 5 ) = 'PLSN'
        lchin(nchin + 5 ) = model(1:8)//'.LNNO'
        lpain(nchin + 6 ) = 'PLST'
        lchin(nchin + 6 ) = model(1:8)//'.LTNO'
        lpain(nchin + 7 ) = 'PSTANO'
        lchin(nchin + 7 ) = model(1:8)//'.STNO'
        lpain(nchin + 8 ) = 'PPMILTO'
        lchin(nchin + 8 ) = model(1:8)//'.TOPOSE.PMI'
        lpain(nchin + 9 ) = 'PFISNO'
        lchin(nchin + 9 ) = model(1:8)//'.FISSNO'
        lpain(nchin + 10) = 'PPINTER'
        lchin(nchin + 10) = model(1:8)// '.TOPOFAC.OE'
        lpain(nchin + 11) = 'PAINTER'
        lchin(nchin + 11) = model(1:8)// '.TOPOFAC.AI'
        lpain(nchin + 12) = 'PCFACE'
        lchin(nchin + 12) = model(1:8)// '.TOPOFAC.CF'
        lpain(nchin + 13) = 'PLONGCO'
        lchin(nchin + 13) = model(1:8)// '.TOPOFAC.LO'
        lpain(nchin + 14) = 'PBASECO'
        lchin(nchin + 14) = model(1:8)// '.TOPOFAC.BA'
        nchin = nchin+nbadd
!
    elseif ((option.eq.'CHAR_THER_PARO_F').or.&
            (option.eq.'CHAR_THER_PARO_R')) then
!
        nbadd = 7
        ASSERT(nchin+nbadd .le. mxchin)
        lpain(nchin + 1 ) = 'PPINTER'
        lchin(nchin + 1 ) = model(1:8)//'.TOPOFAC.OE'
        lpain(nchin + 2 ) = 'PAINTER'
        lchin(nchin + 2 ) = model(1:8)//'.TOPOFAC.AI'
        lpain(nchin + 3 ) = 'PCFACE'
        lchin(nchin + 3 ) = model(1:8)//'.TOPOFAC.CF'
        lpain(nchin + 4 ) = 'PLONGCO'
        lchin(nchin + 4 ) = model(1:8)//'.TOPOFAC.LO'
        lpain(nchin + 5 ) = 'PLST'
        lchin(nchin + 5 ) = model(1:8)//'.LTNO'
        lpain(nchin + 6 ) = 'PSTANO'
        lchin(nchin + 6 ) = model(1:8)//'.STNO'
        lpain(nchin + 7 ) = 'PBASECO'
        lchin(nchin + 7 ) = model(1:8)//'.TOPOFAC.BA'
        nchin = nchin+nbadd
!
    else
        ASSERT(.false.)
    endif
!
end subroutine
