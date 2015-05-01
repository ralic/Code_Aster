!
! COPYRIGHT (C) 1991 - 2015  EDF R&D                WWW.CODE-ASTER.ORG
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
#include "asterf_types.h"
!
interface
    subroutine vplecs(eigsol,&
                  itemax, maxitr, nbborn, nitv, nborto, nbvec2, nbvect, nbrss, nfreq, nperm,&
                  alpha, omecor, freq1, freq2, precdc, precsh, prorto, prsudg, seuil, tol, toldyn,&
                  tolsor,&
                  appr, arret, method, typevp, matra, matrb, matrc, modrig, optiof, stoper, sturm,&
                  typeqz, typres, amor, masse, raide, tabmod,&
                  lc, lkr, lns, lpg, lqz)
!                    
    character(len=19) , intent(in)    :: eigsol
!!
    integer           , intent(out)   :: itemax
    integer           , intent(out)   :: maxitr
    integer           , intent(out)   :: nbborn
    integer           , intent(out)   :: nitv
    integer           , intent(out)   :: nborto
    integer           , intent(out)   :: nbvec2
    integer           , intent(out)   :: nbvect
    integer           , intent(out)   :: nbrss
    integer           , intent(out)   :: nfreq
    integer           , intent(out)   :: nperm
!
    real(kind=8)      , intent(out)   :: alpha
    real(kind=8)      , intent(out)   :: omecor
    real(kind=8)      , intent(out)   :: freq1
    real(kind=8)      , intent(out)   :: freq2
    real(kind=8)      , intent(out)   :: precdc
    real(kind=8)      , intent(out)   :: precsh
    real(kind=8)      , intent(out)   :: prorto
    real(kind=8)      , intent(out)   :: prsudg
    real(kind=8)      , intent(out)   :: seuil
    real(kind=8)      , intent(out)   :: tol
    real(kind=8)      , intent(out)   :: toldyn
    real(kind=8)      , intent(out)   :: tolsor
!
    character(len=1)  , intent(out)   :: appr
!
    character(len=8)  , intent(out)   :: arret
    character(len=8)  , intent(out)   :: method
!
    character(len=9)  , intent(out)   :: typevp
!
    character(len=14) , intent(out)   :: matra
    character(len=14) , intent(out)   :: matrb
    character(len=14) , intent(out)   :: matrc
!    
    character(len=16) , intent(out)   :: modrig
    character(len=16) , intent(out)   :: optiof
    character(len=16) , intent(out)   :: stoper
    character(len=16) , intent(out)   :: sturm
    character(len=16) , intent(out)   :: typeqz
    character(len=16) , intent(out)   :: typres
!
    character(len=19) , intent(out)   :: amor
    character(len=19) , intent(out)   :: masse
    character(len=19) , intent(out)   :: raide
    character(len=19) , intent(out)   :: tabmod    
!
    aster_logical   , intent(out)   :: lc
    aster_logical   , intent(out)   :: lkr
    aster_logical   , intent(out)   :: lns
    aster_logical   , intent(out)   :: lpg
    aster_logical   , intent(out)   :: lqz
!    
    end subroutine vplecs
end interface
