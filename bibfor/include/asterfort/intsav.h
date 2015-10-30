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
#include "intdef.h"
interface
    subroutine intsav(sd_int_, ip, lonvec, iocc, kscal,&
                      iscal, rscal, cscal, kvect, ivect,&
                      rvect, cvect, buffer)
        character(len=*)          , intent(in) :: sd_int_
        integer                   , intent(in) :: ip
        integer                   , intent(in) :: lonvec
        integer,          optional, intent(in) :: iocc
        character(len=*), optional, intent(in) :: kscal
        integer,          optional, intent(in) :: iscal
        real(kind=8),     optional, intent(in) :: rscal
        complex(kind=8),  optional, intent(in) :: cscal   
        character(len=*), optional, intent(in) :: kvect(lonvec)
        integer,          optional, intent(in) :: ivect(lonvec)
        real(kind=8),     optional, intent(in) :: rvect(lonvec)
        complex(kind=8),  optional, intent(in) :: cvect(lonvec)
        integer, pointer, optional, intent(in) :: buffer(:)
    end subroutine intsav
end interface
