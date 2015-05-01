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
    subroutine xlmail(fiss, nmaen1, nmaen2, nmaen3, nmafon,&
                      jmaen1, jmaen2, jmaen3, jmafon, nfon,&
                      jfon, nbfond, jbas, jtail, jfonmu,&
                      ndim, goinop)
        character(len=8) :: fiss
        integer :: nmaen1
        integer :: nmaen2
        integer :: nmaen3
        integer :: nmafon
        integer :: jmaen1
        integer :: jmaen2
        integer :: jmaen3
        integer :: jmafon
        integer :: nfon
        integer :: jfon
        integer :: nbfond
        integer :: jbas
        integer :: jtail
        integer :: jfonmu
        integer :: ndim
        aster_logical :: goinop
    end subroutine xlmail
end interface
