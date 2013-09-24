subroutine post_op()
    implicit none
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
#include "asterfort/detmat.h"
#include "asterfort/jedetv.h"
#include "asterfort/jelibz.h"
#include "asterfort/jerecu.h"
#include "asterfort/jereou.h"
!
! This subroutine is called after the execution of each operator to clean
! the memory of temporary objects (volatile), matrix...
!
!   Delete matrix and their mumps/petsc associated instances
    call detmat()
!
!   free objects kept in memory using jeveut
    call jelibz('G')
!
!   delete objects on the volatile database
    call jedetv()
!
    call jereou('V', 0.01d0)
    call jerecu('G')
!
end subroutine post_op
