subroutine lccata(iunit)
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
#include "asterfort/caver1.h"
#include "asterfort/lctel2.h"
#include "asterfort/lctel3.h"
#include "asterfort/lecojb.h"
    integer :: iunit, mxobj, iret, i
    parameter        ( mxobj = 50 )
!
    character(len=24) :: nomobj
!
!
!     LECTURE DU FICHIER CONTENANT LES OBJETS JEVEUX AU FORMAT OJB :
!     --------------------------------------------------------------
    rewind(iunit)
    do 1,i=1,mxobj
    call lecojb(nomobj, iunit, 'G', iret)
    if (iret .gt. 0) goto 2
    write(6,*) ' OBJET LU :',nomobj
    1 end do
 2  continue
    write(6,*) ' NB_OBJETS LUS :',i
!
!
!     CREATION D'OBJETS SUPPLEMENTAIRES
!     ----------------------------------------------------
    call lctel2()
    call lctel3()
!
!
!     VERIFICATION DE COHERENCE DES CATALOGUES
!     ----------------------------------------------------
    call caver1()
!
!
!
end subroutine
