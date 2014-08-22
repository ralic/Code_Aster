subroutine xfem_count_no(neq, deeq, k8cmp, nbnomax, ino_xfem, is_xfem, nbnoxfem)
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
! BUT : 
! * MARQUAGE DES NOEUDS XFEM : IS_XFEM(NBNOMAX)
! * BASCULEMENT VERS UN STOCKAGE LOCAL DES NOEUDS XFEM : INO_XFEM(NBNOMAX)

!-----------------------------------------------------------------------
!
! ARGUMENTS :
!------------
!
! SORTIES :
!     - IS_XFEM  :: RETOURNE .TRUE. SI ON A TROUVE UN NOEUD XFEM 
!     - INO_XFEM :: RETOURNE LA NUMEROTATION LOCALE DES NOEUDS XFEM
!         SI INO_XFEM(I)>0 : LE NOEUD A ETE MARQUE => NUMERO DE NOEUD LOCAL
!         SI INO_XFEM(I)=0 : LE NOEUD N A PAS ETE MARQUE
!     - NBNOXFEM :: NOMBRE DE NOEUDS MARQUES  
!-----------------------------------------------------------------------
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/xfem_cmps.h"
!-----------------------------------------------------------------------
!
    character(len=8) :: k8cmp(*)
    integer :: deeq(*), neq, nbnomax, nbnoxfem
    integer :: ino_xfem(nbnomax)
    aster_logical :: is_xfem(nbnomax)
!
!-----------------------------------------------------------------------
!
    character(len=8) :: nocmp
    integer :: ieq, nuno, nucmp
!
!-----------------------------------------------------------------------
!
    call jemarq()
!
    nbnoxfem=0
    do 10 ieq = 1, neq
       nuno=deeq(2*(ieq-1)+1)
       nucmp=deeq(2*(ieq-1)+2)
       nocmp=k8cmp(nucmp)
       if(nuno .lt. 1) goto 10
       if(is_xfem(nuno)) goto 10
       if (xfem_cmps(nocmp)) then
          nbnoxfem=nbnoxfem+1
          ino_xfem(nuno)=nbnoxfem
          is_xfem(nuno)=.true.
       endif        
10  enddo
    ASSERT(nbnoxfem .gt. 0)
!
    call jedema()
!
end subroutine
