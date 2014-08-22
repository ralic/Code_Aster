subroutine xfem_count_ddl(neq, deeq, k8cmp, nbnomax, ino_xfem, is_xfem, &
                              nbnoxfem, ieq_loc, neq_mloc, maxi_ddl)
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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  CALCUL DE 2 INFOS
!   * NEQ_MLOC :: LE NOMBRE TOTAL DE DDL A CONSIDERER POUR UN NOUED XFEM DONNE
!   * IEQ_LOC :: LA POSITION LOCALE D UN DDL (IF IEQ_LOC(NUNO)<> 0 <=> DDL MARQUE)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!-----------------------------------------------------------------------
!
! ARGUMENTS :
!------------
!
!-----------------------------------------------------------------------
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/xfem_cmps.h"
!-----------------------------------------------------------------------
!
    character(len=8) :: k8cmp(*)
    integer :: deeq(*), neq, nbnomax, nbnoxfem, maxi_ddl
    integer :: ino_xfem(nbnomax)
    integer :: ieq_loc(neq), neq_mloc(nbnoxfem)
    aster_logical :: is_xfem(nbnomax)
!
!-----------------------------------------------------------------------
!
    character(len=8) :: nocmp
    integer :: ieq, nuno, nucmp, j
    integer :: ipos
    integer :: ddlmax
    parameter (ddlmax=21)
!
!   REMARQUE: ON A AU PLUS 21 DDLS POUR UN ELEMENT XFEM (cf. 3D_XHTC)
!-----------------------------------------------------------------------
!
    call jemarq()
!
    do 20 ieq = 1, neq
       nuno=deeq(2*(ieq-1)+1)
       nucmp=deeq(2*(ieq-1)+2)
       nocmp=k8cmp(nucmp)
!       write(32,*) ieq, nuno, nocmp
       if(nuno .lt. 1) goto 20
       if(.not. is_xfem(nuno)) goto 20
       if(.not. xfem_cmps(nocmp,'OUI')) goto 20
       ipos=neq_mloc(ino_xfem(nuno))+1
       ieq_loc(ieq)=ipos
       neq_mloc(ino_xfem(nuno))=ipos
20  enddo
!
    maxi_ddl=0
    do j=1,nbnoxfem
       maxi_ddl=max(neq_mloc(j),maxi_ddl)
    enddo
!
    ASSERT(maxi_ddl .le. ddlmax)
!
    call jedema()
!
end subroutine
