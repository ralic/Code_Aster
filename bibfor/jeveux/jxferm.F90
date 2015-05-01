subroutine jxferm(iclas)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! aslint: disable=W1303
! for the path name
    implicit none
#include "asterc/closdr.h"
#include "asterfort/codent.h"
#include "asterfort/get_jvbasename.h"
#include "asterfort/utmess.h"
    integer :: iclas
!     ------------------------------------------------------------------
    integer :: n
!-----------------------------------------------------------------------
    integer :: k
!-----------------------------------------------------------------------
    parameter      ( n = 5 )
    character(len=2) :: dn2
    character(len=5) :: classe
    character(len=8) :: nomfic, kstout, kstini
    common /kficje/  classe    , nomfic(n) , kstout(n) , kstini(n) ,&
     &                 dn2(n)
    character(len=8) :: nombas
    common /kbasje/  nombas(n)
    integer :: idn, iext, nbenrg
    common /iextje/  idn(n) , iext(n) , nbenrg(n)
    character(len=128) :: repglo, repvol
    common /banvje/  repglo,repvol
    integer :: lrepgl, lrepvo
    common /balvje/  lrepgl,lrepvo
!     ------------------------------------------------------------------
    character(len=512) :: nom512
    integer :: ier
! DEB ------------------------------------------------------------------
    ier = 0
    do 1 k = 1, iext(iclas)
        call get_jvbasename(nomfic(iclas)(1:4), k, nom512)
        call closdr(nom512, ier)
        if (ier .ne. 0) then
            call utmess('F', 'JEVEUX_11', sk=nombas(iclas))
        endif
 1  end do
!
end subroutine
