subroutine editgd(ncmp,nedit,dg,ncmpmx,ctype,&
                  jnocmp,jncmp,jvalv,jvale)
    implicit none
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
!
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
#include "asterc/indik8.h"
#include "asterfort/jacopo.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
!
    integer, intent(in) :: ncmp
    integer, intent(in) :: nedit
    integer, intent(inout) :: dg(*)
    integer, intent(in) :: ncmpmx
    character(len=8), intent(in) :: ctype
    integer, intent(in) :: jnocmp
    integer, intent(in) :: jncmp
    integer, intent(in) :: jvalv
    integer, intent(in) :: jvale
! ----------------------------------------------------------------------
!     entrees:
!     ncmp  : nombre de cmp a stocker
!
!     sorties:
!     dg  : descripteur_grandeur a mettre a jour
!
! ----------------------------------------------------------------------
    integer :: ior
!
!     VARIABLES LOCALES:
!     ------------------
    integer :: i, j, iec, reste, code, deb2, debgd
    integer :: lshift
    character(len=8) :: nomcmp
    character(len=24) :: valk
!-----------------------------------------------------------------------
    integer :: ico, indgd, ncmp2
!-----------------------------------------------------------------------
    debgd = (nedit-1)*ncmpmx
!
!   -- on compte le nombre de cmps a noter reellement :
    ncmp2=0
    do i=1,ncmp
        if (zk8(jncmp-1+i)(1:1) .ne. ' ') ncmp2=ncmp2+1
    enddo
!
    indgd = 0
    ico=0
    do i = 1, ncmpmx
        nomcmp = zk8(jnocmp-1+i)
        j = indik8(zk8(jncmp),nomcmp,1,ncmp)
        if (j .ne. 0) then
            ico=ico+1
            indgd = indgd + 1
            iec = (i-1)/30 + 1
            reste = i - 30* (iec-1)
            code = lshift(1,reste)
            dg(iec) = ior(dg(iec),code)
            deb2 = debgd + indgd
            call jacopo(1, ctype, jvalv+j-1, jvale+deb2-1)
        endif
    enddo
    if (ico .ne. ncmp2) then
        call utmess('F+', 'CALCULEL6_68')
        do i = 1, ncmpmx
            nomcmp = zk8(jnocmp-1+i)
            valk = nomcmp
            call utmess('F+', 'CALCULEL6_69', sk=valk)
        enddo
        call utmess('F+', 'CALCULEL6_70')
        do i = 1, ncmp
            nomcmp = zk8(jncmp-1+i)
            valk = nomcmp
            call utmess('F+', 'CALCULEL6_71', sk=valk)
         enddo
         call utmess('F', 'VIDE_1')
    endif
!
end subroutine
