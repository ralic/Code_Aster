subroutine tbadap(nomta, nbpar, nompar, vi, vr,&
                   vc, vk)
    implicit none
#include "jeveux.h"
#include "asterc/ismaem.h"
#include "asterc/r8vide.h"
#include "asterfort/codent.h"
#include "asterfort/jedema.h"
#include "asterfort/jedup1.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    integer :: nbpar, vi(*)
    real(kind=8) :: vr(*)
    complex(kind=8) :: vc(*)
    character(len=*) :: nomta, nompar(*), vk(*)
! ----------------------------------------------------------------------
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
!   Check if an optimized table is input and duplicate if necessary
!   the mask objects in order to allow for tbajli to work
!   correctly. The entry parameters are exactly the same as those 
!   of tbajli.
! ----------------------------------------------------------------------
    integer :: nbpara, nblign, add_c, noadd_c, save_ind
    integer ::  i, j, ki, kr, kc, kk
    character(len=1) :: base
    character(len=3) :: type
    character(len=4) :: knume
    character(len=19) :: nomtab
    character(len=24) :: nomjvl, inpar, jnpar, nomjvl1, nomjvl2

    character(len=24), pointer :: tblp(:) => null()
    integer, pointer :: tbnp(:) => null()

    integer, pointer :: add_i(:) => null()
    integer, pointer :: noadd_i(:) => null()
    character(len=24), pointer :: add(:) => null()
    character(len=24), pointer :: noadd(:) => null()
! ----------------------------------------------------------------------
!
    call jemarq()
!
    nomtab = nomta

    call jeveuo(nomtab//'.TBLP', 'E', vk24=tblp)
    call jeveuo(nomtab//'.TBNP', 'E', vi=tbnp)
    nbpara = tbnp(1)
    nblign = tbnp(2)


    AS_ALLOCATE(vk24=add, size=nbpar)
    AS_ALLOCATE(vi=add_i, size=nbpar)
    AS_ALLOCATE(vk24=noadd, size=nbpara)
    AS_ALLOCATE(vi=noadd_i, size=nbpara)

    ki = 0
    kr = 0
    kc = 0
    kk = 0

    add_c = 0
    noadd_c = 0

!   This loops fills up two chain arrays, /add/ and /noadd/, as well as two 
!   integer arrays /add_i/ and /noadd_i/. Chain arrays contain the names of the 
!   jeveux object representing the masks for each parameter. The index of the 
!   parameter is saved in the XXadd_i array.

!   A to-be-added parameter is defined as a parameter whose value is to be included
!   in the following line.
    do i = 1, nbpara
        inpar = tblp(1+4*(i-1) )
        do j = 1, nbpar
            jnpar = nompar(j)
            if (jnpar .eq. inpar) then
                type = tblp(1+4*(i-1)+1)(1:3)

                if (type(1:1) .eq. 'I') then
                    ki = ki + 1
                    if (vi(ki) .eq. ismaem()) goto 10
                else if (type(1:1) .eq. 'R') then
                    kr = kr + 1
                    if (vr(kr) .eq. r8vide()) goto 10
                else if (type(1:1) .eq. 'C') then
                    kc = kc + 1
                    if (dble(vc(kc)) .eq. r8vide() .and. dimag( vc(kc)) .eq. r8vide()) &
                        goto 10
                else if (type(1:1) .eq. 'K') then
                    kk = kk + 1
                    if (vk(kk)(1:7) .eq. '???????') goto 10
                endif

!               Parameter of index /i/ is not empty, it needs to be saved
                nomjvl = tblp(1+4*(i-1)+3)

                add_c = add_c + 1
                add(add_c) = nomjvl
                add_i(add_c) = i
                goto 20 
            end if
10          continue            
        end do

!       Parameter of index /i/ is not to be saved
        nomjvl = tblp(1+4*(i-1)+3)
        noadd_c = noadd_c + 1
        noadd(noadd_c) = nomjvl
        noadd_i(noadd_c) = i
20      continue
    end do

!   If all parameters are to be saved, or, if all of them are *not* to be saved
!   then there is no possibility of conflict
    if ((add_c .eq. nbpara) .or. (noadd_c .eq. nbpara)) goto 30


!   Otherwise, we need to cross-check the /add/ and /noadd/ arrays for
!   duplicate entries, and do the necessary for expanding the optimized table

    call jelira(add(1), 'CLAS', cval=base)

!   Conflict detection
    do i = 1, add_c
        nomjvl1 = add(i)
        do j = 1, noadd_c
            nomjvl2 = noadd(j)
            if (nomjvl1 .eq. nomjvl2) then
!               Conflict detected, compare the parameter indices
                read (nomjvl1(21:24),'(I16)') save_ind
                if (add_i(i) .eq. save_ind) then
!                   The index of the paramater to be added is that of the reference  
!                   parameter, then duplicate its existing logicals and create 
!                   a new object for parameter of index noadd(j)
                    call codent(noadd_i(j), 'D0', knume)
                    nomjvl2 = nomtab(1:17)//'LG.'//knume
                    call jedup1(nomjvl1,base,nomjvl2)
                    tblp(1+4*(noadd_i(j)-1)+3) = nomjvl2
                else if (noadd_i(j) .eq. save_ind) then
!                   The index of the paramater to be added is not that of the reference  
!                   parameter, then duplicate the existing logicals and create 
!                   a new object for parameter of index add(j)
                    call codent(add_i(i), 'D0', knume)
                    nomjvl2 = nomtab(1:17)//'LG.'//knume
                    call jedup1(nomjvl1,base,nomjvl2)
                    tblp(1+4*(add_i(i)-1)+3) = nomjvl2
                end if
            end if
        end do
    end do


30  continue

    AS_DEALLOCATE(vk24=add)
    AS_DEALLOCATE(vi=add_i)
    AS_DEALLOCATE(vk24=noadd)
    AS_DEALLOCATE(vi=noadd_i)

    call jedema()
end subroutine
