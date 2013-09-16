subroutine verift(fami, kpg, ksp, poum, imate,&
                  materi, compor, ndim, epsth, iret)
    implicit none
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
#include "jeveux.h"
#include "asterfort/rcvalb.h"
#include "asterfort/rcvarc.h"
#include "asterfort/tecael.h"
#include "asterfort/utmess.h"
    character(len=*) :: fami, poum, compor
    integer :: kpg, ksp, ndim, iret, imate
    real(kind=8) :: epsth(ndim)
    character(len=8) :: materi
!
!  FAMI : FAMILLE DE POINTS DE GAUSS
!  KPG  : NUMERO DU POINT DE GAUSS
!  KSP  : NUMERO DU SOUS-POINT DE GAUSS
!  POUM : '+' SI TEMPERATURE EN TEMPS +
!         '-' SI TEMPERATURE EN TEMPS -
!         'T' SI TEMPERATURE EN TEMPS + ET -
! IMATE : MATERIAU
! COMPOR : COMPORTEMENT
! NDIM  : 1 SI ISOTROPE
!         2 SI ISOTROPE TRANSVERSE (OU METALLURGIQUE)
!         3 SI ORTHOTROPE
! EPSTH : DILATATION
! IRET  : CODE RETOUR CONCERNANT LA TEMPERATURE 0 SI OK
!                                               1 SI NOOK
!
!
!
!
    integer :: codrem(3), codrep(3)
    character(len=8) :: nomres(3), valek(2)
    integer :: iret1, iret2, iret3, ind, somire, iadzi, iazk24
    real(kind=8) :: tm, tref, tp, valrep(3), valrem(3)
!
!
!
!     -- S'IL N'Y A PAS DE TEMPERATURE, EPSTH=0.
    call rcvarc(' ', 'TEMP', '+', fami, kpg,&
                ksp, tref, iret1)
    if (iret1 .ne. 0) then
        do 1 ind = 1, ndim
            epsth(ind) = 0.d0
 1      continue
        iret=1
        goto 9999
    endif
!
!
    iret = 0
    iret2 = 0
    iret3 = 0
!
    call rcvarc(' ', 'TEMP', 'REF', fami, kpg,&
                ksp, tref, iret1)
!
    if (compor .eq. 'ELAS_META') then
        if (ndim .eq. 2) then
            nomres(1) = 'C_ALPHA'
            nomres(2) = 'F_ALPHA'
        endif
    else
        if (ndim .eq. 1) then
            nomres(1) = 'ALPHA'
        else if (ndim.eq.2) then
            nomres(1) = 'ALPHA_L'
            nomres(2) = 'ALPHA_N'
        else
            nomres(1) = 'ALPHA_L'
            nomres(2) = 'ALPHA_T'
            nomres(3) = 'ALPHA_N'
        endif
    endif
!
!
!
!
!
    if (poum .eq. 'T') then
!
        call rcvarc(' ', 'TEMP', '-', fami, kpg,&
                    ksp, tm, iret2)
        call rcvalb(fami, kpg, ksp, '-', imate,&
                    materi, compor, 0, ' ', [0.d0],&
                    ndim, nomres, valrem, codrem, 0)
        call rcvarc(' ', 'TEMP', '+', fami, kpg,&
                    ksp, tp, iret3)
        call rcvalb(fami, kpg, ksp, '+', imate,&
                    materi, compor, 0, ' ', [0.d0],&
                    ndim, nomres, valrep, codrep, 0)
!
        somire = iret2 + iret3
!
        if (somire .eq. 0) then
!
            if (iret1 .eq. 1) then
                call tecael(iadzi, iazk24)
                valek(1) = zk24(iazk24-1+3) (1:8)
                call utmess('F', 'CALCULEL_8', sk=valek(1))
            endif
            do 5 ind = 1, ndim
                if ((codrem(ind).ne.0) .or. (codrep(ind).ne.0)) then
                    call tecael(iadzi, iazk24)
                    valek(1)= zk24(iazk24-1+3) (1:8)
                    valek(2)=nomres(ind)
                    call utmess('F', 'CALCULEL_32', nk=2, valk=valek)
                endif
 5          continue
!
            do 10 ind = 1, ndim
                epsth(ind) = valrep(ind)*(tp-tref)-valrem(ind)*(tm- tref)
10          continue
!
        else
!
            do 20 ind = 1, ndim
                epsth(ind) = 0.d0
20          continue
!
        endif
!
    else
        call rcvarc(' ', 'TEMP', poum, fami, kpg,&
                    ksp, tm, iret2)
        call rcvalb(fami, kpg, ksp, poum, imate,&
                    materi, compor, 0, ' ', [0.d0],&
                    ndim, nomres, valrem, codrem, 0)
        somire = iret2 + iret3
!
        if (somire .eq. 0) then
!
            if (iret1 .eq. 1) then
                call tecael(iadzi, iazk24)
                valek(1) = zk24(iazk24-1+3) (1:8)
                call utmess('F', 'CALCULEL_8', sk=valek(1))
            endif
            do 35 ind = 1, ndim
                if (codrem(ind) .ne. 0) then
                    call tecael(iadzi, iazk24)
                    valek(1)= zk24(iazk24-1+3) (1:8)
                    valek(2)=nomres(ind)
                    call utmess('F', 'CALCULEL_32', nk=2, valk=valek)
                endif
35          continue
!
            do 30 ind = 1, ndim
                epsth(ind) = valrem(ind)*(tm-tref)
30          continue
!
        else
!
            do 40 ind = 1, ndim
                epsth(ind) = 0.d0
40          continue
!
        endif
!
    endif
!
    if ((iret2+iret3) .ge. 1) iret = 1
!
9999  continue
end subroutine
