subroutine ntfcma(compo, jmat, ifon)
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
! ----------------------------------------------------------------------
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/utmess.h"
    integer :: imate, ifon(3)
    character(len=*) :: compo
! ----------------------------------------------------------------------
!     OBTENTION DES ADRESSES DES FONCTIONS BETA ET LAMBDA DANS LE
!     MATERIAU CODE IMATE
! IN  COMPO  : NOM DU COMPORTEMENT CHERCHE
! IN  IMATE  : ADRESSE DU MATERIAU CODE
! OUT IFON   : ADRESSE RELATIVE DES PARAMETRES BETA ET LAMBDA
!      IFON(1) : ADRESSE RELATIVE DU PARAMETRE BETA OU -1 SI BETA ABSENT
!      IFON(2) : ADRESSE RELATIVE DU PARAMETRE LAMBDA
!      IFON(3) : ADRESSE DU NOM DE LA FONCTION AFFINITE SI THER_HYDR
!
!
!
!
    integer :: ipi, k, nbmat
! ----------------------------------------------------------------------
! PARAMETER ASSOCIE AU MATERIAU CODE
!
!-----------------------------------------------------------------------
    integer :: idf, jmat, lfct, lmat
    character(len=16) :: valk(2), compo2
!-----------------------------------------------------------------------
    parameter        ( lmat = 7 , lfct = 9 )
! DEB ------------------------------------------------------------------
!
!
    nbmat=zi(jmat)
    ASSERT(nbmat.eq.1)
    imate = jmat+zi(jmat+nbmat+1)
!    
    ASSERT( compo(1:7).eq. 'THER_NL' .or. compo(1:9).eq. 'THER_HYDR' .or. compo.eq. ' ' )
!
    if (compo .eq. ' ') then
        do k = 1, zi(imate+1)
            if ('THER_NL' .eq. zk32(zi(imate)+k-1)(1:7)) then
                ipi = zi(imate+2+k-1)
                compo2 = 'THER_NL'
                goto 11
            endif
        end do
        do k = 1, zi(imate+1)
            if ('THER_HYDR' .eq. zk32(zi(imate)+k-1)(1:9)) then
                ipi = zi(imate+2+k-1)
                compo2 = 'THER_HYDR'
                goto 11
            endif
        end do
    else
        do k = 1, zi(imate+1)
            if (compo(1:9) .eq. zk32(zi(imate)+k-1)(1:9)) then
                ipi = zi(imate+2+k-1)
                compo2 = compo
                goto 11
            endif
        end do
    endif
    do k = 1, zi(imate+1)
        if ('THER_ ' .eq. zk32(zi(imate)+k-1)(1:5)) then
            valk(1) = zk32(zi(imate)+k-1)
            valk(2) = compo
            if (compo .eq. ' ') then
                call utmess('F', 'ELEMENTS2_65', sk=valk(1))
            else
                call utmess('F', 'ELEMENTS2_64', nk=2, valk=valk)
            endif
        endif
    end do
    if (compo .eq. ' ') then
        call utmess('F', 'ELEMENTS2_66')
    else
        call utmess('F', 'ELEMENTS2_63', sk=compo)
    endif
 11 continue
    idf = zi(ipi)+zi(ipi+1)
    do k = 1, zi(ipi+2)
        if ('BETA    ' .eq. zk16(zi(ipi+3)+idf+k-1)) then
            ifon(1) = ipi+lmat-1+lfct*(k-1)
            goto 25
        endif
    end do
    call utmess('F', 'MODELISA5_44')
 25 continue
    do k = 1, zi(ipi+2)
        if ('LAMBDA  ' .eq. zk16(zi(ipi+3)+idf+k-1)) then
            ifon(2) = ipi+lmat-1+lfct*(k-1)
            goto 35
        endif
    end do
    call utmess('F', 'MODELISA5_45')
 35 continue
    if (compo2(1:9) .eq. 'THER_HYDR') then
        do k = 1, zi(ipi+2)
            if ('AFFINITE  ' .eq. zk16(zi(ipi+3)+idf+k-1)) then
                ifon(3) = ipi+lmat-1+lfct*(k-1)
                goto 45
            endif
        end do
        call utmess('F', 'MODELISA5_47')
 45     continue
    endif
!
! FIN ------------------------------------------------------------------
end subroutine
