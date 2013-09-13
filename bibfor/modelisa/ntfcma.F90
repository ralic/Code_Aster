subroutine ntfcma(jmat, ifon)
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
! ----------------------------------------------------------------------
!     OBTENTION DES ADRESSES DES FONCTIONS BETA ET LAMBDA DANS LE
!     MATERIAU CODE IMATE
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
!-----------------------------------------------------------------------
    parameter        ( lmat = 7 , lfct = 9 )
! DEB ------------------------------------------------------------------
!
!
    nbmat=zi(jmat)
    ASSERT(nbmat.eq.1)
    imate = jmat+zi(jmat+nbmat+1)
!
    do 10 k = 1, zi(imate+1)
        if ('THER_NL ' .eq. zk16(zi(imate)+k-1)(1:8)) then
            ipi = zi(imate+2+k-1)
            goto 11
        endif
10  end do
    goto 35
11  continue
    idf = zi(ipi)+zi(ipi+1)
    do 20 k = 1, zi(ipi+2)
        if ('BETA    ' .eq. zk8(zi(ipi+3)+idf+k-1)) then
            ifon(1) = ipi+lmat-1+lfct*(k-1)
            goto 25
        endif
20  end do
    call utmess('F', 'MODELISA5_44')
25  continue
    do 30 k = 1, zi(ipi+2)
        if ('LAMBDA  ' .eq. zk8(zi(ipi+3)+idf+k-1)) then
            ifon(2) = ipi+lmat-1+lfct*(k-1)
            goto 75
        endif
30  end do
    call utmess('F', 'MODELISA5_45')
35  continue
    do 40 k = 1, zi(imate+1)
        if ('THER_HYDR ' .eq. zk16(zi(imate)+k-1)(1:9)) then
            ipi = zi(imate+2+k-1)
            goto 41
        endif
40  end do
    call utmess('F', 'ELEMENTS2_63')
41  continue
    idf = zi(ipi)+zi(ipi+1)
    do 50 k = 1, zi(ipi+2)
        if ('BETA    ' .eq. zk8(zi(ipi+3)+idf+k-1)) then
            ifon(1) = ipi+lmat-1+lfct*(k-1)
            goto 55
        endif
50  end do
    call utmess('F', 'MODELISA5_44')
55  continue
    do 60 k = 1, zi(ipi+2)
        if ('LAMBDA  ' .eq. zk8(zi(ipi+3)+idf+k-1)) then
            ifon(2) = ipi+lmat-1+lfct*(k-1)
            goto 65
        endif
60  end do
    call utmess('F', 'MODELISA5_45')
65  continue
    do 70 k = 1, zi(ipi+2)
        if ('AFFINITE  ' .eq. zk8(zi(ipi+3)+idf+k-1)) then
            ifon(3) = ipi+lmat-1+lfct*(k-1)
            goto 75
        endif
70  end do
    call utmess('F', 'MODELISA5_47')
75  continue
!
! FIN ------------------------------------------------------------------
end subroutine
