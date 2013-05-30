subroutine dclhoa(n, tab, iran)
!
    implicit none
!
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!======================================================================
!
!     EVALUE L ORDRE DES RACINES DANS IRAN AVEC N GRAND
!
! IN  N : NOMBRE DE RACINE DU POLYNOME
! IN  TAB : TABLEAU DES RACINES DU POLYNOME
!
! OUT IRAN : RANG DE LA RACINE
!
    include 'asterfort/assert.h'
    integer :: i, j, ia, ib, ir, iq, ira, irb, iaux, n
    integer :: lgpile, kopf, ipil1(100), ipil2(100), iran(*)
!
    real(kind=8) :: pivot, tab(*)
!
    lgpile = 100
!
    do 10, i=1,n
    iran(i) = i
    10 end do
!
    if (n .le. 1) goto 100
!
    j = 0
    ipil1(1) = 1
    ipil2(1) = n
    kopf = 1
!
20  continue
!
    ia = ipil1(kopf)
    ib = ipil2(kopf)
    kopf = kopf-1
!
30  continue
!
    if (ia .lt. ib) then
        pivot = tab(iran(ia))
        iq = ia+1
        ir = ib
!
40      continue
!
        if (iq .le. ir) then
            j = ir-1
!
            do 45, i = iq,j
            if (tab(iran(i)) .gt. pivot) goto 50
45          continue
!
50          continue
!
            iq = i + 1
!
            do 60, j = ir,i+1,-1
            if (tab(iran(j)) .lt. pivot) goto 70
60          continue
!
70          continue
!
            iaux = iran(i)
            iran(i) = iran(j)
            iran(j) = iaux
            ir = j - 1
            goto 40
        endif
!
        if (i .eq. j .and. tab(iran(j)) .lt. pivot) ir = j
!
        iaux = iran(ia)
        iran(ia) = iran(ir)
        iran(ir) = iaux
        ira = ir-1
        irb = ir+1
!
        if (ir-ia .gt. ib-ir) then
            if (ia .lt. ira) then
                call assert(kopf .lt. lgpile)
                kopf = kopf + 1
                ipil1(kopf) = ia
                ipil2(kopf) = ira
            endif
!
            ia = irb
!
        else
            if (irb .lt. ib) then
                call assert(kopf .lt. lgpile)
                kopf = kopf+1
                ipil1(kopf) = irb
                ipil2(kopf) = ib
            endif
!
            ib = ira
!
        endif
!
        goto 30
!
    endif
!
    if (kopf .gt. 0) goto 20
!
100  continue
!
end subroutine
