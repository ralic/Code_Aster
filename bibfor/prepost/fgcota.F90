subroutine fgcota(npic, pic, ncyc, sigmin, sigmax)
!       ================================================================
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
!       ----------------------------------------------------------------
!      COMPTAGE DES CYCLES POUR LA METHODE TAHERI
!       ----------------------------------------------------------------
!      IN  NPIC    NOMBRE  DE PICS
!          PIC     VALEURS DES PICS
!      OUT NCYC    NOMBRE  DE  CYCLE
!      OUT SIGMAX  CONTRAINTES MAXIMALES DES CYCLES
!          SIGMIN  CONTRAINTES MINIMALES DES CYCLES
!       ----------------------------------------------------------------
    implicit none
#include "asterfort/u2mess.h"
    real(kind=8) :: pic(*), e1, e2, sigmax(*), sigmin(*)
    integer :: npic, ncyc, k
    logical :: cyczer
!       ----------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i
!-----------------------------------------------------------------------
    ncyc = 0
    i = 1
    cyczer = .true.
!
!
    do 21 k = 2, npic
        if ((pic(k) .gt. pic(1)) .or. (pic(k) .lt. pic(1))) then
            cyczer = .false.
        endif
21  end do
!
    if (cyczer) then
        sigmax(1) = pic(1)
        sigmin(1) = pic(1)
        ncyc = 1
!
        call u2mess('A', 'FATIGUE1_39')
!
        goto 999
    endif
!
 2  continue
    if (i+2 .gt. npic) then
        goto 100
    endif
    e1 = abs ( pic(i+1) - pic(i) )
    e2 = abs ( pic(i+2) - pic(i+1) )
!
    if (e1 .ge. e2) then
        ncyc = ncyc + 1
        if (pic(i) .ge. pic(i+1)) then
            sigmax(ncyc) = pic(i)
            sigmin(ncyc) = pic(i+1)
        else
            sigmax(ncyc) = pic(i+1)
            sigmin(ncyc) = pic(i)
        endif
    else
        ncyc = ncyc + 1
        if (pic(i+1) .ge. pic(i+2)) then
            sigmax(ncyc) = pic(i+1)
            sigmin(ncyc) = pic(i+2)
        else
            sigmax(ncyc) = pic(i+2)
            sigmin(ncyc) = pic(i+1)
        endif
    endif
    i= i+2
    goto 2
!
!  --- TRAITEMENT DU RESIDU -------
!
100  continue
    if (i+1 .eq. npic) then
        ncyc = ncyc+1
        if (pic(i) .ge. pic(i+1)) then
            sigmax(ncyc) = pic(i)
            sigmin(ncyc) = pic(i+1)
        else
            sigmax(ncyc) = pic(i+1)
            sigmin(ncyc) = pic(i)
        endif
    endif
!
999  continue
!
end subroutine
