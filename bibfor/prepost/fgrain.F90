subroutine fgrain(pic, npic, itrv, ncyc, sigmin,&
                  sigmax)
!       ================================================================
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
!       ----------------------------------------------------------------
!      COMPTAGE DES CYCLES PAR LA METHODE RAINFLOW (POSTDAM)
!       ----------------------------------------------------------------
!      IN  PIC     VECTEUR  DES PICS
!          NPIC    NOMBRE   DE  PICS
!          ITRV    VECTEUR  DE TRAVAIL ENTIER
!      OUT SIGMAX  CONTRAINTES MAXIMALES DES CYCLES
!          SIGMIN  CONTRAINTES MINIMALES DES CYCLES
!      OUT  NCYC    NOMBRE  DE  CYCLE
!       ----------------------------------------------------------------
!
    implicit none
#include "asterf_types.h"
#include "asterfort/infniv.h"
#include "asterfort/utmess.h"
    real(kind=8) :: pic(*), x, y, e1, e2, e3, sigmax(*), sigmin(*)
    real(kind=8) :: r1, r2, rd, rad
    integer :: npic, ncyc, itrv(*), npicb
    aster_logical :: lresi, cyczer
!       ----------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, ifm, j, k, niv, npicr
!-----------------------------------------------------------------------
    lresi = .false.
    npicb = npic
    cyczer = .true.
!
!     --- RECUPERATION DU NIVEAU D'IMPRESSION ---
!
    call infniv(ifm, niv)
!
    do 20 i = 1, npicb
        itrv(i) = i
 20 continue
    ncyc = 0
!
    do 21 i = 2, npicb
        if ((pic(i) .gt. pic(1)) .or. (pic(i) .lt. pic(1))) then
            cyczer = .false.
        endif
 21 continue
!
    if (cyczer) then
        sigmax(1) = pic(1)
        sigmin(1) = pic(1)
        ncyc = 1
!
        call utmess('A', 'FATIGUE1_39')
!
        goto 999
    endif
!
!
  1 continue
    i = 1
    j = 1
!
  2 continue
    if (i+3 .gt. npicb) then
        goto 100
    endif
    e1 = abs ( pic(itrv(i+1)) - pic(itrv(i)) )
    e2 = abs ( pic(itrv(i+2)) - pic(itrv(i+1)))
    e3 = abs ( pic(itrv(i+3)) - pic(itrv(i+2)))
!
    if (e1 .ge. e2 .and. e3 .ge. e2) then
        ncyc = ncyc + 1
        if (pic(itrv(i+1)) .ge. pic(itrv(i+2))) then
            sigmax(ncyc) = pic(itrv(i+1))
            sigmin(ncyc) = pic(itrv(i+2))
        else
            sigmax(ncyc) = pic(itrv(i+2))
            sigmin(ncyc) = pic(itrv(i+1))
        endif
        do 3 k = i+2, j+2, -1
            itrv(k) = itrv(k-2)
  3     continue
        j=j+2
        i=j
        goto 2
    else
        i=i+1
        goto 2
    endif
!
!  --- TRAITEMENT DU RESIDU -------
!
100 continue
    if (.not.lresi) then
        npicr = npicb-2*ncyc
        do 101 i = 1, npicr
            itrv(i)= itrv(2*ncyc+i)
101     continue
        r1 = pic(itrv(1))
        r2 = pic(itrv(2))
        rad= pic(itrv(npicr-1))
        rd = pic(itrv(npicr))
        x = (rd-rad)*(r2-r1)
        y = (rd-rad)*(r1-rd)
        if (x .gt. 0.d0 .and. y .lt. 0.d0) then
            do 102 i = 1, npicr
                itrv(i+npicr)= itrv(i)
102         continue
            npicb = 2*npicr
        else if (x.gt.0.d0.and.y.ge.0.d0) then
! -- ON ELIMINE  R1 ET RN
            do 103 i = npicr, 2, -1
                itrv(i+npicr-2)= itrv(i)
103         continue
            npicb = 2*npicr-2
        else if (x.lt.0.d0.and.y.lt.0.d0) then
! -- ON ELIMINE R1
            do 104 i = npicr, 2, -1
                itrv(i+npicr-1)= itrv(i)
104         continue
            npicb = 2*npicr-1
        else if (x.lt.0.d0.and.y.ge.0.d0) then
! -- ON ELIMINE RN
            do 105 i = npicr, 1, -1
                itrv(i+npicr-1)= itrv(i)
105         continue
            npicb = 2*npicr-1
        endif
        lresi = .true.
        goto 1
    endif
!
!     --- IMPRESSION DES PICS EXTRAITS DE LA FONCTION ----
    if (niv .eq. 2) then
        write (ifm,*)
        write (ifm,'(1X,A)') 'PICS APRES LE COMPTAGE RAINFLOW'
        write (ifm,*)
        write (6,*) 'NOMBRE DE CYCLES = ', ncyc
        write (ifm,*)
        write (ifm,'(1X,A)') '     CHARGEMENT_MAX     CHARGEMENT_MIN'
        write (ifm,*)
        write (ifm,'(2(1X,E18.6))') (sigmax(i),sigmin(i),i=1,ncyc)
!         DO 106 I=1,NCYC
!             WRITE (IFM,'(2(1X,E18.6))'), SIGMAX(I),SIGMIN(I)
! 106     CONTINUE
!
    endif
!
!
999 continue
!
!
!
end subroutine
