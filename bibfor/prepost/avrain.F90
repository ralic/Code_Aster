subroutine avrain(nbvec, nbordr, itrv, npic, pic,&
                  opic, fatsoc, ncycl, vmin, vmax,&
                  omin, omax)
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
! person_in_charge: van-xuan.tran at edf.fr
    implicit      none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
    integer :: nbvec, nbordr, itrv(2*(nbordr+2)), npic(nbvec)
    integer :: opic(nbvec*(nbordr+2)), ncycl(nbvec)
    integer :: omin(nbvec*(nbordr+2)), omax(nbvec*(nbordr+2))
    real(kind=8) :: pic(nbvec*(nbordr+2)), fatsoc
    real(kind=8) :: vmin(nbvec*(nbordr+2)), vmax(nbvec*(nbordr+2))
! ----------------------------------------------------------------------
! BUT: COMPTAGE DE CYCLE PAR LA METHODE RAINFLOW (POSTDAM)
! ----------------------------------------------------------------------
! ARGUMENTS:
! NBVEC     IN   I  : NOMBRE DE VECTEURS NORMAUX.
! NBORDR    IN   I  : NOMBRE DE NUMERO D'ORDRE.
! ITRV      IN   I  : VECTEUR DE TRAVAIL ENTIER (POUR LES NUME_ORDRE)
! NPIC      IN   I  : NOMBRE DE PICS DETECTES POUR TOUS LES VECTEURS
!                     NORMAUX APRES REARANGEMENT DES PICS.
! PIC       IN   R  : VALEUR DES PICS DETECTES POUR TOUS LES VECTEURS
!                     NORMAUX APRES REARANGEMENT DES PICS.
! OPIC      IN   I  : NUMEROS D'ORDRE ASSOCIES AUX PICS DETECTES POUR
!                     TOUS LES VECTEURS NORMAUX APRES REARANGEMENT
!                     DES PICS.
! FATSOC     IN  R  : COEFFICIENT PERMETTANT D'UTILISER LES MEMES
!                     ROUTINES POUR LE TRAITEMENT DES CONTRAINTES ET
!                     DES DEFORMATIONS.
! NCYCL     OUT  I  : NOMBRE DE CYCLES ELEMENTAIRES POUR TOUS LES
!                     VECTEURS NORMAUX.
! VMIN      OUT  R  : VALEURS MIN DES CYCLES ELEMENTAIRES POUR TOUS LES
!                     VECTEURS NORMAUX.
! VMAX      OUT  R  : VALEURS MAX DES CYCLES ELEMENTAIRES POUR TOUS LES
!                     VECTEURS NORMAUX.
! OMIN      OUT  I  : NUMEROS D'ORDRE ASSOCIES AUX VALEURS MIN DES
!                     CYCLES ELEMENTAIRES POUR TOUS LES VECTEURS
!                     NORMAUX.
! OMAX      OUT  I  : NUMEROS D'ORDRE ASSOCIES AUX VALEURS MAX DES
!                     CYCLES ELEMENTAIRES POUR TOUS LES VECTEURS
!                     NORMAUX.
!
!-----------------------------------------------------------------------
!     ------------------------------------------------------------------
    integer :: ivect, i, npicb, adrs, j, k, npicr
!
    real(kind=8) :: e1, e2, e3, r1, r2, rad, rd, x, y
!
    logical :: lresi
!
!-----------------------------------------------------------------------
!234567                                                              012
!
    call jemarq()
!
    do 10 ivect = 1, nbvec
!
! LE TEST SI (NPIC(IVECT) .EQ. 0) EST EQUIVALENT
! AU TEST SI (IFLAG(IVECT) .EQ. 3).
        if (npic(ivect) .eq. 0) then
            goto 10
        endif
!
        ASSERT((nbordr+2) .ge. npic(ivect))
        adrs = (ivect-1)*(nbordr+2)
        lresi = .false.
        npicb = npic(ivect)
!
        do 20 i = 1, npicb
            itrv(i) = i
20      continue
!
        ncycl(ivect) = 0
!
 1      continue
!
        i = 1
        j = 1
!
 2      continue
!
        if (i+3 .gt. npicb) then
            goto 100
        endif
!
        e1 = abs ( pic(adrs + itrv(i+1)) - pic(adrs + itrv(i)) )
        e2 = abs ( pic(adrs + itrv(i+2)) - pic(adrs + itrv(i+1)) )
        e3 = abs ( pic(adrs + itrv(i+3)) - pic(adrs + itrv(i+2)) )
!
        if ((e1.ge. e2) .and. (e3 .ge. e2)) then
            ncycl(ivect) = ncycl(ivect) + 1
            if (pic(adrs+itrv(i+1)) .ge. pic(adrs+itrv(i+2))) then
                vmax(adrs+ncycl(ivect)) = pic(adrs + itrv(i+1))/ fatsoc
                vmin(adrs+ncycl(ivect)) = pic(adrs + itrv(i+2))/ fatsoc
                omax(adrs+ncycl(ivect)) = opic(adrs + itrv(i+1))
                omin(adrs+ncycl(ivect)) = opic(adrs + itrv(i+2))
            else
                vmax(adrs+ncycl(ivect)) = pic(adrs + itrv(i+2))/ fatsoc
                vmin(adrs+ncycl(ivect)) = pic(adrs + itrv(i+1))/ fatsoc
                omax(adrs+ncycl(ivect)) = opic(adrs + itrv(i+2))
                omin(adrs+ncycl(ivect)) = opic(adrs + itrv(i+1))
            endif
!
            do 30 k = i+2, j+2, -1
                itrv(k) = itrv(k-2)
30          continue
!
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
100      continue
!
        if (.not. lresi) then
            npicr = npicb - 2*ncycl(ivect)
            do 110 i = 1, npicr
                itrv(i) = itrv(2*ncycl(ivect)+i)
110          continue
            r1 = pic(adrs + itrv(1))
            r2 = pic(adrs + itrv(2))
            rad= pic(adrs + itrv(npicr-1))
            rd = pic(adrs + itrv(npicr))
            x = (rd-rad)*(r2-r1)
            y = (rd-rad)*(r1-rd)
            if ((x .gt. 0.d0) .and. (y .lt. 0.d0)) then
                do 120 i = 1, npicr
                    itrv(i+npicr) = itrv(i)
120              continue
                npicb = 2*npicr
            else if ((x .gt. 0.d0) .and. (y .ge. 0.d0)) then
! -- ON ELIMINE  R1 ET RN
                do 130 i = npicr, 2, -1
                    itrv(i+npicr-2) = itrv(i)
130              continue
                npicb = 2*npicr - 2
            else if ((x .lt. 0.d0) .and. (y .lt. 0.d0)) then
! -- ON ELIMINE R1
                do 140 i = npicr, 2, -1
                    itrv(i+npicr-1) = itrv(i)
140              continue
                npicb = 2*npicr - 1
            else if ((x .lt. 0.d0) .and. (y .ge. 0.d0)) then
! -- ON ELIMINE RN
                do 150 i = npicr, 1, -1
                    itrv(i+npicr-1) = itrv(i)
150              continue
                npicb = 2*npicr - 1
            endif
            lresi = .true.
            goto 1
        endif
!
!
        ASSERT((nbordr+2) .ge. ncycl(ivect))
!
10  end do
!
    call jedema()
!
end subroutine
