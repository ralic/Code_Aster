subroutine avrain(nbvec, nbordr, jitrv, npic, jpic,&
                  jopic, fatsoc, ncycl, jvmin, jvmax,&
                  jomin, jomax)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: van-xuan.tran at edf.fr
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
    integer :: nbvec, nbordr, jitrv, npic(nbvec)
    integer :: jopic, ncycl(nbvec), jpic
!    integer :: omin(nbvec*(nbordr+2)), omax(nbvec*(nbordr+2))
    real(kind=8) :: fatsoc
!    real(kind=8) :: vmin(nbvec*(nbordr+2)), vmax(nbvec*(nbordr+2))
    integer :: jomin, jomax, jvmin, jvmax
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
! JVMIN      OUT  I  : ADDRESEE JEUVEUX DES VALEURS MIN DES CYCLES ELEMENTAIRES
!                     POUR TOUS LES VECTEURS NORMAUX.
! JVMAX      OUT  I  : ADDRESEE JEUVEUX DES VALEURS MAX DES CYCLES ELEMENTAIRES
!                     POUR TOUS LES VECTEURS NORMAUX.
! JOMIN      OUT  I  : ADDRESEE JEUVEUX DES NUMEROS D'ORDRE ASSOCIES AUX
!                     VALEURS  MIN DESCYCLES ELEMENTAIRES POUR TOUS LES VECTEURS
!                      NORMAUX.
! JOMAX      OUT  I  : ADDRESEE JEUVEUX DES NUMEROS D'ORDRE ASSOCIES AUX VALEURS
!
!-----------------------------------------------------------------------
!     ------------------------------------------------------------------
    integer :: ivect, i, npicb, adrs, j, k, npicr
!
    real(kind=8) :: e1, e2, e3, r1, r2, rad, rd, x, y
!
    aster_logical :: lresi
!
!-----------------------------------------------------------------------
!234567                                                              012
!
    call jemarq()
!
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
            zi(jitrv + i) = i
 20     continue
!
        ncycl(ivect) = 0
!
  1     continue
!
        i = 1
        j = 1
!
  2     continue
!
        if (i+3 .gt. npicb) then
            goto 100
        endif
!
        e1 = abs ( zr(jpic + adrs + zi(jitrv + i+1)) -  zr(jpic + adrs + zi(jitrv + i)) )
        e2 = abs ( zr(jpic + adrs + zi(jitrv + i+2)) -  zr(jpic + adrs + zi(jitrv + i+1)) )
        e3 = abs ( zr(jpic + adrs + zi(jitrv + i+3)) -  zr(jpic + adrs + zi(jitrv + i+2)) )
!
        if ((e1.ge. e2) .and. (e3 .ge. e2)) then
            ncycl(ivect) = ncycl(ivect) + 1
            if (zr(jpic+ adrs+ zi(jitrv + i+1)) .ge. zr(jpic + adrs+ zi(jitrv + i+2))) then
                zr(jvmax+adrs+ncycl(ivect)) = zr(jpic + adrs +  zi(jitrv + i+1))/ fatsoc
                zr(jvmin+adrs+ncycl(ivect)) = zr(jpic + adrs +  zi(jitrv + i+2))/ fatsoc
                zi(jomax+adrs+ncycl(ivect)) = zi(jopic + adrs +  zi(jitrv + i+1))
                zi(jomin+adrs+ncycl(ivect)) = zi(jopic + adrs +  zi(jitrv + i+2))
            else
                zr(jvmax+adrs+ncycl(ivect)) = zr(jpic + adrs +  zi(jitrv + i+2))/ fatsoc
                zr(jvmin+adrs+ncycl(ivect)) = zr(jpic + adrs +  zi(jitrv + i+1))/ fatsoc
                zi(jomax+adrs+ncycl(ivect)) = zi(jopic + adrs +  zi(jitrv + i+2))
                zi(jomin+adrs+ncycl(ivect)) = zi(jopic + adrs +  zi(jitrv + i+1))
            endif
!
            do 30 k = i+2, j+2, -1
                zi(jitrv+ k) = zi(jitrv + k-2)
 30         continue
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
100     continue
!
        if (.not. lresi) then
            npicr = npicb - 2*ncycl(ivect)
            do 110 i = 1, npicr
                zi(jitrv + i) = zi(jitrv + 2*ncycl(ivect)+i)
110         continue
            r1 = zr(jpic + adrs + zi(jitrv + 1))
            r2 = zr(jpic + adrs + zi(jitrv + 2))
            rad= zr(jpic + adrs + zi(jitrv + npicr-1))
            rd = zr(jpic + adrs + zi(jitrv + npicr))
            x = (rd-rad)*(r2-r1)
            y = (rd-rad)*(r1-rd)
            if ((x .gt. 0.d0) .and. (y .lt. 0.d0)) then
                do 120 i = 1, npicr
                    zi(jitrv+i+npicr) = zi(jitrv + i)
120             continue
                npicb = 2*npicr
            else if ((x .gt. 0.d0) .and. (y .ge. 0.d0)) then
! -- ON ELIMINE  R1 ET RN
                do 130 i = npicr, 2, -1
                    zi(jitrv + i+npicr-2) = zi(jitrv + i)
130             continue
                npicb = 2*npicr - 2
            else if ((x .lt. 0.d0) .and. (y .lt. 0.d0)) then
! -- ON ELIMINE R1
                do 140 i = npicr, 2, -1
                    zi(jitrv + i+npicr-1) = zi(jitrv + i)
140             continue
                npicb = 2*npicr - 1
            else if ((x .lt. 0.d0) .and. (y .ge. 0.d0)) then
! -- ON ELIMINE RN
                do 150 i = npicr, 1, -1
                    zi(jitrv + i+npicr-1) = zi(jitrv + i)
150             continue
                npicb = 2*npicr - 1
            endif
            lresi = .true.
            goto 1
        endif
!
!
        ASSERT((nbordr+2) .ge. ncycl(ivect))
!
 10 end do
!
    call jedema()
!
end subroutine
