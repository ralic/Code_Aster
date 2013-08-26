subroutine avpic2(method, nbvec, nbordr, jrtrv, jitrv,&
                  npoin, jvalpo, jvalor, npic, jpic,&
                  jordpi)
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
#include "asterfort/u2mesk.h"
    integer :: nbvec, nbordr, npoin(nbvec), jvalor
    integer :: npic(nbvec), jordpi
    integer :: jitrv

    character(len=8) :: method
    integer ::jrtrv, jvalpo, jpic
! ----------------------------------------------------------------------
! BUT: EXTRACTION DES PICS POUR RAINFLOW <=> REARANGEMENT DES PICS,
!      PIC LE PLUS GRAND AU DEBUT ET A LA FIN.
! ----------------------------------------------------------------------
! ARGUMENTS:
! METHOD    IN   K  : METHODE D'EXTRACTION DES PICS, PAR EXEMPLE :
!                     RAINFLOW.
! NBVEC     IN   I  : NOMBRE DE VECTEURS NORMAUX.
! NBORDR    IN   I  : NOMBRE DE NUMERO D'ORDRE.
! RTRV      IN   R  : VECTEUR DE TRAVAIL REEL (POUR LES POINTS)
! ITRV      IN   I  : VECTEUR DE TRAVAIL ENTIER (POUR LES NUME_ORDRE)
! NPOIN     IN   I  : NOMBRE DE PICS DETECTES POUR TOUS LES VECTEURS
!                     NORMAUX.
! VALPOI    IN   R  : VALEUR DES PICS DETECTES POUR TOUS LES VECTEURS
!                     NORMAUX.
! VALORD    IN   I  : NUMEROS D'ORDRE ASSOCIES AUX PICS DETECTES POUR
!                     TOUS LES VECTEURS NORMAUX.
! NPIC      OUT  I  : NOMBRE DE PICS DETECTES POUR TOUS LES VECTEURS
!                     NORMAUX APRES REARANGEMENT DES PICS.
! PIC       OUT  R  : VALEUR DES PICS DETECTES POUR TOUS LES VECTEURS
!                     NORMAUX APRES REARANGEMENT DES PICS.
! ORDPIC    OUT  I  : NUMEROS D'ORDRE ASSOCIES AUX PICS DETECTES POUR
!                     TOUS LES VECTEURS NORMAUX APRES REARANGEMENT
!                     DES PICS.
!
!-----------------------------------------------------------------------
!     ------------------------------------------------------------------
    integer :: ivect, adrs, i, nmax, ntrv, ointer
    real(kind=8) :: epsilo, pmax, pinter, dp1, dp2
    character(len=8) :: k8b
!-----------------------------------------------------------------------
!234567                                                              012
!-----------------------------------------------------------------------
    epsilo = 1.0d-7
!-----------------------------------------------------------------------
!
    call jemarq()
!
    if (method .ne. 'RAINFLOW') then
        k8b = method(1:8)
        call u2mesk('F', 'PREPOST_4', 1, k8b)
    endif
!
    do 10 ivect = 1, nbvec
!
! LE TEST SI (NPOIN(IVECT) .EQ. 0) EST EQUIVALENT
! AU TEST SI (IFLAG(IVECT) .EQ. 3).
        if (npoin(ivect) .eq. 0) then
            goto 10
        endif
!
! ----- RECHERCHE DU POINT LE PLUS GRAND EN VALEUR ABSOLUE -----
!
        ASSERT(nbordr .ge. npoin(ivect))
        adrs = (ivect-1)*(nbordr+2)
!
        pmax = abs(zr(jvalpo + (ivect-1)*nbordr + 1))
        nmax = 1
        do 20 i = 2, npoin(ivect)
            if (abs(zr(jvalpo + (ivect-1)*nbordr + i)) .gt. & 
                    pmax*(1.0d0+ epsilo)) then

                pmax = abs(zr(jvalpo + (ivect-1)*nbordr + i))
                nmax = i
            endif
20      continue
        pmax = zr(jvalpo + (ivect-1)*nbordr + nmax)
        ntrv = npoin(ivect)
!
! ----- REARANGEMENT AVEC POINT LE PLUS GRAND AU DEBUT
!       ET A LA FIN                                    -----
!
        do 30 i = nmax, npoin(ivect)
            zr(jrtrv + i-nmax+1) = zr(jvalpo + (ivect-1)*nbordr + i)
            zi(jitrv + i-nmax+1) = zi(jvalor + (ivect-1)*nbordr + i)
30      continue
        do 40 i = 1, nmax-1
            zr(jrtrv +npoin(ivect)+i-nmax+1) = zr(jvalpo + (ivect-1)*nbordr + i)
            zi(jitrv +npoin(ivect)+i-nmax+1) = zi(jvalor + (ivect-1)*nbordr + i)
40      continue
!
! ----- EXTRACTION DES PICS SUR LE VECTEUR REARANGE -----
!
! 1. LE PREMIER POINT EST UN PIC
!
        npic(ivect) = 1
        zr(jpic+ adrs + 1) = zr(jrtrv + 1)
        pinter = zr(jrtrv+2)
        zi(jordpi + adrs + 1) = zi(jitrv + 1)
        ointer = zi(jitrv+ 2)
!
! 2. RECHERCHE DE TOUS LES PICS
!
        do 50 i = 3, ntrv
            dp1 = pinter - zr(jpic + adrs + npic(ivect))
            dp2 = zr(jrtrv + i) - pinter
!
! 2.1 ON CONSERVE LE POINT INTERMEDIAIRE COMME UN PIC
!
            if (dp1*dp2 .lt. -epsilo) then
                npic(ivect) = npic(ivect) + 1
                zr(jpic + adrs + npic(ivect)) = pinter
                zi(jordpi + adrs + npic(ivect)) = ointer
            endif
!
! 2.2 LE DERNIER POINT DEVIENT POINT INTERMEDIAIRE
!
            pinter = zr(jrtrv + i)
            ointer = zi(jitrv + i)
50      continue
!
! 3. LE DERNIER POINT EST UN PIC
!
        npic(ivect) = npic(ivect) + 1
        zr(jpic + adrs + npic(ivect)) = zr(jrtrv + ntrv)
        zi(jordpi + adrs + npic(ivect)) = zi(jitrv + ntrv)
!
10  end do
!
    call jedema()
!
end subroutine
