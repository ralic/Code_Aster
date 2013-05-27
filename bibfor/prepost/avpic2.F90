subroutine avpic2(method, nbvec, nbordr, rtrv, itrv,&
                  npoin, valpoi, valord, npic, pic,&
                  ordpic)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! person_in_charge: jean.angles at edf.fr
    implicit      none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/u2mesk.h'
    integer :: nbvec, nbordr, npoin(nbvec), valord(nbvec*nbordr)
    integer :: npic(nbvec), ordpic(nbvec*(nbordr+2))
    integer :: itrv(2*(nbordr+2))
    real(kind=8) :: rtrv(nbordr+2), valpoi(nbvec*nbordr)
    real(kind=8) :: pic(nbvec*(nbordr+2))
    character(len=8) :: method
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
        call assert(nbordr .ge. npoin(ivect))
        adrs = (ivect-1)*(nbordr+2)
!
        pmax = abs(valpoi((ivect-1)*nbordr + 1))
        nmax = 1
        do 20 i = 2, npoin(ivect)
            if (abs(valpoi((ivect-1)*nbordr + i)) .gt. pmax*(1.0d0+ epsilo)) then
                pmax = abs(valpoi((ivect-1)*nbordr + i))
                nmax = i
            endif
20      continue
        pmax = valpoi((ivect-1)*nbordr + nmax)
        ntrv = npoin(ivect)
!
! ----- REARANGEMENT AVEC POINT LE PLUS GRAND AU DEBUT
!       ET A LA FIN                                    -----
!
        do 30 i = nmax, npoin(ivect)
            rtrv(i-nmax+1) = valpoi((ivect-1)*nbordr + i)
            itrv(i-nmax+1) = valord((ivect-1)*nbordr + i)
30      continue
        do 40 i = 1, nmax-1
            rtrv(npoin(ivect)+i-nmax+1) = valpoi((ivect-1)*nbordr + i)
            itrv(npoin(ivect)+i-nmax+1) = valord((ivect-1)*nbordr + i)
40      continue
!
! ----- EXTRACTION DES PICS SUR LE VECTEUR REARANGE -----
!
! 1. LE PREMIER POINT EST UN PIC
!
        npic(ivect) = 1
        pic(adrs + 1) = rtrv(1)
        pinter = rtrv(2)
        ordpic(adrs + 1) = itrv(1)
        ointer = itrv(2)
!
! 2. RECHERCHE DE TOUS LES PICS
!
        do 50 i = 3, ntrv
            dp1 = pinter - pic(adrs + npic(ivect))
            dp2 = rtrv(i) - pinter
!
! 2.1 ON CONSERVE LE POINT INTERMEDIAIRE COMME UN PIC
!
            if (dp1*dp2 .lt. -epsilo) then
                npic(ivect) = npic(ivect) + 1
                pic(adrs + npic(ivect)) = pinter
                ordpic(adrs + npic(ivect)) = ointer
            endif
!
! 2.2 LE DERNIER POINT DEVIENT POINT INTERMEDIAIRE
!
            pinter = rtrv(i)
            ointer = itrv(i)
50      continue
!
! 3. LE DERNIER POINT EST UN PIC
!
        npic(ivect) = npic(ivect) + 1
        pic(adrs + npic(ivect)) = rtrv(ntrv)
        ordpic(adrs + npic(ivect)) = itrv(ntrv)
!
10  end do
!
    call jedema()
!
end subroutine
