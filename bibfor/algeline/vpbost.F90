subroutine vpbost(typres, nbmode, nbvect, omeshi, valpro,&
                  nvpro, vpinf, vpmax, precdc, method,&
                  omecor)
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     RECTIFIE LES VALEURS PROPRES
!-----------------------------------------------------------------------
!     IN  : TYPRES  : TYPE DE RESULTAT (DYNAMIQUE OU FLAMBEMENT)
!     IN  : NBMODE  : NOMBRE DE MODE DEMANDES
!     IN  : NBVECT  : NOMBRE DE VECTEURS UTILISES AU COURS DU CALCUL
!     IN  : OMESHI  : DECALAGE UTILISE POUR LE CALCUL
!     IN  : OMECOR  : OMEGA2 DE CORPS RIGIDE
!     IN  : VALPRO  : VALEURS PROPRES
!     IN  : NVPRO   : DIMENSION DU VECTEUR VALPRO
!     IN  : METHOD  : TYPE DE METHODE
!     IN  : PRECDC  : POURCENTAGE D'AUGMENTATION DES BORNES
!     OUT : VPINF : PLUS PETIT OMEGA2 CALCULE ET RETENU
!     OUT : VPMAX : PLUS GRAND OMEGA2 CALCULE ET RETENU
!----------------------------------------------------------------------
!
    implicit none
!
#include "asterc/r8prem.h"
#include "asterfort/freqom.h"
#include "asterfort/infniv.h"
#include "asterfort/u2mesr.h"
#include "asterfort/u2mess.h"
    integer :: nbmode, nbvect, nvpro
    real(kind=8) :: valpro(nvpro), precdc, omeshi, omecor, vpinf, vpmax
    character(len=8) :: method
    character(len=16) :: typres
!     ------------------------------------------------------------------
    real(kind=8) :: vpinf2, vpmax2, tole
    real(kind=8) :: valr(2)
    logical :: loginf, logmax
    integer :: niv, ifm, i
!     ------------------------------------------------------------------
!
!     ------------------------------------------------------------------
!     --------  RECTIFICATION DES FREQUENCES DUE AU SHIFT  -------------
!     --------     DETERMINATION DE LA POSITION MODALE     -------------
!     ------------------------------------------------------------------
!
!     ---RECUPERATION DU NIVEAU D'IMPRESSION----
    call infniv(ifm, niv)
!     -------------------------------------------
!
    do 5 i = 1, nbvect
        valpro(i) = valpro(i) + omeshi
 5  end do
!
    vpinf = valpro(1)
    vpmax = valpro(1)
    do 10 i = 2, nbmode
        if (valpro(i) .lt. vpinf) then
            vpinf = valpro(i)
        endif
        if (valpro(i) .gt. vpmax) then
            vpmax = valpro(i)
        endif
10  end do
    if (niv .ge. 1) then
        write(ifm,1600)
        if (typres .eq. 'DYNAMIQUE') then
            valr(1) = freqom(vpinf)
            valr(2) = freqom(vpmax)
            call u2mesr('I', 'ALGELINE6_16', 2, valr)
        else
            valr(1) = vpinf
            valr(2) = vpmax
            call u2mesr('I', 'ALGELINE6_17', 2, valr)
        endif
    endif
!
    if (method .eq. 'SORENSEN') then
        if (abs(vpmax) .le. omecor) then
            vpmax=omecor
        endif
        if (abs(vpinf) .le. omecor) then
            vpinf=-omecor
        endif
        vpinf = vpinf * (1.d0 - sign(precdc,vpinf))
        vpmax = vpmax * (1.d0 + sign(precdc,vpmax))
    endif
!
!     -----POUR LES OPTIONS JACOBI ET LANCZOS---
!
    loginf = .false.
    logmax = .false.
    if (method .ne. 'SORENSEN') then
        do 20 i = nbmode+1, nbvect
            if (valpro(i) .le. vpinf) then
                if (.not.loginf) then
                    loginf = .true.
                    vpinf2 = valpro(i)
                endif
            endif
            if (valpro(i) .ge. vpmax) then
                if (.not.logmax) then
                    logmax = .true.
                    vpmax2 = valpro(i)
                endif
            endif
20      end do
!
!     ----ON REGARDE L'ECART QU'IL Y A ENTRE FREQMIN ET LA
!         FREQUENCE PRECEDENTE, PUIS ON RECALCULE FREQMIN-----
!
        if (loginf) then
            if (vpinf2 .lt. vpinf) then
                if (vpmax .gt. r8prem()) then
                    tole=(abs(vpinf2-vpinf)/vpinf)
                    if (tole .lt. precdc) then
                        call u2mess('A', 'ALGELINE3_58')
                        valr(1) = freqom(vpinf2)
                        call u2mesr('A', 'ALGELINE4_66', 1, valr)
                        vpinf = vpinf * (1.d0 - sign(precdc,vpinf))
                    endif
                else
                    tole=abs(vpinf2-vpinf)
                    if (tole .lt. precdc) then
                        call u2mess('A', 'ALGELINE3_58')
                        valr(1) = freqom(vpinf2)
                        call u2mesr('A', 'ALGELINE4_66', 1, valr)
                        vpinf = vpinf * (1.d0 - sign(precdc,vpinf))
                    endif
                endif
                vpinf = 0.5d0 * (vpinf+vpinf2)
            else
                vpinf = vpinf * (1.d0 - sign(precdc,vpinf))
            endif
        else
            vpinf = vpinf * (1.d0 - sign(precdc,vpinf))
        endif
!
!     -----ON FAIT LES MEMES CALCULS AVEC FREQMAX------
!
        if (logmax) then
            if (vpmax2 .gt. vpmax) then
                if (vpinf .gt. r8prem()) then
                    tole=(abs(vpmax2-vpmax)/vpmax)
                    if (tole .lt. precdc) then
                        call u2mess('A', 'ALGELINE3_58')
                        valr(1) = freqom(vpmax2)
                        call u2mesr('A', 'ALGELINE4_66', 1, valr)
                        vpmax = vpmax * (1.d0 + sign(precdc,vpmax))
                    endif
                else
                    tole=abs(vpmax2-vpmax)
                    if (tole .lt. precdc) then
                        call u2mess('A', 'ALGELINE3_58')
                        valr(1) = freqom(vpmax2)
                        call u2mesr('A', 'ALGELINE4_66', 1, valr)
                        vpmax = vpmax * (1.d0 + sign(precdc,vpmax))
                    endif
                endif
                vpmax = 0.5d0 * (vpmax+vpmax2)
            else
                vpmax = vpmax * (1.d0 + sign(precdc,vpmax))
            endif
        else
            vpmax = vpmax * (1.d0 + sign(precdc,vpmax))
        endif
    endif
!
!     -----DETERMINATION DE FREQMIN ET FREQMAX-----
!
    if (abs(vpmax) .le. omecor) then
        vpmax=omecor
    endif
    if (abs(vpinf) .le. omecor) then
        vpinf=-omecor
    endif
!
!      -----IMPRESSIONS-----
!
    if (loginf) then
        if (niv .ge. 1) then
            if (typres .eq. 'DYNAMIQUE') then
                call u2mesr('I', 'ALGELINE6_18', 1, freqom(vpinf2))
            else
                call u2mesr('I', 'ALGELINE6_20', 1, vpinf2)
            endif
        endif
!
    endif
    if (logmax) then
        if (niv .ge. 1) then
            if (typres .eq. 'DYNAMIQUE') then
                call u2mesr('I', 'ALGELINE6_19', 1, freqom(vpmax2))
            else
                call u2mesr('I', 'ALGELINE6_21', 1, vpmax2)
            endif
        endif
    endif
!
    if (niv .ge. 1) then
        write(ifm,1600)
    endif
!
    1600 format (72('-'))
!
end subroutine
