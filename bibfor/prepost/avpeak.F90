subroutine avpeak(valaxe, nbvec, nbordr, pseuil, iflag,&
                  npoin, valpoi, valord)
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
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
    integer :: nbvec, nbordr, npoin(nbvec), valord(nbvec*nbordr)
    integer :: iflag(nbvec)
    real(kind=8) :: valaxe(nbvec*nbordr), pseuil, valpoi(nbvec*nbordr)
! ----------------------------------------------------------------------
! BUT: EXTRAIRE LES PICS D'UNE FONCTION.
! ----------------------------------------------------------------------
! ARGUMENTS:
! VALAXE    IN   R  : VECTEUR CONTENANT L'HISTORIQUE DES PROJECTIONS
!                     POUR TOUS LES VECTEURS NORMAUX (n) ET TOUS LES
!                     NUMEROS D'ORDRE.
! NBVEC     IN   I  : NOMBRE DE VECTEURS NORMAUX.
! NBORDR    IN   I  : NOMBRE DE NUMERO D'ORDRE.
! PSEUIL    IN   R  : SEUIL DE DTECTION DES PICS
! IFLAG     IN   I  : VECTEUR DE DRAPEAUX QUI INDIQUE :
!                      - IFLAG(i) = 0 --> CAS GENERAL ;
!                      - IFLAG(i) = 1 --> CAS OU LES POINTS DANS LE
!                                         PLAN DE CISAILLEMENT SONT
!                                         ALIGNES VERTICALEMENT ;
!                      - IFLAG(i) = 2 --> CAS OU LES POINTS DANS LE
!                                         PLAN DE CISAILLEMENT SONT
!                                         ALIGNES HORIZONTALEMENT ;
!                      - IFLAG(i) = 3 --> CAS OU LES POINTS DANS LE
!                                         PLAN DE CISAILLEMENT SONT
!                                         CONTENUS DANS UN CADRE DE
!                                         COTES INFERIEURS A EPSILO.
! NPOIN     OUT  I  : NOMBRE DE PICS DETECTES POUR TOUS LES VECTEURS
!                     NORMAUX.
! VALPOI    OUT  R  : VALEUR DES PICS DETECTES POUR TOUS LES VECTEURS
!                     NORMAUX.
! VALORD    OUT  I  : NUMEROS D'ORDRE ASSOCIES AUX PICS DETECTES POUR
!                     TOUS LES VECTEURS NORMAUX.
!
!-----------------------------------------------------------------------
!     ------------------------------------------------------------------
    integer :: ivect, iordr, pass, sortie, ordmax, ordmin
!
    real(kind=8) :: vmin, vmax, valeur, epsilo
!
!-----------------------------------------------------------------------
!234567                                                              012
!-----------------------------------------------------------------------
    epsilo = 1.0d-7
!-----------------------------------------------------------------------
!
    call jemarq()
!
    do 10 ivect = 1, nbvec
!
        if (iflag(ivect) .eq. 3) then
            goto 10
        endif
!
! ----- LE PREMIER POINT EST UN PIC -----
!
        npoin(ivect) = 1
        valpoi((ivect-1)*nbordr + 1) = valaxe((ivect-1)*nbordr + 1)
        valord((ivect-1)*nbordr + 1) = 1
        vmax = valpoi((ivect-1)*nbordr + 1)
        vmin = valpoi((ivect-1)*nbordr + 1)
        pass = 0
        sortie = 2
!
! ----- RECHERCHE DES PICS INTERMEDIAIRES -----
!
        do 20 iordr = 2, nbordr
            valeur = valaxe((ivect-1)*nbordr + iordr)
            if (vmax .lt. valeur) then
                vmax = valeur
                ordmax = iordr
            endif
            if (vmin .gt. valeur) then
                vmin = valeur
                ordmin = iordr
            endif
            if (pass .eq. 0) then
                if ((valeur-vmin) .gt. pseuil) then
                    sortie = 1
                    pass = 1
                endif
                if ((vmax-valeur) .gt. pseuil) then
                    sortie = 0
                    pass = 1
                endif
            endif
            if ((sortie .eq. 1) .and. ((vmax-valeur)-pseuil .gt. epsilo)) then
                npoin(ivect) = npoin(ivect) + 1
                valpoi((ivect-1)*nbordr + npoin(ivect)) = vmax
                valord((ivect-1)*nbordr + npoin(ivect)) = ordmax
                vmin = valeur
                ordmin = iordr
                sortie = 0
            endif
            if ((sortie .eq. 0) .and. ((valeur-vmin)-pseuil .gt. epsilo)) then
                npoin(ivect) = npoin(ivect) + 1
                valpoi((ivect-1)*nbordr + npoin(ivect)) = vmin
                valord((ivect-1)*nbordr + npoin(ivect)) = ordmin
                vmax = valeur
                ordmax = iordr
                sortie = 1
            endif
20      continue
!
        if (sortie .eq. 0) then
            npoin(ivect) = npoin(ivect) + 1
            valpoi((ivect-1)*nbordr + npoin(ivect)) = vmin
            valord((ivect-1)*nbordr + npoin(ivect)) = ordmin
        endif
        if (sortie .eq. 1) then
            npoin(ivect) = npoin(ivect) + 1
            valpoi((ivect-1)*nbordr + npoin(ivect)) = vmax
            valord((ivect-1)*nbordr + npoin(ivect)) = ordmax
        endif
10  end do
!
    call jedema()
end subroutine
