subroutine rebdfr(freq, nfi, nff, freqi, freqf,&
                  nmodi, nmodf, nbm, npv)
    implicit none
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!     BUT : DEFINIR LA BANDE DE FREQUENCE POUR LE CALCUL DU SPECTRE
!     ---   D EXCITATION (ROUTINE APPELEE PAR OP0146).
!
!     - SI LA FREQUENCE INITIALE ET LA FREQUENCE FINALE NE SONT PAS
!     DEFINIES PAR L UTILISATEUR, ELLES SONT CALCULEES A PARTIR DES
!     FREQUENCES MODALES.
!     FREQI = FMIN/2
!     FREQF = FMAX/2
!     (FMIN ET FMAX SONT RESPECTIVEMENT LA PREMIERE ET LA DERNIERE FREQ.
!      MODALES PRISES EN COMPTE).
!     - EN FONCTION DE FREQI ET FREQF DEFINIES PAR L UTILISATEUR,
!     ON RECHERCHE LES MODES PRIS EN COMPTE.
!-----------------------------------------------------------------------
!     IN  : FREQ  : CARACTERIST. MODALES DE LA BASE DE CONCEPT MELASFLU
!     IN  : NFI   : SI NFI .EQ. 0 , FREQI EST CALCULEE
!     IN  : NFF   : SI NFF .EQ. 0 , FREQF EST CALCULEE
!     I/O : FREQI : FREQUENCE INITIALE DE LA BANDE DE FREQUENCE
!     I/O : FREQF : FREQUENCE FINALE   DE LA BANDE DE FREQUENCE
!     OUT : NMODI : NUMERO D ORDRE DU PREMIER MODE PRIS EN COMPTE
!     OUT : NMODF : NUMERO D ORDRE DU DERNIER MODE PRIS EN COMPTE
!     IN  : NBM   : NBR. DE MODES DE LA BASE MODALE
!     IN  : NPV   : NOMBRE DE VITESSES ETUDIEES
!-----------------------------------------------------------------------
!
    include 'jeveux.h'
    include 'asterc/r8maem.h'
    include 'asterfort/u2mess.h'
    integer :: nfi, nff, nmodi, nmodf
    real(kind=8) :: freqi, freqf, freq(2, nbm, npv)
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, ind, j, nbm, npv
    real(kind=8) :: frqmax, frqmin, frqmma
!-----------------------------------------------------------------------
    nmodi = 0
    nmodf = 0
    frqmin = r8maem()
    frqmma = 0.d0
    frqmax = 0.d0
!
! 1. --- DECOUPAGE DE LA BANDE DE FREQUENCE ---
!
! ---- RECHERCHE DE LA FREQUENCE INITIALE  ET
!                DE LA FREQUENCE FINALE
!
    if (nfi .eq. 0) then
        do 10 i = 1, npv
            if (freq(1,1,i) .gt. 0.d0) then
                frqmin = min(frqmin,freq(1,1,i))
                frqmma = max(frqmma,freq(1,1,i))
            endif
10      continue
        nmodi = 1
        freqi = frqmin/2.d0
    endif
!
    if (nff .eq. 0) then
        do 20 i = 1, npv
            if (freq(1,nbm,i) .gt. 0.d0) then
                frqmax = max(frqmax,freq(1,nbm,i))
            endif
20      continue
        nmodf = nbm
        freqf = frqmax + ( frqmma/2.d0)
    endif
!
! 2. ---- ON SECTIONNE LES MODES COMPRIS ENTRE FREQI ET FREQF
!
    if (nmodi .eq. 0) then
        do 30 i = 1, nbm
            ind = 1
            do 40 j = 1, npv
                if (freq(1,i,j) .le. freqi) then
                    ind = 0
                endif
40          continue
            if (ind .eq. 1) then
                nmodi = i
                goto 900
            endif
30      continue
    endif
900  continue
    if (nmodi .eq. 0) nmodi = 1
!
    if (nmodf .eq. 0) then
        do 50 i = nbm, 1, -1
            ind = 1
            do 60 j = 1, npv
                if (freq(1,i,j) .gt. freqf) then
                    ind = 0
                endif
60          continue
            if (ind .eq. 1) then
                nmodf = i
                goto 901
            endif
50      continue
    endif
901  continue
    if (nmodf .eq. 0) nmodi = nbm
!
    if (nmodf .lt. nmodi) then
        call u2mess('F', 'MODELISA6_95')
    endif
!
end subroutine
