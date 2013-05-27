subroutine rectfr(nbmode, nbvect, omeshi, npivot, nblagr,&
                  valpro, nvpro, resufi, resufr, nfreq)
    implicit   none
    integer :: nbmode, nbvect, npivot, nblagr, nvpro, nfreq
    integer :: resufi(nfreq, *)
    real(kind=8) :: valpro(nvpro), resufr(nfreq, *)
!     ------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     ------------------------------------------------------------------
!     IN  : NBMODE  : NOMBRE DE MODE DEMANDES
!     IN  : NBVECT  : NOMBRE DE VECTEURS UTILISES AU COURS DU CALCUL
!     IN  : OMESHI  : DECALAGE UTILISE POUR LE CALCUL
!     IN  : NPIVOT  : NOMBRE DE PIVOTS NEGATIFS, POUR RECTIFIER LA
!                     POSITION DES MODES
!     IN  : NBLAGR  : NOMBRE DE PARAMETRES DE LAGRANGE
!     IN  : VALPRO  : VALEURS PROPRES
!     IN  : NVPRO   : DIMENSION DU VECTEUR VALPRO
!     OUT : RESUFI  : ON RANGE DANS LA STRUCTURE RESULTAT
!     OUT : RESUFR  : ON RANGE DANS LA STRUCTURE RESULTAT
!     IN  : NFREQ   : PREMIERE DIMENSION DU TABLEAU RESUFR
!     ------------------------------------------------------------------
    integer :: ineg, ip, im, in, ivec, ifreq
    real(kind=8) :: om, omeshi
!     ------------------------------------------------------------------
!
!     ------------------------------------------------------------------
!     --------  RECTIFICATION DES FREQUENCES DUE AU SHIFT  -------------
!     --------     DETERMINATION DE LA POSITION MODALE     -------------
!     ------------------------------------------------------------------
!
    ineg = 0
    ip = 0
    im = 1
    do 10 ivec = 1, nbvect
        om = valpro(ivec)
        if (om .gt. 0.0d0) then
            ip = ip + 1
            in = ip
        else
            im = im - 1
            in = im
        endif
!
        om = om + omeshi
        if (om .lt. 0.0d0) then
            ineg = ineg + 1
        endif
        if (ivec .le. nbmode) then
            resufi(ivec,1) = npivot+in
            resufr(ivec,2) = om
        endif
10  end do
    if (ineg .eq. nbvect) then
        do 20 ivec = 1, nbmode
            resufi(ivec,1) = npivot + ivec
20      continue
    endif
!
!     ------------------------------------------------------------------
!     -- RECTIFICATION DE LA POSITION MODALE (A CAUSE DES LAGRANGE) ----
!     ------------------------------------------------------------------
!
    do 30 ifreq = 1, nbmode
        resufi(ifreq,1) = resufi(ifreq,1) - nblagr
30  end do
!
end subroutine
