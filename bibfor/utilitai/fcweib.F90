subroutine fcweib(nrupt, cals, sk, sigw, nur,&
                  nt, nbres, indtp, nbtp, m,&
                  fc, dfc)
    implicit none
    include 'asterc/r8maem.h'
    include 'asterfort/u2mesg.h'
    integer :: nrupt, nur(*), nt(*), nbres, indtp(*), nbtp
    real(kind=8) :: sigw(*), m, fc, dfc, s1, s2, sk(*)
    logical :: cals
!     ----------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     ----------------------------------------------------------------
!     AUTEUR : M. BONNAMY
!     ----------------------------------------------------------------
!
!     BUT: CALCUL DE RECALAGE DES PARAMETRES DE WEIBULL PAR LA
!          METHODE DU MAXIMUM DE VRAISSEMBLANCE
!
!     ----------------------------------------------------------------
!
!     NRUPT        /IN/:NOMBRE DE CONTRAINTES
!     CALS         /IN/:TRUE SI SIGMA_U EST FIXE
!     SK           /IN/:PARAMETRE SIGMA-U(K) DE WEIBULL
!     SIGW         /IN/:CONTRAINTES DE WEIBULL AUX INSTANTS DE RUPTURE
!     NUR          /IN/:NUMERO DE RESULTAT ASSOCIEE A
!                       LA CONTRAINTE SIGW(I)
!     NT           /IN/:DIMENSION DE LA SOUS-BASE CORRESPONDANT A LA
!                       TEMPERATURE T
!     NBRES        /IN/:NOMBRE DE BASES DE RESULTATS
!     INDTP        /IN/:INDICE DE TEMPERATURE POUR CHAQUE RESULTAT
!     NBTP         /IN/:NOMBRE DE TEMPERATURE DIFFERENTES
!
!     M            /OUT/:PARAMETRE M(K+1)DE WEIBULL
!     FC           /OUT/:FONCTION F(M) DE WEIBULL
!     DFC          /OUT/:DERIVEE DE LA FONCTION DF(M) DE WEIBULL
!
!
!     ----------------------------------------------------------------
!
    real(kind=8) :: swm, slw, slwm, sl2wm, sl2bwm, snt, maxr, maxm
    real(kind=8) :: valr
    integer :: i, itp, ir
!
!     ----------------------------------------------------------------
!
    slw = 0.d0
    sl2bwm = 0.d0
    maxr = r8maem()
    maxm = log (maxr) / log ( nrupt*sigw(nrupt) )
    if (m .ge. maxm) then
        valr = maxm
        call u2mesg('S', 'UTILITAI8_22', 0, ' ', 0,&
                    0, 1, valr)
    endif
    if (m .le. 0.d0) then
        valr = m
        call u2mesg('S', 'UTILITAI8_23', 0, ' ', 0,&
                    0, 1, valr)
    endif
!
    do 10 i = 1, nrupt
!
        if (cals) then
            slw = slw + (log ( sigw(i)/sk(1) ) ) * ( 1.d0-(sigw(i)/ sk(1))**m)
            sl2bwm = sl2bwm + (&
                     ( sigw(i)/sk(1)) ** m ) * ( log (sigw( i)/sk(1)) * log (sigw(i)/sk(1) ))
        else
            slw = slw + log ( sigw(i) )
        endif
!
10  continue
!
    s1 = 0.d0
    s2 = 0.d0
    do 210 itp = 1, nbtp
!
        snt = 0.d0
!
        do 200 ir = 1, nbres
!
            if (indtp(ir) .eq. itp) snt = snt + nt(ir)
!
200      continue
!
        swm = 0.d0
        slwm = 0.d0
        sl2wm = 0.d0
        do 300 i = 1, nrupt
!
            if (indtp(nur(i)) .eq. itp) then
                swm = swm + sigw(i) ** m
                slwm = slwm + (sigw(i) ** m )* ( log ( sigw(i) ) )
                sl2wm = sl2wm + ( sigw(i) ** m )* ( log ( sigw(i) ) * log ( sigw(i) ))
            endif
!
300      continue
!
        s1 = s1 + snt * slwm/swm
        s2 = s2 + snt * ( (sl2wm/swm)*swm - (slwm/swm)*slwm ) /swm
!
210  continue
!
    if (cals) then
        fc = nrupt
        fc = fc / m + slw
        dfc = nrupt
        dfc = - dfc * ( 1.d0/(m*m) ) - sl2bwm
    else
        fc = nrupt
        fc = fc / m + slw - s1
        dfc = nrupt
        dfc = - dfc * ( 1.d0/(m*m) ) - s2
    endif
!
!
end subroutine
