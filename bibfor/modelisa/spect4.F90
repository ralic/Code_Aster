function spect4(xx, y, xlc, vitn, rhoe,&
                defm, nbp, im, jm)
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
!  CALCUL DE
!   F(X,Y) = RHOE(X)*RHOE(Y)*DEFM(X,I)*DEFM(Y,J)*U(X)*U(X)*
!            U(Y)*U(Y)*EXP(-ABS(Y-X)/XLC)
!  (APPEL: ROUTINE PRINCIPALE OP0146)
!-----------------------------------------------------------------------
! IN  : XX,Y  : VARIABLES DE LA FONCTION F(XX,Y)
! IN  : XLC   : LONGUEUR DE CORRELATION
! IN  : VITN  : VITESSE NORMALISEE,(VECTEUR DE DIM=2*NBP)
! IN  : RHOE  : MASSE VOL. DU FLUIDE EXTERIEUR, (VECTEUR DE DIM=2*NBP)
! IN  : DEFM  : DEFORMEES MODALES (CONCEPT MELASFLU)
! IN  : NBP   : NOMBRE DE POINTS DE LA DISCR. SPATIALE
! IN  : IM,IM : NUMEROS D ORDRE DES MODES DU CONCEPT MELASFLU
!-----------------------------------------------------------------------
!
    include 'jeveux.h'
    real(kind=8) :: defm(nbp, *), vitn(nbp, *), rhoe(nbp, *), xlc, xx, y
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, im, jm, nbp
    real(kind=8) :: delta, phix, phiy, rox, roy, spect4, ux
    real(kind=8) :: uy
!-----------------------------------------------------------------------
    if (xx .le. rhoe(2,1)) then
        i = 2
    else if (xx .gt. rhoe(nbp-1,1)) then
        i = nbp
    else
        i =3
10      continue
        if (xx .gt. rhoe(i,1)) then
            i = i+1
            goto 10
        endif
    endif
!
    delta = (xx-rhoe(i-1,1)) / (rhoe(i,1)-rhoe(i-1,1))
!
    phix = defm(i-1,im) + delta*(defm(i,im)-defm(i-1,im))
    ux = vitn(i-1,2) + delta*(vitn(i,2)-vitn(i-1,2))
    rox = rhoe(i-1,2) + delta*(rhoe(i,2)-rhoe(i-1,2))
!
    if (y .le. rhoe(2,1)) then
        i = 2
    else if (y .gt. rhoe(nbp-1,1)) then
        i = nbp
    else
        i =3
20      continue
        if (y .gt. rhoe(i,1)) then
            i = i+1
            goto 20
        endif
    endif
!
    delta = (y-rhoe(i-1,1)) / (rhoe(i,1)-rhoe(i-1,1))
!
    phiy = defm(i-1,jm) + delta*(defm(i,jm)-defm(i-1,jm))
    uy = vitn(i-1,2) + delta*(vitn(i,2)-vitn(i-1,2))
    roy = rhoe(i-1,2) + delta*(rhoe(i,2)-rhoe(i-1,2))
!
    spect4 = exp(-abs(xx-y)/xlc) * phix * phiy* rox * roy * ux*ux * uy*uy
!
end function
