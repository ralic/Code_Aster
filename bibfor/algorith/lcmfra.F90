subroutine lcmfra(vp, itemax, precvg, chi, iret)
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
    implicit none
    integer,intent(in)      :: itemax
    real(kind=8),intent(in) :: vp(3), precvg
    integer,intent(out)     :: iret
    real(kind=8),intent(out):: chi
! --------------------------------------------------------------------------------------------------
!  CRITERE ENDO_FISS_EXP: CALCUL DU RAYON SEUIL
! --------------------------------------------------------------------------------------------------
! IN  VP      CONTRAINTES PRINCIPALES RANGEES DANS L'ORDRE DECROISSANT
! IN  ITEMAX  NOMBRE MAX D'ITERATIONS POUR LA RESOLUTION SCALAIRE
! IN  PRECVG  PRECISION DE LA RESOLUTION SCALAIRE : CHI*DCHI < PRECVG
! OUT CHI     SCALING TEL QUE VP/CHI SOIT SUR LA FRONTIERE DU CRITERE
! OUT ITET    CODE RETOUR: 0=OK, 1=NON CONVERGENCE
! --------------------------------------------------------------------------------------------------
    integer :: iter
    real(kind=8) :: nors, n(3), tra, nora, ymax, y, e(3), nore, f, df, gamma, prectr
    real(kind=8) :: precy3, yt, ft
! --------------------------------------------------------------------------------------------------
    real(kind=8) :: tau, sig0, beta
    common /lcmmf/ tau,sig0,beta
! --------------------------------------------------------------------------------------------------
!
!  INITIALISATION
    iret = 0
    prectr = 1.d-3*tau
    gamma = 3*beta**2-1.d0/3.d0
!
!
!  NORMALISATION
    nors = sqrt((vp(1)**2+vp(2)**2+vp(3)**2))
    if (nors .eq. 0.d0) then
        chi = 0
        goto 999
    endif
!
    n(1) = vp(1)/nors
    n(2) = vp(2)/nors
    n(3) = vp(3)/nors
!
!
!  COEFFICIENT LINEAIRE
    tra = n(1)+n(2)+n(3)
    nora = sqrt(1 + gamma*tra**2)
!
!
!  BORNE MAX
    ymax = tau/nora
    y = ymax
    if (n(1) .gt. 0) y = min(y,log(tau)/n(1))
!
!
!  RESOLUTION PAR UNE METHODE DE NEWTON

!  PRECISION REQUISE
    precy3 = (sig0/nors)**2 * precvg
    do iter = 1, itemax
        e = exp(2*y*n)
        nore = sqrt(sum(e))
!
        f = nora*y + nore - tau
        df = nora + dot_product(n,e) / nore
!
        if (abs(f) .le. prectr) then
            yt = y - sign(1.d0,f)*precy3*y**3
            if (yt<=0 .or. yt>=ymax) goto 20
            ft = nora*yt + sqrt(sum(exp(2*yt*n))) - tau
            if (ft*f .le. 0) goto 20
        end if

        y = y - f/df
    end do
!
!  ECHEC DANS LA RESOLUTION
    iret = 1
    goto 999
!
!  SUCCES DE LA RESOLUTION
20  continue
    chi = nors/(y*sig0)
!
!
999 continue
end subroutine
