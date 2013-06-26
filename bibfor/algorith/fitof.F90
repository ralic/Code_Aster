function fitof(phi, f1, f2, amor, horig)
    implicit none
!     ------------------------------------------------------------------
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
! CETTE FONCTION RENVOIT LA FREQUENCE F TELLE QUE
!     PHASE(H-HORIG)=PHI OU H=1/(1-F**2+2I*F*AMOR)
!
! IN  :PHI      R8  :PHASE
! IN  :F1,F2    R8  :BORNES DU DOMAINE DANS LEQUEL ON RECHERCHE
! IN  :AMOR     R8  :AMORTISSEMENT REDUIT
! IN  :HORIG    C16 :DECALAGE DE L'ORIGINE POUR LES ANGLES
!     ------------------------------------------------------------------
    include 'asterfort/phase.h'
    include 'asterfort/transf.h'
    real(kind=8) :: phi, f1, f2, f3, eps, phi1, phi2, phi3, df, fitof, amor, fx1
    real(kind=8) :: fx2
    complex(kind=8) :: h1, h2, h3, horig
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    eps=0.000001d0
    fx1=f1
    fx2=f2
    call transf(fx1, amor, h1)
    call transf(fx2, amor, h2)
    phi1=phase((h1-horig)/dcmplx(0.d0,1.d0))
    phi2=phase((h2-horig)/dcmplx(0.d0,1.d0))
    if (((phi1-phi)*(phi2-phi)) .gt. 0.d0) then
    endif
101  continue
    f3=(fx1+fx2)/2.d0
    call transf(f3, amor, h3)
    phi3=phase((h3-horig)/dcmplx(0.d0,1.d0))
    df=abs(f3-fx1)
    if (df .lt. eps) then
        fitof=f3
        goto 9999
    endif
    if (phi3 .ge. phi) then
        fx1=f3
    else
        fx2=f3
    endif
    goto 101
9999  continue
end function
