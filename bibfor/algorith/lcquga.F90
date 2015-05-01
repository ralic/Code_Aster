subroutine lcquga(mode, eps, gameps, dgamde, itemax, precvg, iret)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
    integer,intent(in)      :: mode, itemax
    real(kind=8),intent(in) :: eps(6), precvg
    real(kind=8),intent(out):: gameps, dgamde(6)
    integer, intent(out)    :: iret
! --------------------------------------------------------------------------------------------------
!  CALCUL DE GAMMA(EPS) POUR LA LOI ENDO_SCALAIRE AVEC GRAD_VARI
! --------------------------------------------------------------------------------------------------
!  IN  MODE    FONCTION RECHERCHEE (0=VALEUR, 1=VAL ET DER)
!  IN  EPS     VALEUR DE L'ARGUMENT EPS(1:NDIMSI)
!  OUT GAMEPS  FONCTION GAMMA(EPS) - SI MODE=1 OU MODE=0
!  OUT DGAMDE  DERIVEE DE GAMMA    - SI MODE=1
!  IN  ITEMAX  INUTILISE
!  IN  PRECVG  INUTILISE
!  OUT IRET    SYSTEMATIQUEMENT A ZERO
! --------------------------------------------------------------------------------------------------
    real(kind=8),parameter,dimension(6):: kr=(/1.d0,1.d0,1.d0,0.d0,0.d0,0.d0/)
! --------------------------------------------------------------------------------------------------
    real(kind=8) :: epseq2, ueps, heps, coefh, coefs, treps, epsdv(6)
! --------------------------------------------------------------------------------------------------
    real(kind=8) :: pct, pch, pcs
    common /lcmqu/ pch,pct,pcs
! --------------------------------------------------------------------------------------------------
!
!  INITIALISATION
!
    iret = 0
!
!
!  CALCUL DE LA VALEUR
!
    treps = eps(1)+eps(2)+eps(3)
    epsdv = eps - treps/3*kr
    epseq2 = 1.5d0*dot_product(epsdv,epsdv)
    ueps = pch*treps**2+pcs*epseq2
    heps = pct*treps+sqrt(ueps)
    gameps = heps*heps
!
!
!  CALCUL DE LA DERIVEE
!
    if (mode .eq. 1) then
        if (ueps .ne. 0) then
            coefh = pct + pch*treps/sqrt(ueps)
            coefs = 1.5d0*pcs/sqrt(ueps)
            dgamde = 2*heps*(coefh*kr+coefs*epsdv)
        else
            dgamde = 0
        endif
    endif
!
!
end subroutine
