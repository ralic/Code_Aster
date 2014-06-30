subroutine defgen(testl1, testl2, nno, r, x3,&
                  sina, cosa, cour, vf, dfds,&
                  depl, eps, epsx3)
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
    implicit none
!
    integer :: nno
    logical(kind=1) :: testl1, testl2
    real(kind=8) :: r, x3, sina, cosa, cour, vf(*), dfds(*), depl(*), eps(*)
    real(kind=8) :: epsx3
    real(kind=8) :: uxl(3), uyl(3), betasl(3)
    real(kind=8) :: ess, kss, ett, ktt, gs
!
!     CALCUL DES DEFORMATIONS GENERALISEES : ESS , ETT , KSS , KTT , GS
!
!     PARTITION DU DEPL EN UX, UY ET BETAS
!
!     DO 10 INO=1,NNO
!-----------------------------------------------------------------------
    integer :: i
    real(kind=8) :: betas, dbtds, duxds, duyds, rhos, rhot, ux
    real(kind=8) :: uy
!-----------------------------------------------------------------------
    uxl(1)=depl(1)
    uxl(2)=depl(4)
    uxl(3)=depl(7)
!
    uyl(1)=depl(2)
    uyl(2)=depl(5)
    uyl(3)=depl(8)
!
    betasl(1)=depl(3)
    betasl(2)=depl(6)
    betasl(3)=depl(9)
!10   CONTINUE
!
    ux   =0.d0
    uy   =0.d0
    betas=0.d0
!
    duxds=0.d0
    duyds=0.d0
    dbtds=0.d0
    do 20 i = 1, nno
        ux =ux+vf(i)*uxl(i)
        uy =uy+vf(i)*uyl(i)
        betas=betas+vf(i)*betasl(i)
!
        duxds=duxds+dfds(i)*uxl(i)
        duyds=duyds+dfds(i)*uyl(i)
        dbtds=dbtds+dfds(i)*betasl(i)
20  end do
!
!     ESS  ,  KSS  ,  ETT  ,  KTT  ,  GS
!
    ess = duyds*cosa-duxds*sina
    kss = dbtds
    ett = ux/r
    ktt = -sina/r*betas
    gs = betas+duxds*cosa+duyds*sina
!
    if (testl1) then
        rhos=1.d0
    else
        rhos=1.d0 + x3 * cour
    endif
    if (testl2) then
        rhot=1.d0
    else
        rhot=1.d0 + x3 * cosa / r
    endif
!
    eps(1) = (ess + x3*kss) / rhos
    eps(2) = (ett + x3*ktt) / rhot
    eps(3) = 0.d0
    eps(4) = 0.d0
!
    epsx3 = 0.5d0 / rhos * gs
!
end subroutine
