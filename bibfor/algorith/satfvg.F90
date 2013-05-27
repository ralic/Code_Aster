subroutine satfvg(sr, pr, n, m, pc,&
                  s, dsdpc)
! ======================================================================
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ======================================================================
!
! SATFVG : CALCUL DE LA SAT PAR FORMULE DE VAN-GENUCHTEN
    implicit none
! IN
    real(kind=8) :: sr, pr, n, m, pc
! OUT
    real(kind=8) :: s, dsdpc
    s=sr+(1-sr)*((pc/pr)**n+1.d0)**(-m)
    dsdpc=-n*m*((1.d0-sr)/pr)*(((pc/pr)**n+1.d0)**(-m-1.d0))*&
     &   ((pc/pr)**(n-1.d0))
end subroutine
