subroutine epsett(applic, nbrddl, depl, btild, sgmtd,&
                  epsi, wgt, effint)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
!
    integer :: nbrddl, i, k
    character(len=6) :: applic
    real(kind=8) :: btild(4, *), depl(*), epsi(*), sgmtd(*), effinb
    real(kind=8) :: wgt, effint(*)
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    if (applic .eq. 'DEFORM') then
!
!     CALCULS DES COMPOSANTES DE DEFORMATIONS TRIDIMENSIONNELLES :
!     EPSXX, EPSYY, EPSXY, EPSXZ (CE SONT LES COMPOSANTES TILDE)
!
        do 10 i = 1, 4
            epsi(i)=0.d0
            do 20 k = 1, nbrddl
                epsi(i)=epsi(i)+btild(i,k)*depl(k)
20          end do
10      end do
!
    else if (applic .eq. 'EFFORI') then
!
!     CALCULS DES EFFORTS INTERIEURS
!
        do 30 i = 1, nbrddl
            effinb=0.d0
            do 40 k = 1, 4
                effinb=effinb+btild(k,i)*sgmtd(k)
40          continue
            effint(i)=effint(i)+wgt*effinb
30      end do
!
    endif
end subroutine
