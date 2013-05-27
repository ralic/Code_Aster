subroutine nmmess(code, dp0, dp1, dp, f,&
                  nit, nitmax, iret)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! TOLE CRP_7
!
    implicit none
    include 'jeveux.h'
    include 'asterfort/tecael.h'
    include 'asterfort/u2mesg.h'
    interface
    function f(x)
        real(kind=8) :: f, x
    end function f
    end interface
    integer :: nit, nitmax, iret
    real(kind=8) :: dp0, dp1, dp
    character(len=1) :: code
!
! ......................................................................
!    - FONCTION REALISEE: MESSAGE D'ERREUR DETAILLE EN CAS DE NON
!                         CONVERGENCE DANS LES ROUTINES DE RECHERCHE
!                         DE ZERO DE F(DP)
!
!    - ARGUMENTS:
!        DONNEES:     CODE    : 'A', 'E' OU 'F' PASSE A U2MESG
!                     DP0     : DP INITIAL (0 EN GENERAL)
!                     DP1     : DP MAXI ESTIME
!                     DP      : DERNIER DP CALCULE
!                     F       : NOM DE LA FONCTION
!                     NIT     : NOMBRE D'ITERATIONS ATTEINT
!                     NITMAX  : NOMBRE D'ITERATIONS MAXIMUM
!                     IRET    : CODE RETOUR DE L'ALGO DE RECHERCHE
!                               IRET = 0 : OK
!                               IRET = 1 : ON NE TROUVE PAS DPMAX
!                               IRET = 2 : NITER INSUFFISANT
!                               IRET = 3 : F(XMIN) > 0
! ......................................................................
!
    character(len=8) :: nomail
    character(len=24) :: valk
!
    integer :: iadzi, iazk24, nbp, i
    integer :: vali(2)
    real(kind=8) :: dpi, f0, f1, fp, fi
    real(kind=8) :: valr(2)
!
    if (iret .eq. 0) goto 9999
!
    call tecael(iadzi, iazk24)
    nomail= zk24(iazk24-1+3)(1:8)
!
!
    if (iret .eq. 1) then
        call u2mesg(code//'+', 'ALGORITH15_45', 0, ' ', 0,&
                    0, 0, 0.d0)
    else if (iret.eq.2) then
        call u2mesg(code//'+', 'ALGORITH15_46', 0, ' ', 0,&
                    0, 0, 0.d0)
    else if (iret.eq.3) then
        call u2mesg(code//'+', 'ALGORITH15_47', 0, ' ', 0,&
                    0, 0, 0.d0)
    endif
!
    valk = nomail
    vali (1) = nit
    vali (2) = nitmax
    call u2mesg(code//'+', 'ALGORITH15_48', 1, valk, 2,&
                vali, 0, 0.d0)
    fp = f(dp)
    valr (1) = dp
    valr (2) = fp
    call u2mesg(code//'+', 'ALGORITH15_49', 0, ' ', 0,&
                0, 2, valr)
    f0 = f(dp0)
    valr (1) = dp0
    valr (2) = f0
    call u2mesg(code//'+', 'ALGORITH15_50', 0, ' ', 0,&
                0, 2, valr)
    f1 = f(dp1)
    valr (1) = dp1
    valr (2) = f1
    call u2mesg(code//'+', 'ALGORITH15_51', 0, ' ', 0,&
                0, 2, valr)
    nbp=100
!
    vali (1) = nbp
    call u2mesg(code//'+', 'ALGORITH15_52', 0, ' ', 1,&
                vali, 0, 0.d0)
    do 10 i = 1, nbp
        dpi=dp0+i*(dp1-dp0)/nbp
        fi = f(dpi)
        valr (1) = dpi
        valr (2) = fi
        call u2mesg(code//'+', 'ALGORITH15_53', 0, ' ', 0,&
                    0, 2, valr)
10  end do
!
    call u2mesg(code, 'VIDE_1', 0, ' ', 0,&
                0, 0, 0.d0)
9999  continue
end subroutine
