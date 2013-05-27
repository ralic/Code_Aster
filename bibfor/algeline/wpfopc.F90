subroutine wpfopc(lmasse, lamor, lraide, fmin, sigma,&
                  matopa, raide, lqz, solveu)
    implicit none
    include 'jeveux.h'
!
    include 'asterc/getvr8.h'
    include 'asterc/r8depi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/mtcmbl.h'
    include 'asterfort/mtdefs.h'
    include 'asterfort/mtdscr.h'
    include 'asterfort/preres.h'
    include 'asterfort/u2mesr.h'
    include 'asterfort/u2mess.h'
    character(len=*) :: matopa, raide
    character(len=19) :: solveu
    integer :: lmasse, lamor, lraide
    real(kind=8) :: fmin
    complex(kind=8) :: sigma
    logical :: lqz
!
!     -----------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     DETERMINATION D'UN SHIFT ET CALCUL DE LA MATRICE SHIFTEE
!     DANS LE CAS QUADRATIQUE COMPLEXE
!     ------------------------------------------------------------------
! OUT LDYNAM  : IS : POINTEUR SUR LA FACTORISEE DE LA MATRICE DYNAMIQUE
!                    INDUITE PAR L'OPTION
! OUT SIGMA   : C16: SHIFT
! IN  LQZ     : METHODE QZ OU NON
! IN  SOLVEU : K19 : SD SOLVEUR POUR PARAMETRER LE SOLVEUR LINEAIRE
!     ------------------------------------------------------------------
!
!
    integer :: lmat(3), lmatra, ibid
    real(kind=8) :: ashift, constc(6), valr(2)
    character(len=1) :: typcst(3), base
    character(len=8) :: namddl
    character(len=19) :: matpre, matass
    character(len=24) :: nmat(3), nmatra
    integer :: iarg
!
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    real(kind=8) :: fshift
!-----------------------------------------------------------------------
    data namddl/'        '/
!     ------------------------------------------------------------------
!
    call jemarq()
    lmat(1) = lmasse
    nmat(1) = zk24(zi(lmat(1)+1))
    lmat(2) = lamor
    nmat(2) = zk24(zi(lmat(2)+1))
    lmat(3) = lraide
    nmat(3) = zk24(zi(lmat(3)+1))
!
    fshift = r8depi()*fmin
    ashift = 0.d0
!
    call getvr8('CALC_FREQ', 'AMOR_REDUIT', 1, iarg, 1,&
                ashift, ibid)
!
    if (abs(ashift) .ge. 1.d0) then
        ashift = 0.95d0
        valr (1) = 1.d0
        valr (2) = 0.95d0
        call u2mess('I+', 'ALGELINE4_94')
        call u2mesr('I', 'ALGELINE4_96', 2, valr)
    endif
!
    ashift = - (ashift*fshift)/sqrt(1.d0-ashift*ashift)
    sigma = dcmplx(ashift,fshift)
!
! --- POUR QZ CALCUL DE LA MATRICE SHIFTEE ET DE SA FACTORISEE INUTILE
    if (lqz) goto 999
!
!     --- DECALAGE COMPLEXE ---
    call mtdefs(matopa, raide, 'V', 'C')
    call mtdscr(matopa)
    nmatra=matopa(1:19)//'.&INT'
    call jeveuo(matopa(1:19)//'.&INT', 'E', lmatra)
    constc(1) = dble(sigma*sigma)
    constc(2) = dimag(sigma*sigma)
    constc(3) = dble(sigma)
    constc(4) = dimag(sigma)
    constc(5) = 1.d0
    constc(6) = 0.d0
    typcst(1) = 'C'
    typcst(2) = 'C'
    typcst(3) = 'C'
    call mtcmbl(3, typcst, constc, nmat, nmatra,&
                namddl, ' ', 'ELIM=')
!     --- FACTORISATION DES MATRICES ---
!
    base='V'
    matpre=' '
    matass=zk24(zi(lmatra+1))
    call preres(solveu, base, ibid, matpre, matass,&
                ibid, 1)
!
999  continue
    call jedema()
end subroutine
