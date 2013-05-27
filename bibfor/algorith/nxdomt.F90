subroutine nxdomt(parmei, parmer)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
    include 'asterc/getfac.h'
    include 'asterc/getvis.h'
    include 'asterc/getvr8.h'
    include 'asterfort/assert.h'
    include 'asterfort/u2mess.h'
    integer :: parmei(2)
    real(kind=8) :: parmer(2)
! ----------------------------------------------------------------------
!     SAISIE DES DONNEES DE LA METHODE DE RESOLUTION
!
! OUT PARMEI  : PARAMETRES ENTIERS DE LA METHODE
!               PARMEI(1) = REAC_ITER
!               PARMEI(2) = ITER_LINE_MAXI
! OUT PARMER  : PARAMETRES REELS DE LA METHODE
!               PARMER(1) = PARM_THETA
!               PARMER(2) = RESI_LINE_RELA
!
! ----------------------------------------------------------------------
    integer :: iocc, n1
    real(kind=8) :: theta
    integer :: iarg
! DEB ------------------------------------------------------------------
!
    call getvr8(' ', 'PARM_THETA', 0, iarg, 1,&
                theta, n1)
    if ((theta.lt.0.0d0) .or. (theta.gt.1.0d0)) then
        call u2mess('F', 'ALGORITH9_4')
    endif
    parmer(1) = theta
!
    call getfac('NEWTON', iocc)
    if (iocc .eq. 1) then
        call getvis('NEWTON', 'REAC_ITER', 1, iarg, 1,&
                    parmei(1), n1)
        call assert(parmei(1).ge.0)
!
        call getvr8('NEWTON', 'RESI_LINE_RELA', 1, iarg, 1,&
                    parmer(2), n1)
        call getvis('NEWTON', 'ITER_LINE_MAXI', 1, iarg, 1,&
                    parmei(2), n1)
    endif
! FIN ------------------------------------------------------------------
end subroutine
