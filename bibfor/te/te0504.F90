subroutine te0504(option, nomte)
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
!
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/elref4.h'
    include 'asterfort/foderi.h'
    include 'asterfort/jevech.h'
    include 'asterfort/lteatt.h'
    include 'asterfort/vff2dn.h'
    character(len=16) :: option, nomte
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES MATRICES ELEMENTAIRES
!                          OPTION : 'RIGI_THER_FLUTNL'
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
    real(kind=8) :: poids, r, nx, ny, alpha, alpha0
    integer :: nno, kp, npg, ipoids, ivf, idfde, igeom
    integer :: imattt, i, j, ij, l, li, lj, iflux
    logical :: laxi
!
!-----------------------------------------------------------------------
    integer :: itemp, itemps, jgano, ndim, nnos
    real(kind=8) :: tpgi
!-----------------------------------------------------------------------
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
!
    laxi = .false.
    if (lteatt(' ','AXIS','OUI')) laxi = .true.
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PFLUXNL', 'L', iflux)
    call jevech('PTEMPER', 'L', itemp)
    call jevech('PTEMPSR', 'L', itemps)
    call jevech('PMATTTR', 'E', imattt)
!
    if (zk8(iflux) (1:7) .eq. '&FOZERO') goto 50
!
    do 40 kp = 1, npg
        call vff2dn(ndim, nno, kp, ipoids, idfde,&
                    zr(igeom), nx, ny, poids)
        r = 0.d0
        tpgi = 0.d0
        do 10 i = 1, nno
            l = (kp-1)*nno + i
            r = r + zr(igeom+2*i-2)*zr(ivf+l-1)
            tpgi = tpgi + zr(itemp+i-1)*zr(ivf+l-1)
10      continue
        if (laxi) poids = poids*r
        call foderi(zk8(iflux), tpgi, alpha, alpha0)
        ij = imattt - 1
        do 30 i = 1, nno
            li = ivf + (kp-1)*nno + i - 1
!
            do 20 j = 1, i
                lj = ivf + (kp-1)*nno + j - 1
                ij = ij + 1
                zr(ij) = zr(ij) - poids*alpha0*zr(li)*zr(lj)
20          continue
30      continue
40  end do
50  continue
end subroutine
