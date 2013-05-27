subroutine te0105(option, nomte)
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/cq3d2d.h'
    include 'asterfort/dfdm1d.h'
    include 'asterfort/dfdm2d.h'
    include 'asterfort/elref4.h'
    include 'asterfort/fointe.h'
    include 'asterfort/jevech.h'
    character(len=16) :: option, nomte
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
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES
!                          OPTION : 'CHAR_THER_FLUN_F'
!                          CAS COQUE
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
    integer :: nbres
    parameter (nbres=4)
    character(len=8) :: nompar(nbres)
    real(kind=8) :: pc(3), valpar(nbres), zero, un
    real(kind=8) :: coor2d(18), x, y, z, theta, fplnp1, fpln, fmonp1, fmon
    real(kind=8) :: dfdx(9), dfdy(9), poids, flux, cour, cosa, sina
    real(kind=8) :: matnp(9), fluxp1, coef, long, rp1, rp2, rp3, poid
    integer :: nno, kp, npg1, gi, pi, ivectt, iflux, itemps, nnos
    integer :: ipoids, ivf, idfde, igeom, ndim, jgano, i, k, ier
!
!
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg1, ipoids, ivf, idfde, jgano)
!
    zero = 0.d0
    un = 1.d0
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PFLUXNF', 'L', iflux)
    call jevech('PTEMPSR', 'L', itemps)
    call jevech('PVECTTR', 'E', ivectt)
!
    theta = zr(itemps+2)
!
    if (nomte .ne. 'THCPSE3 ' .and. nomte .ne. 'THCASE3 ' .and. nomte .ne. 'THCOSE3 ' .and.&
        nomte .ne. 'THCOSE2 ') then
!
        nompar(1) = 'X'
        nompar(2) = 'Y'
        nompar(3) = 'Z'
        nompar(4) = 'INST'
!
        call cq3d2d(nno, zr(igeom), 1.d0, zero, coor2d)
!
        do 40 kp = 1, npg1
            k = (kp-1)*nno
            call dfdm2d(nno, kp, ipoids, idfde, coor2d,&
                        dfdx, dfdy, poids)
            x = zero
            y = zero
            z = zero
            do 10 i = 1, nno
                x = x + zr(igeom+3* (i-1))*zr(ivf+k+i-1)
                y = y + zr(igeom+3* (i-1)+1)*zr(ivf+k+i-1)
                z = z + zr(igeom+3* (i-1)+2)*zr(ivf+k+i-1)
10          continue
            valpar(1) = x
            valpar(2) = y
            valpar(3) = z
            valpar(4) = zr(itemps)
            call fointe('FM', zk8(iflux), 4, nompar, valpar,&
                        fmonp1, ier)
            call fointe('FM', zk8(iflux+1), 4, nompar, valpar,&
                        fplnp1, ier)
            valpar(4) = zr(itemps) - zr(itemps+1)
            call fointe('FM', zk8(iflux), 4, nompar, valpar,&
                        fmon, ier)
            call fointe('FM', zk8(iflux+1), 4, nompar, valpar,&
                        fpln, ier)
            pc(1) = zero
            pc(2) = theta* (fmonp1) + (un-theta)* (fmon)
            pc(3) = theta* (fplnp1) + (un-theta)* (fpln)
            do 30 gi = 1, nno
                do 20 pi = 1, 3
                    i = 3* (gi-1) + pi - 1 + ivectt
                    zr(i) = zr(i) + pc(pi)*zr(ivf+k+gi-1)*poids
20              continue
30          continue
40      continue
!
        else if (nomte(1:8).eq.'THCPSE3 ' .or. nomte(1:8).eq.'THCASE3 ')&
    then
!
        nompar(1) = 'X'
        nompar(2) = 'Y'
        nompar(3) = 'INST'
!
        do 80 kp = 1, npg1
            k = (kp-1)*nno
            call dfdm1d(nno, zr(ipoids+kp-1), zr(idfde+k), zr(igeom), dfdx,&
                        cour, poids, cosa, sina)
            x = zero
            y = zero
            do 50 i = 1, nno
                x = x + zr(igeom+2* (i-1))*zr(ivf+k+i-1)
                y = y + zr(igeom+2* (i-1)+1)*zr(ivf+k+i-1)
50          continue
!
            if (nomte .eq. 'THCASE3') poids = poids*x
!
            valpar(1) = x
            valpar(2) = y
            valpar(3) = zr(itemps)
            call fointe('FM', zk8(iflux), 3, nompar, valpar,&
                        fmonp1, ier)
            call fointe('FM', zk8(iflux+1), 3, nompar, valpar,&
                        fplnp1, ier)
            valpar(3) = zr(itemps) - zr(itemps+1)
            call fointe('FM', zk8(iflux), 3, nompar, valpar,&
                        fmon, ier)
            call fointe('FM', zk8(iflux+1), 3, nompar, valpar,&
                        fpln, ier)
            pc(1) = zero
            pc(2) = theta* (fmonp1) + (un-theta)* (fmon)
            pc(3) = theta* (fplnp1) + (un-theta)* (fpln)
            do 70 gi = 1, nno
                do 60 pi = 1, 3
                    i = 3* (gi-1) + pi - 1 + ivectt
                    zr(i) = zr(i) + pc(pi)*zr(ivf+k+gi-1)*poids
60              continue
70          continue
80      continue
!
        else if (nomte(1:8).eq.'THCOSE3 ' .or. nomte(1:8).eq.'THCOSE2 ')&
    then
!
        long = (&
               zr(igeom+3)-zr(igeom))**2 + (zr(igeom+3+1)-zr(igeom+1) )**2 + (zr(igeom+3+2)-zr(ig&
               &eom+2)&
               )**2
        long = sqrt(long)/2.d0
!       EP  =EP/2.D0
!
        nompar(1) = 'X'
        nompar(2) = 'Y'
        nompar(3) = 'Z'
        nompar(4) = 'INST'
!
        rp1 = 1.33333333333333D0
        rp2 = 0.33333333333333D0
        rp3 = 0.33333333333333D0
!
        do 110 kp = 1, npg1
            k = (kp-1)*nno
!
            x = zero
            y = zero
            z = zero
            do 90 i = 1, nno
                x = x + zr(igeom+3* (i-1))*zr(ivf+k+i-1)
                y = y + zr(igeom+3* (i-1)+1)*zr(ivf+k+i-1)
                z = y + zr(igeom+3* (i-1)+2)*zr(ivf+k+i-1)
90          continue
!
            valpar(1) = x
            valpar(2) = y
            valpar(3) = z
!
            valpar(4) = zr(itemps)
            call fointe('FM', zk8(iflux), 4, nompar, valpar,&
                        fluxp1, ier)
            valpar(4) = zr(itemps) - zr(itemps+1)
            call fointe('FM', zk8(iflux), 4, nompar, valpar,&
                        flux, ier)
!
!      IMPORTANT: FLUXP1 OU FLUX = FLUX * EPAISSEUR
!
            coef = (theta*fluxp1+ (un-theta)*flux)/2.d0
!
            poid = zr(ipoids-1+kp)
!
            matnp(1) = rp1*poid*zr(ivf-1+k+1)
            matnp(2) = rp2*poid*zr(ivf-1+k+1)
            matnp(3) = rp3*poid*zr(ivf-1+k+1)
!
            matnp(4) = rp1*poid*zr(ivf-1+k+2)
            matnp(5) = rp2*poid*zr(ivf-1+k+2)
            matnp(6) = rp3*poid*zr(ivf-1+k+2)
!
            if (nomte(1:8) .eq. 'THCOSE3 ') then
                matnp(7) = rp1*poid*zr(ivf-1+k+3)
                matnp(8) = rp2*poid*zr(ivf-1+k+3)
                matnp(9) = rp3*poid*zr(ivf-1+k+3)
            endif
!
            do 100 i = 1, 3*nno
                zr(ivectt-1+i) = zr(ivectt-1+i) + coef*long*matnp(i)
100          continue
110      continue
!
    endif
!
end subroutine
