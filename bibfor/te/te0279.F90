subroutine te0279(option, nomte)
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
    include 'jeveux.h'
!
    include 'asterfort/dfdm3d.h'
    include 'asterfort/elref4.h'
    include 'asterfort/jevech.h'
    include 'asterfort/lteatt.h'
    include 'asterfort/ntfcma.h'
    include 'asterfort/rcdiff.h'
    include 'asterfort/rcfode.h'
    character(len=16) :: nomte, option
! ----------------------------------------------------------------------
!
!    - FONCTION REALISEE:  CALCUL DES MATRICES TANGENTES ELEMENTAIRES
!                          OPTION : 'MTAN_RIGI_MASS'
!                          ELEMENTS 3D ISO PARAMETRIQUES LUMPES
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
!
! THERMIQUE NON LINEAIRE
!
!
!
!
!
    real(kind=8) :: rhocp, lambda, theta, deltat, khi, tpgi
    real(kind=8) :: dfdx(27), dfdy(27), dfdz(27), poids, r8bid
    real(kind=8) :: tpsec, diff
    integer :: ipoids, ivf, idfde, igeom, imate
    integer :: jgano, nno, kp, npg, i, j, ij, l, imattt, itemps, ifon(3)
    integer :: isechi, isechf
    integer :: icomp, itempi, nnos, ndim
    integer :: npg2, ipoid2, ivf2, idfde2
! DEB ------------------------------------------------------------------
    if ((lteatt(' ','LUMPE','OUI')) .and. (nomte(6:10).ne.'PYRAM')) then
        call elref4(' ', 'NOEU', ndim, nno, nnos,&
                    npg2, ipoid2, ivf2, idfde2, jgano)
    else
        call elref4(' ', 'MASS', ndim, nno, nnos,&
                    npg2, ipoid2, ivf2, idfde2, jgano)
    endif
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call jevech('PTEMPSR', 'L', itemps)
    call jevech('PTEMPEI', 'L', itempi)
    call jevech('PCOMPOR', 'L', icomp)
    call jevech('PMATTTR', 'E', imattt)
!
    deltat = zr(itemps+1)
    theta = zr(itemps+2)
    khi = zr(itemps+3)
!
    if (zk16(icomp) (1:5) .eq. 'THER_') then
!
        call ntfcma(zi(imate), ifon)
!
        do 40 kp = 1, npg
            l = (kp-1)*nno
            call dfdm3d(nno, kp, ipoids, idfde, zr(igeom),&
                        dfdx, dfdy, dfdz, poids)
            tpgi = 0.d0
            do 10 i = 1, nno
                tpgi = tpgi + zr(itempi+i-1)*zr(ivf+l+i-1)
10          continue
            call rcfode(ifon(2), tpgi, lambda, r8bid)
!
            do 30 i = 1, nno
!CDIR$ IVDEP
                do 20 j = 1, i
                    ij = (i-1)*i/2 + j
                    zr(imattt+ij-1) = zr(imattt+ij-1) + poids*theta* lambda* (dfdx(i)*dfdx(j)+ df&
                                      &dy(i)*dfdy(j)+dfdz(i)* dfdz(j))
20              continue
30          continue
40      continue
!
        do 80 kp = 1, npg2
            l = (kp-1)*nno
            call dfdm3d(nno, kp, ipoid2, idfde2, zr(igeom),&
                        dfdx, dfdy, dfdz, poids)
            tpgi = 0.d0
            do 50 i = 1, nno
                tpgi = tpgi + zr(itempi+i-1)*zr(ivf2+l+i-1)
50          continue
            call rcfode(ifon(1), tpgi, r8bid, rhocp)
!
            do 70 i = 1, nno
!CDIR$ IVDEP
                do 60 j = 1, i
                    ij = (i-1)*i/2 + j
                    zr(imattt+ij-1) = zr(imattt+ij-1) + poids*khi* rhocp*zr(ivf2+l+i-1)* zr(ivf2+&
                                      &l+j-1)/deltat
60              continue
70          continue
80      continue
!
    else if (zk16(icomp) (1:5).eq.'SECH_') then
!
! --- SECHAGE
!
        if (zk16(icomp) (1:12) .eq. 'SECH_GRANGER' .or. zk16(icomp) (1: 10) .eq.&
            'SECH_NAPPE') then
            call jevech('PTMPCHI', 'L', isechi)
            call jevech('PTMPCHF', 'L', isechf)
        else
!          POUR LES AUTRES LOIS, PAS DE CHAMP DE TEMPERATURE
!          ISECHI ET ISECHF SONT FICTIFS
            isechi = itempi
            isechf = itempi
        endif
        do 90 kp = 1, npg
            l = (kp-1)*nno
            call dfdm3d(nno, kp, ipoids, idfde, zr(igeom),&
                        dfdx, dfdy, dfdz, poids)
            tpgi = 0.d0
            tpsec = 0.d0
            do 100 i = 1, nno
                tpgi = tpgi + zr(itempi+i-1)*zr(ivf+l+i-1)
                tpsec = tpsec + zr(isechf+i-1)*zr(ivf+l+i-1)
100          continue
            call rcdiff(zi(imate), zk16(icomp), tpsec, tpgi, diff)
            do 110 i = 1, nno
!
                do 120 j = 1, i
                    ij = (i-1)*i/2 + j
                    zr(imattt+ij-1) = zr(imattt+ij-1) + poids* (theta* diff* (dfdx(i)*dfdx(j)+ df&
                                      &dy(i)*dfdy(j)+dfdz(i)* dfdz(j)))
120              continue
110          continue
90      continue
        do 91 kp = 1, npg2
            l = (kp-1)*nno
            call dfdm3d(nno, kp, ipoid2, idfde2, zr(igeom),&
                        dfdx, dfdy, dfdz, poids)
            do 111 i = 1, nno
!
                do 121 j = 1, i
                    ij = (i-1)*i/2 + j
                    zr(imattt+ij-1) = zr(imattt+ij-1) + poids* (khi*zr(ivf2+l+i-1)*zr(ivf2+l+j-1)&
                                      &/deltat)
121              continue
111          continue
91      continue
!
    endif
!
! FIN ------------------------------------------------------------------
end subroutine
