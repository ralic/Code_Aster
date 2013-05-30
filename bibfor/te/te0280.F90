subroutine te0280(option, nomte)
    implicit   none
    include 'jeveux.h'
!
    include 'asterfort/dfdm2d.h'
    include 'asterfort/elref4.h'
    include 'asterfort/fointe.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jevech.h'
    character(len=16) :: option, nomte
!.......................................................................
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
!  CALCUL DU TAUX DE RESTITUTION D'ENERGIE ELEMENTAIRE
!  BORDS ELEMENTS ISOPARAMETRIQUES 3D AVEC PRESSION
!
!  OPTION : 'CALC_G'   (CHARGES REELLES)
!           'CALC_G_F' (CHARGES FONCTIONS)
!
! ENTREES  ---> OPTION : OPTION DE CALCUL
!          ---> NOMTE  : NOM DU TYPE ELEMENT
!
! VECTEURS DIMENSIONNES POUR  NNO = 9 , NPG = 9
!.......................................................................
!
    integer :: ndim, nno, npg1, compt, iforf
    integer :: ipoids, ivf, idfde, i, j, k, kp, iforc
    integer :: idepl, ipres, ithet, igthet, igeom, ipref, itemps, icode
    integer :: nnos, jgano
!
    real(kind=8) :: a1(3), a2(3), a3(3), i1(3), i2(3), epsi, dfdx(9), dfdy(9)
    real(kind=8) :: coor(18), depl(3), valpar(4)
    real(kind=8) :: a1norm, a3norm, i2norm, divt, tcla, thetx, thety, thetz
    real(kind=8) :: dth1d1, dth2d2, poids, th1, th2, tsom, tsurf, tsurp
    real(kind=8) :: forc, dford1(3), dford2(3), dfor(3), coorg(3)
!                                         NNO       3*NNO
    real(kind=8) :: presg, forcg(3), presn(9), forcn(27)
!
    character(len=8) :: nompar(4)
!
    logical :: fonc
!.......................................................................
!
    call jemarq()
!
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg1, ipoids, ivf, idfde, jgano)
    call jevech('PTHETAR', 'L', ithet)
    tcla = 0.d0
    tsurf = 0.d0
    tsurp = 0.d0
    call jevech('PGTHETA', 'E', igthet)
!
! - PAS DE CALCUL DE G POUR LES ELEMENTS OU LA VALEUR DE THETA EST NULLE
!
    compt = 0
    epsi = 1.d-10
    do 20 i = 1, nno
        thetx = zr(ithet + 3*(i - 1) + 1 - 1)
        thety = zr(ithet + 3*(i - 1) + 2 - 1)
        thetz = zr(ithet + 3*(i - 1) + 3 - 1)
        if ((abs(thetx).lt.epsi) .and. (abs(thety).lt.epsi) .and. (abs( thetz).lt.epsi)) then
            compt = compt + 1
        endif
20  end do
    if (compt .eq. nno) goto 9999
!
! RECUPERATION DES CHAMPS LOCAUX
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PDEPLAR', 'L', idepl)
    if ((option.eq.'CALC_G_F') .or. (option.eq.'CALC_G_GLOB_F')) then
        fonc = .true.
        call jevech('PFF2D3D', 'L', iforf)
        call jevech('PPRESSF', 'L', ipref)
        call jevech('PTEMPSR', 'L', itemps)
        nompar(1) = 'X'
        nompar(2) = 'Y'
        nompar(3) = 'Z'
        nompar(4) = 'INST'
        valpar(4) = zr(itemps)
    else
        fonc =.false.
        call jevech('PFR2D3D', 'L', iforc)
        call jevech('PPRESSR', 'L', ipres)
    endif
!
! - SI CHARGE FONCTION RECUPERATION DES VALEURS AUX PG ET NOEUDS
!
    if (fonc) then
        do 70 i = 1, nno
            do 80 j = 1, 3
                valpar(j) = zr(igeom+3*(i-1)+j-1)
80          continue
            call fointe('FM', zk8(ipref), 4, nompar, valpar,&
                        presn(i), icode)
            do 75 j = 1, 3
                call fointe('FM', zk8(iforf+j-1), 4, nompar, valpar,&
                            forcn(3*(i-1)+j), icode)
75          continue
70      continue
    endif
!
! CALCUL DU REPERE LOCAL ( A1, A2, A3)
!
    do 130 j = 1, 3
        a1(j) = zr(igeom+3*(2-1)+j-1)- zr(igeom+3*(1-1)+j-1)
        a2(j) = zr(igeom+3*(3-1)+j-1)- zr(igeom+3*(1-1)+j-1)
130  end do
!
    a3(1) = a1(2)*a2(3)- a1(3)*a2(2)
    a3(2) = a1(3)*a2(1)- a1(1)*a2(3)
    a3(3) = a1(1)*a2(2)- a1(2)*a2(1)
!
! CALCUL DU REPERE LOCAL ORTHONORME ( I1, I2, A3)
!
    i2(1) = a3(2)*a1(3)-a3(3)*a1(2)
    i2(2) = a3(3)*a1(1)-a3(1)*a1(3)
    i2(3) = a3(1)*a1(2)-a3(2)*a1(1)
!
    a1norm = sqrt(a1(1)*a1(1)+a1(2)*a1(2)+a1(3)*a1(3))
    i2norm = sqrt(i2(1)*i2(1)+i2(2)*i2(2)+i2(3)*i2(3))
    a3norm = sqrt(a3(1)*a3(1)+a3(2)*a3(2)+a3(3)*a3(3))
    do 150 i = 1, 3
        i1(i) = a1(i) / a1norm
        i2(i) = i2(i) / i2norm
        a3(i) = a3(i) / a3norm
150  end do
!
    do 1400 i = 1, nno
        coor(2*i-1) = 0.d0
        coor(2*i ) = 0.d0
        do 1410 j = 1, 3
            coor(2*i-1)= coor(2*i-1)+( zr(igeom+3*(i-1)+j-1)- zr(&
            igeom+j-1))*i1(j)
            coor(2*i )= coor(2*i )+( zr(igeom+3*(i-1)+j-1)- zr(igeom+&
            j-1))*i2(j)
1410      continue
1400  end do
!
! --- BOUCLE SUR LES POINTS DE GAUSS
!
    do 800 kp = 1, npg1
        k = (kp-1)*nno
!
        do 810 j = 1, 3
            depl(j) = 0.d0
            dford1(j) = 0.d0
            dford2(j) = 0.d0
            dfor(j) = 0.d0
            coorg(j) = 0.d0
810      continue
        th1 = 0.d0
        th2 = 0.d0
        dth1d1 = 0.d0
        dth2d2 = 0.d0
!
        do 820 i = 1, nno
            do 830 j = 1, 3
                coorg(j) = coorg(j)+zr(ivf+k+i-1)*zr(igeom+3*(i-1)+j- 1)
830          continue
820      continue
!
        call dfdm2d(nno, kp, ipoids, idfde, coor,&
                    dfdx, dfdy, poids)
!
        if (fonc) then
            do 60 j = 1, 3
                valpar(j) = coorg(j)
60          continue
            call fointe('FM', zk8(ipref), 4, nompar, valpar,&
                        presg, icode)
            do 65 j = 1, 3
                call fointe('FM', zk8(iforf+j-1), 4, nompar, valpar,&
                            forcg(j), icode)
65          continue
!
            do 400 i = 1, nno
                do 410 j = 1, 3
                    dford1(j) = dford1(j) + ( forcn(3*(i-1)+j) - presn(i)*a3(j) ) * dfdx(i)
                    dford2(j) = dford2(j) + ( forcn(3*(i-1)+j) - presn(i)*a3(j) ) * dfdy(i)
410              continue
400          continue
        else
            presg = 0.d0
            forcg(1) = 0.d0
            forcg(2) = 0.d0
            forcg(3) = 0.d0
            do 4 i = 1, nno
                presg = presg + zr(ipres+i-1)*zr(ivf+k+i-1)
                do 6 j = 1, 3
                    forcg(j)=forcg(j)+zr(iforc+3*(i-1)+j-1)*zr(ivf+k+&
                    i-1)
 6              continue
 4          continue
        endif
!
        do 300 i = 1, nno
            do 310 j = 1, 3
                depl(j) = depl(j)+ zr(ivf+k+i-1)*zr(idepl+3*(i-1)+j-1)
                th1 = th1 + zr(ivf+k+i-1)*zr(ithet+3*(i-1)+j-1)*i1(j)
                th2 = th2 + zr(ivf+k+i-1)*zr(ithet+3*(i-1)+j-1)*i2(j)
                dth1d1= dth1d1+ zr(ithet+3*(i-1)+j-1)*i1(j)*dfdx(i)
                dth2d2= dth2d2+ zr(ithet+3*(i-1)+j-1)*i2(j)*dfdy(i)
310          continue
300      continue
!
        do 320 j = 1, 3
            dfor(j) = dfor(j) + dford1(j)*th1+dford2(j)*th2
320      continue
!
        divt = dth1d1+dth2d2
        do 510 j = 1, 3
            forc = forcg(j) - presg*a3(j)
            tcla = tcla + poids*(forc*divt+dfor(j))*depl(j)
510      continue
800  end do
9999  continue
!
    tsom = tcla + tsurf + tsurp
    zr(igthet) = tsom
!
    call jedema()
end subroutine
