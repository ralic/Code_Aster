subroutine te0298(option, nomte)
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
!  OPTION : 'G_BILI'   (CHARGES REELLES)
!           'G_BILI_F' (CHARGES FONCTIONS)
!
! ENTREES  ---> OPTION : OPTION DE CALCUL
!          ---> NOMTE  : NOM DU TYPE ELEMENT
!
! VECTEURS DIMENSIONNES POUR  NNO = 9 , NPG = 9
!.......................................................................
!
    integer :: ndim, nno, npg1, compt
    integer :: iforfu, iforfv
    integer :: ipoids, ivf, idfde, i, j, k, kp, iforcu, iforcv
    integer :: ideplu, ipresu, ideplv, ipresv
    integer :: ithet, igthet, igeom
    integer :: iprefu, iprefv, itempu, itempv, icode
    integer :: nnos, jgano
!
    real(kind=8) :: a1(3), a2(3), a3(3), i1(3), i2(3), epsi
    real(kind=8) :: dfdx(9), dfdy(9)
    real(kind=8) :: coor(18), deplu(3), deplv(3)
    real(kind=8) :: vaparu(4), vaparv(4)
    real(kind=8) :: a1norm, a3norm, i2norm, divt, tcla, thetx, thety, thetz
    real(kind=8) :: dth1d1, dth2d2, poids, th1, th2, forcu, forcv
    real(kind=8) :: dfodu1(3), dfodu2(3), dfodv1(3), dfodv2(3)
    real(kind=8) :: dforu(3), dforv(3), coorg(3)
    real(kind=8) :: presgu, presgv, forcgu(3), forcgv(3)
    real(kind=8) :: presnu(9), presnv(9)
    real(kind=8) :: forcnu(27), forcnv(27)
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
! RECUPERATION CHARGE, MATER...
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PDEPLAU', 'L', ideplu)
    call jevech('PDEPLAV', 'L', ideplv)
    if (option .eq. 'G_BILI_F') then
        fonc = .true.
        call jevech('UPFF23D', 'L', iforfu)
        call jevech('UPFF23D', 'L', iforfv)
        call jevech('UPRESSF', 'L', iprefu)
        call jevech('VPRESSF', 'L', iprefv)
        call jevech('UTEMPSR', 'L', itempu)
        call jevech('UTEMPSR', 'L', itempv)
        nompar(1) = 'X'
        nompar(2) = 'Y'
        nompar(3) = 'Z'
        nompar(4) = 'INST'
        vaparu(4) = zr(itempu)
        vaparv(4) = zr(itempv)
    else
        fonc =.false.
        call jevech('UPFR23D', 'L', iforcu)
        call jevech('VPFR23D', 'L', iforcv)
        call jevech('UPRESSR', 'L', ipresu)
        call jevech('VPRESSR', 'L', ipresv)
    endif
!
!
! - SI CHARGE FONCTION RECUPERATION DES VALEURS AUX PG ET NOEUDS
!
    if (fonc) then
        do 70 i = 1, nno
            do 80 j = 1, 3
                vaparu(j) = zr(igeom+3*(i-1)+j-1)
                vaparv(j) = zr(igeom+3*(i-1)+j-1)
80          continue
            call fointe('FM', zk8(iprefu), 4, nompar, vaparu,&
                        presnu(i), icode)
            call fointe('FM', zk8(iprefv), 4, nompar, vaparv,&
                        presnv(i), icode)
            do 75 j = 1, 3
                call fointe('FM', zk8(iforfu+j-1), 4, nompar, vaparu,&
                            forcnu(3*(i-1)+j), icode)
                call fointe('FM', zk8(iforfv+j-1), 4, nompar, vaparv,&
                            forcnv(3*(i-1)+j), icode)
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
            deplu(j) = 0.d0
            deplv(j) = 0.d0
            dfodu1(j) = 0.d0
            dfodu2(j) = 0.d0
            dfodv1(j) = 0.d0
            dfodv2(j) = 0.d0
            dforu(j) = 0.d0
            dforv(j) = 0.d0
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
                vaparu(j) = coorg(j)
                vaparv(j) = coorg(j)
60          continue
            call fointe('FM', zk8(iprefu), 4, nompar, vaparu,&
                        presgu, icode)
            call fointe('FM', zk8(iprefv), 4, nompar, vaparv,&
                        presgv, icode)
            do 65 j = 1, 3
                call fointe('FM', zk8(iforfu+j-1), 4, nompar, vaparu,&
                            forcgu(j), icode)
                call fointe('FM', zk8(iforfv+j-1), 4, nompar, vaparv,&
                            forcgv(j), icode)
65          continue
!
            do 400 i = 1, nno
                do 410 j = 1, 3
                    dfodu1(j) = dfodu1(j) + ( forcnu(3*(i-1)+j) - presnu(i)*a3(j) ) * dfdx(i)
                    dfodv1(j) = dfodv1(j) + ( forcnv(3*(i-1)+j) - presnv(i)*a3(j) ) * dfdx(i)
!
                    dfodu2(j) = dfodu2(j) + ( forcnu(3*(i-1)+j) - presnu(i)*a3(j) ) * dfdy(i)
                    dfodv2(j) = dfodv2(j) + ( forcnv(3*(i-1)+j) - presnv(i)*a3(j) ) * dfdy(i)
410              continue
400          continue
        else
            presgu = 0.d0
            forcgu(1) = 0.d0
            forcgu(2) = 0.d0
            forcgu(3) = 0.d0
            presgv = 0.d0
            forcgv(1) = 0.d0
            forcgv(2) = 0.d0
            forcgv(3) = 0.d0
            do 4 i = 1, nno
                presgu = presgu + zr(ipresu+i-1)*zr(ivf+k+i-1)
                presgv = presgv + zr(ipresv+i-1)*zr(ivf+k+i-1)
                do 6 j = 1, 3
                    forcgu(j)=forcgu(j)+ zr(iforcu+3*(i-1)+j-1)*zr(&
                    ivf+k+i-1)
                    forcgv(j)=forcgv(j)+ zr(iforcv+3*(i-1)+j-1)*zr(&
                    ivf+k+i-1)
 6              continue
 4          continue
        endif
!
        do 300 i = 1, nno
            do 310 j = 1, 3
                deplu(j) = deplu(j)+ zr(ivf+k+i-1)*zr(ideplu+3*(i-1)+ j-1)
                deplv(j) = deplv(j)+ zr(ivf+k+i-1)*zr(ideplv+3*(i-1)+ j-1)
!
                th1 = th1 + zr(ivf+k+i-1)*zr(ithet+3*(i-1)+j-1)*i1(j)
                th2 = th2 + zr(ivf+k+i-1)*zr(ithet+3*(i-1)+j-1)*i2(j)
                dth1d1= dth1d1+ zr(ithet+3*(i-1)+j-1)*i1(j)*dfdx(i)
                dth2d2= dth2d2+ zr(ithet+3*(i-1)+j-1)*i2(j)*dfdy(i)
310          continue
300      continue
!
        do 320 j = 1, 3
            dforu(j) = dforu(j) + dfodu1(j)*th1+dfodu2(j)*th2
            dforv(j) = dforv(j) + dfodv1(j)*th1+dfodv2(j)*th2
320      continue
!
        divt = dth1d1+dth2d2
!
        do 510 j = 1, 3
            forcu = forcgu(j) - presgu*a3(j)
            forcv = forcgv(j) - presgv*a3(j)
!
            tcla = tcla + poids*0.5d0*(&
                   forcu*divt+dforu(j))*deplv(j)+ poids*0.5d0*(forcv*divt+dforv(j))*deplu(j)
!
510      continue
800  end do
9999  continue
!
    zr(igthet) = tcla
!
    call jedema()
end subroutine
