subroutine te0311(option, nomte)
    implicit none
    include 'jeveux.h'
!
    include 'asterc/r8depi.h'
    include 'asterc/r8prem.h'
    include 'asterfort/assert.h'
    include 'asterfort/dfdm2d.h'
    include 'asterfort/elref4.h'
    include 'asterfort/fointe.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jevech.h'
    include 'asterfort/normev.h'
    include 'asterfort/provec.h'
    include 'asterfort/rcvad2.h'
    include 'asterfort/u2mess.h'
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
!  ET DES FACTEURS D'INTENSITE DES CONTRAINTES K1 K2 K3
!  BORDS ELEMENTS ISOPARAMETRIQUES 3D
!
!  OPTION : 'CALC_K_G'    (CHARGES REELLES)
!           'CALC_K_G_F'  (CHARGES FONCTIONS)
!
! ENTREES  ---> OPTION : OPTION DE CALCUL
!          ---> NOMTE  : NOM DU TYPE ELEMENT
!
! VECTEURS DIMENSIONNES POUR  NNO = 9 , NPG = 9
!.......................................................................
!
    integer :: ndim, nno, npg1, compt, iforf
    integer :: ipoids, ivf, idfde, i, j, k, kp, iforc, ind
    integer :: idepl, ipres, ithet, igthet, igeom, ipref, itemps, icode
    integer :: nnos, jgano, ino, jlsn, jlst
!
    real(kind=8) :: a1(3), a2(3), a3(3), i1(3), i2(3), epsi, dfdx(9), dfdy(9)
    real(kind=8) :: coor(18), depl(3), valpar(4)
    real(kind=8) :: a1norm, a3norm, i2norm, divt, tcla, thetx, thety, thetz
    real(kind=8) :: dth1d1, dth2d2, poids, th1, th2, lsng, lstg
    real(kind=8) :: forc, dford1(3), dford2(3), dfor(3), coorg(3)
!                                NNO      3*NNO
    real(kind=8) :: presg, forcg(3), presn(9), forcn(27)
!
    real(kind=8) :: depi, norme, ff
!
    real(kind=8) :: tcla1, tcla2, tcla3
    real(kind=8) :: e1(3), e2(3), e3(3), p(3, 3)
    real(kind=8) :: phig, rg, valres(3), devres(3)
    real(kind=8) :: e, nu, mu
    real(kind=8) :: cr2, ka, coeff, coeff3
    real(kind=8) :: u1l(3), u1g(3), u2l(3), u2g(3), u3l(3), u3g(3)
    real(kind=8) :: g, k1, k2, k3, prsc
    integer :: ibalo, imate, iret
    character(len=8) :: nomres(3)
    integer :: icodre(3)
!
    character(len=8) :: nompar(4)
    character(len=4) :: fami
!
    logical :: fonc
!.......................................................................
!
    call jemarq()
!
    depi = r8depi()
!
    fami = 'RIGI'
    call elref4(' ', fami, ndim, nno, nnos,&
                npg1, ipoids, ivf, idfde, jgano)
    call jevech('PTHETAR', 'L', ithet)
    tcla = 0.d0
    tcla1 = 0.d0
    tcla2 = 0.d0
    tcla3 = 0.d0
    coeff = 0.d0
    coeff3 = 0.d0
    call jevech('PGTHETA', 'E', igthet)
!
!
! - PAS DE CALCUL DE G POUR LES ELEMENTS OU LA VALEUR DE THETA EST NULLE
!
    compt = 0
    epsi = 1.d-10
    do 20 i = 1, nno
        thetx = zr(ithet+3*(i-1)+1-1)
        thety = zr(ithet+3*(i-1)+2-1)
        thetz = zr(ithet+3*(i-1)+3-1)
        if ((abs(thetx).lt.epsi) .and. (abs(thety).lt.epsi) .and. (abs(thetz).lt.epsi)) then
            compt = compt + 1
        endif
20  end do
    if (compt .eq. nno) goto 9999
!
! RECUPERATION CHARGE, MATER...
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PDEPLAR', 'L', idepl)
!
    call jevech('PBASLOR', 'L', ibalo)
    call jevech('PLSN', 'L', jlsn)
    call jevech('PLST', 'L', jlst)
!
    call jevech('PMATERC', 'L', imate)
!
    if (option .eq. 'CALC_K_G_F') then
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
        fonc = .false.
        call jevech('PFR2D3D', 'L', iforc)
        call jevech('PPRESSR', 'L', ipres)
    endif
!
!
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
! CALCUL DU REPERE LOCAL (A1, A2, A3)
!
    do 130 j = 1, 3
        a1(j) = zr(igeom+3*(2-1)+j-1) - zr(igeom+3*(1-1)+j-1)
        a2(j) = zr(igeom+3*(3-1)+j-1) - zr(igeom+3*(1-1)+j-1)
130  end do
!
    a3(1) = a1(2)*a2(3) - a1(3)*a2(2)
    a3(2) = a1(3)*a2(1) - a1(1)*a2(3)
    a3(3) = a1(1)*a2(2) - a1(2)*a2(1)
!
! CALCUL DU REPERE LOCAL ORTHONORME (I1, I2, A3)
!
    i2(1) = a3(2)*a1(3) - a3(3)*a1(2)
    i2(2) = a3(3)*a1(1) - a3(1)*a1(3)
    i2(3) = a3(1)*a1(2) - a3(2)*a1(1)
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
        coor(2*i) = 0.d0
        do 1410 j = 1, 3
            coor(2*i-1) = coor(2*i-1) + (zr(igeom+3*(i-1)+j-1)-zr( igeom+j-1))*i1(j)
            coor(2*i) = coor(2*i) + (zr(igeom+3*(i-1)+j-1)-zr(igeom+j- 1))*i2(j)
1410      continue
1400  end do
!
! --- BOUCLE SUR LES POINTS DE GAUSS
!
    do 800 kp = 1, npg1
!
        k = (kp-1) * nno
!
        do 810 j = 1, 3
            depl(j) = 0.d0
            dford1(j) = 0.d0
            dford2(j) = 0.d0
            dfor(j) = 0.d0
            coorg(j) = 0.d0
810      continue
!
!
        th1 = 0.d0
        th2 = 0.d0
        dth1d1 = 0.d0
        dth2d2 = 0.d0
        lsng=0.d0
        lstg=0.d0
!
        do 820 i = 1, nno
!
            lsng = lsng + zr(jlsn-1+i) * zr(ivf+k+i-1)
            lstg = lstg + zr(jlst-1+i) * zr(ivf+k+i-1)
!
            do 830 j = 1, 3
                coorg(j) = coorg(j) + zr(ivf+k+i-1)*zr(igeom+3*(i-1)+ j-1)
830          continue
820      continue
!
!       BASE LOCALE ASSOCIEE AU POINT DE GAUSS KP
!       (E1=GRLT,E2=GRLN,E3=E1^E2)
        do 123 i = 1, 3
            e1(i) = 0.d0
            e2(i) = 0.d0
            do 321 ino = 1, nno
                ff = zr(ivf-1+nno*(kp-1)+ino)
                e1(i) = e1(i) + zr(ibalo-1+9*(ino-1)+i+3)*ff
                e2(i) = e2(i) + zr(ibalo-1+9*(ino-1)+i+6)*ff
321          continue
123      continue
!
!       NORMALISATION DE LA BASE
        call normev(e1, norme)
        call normev(e2, norme)
        call provec(e1, e2, e3)
!
!       CALCUL DE LA MATRICE DE PASSAGE P TELLE QUE
!       'GLOBAL' = P * 'LOCAL'
!
        do 124 i = 1, 3
            p(i,1) = e1(i)
            p(i,2) = e2(i)
            p(i,3) = e3(i)
124      continue
!
!       COORDONNÉES POLAIRES DU POINT
        rg=sqrt(lsng**2+lstg**2)
!
        if (rg .gt. r8prem()) then
!         LE POINT N'EST PAS SUR LE FOND DE FISSURE
            phig = sign(1.d0,lsng) * abs(atan2(lsng,lstg))
            iret=1
        else
!         LE POINT EST SUR LE FOND DE FISSURE :
!         L'ANGLE N'EST PAS DÉFINI, ON LE MET À ZÉRO
!         ON NE FERA PAS LE CALCUL DES DÉRIVÉES
            phig=0.d0
            iret=0
        endif
!
!       ON A PAS PU CALCULER LES DERIVEES DES FONCTIONS SINGULIERES
!       CAR ON SE TROUVE SUR LE FOND DE FISSURE
        call assert(iret.ne.0)
!
!
        if ((abs(lsng) .lt. 1.0d-8) .and. (lstg .lt. 0.0d0)) then
!
!         ON DETERMINE SI ON EST SUR LA LEVRE X2 > 0 OU
!         SUR LA LEVRE X2 < 0
!
!         PRODUIT SCALAIRE E2 (AXE X2) * A3 (NORMALE DE L'ELEMENT)
            prsc = 0.0d0
            do 1111 i = 1, 3
                prsc = prsc + e2(i)*a3(i)
1111          continue
            if (prsc .gt. 0.0d0) then
!            ON EST SUR LA LEVRE X2 < 0
                phig = -1.0d0 * abs(phig)
            else
!            ON EST SUR LA LEVRE X2 > 0
                phig = abs(phig)
            endif
        endif
!
!       RECUPERATION DES DONNEES MATERIAU
!
        nomres(1) = 'E'
        nomres(2) = 'NU'
        nomres(3) = 'ALPHA'
!
        call rcvad2(fami, kp, 1, '+', zi(imate),&
                    'ELAS', 3, nomres, valres, devres,&
                    icodre)
        if ((icodre(1).ne.0) .or. (icodre(2).ne.0)) then
            call u2mess('F', 'RUPTURE1_25')
        endif
        if (icodre(3) .ne. 0) then
            valres(3) = 0.d0
            devres(3) = 0.d0
        endif
!
        e = valres(1)
        nu = valres(2)
!
        mu = e / (2.d0*(1.d0+nu))
!
!       COEFFICIENTS DE CALCUL
        cr2 = sqrt(rg) / (2.d0*mu*sqrt(depi))
        ka = 3.d0 - 4.d0*nu
!
        coeff = e / (1.d0-nu*nu)
        coeff3 = 2.d0 * mu
!
!-----------------------------------------------------------------------
!       DEFINITION DU CHAMP SINGULIER AUXILIAIRE U1
!-----------------------------------------------------------------------
!       CHAMP SINGULIER AUXILIAIRE U1 DANS LA BASE LOCALE
        u1l(1) = cr2 * cos(phig*0.5d0) * (ka-cos(phig))
        u1l(2) = cr2 * sin(phig*0.5d0) * (ka-cos(phig))
        u1l(3) = 0.d0
!
!       CHAMP SINGULIER U1 DANS LA BASE GLOBALE
        do 5090 i = 1, 3
            u1g(i) = 0.0d0
            do 5190 ind = 1, 3
                u1g(i) = u1g(i) + p(i,ind)*u1l(ind)
5190          continue
5090      continue
!
!-----------------------------------------------------------------------
!       DEFINITION DU CHAMP SINGULIER AUXILIAIRE U2
!-----------------------------------------------------------------------
!       CHAMP SINGULIER AUXILIAIRE U2 DANS LA BASE LOCALE
        u2l(1) = cr2 * sin(phig*0.5d0) * (ka + 2.d0 + cos(phig))
        u2l(2) = cr2 * cos(phig*0.5d0) * (2.d0 - 1.d0 * (ka+cos(phig)) )
        u2l(3) = 0.d0
!
!       CHAMP SINGULIER U2 DANS LA BASE GLOBALE
        do 6090 i = 1, 3
            u2g(i) = 0.0d0
            do 6190 ind = 1, 3
                u2g(i) = u2g(i) + p(i,ind)*u2l(ind)
6190          continue
6090      continue
!
!-----------------------------------------------------------------------
!       DEFINITION DU CHAMP SINGULIER AUXILIAIRE U3
!-----------------------------------------------------------------------
!       CHAMP SINGULIER AUXILIAIRE U3 DANS LA BASE LOCALE
        u3l(1) = 0.d0
        u3l(2) = 0.d0
        u3l(3) = 4.d0 * cr2 * sin(phig*0.5d0)
!
!       CHAMP SINGULIER U3 DANS LA BASE GLOBALE
        do 7090 i = 1, 3
            u3g(i) = 0.0d0
            do 7190 ind = 1, 3
                u3g(i) = u3g(i) + p(i,ind)*u3l(ind)
7190          continue
7090      continue
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
                            forcg( j), icode)
65          continue
!
            do 400 i = 1, nno
                do 410 j = 1, 3
                    dford1(j) = dford1(j) + (forcn(3*(i-1)+j)-presn(i) *a3(j))*dfdx(i)
                    dford2(j) = dford2(j) + (forcn(3*(i-1)+j)-presn(i) *a3(j))*dfdy(i)
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
                    forcg(j) = forcg(j) + zr(iforc+3*(i-1)+j-1)*zr( ivf+k+i-1)
 6              continue
 4          continue
        endif
!
        do 300 i = 1, nno
            do 310 j = 1, 3
                depl(j) = depl(j) + zr(ivf+k+i-1)*zr(idepl+3*(i-1)+j- 1)
                th1 = th1 + zr(ivf+k+i-1)*zr(ithet+3*(i-1)+j-1)*i1(j)
                th2 = th2 + zr(ivf+k+i-1)*zr(ithet+3*(i-1)+j-1)*i2(j)
                dth1d1 = dth1d1 + zr(ithet+3*(i-1)+j-1)*i1(j)*dfdx(i)
                dth2d2 = dth2d2 + zr(ithet+3*(i-1)+j-1)*i2(j)*dfdy(i)
310          continue
300      continue
!
        do 320 j = 1, 3
            dfor(j) = dfor(j) + dford1(j)*th1 + dford2(j)*th2
320      continue
!
        divt = dth1d1 + dth2d2
!
        do 510 j = 1, 3
            forc = forcg(j) - presg*a3(j)
            tcla = tcla + poids*(forc*divt+dfor(j))*depl(j)
            tcla1 = tcla1 + 0.5d0*poids*(forc*divt+dfor(j))*u1g(j)
            tcla2 = tcla2 + 0.5d0*poids*(forc*divt+dfor(j))*u2g(j)
            tcla3 = tcla3 + 0.5d0*poids*(forc*divt+dfor(j))*u3g(j)
!
510      continue
!
800  end do
9999  continue
!
    g = tcla
    k1 = tcla1 * coeff
    k2 = tcla2 * coeff
    k3 = tcla3 * coeff3
!
    zr(igthet) = g
    zr(igthet+1) = tcla1 * sqrt(coeff)
    zr(igthet+2) = tcla2 * sqrt(coeff)
    zr(igthet+3) = tcla3 * sqrt(coeff3)
    zr(igthet+4) = k1
    zr(igthet+5) = k2
    zr(igthet+6) = k3
!
    call jedema()
end subroutine
