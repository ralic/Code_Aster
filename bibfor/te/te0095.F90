subroutine te0095(option, nomte)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
!
!.......................................................................
! FONCTION REALISEE:
!
!      CALCUL
!      DE LA FORME BILINEAIRE SYMETRIQUE G(U,V)
!      POUR LES ELEMENTS ISOPARAMETRIQUES 3D
!
!      OPTION : 'G_BILI','G_BILI_F'
!
! ENTREES  ---> OPTION : OPTION DE CALCUL
!          ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
    include 'jeveux.h'
    include 'asterc/r8prem.h'
    include 'asterfort/elref4.h'
    include 'asterfort/fointe.h'
    include 'asterfort/gbil3d.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jevech.h'
    include 'asterfort/nmgeom.h'
    include 'asterfort/rccoma.h'
    include 'asterfort/rcvada.h'
    include 'asterfort/rcvalb.h'
    include 'asterfort/rcvarc.h'
    include 'asterfort/tecach.h'
    include 'asterfort/u2mess.h'
    integer :: icodre(3)
    integer :: icorsv(3)
    character(len=8) :: nomres(3), nompar(4)
    character(len=16) :: nomte, option, phenom
!
    real(kind=8) :: epsi
    real(kind=8) :: dfdi(81), f(3, 3), eps(6), fnou(81), fnov(81)
    real(kind=8) :: dudm(3, 4), dvdm(3, 4)
    real(kind=8) :: dfudm(3, 4), dfvdm(3, 4), der(4), dtdm(3, 4)
    real(kind=8) :: rbid, e, nu, alpha, tref, ttrgu, ttrgv, k3a
    real(kind=8) :: thet, tgu, tgv
    real(kind=8) :: xg, yg
    real(kind=8) :: c1, c2, c3, tgm, tgp
    real(kind=8) :: valres(3), devres(3), valrsv(3), devrsv(3)
    real(kind=8) :: vaparu(4), vaparv(4)
    real(kind=8) :: gelem, guv3, g, poids, puls
    real(kind=8) :: tgudm(3), tgvdm(3)
    real(kind=8) :: rho, om, omo
!
    integer :: jgano, ipoids, ivf, idfde, nno, kp, npg1, compt
    integer :: igeom, ithet, ific, idepu, idepv
    integer :: imate, k, i, j, l, ndim, nnos
    integer :: iforfu, iforfv, iforcu, iforcv
    integer :: iret, iepsru, iepsrv, iret0, iret1, iret2, iret3
    integer :: ipesau, ipesav, irotau, irotav, itmpsu, itmpsv
!
    integer :: kk, ier
    logical :: fonc
!
!
    call jemarq()
    epsi = r8prem()
!
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg1, ipoids, ivf, idfde, jgano)
    call jevech('PTHETAR', 'L', ithet)
!
!
! - PAS DE CALCUL DE G POUR LES ELEMENTS OU LA VALEUR DE THETA EST NULLE
!
    compt = 0
    do 30 i = 1, nno
        thet = 0.d0
        do 20 j = 1, ndim
            thet = thet + abs(zr(ithet+ndim* (i-1)+j-1))
20      continue
        if (thet .lt. epsi) compt = compt + 1
30  end do
    if (compt .eq. nno) goto 100
!
! RECUPERATION CHARGE, MATER...
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PDEPLAU', 'L', idepu)
    call jevech('PDEPLAV', 'L', idepv)
    call jevech('PMATERC', 'L', imate)
!
    call tecach('ONN', 'UPESANR', 'L', 1, ipesau,&
                iret)
    call tecach('ONN', 'UROTATR', 'L', 1, irotau,&
                iret)
    call tecach('ONN', 'VPESANR', 'L', 1, ipesav,&
                iret)
    call tecach('ONN', 'VROTATR', 'L', 1, irotav,&
                iret)
!
!
    if (option .eq. 'G_BILI_F') then
        fonc = .true.
        call jevech('UPFFVOL', 'L', iforfu)
        call jevech('VPFFVOL', 'L', iforfv)
!
        call jevech('UTEMPSR', 'L', itmpsu)
        call jevech('VTEMPSR', 'L', itmpsv)
        nompar(1) = 'X'
        nompar(2) = 'Y'
        nompar(3) = 'Z'
        nompar(4) = 'INST'
        vaparu(4) = zr(itmpsu)
        vaparv(4) = zr(itmpsv)
        call tecach('ONN', 'UEPSINF', 'L', 1, iepsru,&
                    iret)
        call tecach('ONN', 'VEPSINF', 'L', 1, iepsrv,&
                    iret)
    else
        fonc = .false.
        call jevech('UPFRVOL', 'L', iforcu)
        call jevech('VPFRVOL', 'L', iforcv)
        call tecach('ONN', 'UEPSINR', 'L', 1, iepsru,&
                    iret)
        call tecach('ONN', 'VEPSINR', 'L', 1, iepsrv,&
                    iret)
    endif
!
    call jevech('PGTHETA', 'E', ific)
!
    guv3 = 0.d0
    call rcvarc(' ', 'TEMP', 'REF', 'NOEU', 1,&
                1, tref, iret0)
    nomres(1) = 'E'
    nomres(2) = 'NU'
    nomres(3) = 'ALPHA'
!
! - RECUPERATION DES CHARGES ET DEFORMATIONS INITIALES ----------------
!
    if (fonc) then
        do 150 i = 1, nno
            do 130 j = 1, ndim
                vaparu(j) = zr(igeom+ndim*(i-1)+j-1)
                vaparv(j) = zr(igeom+ndim*(i-1)+j-1)
130          continue
            do 140 j = 1, ndim
                kk = ndim*(i-1) + j
                call fointe('FM', zk8(iforfu+j-1), 3, nompar, vaparu,&
                            fnou(kk), ier)
                call fointe('FM', zk8(iforfv+j-1), 3, nompar, vaparv,&
                            fnov(kk), ier)
140          continue
150      continue
    else
        do 8000 i = 1, nno
            do 6000 j = 1, ndim
                fnou(ndim*(i-1)+j) = zr(iforcu+ndim*(i-1)+j-1)
                fnov(ndim*(i-1)+j) = zr(iforcv+ndim*(i-1)+j-1)
6000          continue
8000      continue
    endif
!
    if ((ipesau.ne.0) .or. (irotau.ne.0)) then
        call rccoma(zi(imate), 'ELAS', 1, phenom, icodre)
        call rcvalb('RIGI', 1, 1, '+', zi(imate),&
                    ' ', phenom, 1, ' ', rbid,&
                    1, 'RHO', rho, icodre, 1)
        if (ipesau .ne. 0) then
            do 160 i = 1, nno
                do 161 j = 1, ndim
                    kk = ndim*(i-1) + j
                    fnou(kk) = fnou(kk) + rho*zr(ipesau)*zr(ipesau+j)
161              continue
160          continue
        endif
        if (irotau .ne. 0) then
            om = zr(irotau)
            do 170 i = 1, nno
                omo = 0.d0
                do 171 j = 1, ndim
                    omo = omo + zr(irotau+j)*zr(igeom+ndim*(i-1)+j-1)
171              continue
                do 172 j = 1, ndim
                    kk = ndim*(i-1) + j
                    fnou(kk) = fnou(kk) + rho*om*om*(zr(igeom+kk-1)- omo*zr(irotau+j))
172              continue
170          continue
        endif
    endif
!
    if ((ipesav.ne.0) .or. (irotav.ne.0)) then
        call rccoma(zi(imate), 'ELAS', 1, phenom, icodre)
        call rcvalb('RIGI', 1, 1, '+', zi(imate),&
                    ' ', phenom, 1, ' ', rbid,&
                    1, 'RHO', rho, icodre, 1)
        if (ipesav .ne. 0) then
            do 260 i = 1, nno
                do 261 j = 1, ndim
                    kk = ndim*(i-1) + j
                    fnov(kk) = fnov(kk) + rho*zr(ipesav)*zr(ipesav+j)
261              continue
260          continue
        endif
        if (irotav .ne. 0) then
            om = zr(irotav)
            do 270 i = 1, nno
                omo = 0.d0
                do 271 j = 1, ndim
                    omo = omo + zr(irotav+j)*zr(igeom+ndim*(i-1)+j-1)
271              continue
                do 272 j = 1, ndim
                    kk = ndim*(i-1) + j
                    fnov(kk) = fnov(kk) + rho*om*om*(zr(igeom+kk-1)- omo*zr(irotav+j))
272              continue
270          continue
        endif
    endif
!
! ======================================================================
! - BOUCLE SUR LES POINTS DE GAUSS
!
    do 90 kp = 1, npg1
        l = (kp-1)*nno
        tgu = 0.d0
        tgv = 0.d0
        xg = 0.d0
        yg = 0.d0
        do 50 i = 1, 3
            tgudm(i) = 0.d0
            tgvdm(i) = 0.d0
            do 40 j = 1, 4
                dudm(i,j) = 0.d0
                dvdm(i,j) = 0.d0
                dtdm(i,j) = 0.d0
                dfudm(i,j) = 0.d0
                dfvdm(i,j) = 0.d0
40          continue
50      continue
!
! - CALCUL DES ELEMENTS GEOMETRIQUES
!
        call nmgeom(ndim, nno, .false., .false., zr(igeom),&
                    kp, ipoids, ivf, idfde, zr(idepu),&
                    .true., poids, dfdi, f, eps,&
                    rbid)
!
! - CALCULS DES GRADIENTS DE U ET V (DUDM ET DVDM),THETA (DTDM),
!   FU ET FV (DFUDM ET DFVDM)
!   CALCUL DES CHAMPS DE TEMPERATURE (TGU ET TGV) ET DE LEURS GRADIENTS
!   (TGUDM ET TGVDM)AUX POINTS DE GAUSS
!
        iret3 = 0
        do 80 i = 1, nno
            der(1) = dfdi(i)
            der(2) = dfdi(i+nno)
            der(3) = dfdi(i+2*nno)
            der(4) = zr(ivf+l+i-1)
            call rcvarc(' ', 'TEMP', '-', 'NOEU', i,&
                        1, tgm, iret1)
            call rcvarc(' ', 'TEMP', '+', 'NOEU', i,&
                        1, tgp, iret2)
            if ((iret1+iret2) .eq. 0) then
                if (iret0 .eq. 1) call u2mess('F', 'CALCULEL_31')
                tgu = tgu + tgm*der(4)
                tgv = tgv + tgp*der(4)
                do 75 j = 1, ndim
                    tgudm(j) = tgudm(j) + tgm*der(j)
                    tgvdm(j) = tgvdm(j) + tgp*der(j)
75              continue
            else
                iret3 = iret3+1
                tgu=0.d0
                tgv=0.d0
            endif
!
            xg = xg + zr(igeom+2* (i-1))*der(4)
            yg = yg + zr(igeom+2* (i-1)+1)*der(4)
            do 70 j = 1, ndim
                do 60 k = 1, ndim
                    dudm(j,k) = dudm(j,k) + zr(idepu+ndim* (i-1)+j-1)* der(k)
                    dvdm(j,k) = dvdm(j,k) + zr(idepv+ndim* (i-1)+j-1)* der(k)
                    dtdm(j,k) = dtdm(j,k) + zr(ithet+ndim* (i-1)+j-1)* der(k)
                    dfudm(j,k) = dfudm(j,k) + fnou(ndim*(i-1)+j)*der( k)
                    dfvdm(j,k) = dfvdm(j,k) + fnov(ndim*(i-1)+j)*der( k)
60              continue
                dudm(j,4) = dudm(j,4) + zr(idepu+ndim*(i-1)+j-1)*der( 4)
                dvdm(j,4) = dvdm(j,4) + zr(idepv+ndim*(i-1)+j-1)*der( 4)
                dtdm(j,4) = dtdm(j,4) + zr(ithet+ndim*(i-1)+j-1)*der( 4)
                dfudm(j,4) = dfudm(j,4) + fnou(ndim*(i-1)+j)*der(4)
                dfvdm(j,4) = dfvdm(j,4) + fnov(ndim*(i-1)+j)*der(4)
70          continue
80      continue
!
! - RECUPERATION DES DONNEES MATERIAUX
        if (iret3 .eq. 0) then
            ttrgu = tgu - tref
            ttrgv = tgv - tref
        else
            do 85 j = 1, ndim
                tgudm(j) = 0.d0
                tgvdm(j) = 0.d0
85          continue
            ttrgu = 0.d0
            ttrgv = 0.d0
        endif
        call rcvada(zi(imate), 'ELAS', tgu, 3, nomres,&
                    valres, devres, icodre)
        call rcvada(zi(imate), 'ELAS', tgv, 3, nomres,&
                    valrsv, devrsv, icorsv)
        if (iret0 .eq. 0) then
            if ((icodre(3) .ne.0) .or. ((icorsv(3) .ne.0))) then
                call u2mess('F', 'CALCULEL_31')
            endif
        else
            valres(3) = 0.d0
            devres(3) = 0.d0
            valrsv(3) = 0.d0
            devrsv(3) = 0.d0
        endif
        if ((valres(1) .ne. valrsv(1)) .or. ((valres(2) .ne. valrsv(2))) .or.&
            (valres(3) .ne. valrsv(3))) then
            call u2mess('F', 'ELEMENTS3_12')
        endif
        e = valres(1)
        nu = valres(2)
        alpha = valres(3)
        k3a = alpha * e / (1.d0-2.d0*nu)
        c3 = e/ (2.d0* (1.d0+nu))
        c1 = e* (1.d0-nu)/ ((1.d0+nu)* (1.d0-2.d0*nu))
        c2 = nu/ (1.d0-nu)*c1
!
        gelem = 0.d0
! PAS DE TERME DYNAMIQUE DANS GBIL
        puls = 0.d0
        rho = 0.d0
        call gbil3d(dudm, dvdm, dtdm, dfudm, dfvdm,&
                    tgudm, tgvdm, ttrgu, ttrgv, poids,&
                    c1, c2, c3, k3a, rho,&
                    puls, gelem)
        guv3 = guv3 + gelem
90  end do
!
    g = guv3
!
    zr(ific) = g
100  continue
    call jedema()
end subroutine
