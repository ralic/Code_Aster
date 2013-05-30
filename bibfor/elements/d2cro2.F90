subroutine d2cro2(zimat, nmnbn, nmplas, nmdpla, nmddpl,&
                  nmprox, cnbn, cplas, rpara, cief,&
                  cdeps, cdtg, cier, cdepsp, dc1,&
                  dc2)
    implicit none
! ======================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     CALCUL DU MULTIPLICATEUR PLASTIQUE
!     ET DE L INCREMENT DE COURBURE PLASTIQUE
!     DANS LE CAS OU 1 CRITERE PLASTIQUE EST ACTIVE
!     METHODE EXPLICITE AVEC UNE CONDITION DE SECOND ORDRE
!
! IN  ZIMAT : ADRESSE DE LA LISTE DE MATERIAU CODE
! IN  NMNBN : FORCE - BACKFORCE
! IN  NMPLAS : MOMENTS LIMITES DE PLASTICITE
! IN  NMDPLA : DERIVEES DES MOMENTS LIMITES DE PLASTICITE
! IN  NMDDPL : DERIVEES SECONDES DES MOMENTS LIMITES DE PLASTICITE
! IN  NMPROX : NMPROX > 0 : NBN DANS ZONE DE CRITIQUE
! IN  CDTG : MATRICE TANGENTE
! IN  DC1 : MATRICE ELASTIQUE + CONSTANTES DE PRAGER
! IN  DC2 : MATRICE ELASTIQUE + CONSTANTES DE PRAGER
!
! IN/OUT RPARA : LISTES DE PARAMETRES DE TYPE ENTIER
!
! OUT CNBN : NOUVELLE FORCE - BACKFORCE
! OUT CPLAS : NOUVEAUX MOMENTS LIMITES DE PLASTICITE
! OUT CIEF : NOUVEAU CIEF > 0 : NBN HORS DE LA ZONE DE DEFINITION DE MP
! OUT CDEPS : NOUVEL INCREMENT DE DEFORMATION DANS LE REPERE ORTHO
! OUT CIER : NOUVEAU CODE ERREUR
! OUT CDEPSP : NOUVEL INCREMENT DE DEF PLASTIQUE DANS LE REPERE ORTHO
!
    include 'asterfort/dclass.h'
    include 'asterfort/dfplgl.h'
    include 'asterfort/dfuuss.h'
    include 'asterfort/dracsy.h'
    include 'asterfort/fplass.h'
    include 'asterfort/hplass.h'
    include 'asterfort/matmul.h'
    include 'asterfort/nmnet2.h'
    include 'asterfort/r8inir.h'
    integer :: i, j, zimat, nmprox(2)
    integer :: nbroot, iran(8), cief, cier
!
    real(kind=8) :: nmnbn(6), nmplas(2, 3), nmdpla(2, 2)
    real(kind=8) :: nmddpl(2, 2), cnbn(6), cplas(2, 3)
    real(kind=8) :: czef, czeg, cdeps(6), cdtg(6, 6), cdepsp(6)
    real(kind=8) :: dc1(6, 6), dc2(6, 6), normm, tdeph(2, 6), h1(6, 6), h2(6, 6)
    real(kind=8) :: df1(6), df2(6), df(6, 2), tdf(2, 6), dfu1(6), dfu2(6)
    real(kind=8) :: dfu(6, 2)
    real(kind=8) :: ddeps(6), tddeps(1, 6), tdeph1(1, 6), tdeph2(1, 6), dcfu1(6)
    real(kind=8) :: dcfu2(6), tdcfu1(1, 6), tdcfu2(1, 6), auxd1(1), auxe1(1)
    real(kind=8) :: auxe2(1), auxf1(1), auxf2(1), a(2), b(2), c(2), d(2), e(2)
    real(kind=8) :: f(2)
    real(kind=8) :: aa(2), bb(2), cc(2), dd(2), ee(2), ff(2), lambda(2, 2), x(8)
    real(kind=8) :: y(8)
    real(kind=8) :: normxy(8), depsp2(6, 2)
    real(kind=8) :: cp(6), cp2(2, 6), rpara(3)
!
!
    czef = rpara(1)
    czeg = rpara(2)
    normm = rpara(3)
!
!     CALCUL LE GRADIENT DES CRITERES DE PLASICITE
    call dfplgl(nmnbn, nmplas, nmdpla, 1, df1)
    call dfplgl(nmnbn, nmplas, nmdpla, 2, df2)
!
    do 10, j = 1,6
    df(j,1) = df1(j)
    df(j,2) = df2(j)
    tdf(1,j) = df(j,1)
    tdf(2,j) = df(j,2)
    10 end do
!
!     CALUL DES DIRECTIONS DE L ECOULEMENT DES DEFORMATIONS PLASTIQUES
    call dfuuss(nmnbn, nmplas, nmdpla, nmprox, 1,&
                dfu1)
    call dfuuss(nmnbn, nmplas, nmdpla, nmprox, 2,&
                dfu2)
!
    do 20, j = 1,6
    dfu(j,1) = dfu1(j)
    dfu(j,2) = dfu2(j)
    20 end do
!
!     CALCUL LA MATRICE HESSIENNE DES CRITERES DE PLASTICITE
    call hplass(nmnbn, nmplas, nmdpla, nmddpl, 1,&
                h1)
    call hplass(nmnbn, nmplas, nmdpla, nmddpl, 2,&
                h2)
    call matmul(cdtg, cdeps, 6, 6, 1,&
                ddeps)
!
    do 30, j = 1,6
    tddeps(1,j) = ddeps(j)
    30 end do
!
    call matmul(tddeps, h1, 1, 6, 6,&
                tdeph1)
    call matmul(tddeps, h2, 1, 6, 6,&
                tdeph2)
!
    do 40, j = 1,6
    tdeph(1,j) = tdeph1(1,j)
    40 end do
!
    do 50, j = 1,6
    tdeph(2,j) = tdeph2(1,j)
    50 end do
!
    call matmul(dc1, dfu1, 6, 6, 1,&
                dcfu1)
    call matmul(dc2, dfu2, 6, 6, 1,&
                dcfu2)
!
    do 60, j = 1,6
    tdcfu1(1,j) = dcfu1(j)
    tdcfu2(1,j) = dcfu2(j)
    60 end do
!
    call matmul(h1, dcfu2, 6, 6, 1,&
                cp)
    call matmul(tdcfu1, cp, 1, 6, 1,&
                auxd1)
    call matmul(h1, dcfu1, 6, 6, 1,&
                cp)
    call matmul(tdcfu1, cp, 1, 6, 1,&
                auxe1)
    call matmul(h2, dcfu1, 6, 6, 1,&
                cp)
    call matmul(tdcfu1, cp, 1, 6, 1,&
                auxe2)
    call matmul(h1, dcfu2, 6, 6, 1,&
                cp)
    call matmul(tdcfu2, cp, 1, 6, 1,&
                auxf1)
    call matmul(h2, dcfu2, 6, 6, 1,&
                cp)
    call matmul(tdcfu2, cp, 1, 6, 1,&
                auxf2)
!
    do 70, j = 1,6
    cp2(1,j) = tdf(1,j) + 0.5d0*tdeph(1,j)
    cp2(2,j) = tdf(2,j) + 0.5d0*tdeph(2,j)
    70 end do
!
    call matmul(cp2, ddeps, 2, 6, 1,&
                a)
    a(1)=a(1)+fplass(nmnbn,nmplas,1)
    a(2)=a(2)+fplass(nmnbn,nmplas,2)
!
    do 80, j = 1,6
    cp2(1,j) = -tdf(1,j) - tdeph(1,j)
    cp2(2,j) = -tdf(2,j) - tdeph(2,j)
    80 end do
!
    call matmul(cp2, dcfu1, 2, 6, 1,&
                b)
    call matmul(cp2, dcfu2, 2, 6, 1,&
                c)
!
    d(1) = auxd1(1)
    d(2) = d(1)
    e(1) = 0.5d0*auxe1(1)
    e(2) = 0.5d0*auxe2(1)
    f(1) = 0.5d0*auxf1(1)
    f(2) = 0.5d0*auxf2(1)
!
    aa(1)=a(1)-a(2)
    aa(2)=a(1)+a(2)
    bb(1)=b(1)-b(2)
    bb(2)=b(1)+b(2)
    cc(1)=c(1)-c(2)
    cc(2)=c(1)+c(2)
    dd(1)=d(1)-d(2)
    dd(2)=d(1)+d(2)
    ee(1)=e(1)-e(2)
    ee(2)=e(1)+e(2)
    ff(1)=f(1)-f(2)
    ff(2)=f(1)+f(2)
!
!     EVALUE LES RACINES DU POLYNOME
    call dracsy(aa, bb, cc, dd, ee,&
                ff, nbroot, x, y)
!
    if (nbroot .eq. 0) then
        cier=4
        call r8inir(6, 0.0d0, cdepsp, 1)
        goto 100
    endif
!
    if (nbroot .gt. 1) then
        do 85, i=1,nbroot
        normxy(i)=abs(x(i))+abs(y(i))
85      continue
!
!     EVALUE L ORDRE DES RACINES DANS IRAN
        call dclass(nbroot, normxy, iran)
    endif
!
    call r8inir(2*2, 0.0d0, lambda, 1)
!
    do 94, i = 1, nbroot
    if ((x(iran(i)) .ge. 0.d0) .and. (y(iran(i)) .ge. 0.d0)) then
        lambda(1,1)=x(iran(i))
        lambda(2,2)=y(iran(i))
!
        if (abs(lambda(1,1)-lambda(2,2)) .le. 1.d-12*(lambda(1,1)+ lambda(2,2))) then
            lambda(2,2)=lambda(1,1)
        endif
!
        call matmul(dfu, lambda, 6, 2, 2,&
                    depsp2)
!
        do 90, j = 1,6
        cdepsp(j) = depsp2(j,1) + depsp2(j,2)
90      continue
!
!     CALCUL DE CNBN ET CDEPSP QUAND DEUX CRITERES PLAST SONT ACTIVES
        call nmnet2(zimat, nmnbn, cnbn, cplas, czef,&
                    czeg, cief, cdeps, cdtg, cier,&
                    dc1, dc2, depsp2, normm)
!
        if (cier .eq. 0) goto 100
    endif
    94 end do
!
    cier=3
    call r8inir(6, 0.0d0, cdepsp, 1)
!
100  continue
!
    rpara(1) = czef
    rpara(2) = czeg
    rpara(3) = normm
!
end subroutine
