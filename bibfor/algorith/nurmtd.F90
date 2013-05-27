subroutine nurmtd(ndim, nno1, nno2, npg, iw,&
                  vff1, vff2, ivf1, idff1, vu,&
                  vp, typmod, igeom, mate, mini,&
                  matr)
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
! person_in_charge: sebastien.fayolle at edf.fr
! TOLE CRS_1404
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/calkbb.h'
    include 'asterfort/calkbp.h'
    include 'asterfort/calkce.h'
    include 'asterfort/dfdmip.h'
    include 'asterfort/ortrep.h'
    include 'asterfort/pmat.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/tanbul.h'
    logical :: mini
    integer :: ndim, nno1, nno2, npg, iw, idff1
    integer :: mate
    integer :: vu(3, 27), vp(27)
    integer :: ivf1, igeom
    real(kind=8) :: vff1(nno1, npg), vff2(nno2, npg)
    character(len=8) :: typmod(*)
    real(kind=8) :: matr(*)
!
!-----------------------------------------------------------------------
!          CALCUL DES FORCES NODALES POUR LES ELEMENTS
!          INCOMPRESSIBLES POUR LES PETITES DEFORMATIONS
!          3D/D_PLAN/AXIS
!          ROUTINE APPELEE PAR TE0596
!-----------------------------------------------------------------------
! IN  MINI    : STABILISATION BULLE - MINI ELEMENT
! IN  NDIM    : DIMENSION DE L'ESPACE
! IN  NNO1    : NOMBRE DE NOEUDS DE L'ELEMENT LIES AUX DEPLACEMENTS
! IN  NNO2    : NOMBRE DE NOEUDS DE L'ELEMENT LIES A LA PRESSION
! IN  NPG     : NOMBRE DE POINTS DE GAUSS
! IN  IW      : POIDS DES POINTS DE GAUSS
! IN  VFF1    : VALEUR  DES FONCTIONS DE FORME LIES AUX DEPLACEMENTS
! IN  VFF2    : VALEUR  DES FONCTIONS DE FORME LIES A LA PRESSION
! IN  IDFF1   : DERIVEE DES FONCTIONS DE FORME ELEMENT DE REFERENCE
! IN  VU      : TABLEAU DES INDICES DES DDL DE DEPLACEMENTS
! IN  VP      : TABLEAU DES INDICES DES DDL DE PRESSION
! IN  GEOMI   : COORDONEES DES NOEUDS
! IN  TYPMOD  : TYPE DE MODELISATION
! IN  MATE    : MATERIAU CODE
! OUT MATR    : MATRICE DE RIGIDITE
!-----------------------------------------------------------------------
!
    logical :: axi
    integer :: g
    integer :: ia, na, sa, ib, nb, sb, ja, jb
    integer :: os, kk
    integer :: vuiana, vpsa
    integer :: idim
    integer :: idecpg, idecno
    real(kind=8) :: rac2
    real(kind=8) :: r, w, dff1(nno1, ndim)
    real(kind=8) :: dsidep(2*ndim, 2*ndim)
    real(kind=8) :: def(2*ndim, nno1, ndim), deftr(nno1, ndim)
    real(kind=8) :: ddev(2*ndim, 2*ndim), devd(2*ndim, 2*ndim)
    real(kind=8) :: dddev(2*ndim, 2*ndim)
    real(kind=8) :: xyzgau(3), bary(3), repere(7)
    real(kind=8) :: t1
    real(kind=8) :: idev(6, 6), idev2(4, 4)
    real(kind=8) :: alpha, trepst
    real(kind=8) :: presm(nno2), presd(nno2)
    real(kind=8) :: kbb(ndim, ndim), kbp(ndim, nno2)
    real(kind=8) :: kce(nno2, nno2), rce(nno2)
    real(kind=8) :: fm(3, 3)
    character(len=16) :: compor, option
!
    data         fm   / 1.d0, 0.d0, 0.d0,&
     &                    0.d0, 1.d0, 0.d0,&
     &                    0.d0, 0.d0, 1.d0/
    data         idev2/ 2.d0,-1.d0,-1.d0, 0.d0,&
     &                   -1.d0, 2.d0,-1.d0, 0.d0,&
     &                   -1.d0,-1.d0, 2.d0, 0.d0,&
     &                    0.d0, 0.d0, 0.d0, 3.d0/
    data         idev / 2.d0,-1.d0,-1.d0, 0.d0, 0.d0, 0.d0,&
     &                   -1.d0, 2.d0,-1.d0, 0.d0, 0.d0, 0.d0,&
     &                   -1.d0,-1.d0, 2.d0, 0.d0, 0.d0, 0.d0,&
     &                    0.d0, 0.d0, 0.d0, 3.d0, 0.d0, 0.d0,&
     &                    0.d0, 0.d0, 0.d0, 0.d0, 3.d0, 0.d0,&
     &                    0.d0, 0.d0, 0.d0, 0.d0, 0.d0, 3.d0/
!-----------------------------------------------------------------------
!
! - INITIALISATION
    axi = typmod(1).eq.'AXIS'
    rac2 = sqrt(2.d0)
    option = 'RIGI_MECA       '
    compor = 'ELAS            '
!
    call r8inir(nno2, 0.d0, presm, 1)
    call r8inir(nno2, 0.d0, presd, 1)
!
! - RECUPERATION  DES DONNEEES RELATIVES AU REPERE D'ORTHOTROPIE
! - COORDONNEES DU BARYCENTRE ( POUR LE REPRE CYLINDRIQUE )
    bary(1) = 0.d0
    bary(2) = 0.d0
    bary(3) = 0.d0
    do 150 ia = 1, nno1
        do 40 idim = 1, ndim
            bary(idim) = bary(idim)+zr(igeom+idim+ndim*(ia-1)-1)/nno1
40      continue
150  end do
    call ortrep(mate, ndim, bary, repere)
!
! - CALCUL POUR CHAQUE POINT DE GAUSS
    do 1000 g = 1, npg
        idecpg = nno1* (g-1) - 1
!
! - COORDONNEES AU POINT D'INTEGRATION COURANT
        xyzgau(1) = 0.d0
        xyzgau(2) = 0.d0
        xyzgau(3) = 0.d0
        if (ndim .eq. 3) then
            do 31 ia = 1, nno1
                idecno = 3* (ia-1) - 1
                xyzgau(1) = xyzgau(1)+zr(ivf1+ia+idecpg)*zr(igeom+1+ idecno)
                xyzgau(2) = xyzgau(2)+zr(ivf1+ia+idecpg)*zr(igeom+2+ idecno)
                xyzgau(3) = xyzgau(3)+zr(ivf1+ia+idecpg)*zr(igeom+3+ idecno)
31          continue
        endif
!
! - CALCUL DES ELEMENTS GEOMETRIQUES
! - CALCUL DE DFDI,F,EPS,R(EN AXI) ET POIDS
        call dfdmip(ndim, nno1, axi, zr(igeom), g,&
                    iw, vff1(1, g), idff1, r, w,&
                    dff1)
!
! - CALCUL DE LA MATRICE B EPS_ij=B_ijkl U_kl
        if (ndim .eq. 2) then
            do 35 na = 1, nno1
                do 45 ia = 1, ndim
                    def(1,na,ia)= fm(ia,1)*dff1(na,1)
                    def(2,na,ia)= fm(ia,2)*dff1(na,2)
                    def(3,na,ia)= 0.d0
                    def(4,na,ia)=(fm(ia,1)*dff1(na,2)+fm(ia,2)*dff1(&
                    na,1))/rac2
45              continue
35          continue
!
! - TERME DE CORRECTION (3,3) AXI QUI PORTE EN FAIT SUR LE DDL 1
            if (axi) then
                do 47 na = 1, nno1
                    def(3,na,1) = fm(3,3)*vff1(na,g)/r
47              continue
            endif
        else
            do 36 na = 1, nno1
                do 46 ia = 1, ndim
                    def(1,na,ia)= fm(ia,1)*dff1(na,1)
                    def(2,na,ia)= fm(ia,2)*dff1(na,2)
                    def(3,na,ia)= fm(ia,3)*dff1(na,3)
                    def(4,na,ia)=(fm(ia,1)*dff1(na,2)+fm(ia,2)*dff1(&
                    na,1))/rac2
                    def(5,na,ia)=(fm(ia,1)*dff1(na,3)+fm(ia,3)*dff1(&
                    na,1))/rac2
                    def(6,na,ia)=(fm(ia,2)*dff1(na,3)+fm(ia,3)*dff1(&
                    na,2))/rac2
46              continue
36          continue
        endif
!
! - CALCUL DE TRACE(B)
        do 50 na = 1, nno1
            do 49 ia = 1, ndim
                deftr(na,ia) = def(1,na,ia) + def(2,na,ia) + def(3,na, ia)
49          continue
50      continue
!
! - CALCUL DE LA MATRICE D'ELASTICITE BULLE
        call tanbul(option, ndim, g, mate, compor,&
                    .false., .true., alpha, dsidep, trepst)
!
! - CALCUL DE LA MATRICE DE CONDENSATION STATIQUE
        if (mini) then
            call calkbb(nno1, ndim, w, def, dsidep,&
                        kbb)
            call calkbp(nno2, ndim, w, dff1, kbp)
            call calkce(nno1, ndim, kbp, kbb, presm,&
                        presd, kce, rce)
        else
            call r8inir(nno2*nno2, 0.d0, kce, 1)
        endif
!
        if (ndim .eq. 3) then
            call pmat(6, idev/3.d0, dsidep, devd)
            call pmat(6, dsidep, idev/3.d0, ddev)
            call pmat(6, devd, idev/3.d0, dddev)
        else
            call pmat(4, idev2/3.d0, dsidep, devd)
            call pmat(4, dsidep, idev2/3.d0, ddev)
            call pmat(4, devd, idev2/3.d0, dddev)
        endif
!
! - CALCUL DE LA MATRICE DE RIGIDITE
! - TERME K:UX
        do 400 na = 1, nno1
            do 410 ia = 1, ndim
                vuiana = vu(ia,na)
                os = (vuiana-1)*vuiana/2
!
! - TERME K:UU      KUU(NDIM,NNO1,NDIM,NNO1)
                do 420 nb = 1, nno1
                    do 430 ib = 1, ndim
                        if (vu(ib,nb) .le. vuiana) then
                            kk = os+vu(ib,nb)
                            t1 = 0.d0
                            do 440 ja = 1, 2*ndim
                                do 450 jb = 1, 2*ndim
                                    t1 = t1 + def(ja,na,ia)*dddev(ja, jb)*def(jb,nb,ib)
450                              continue
440                          continue
                            matr(kk) = matr(kk) + w*t1
                        endif
430                  continue
420              continue
!
! - TERME K:UP      KUP(NDIM,NNO1,NNO2)
                do 490 sb = 1, nno2
                    if (vp(sb) .lt. vuiana) then
                        kk = os + vp(sb)
                        t1 = deftr(na,ia)*vff2(sb,g)
                        matr(kk) = matr(kk) + w*t1
                    endif
490              continue
410          continue
400      continue
!
! - TERME K:PX
        do 600 sa = 1, nno2
            vpsa = vp(sa)
            os = (vpsa-1)*vpsa/2
!
! - TERME K:PU      KPU(NDIM,NNO2,NNO1)
            do 610 nb = 1, nno1
                do 620 ib = 1, ndim
                    if (vu(ib,nb) .lt. vpsa) then
                        kk = os + vu(ib,nb)
                        t1 = vff2(sa,g)*deftr(nb,ib)
                        matr(kk) = matr(kk) + w*t1
                    endif
620              continue
610          continue
!
! - TERME K:PP      KPP(NNO2,NNO2)
            do 640 sb = 1, nno2
                if (vp(sb) .le. vpsa) then
                    kk = os + vp(sb)
                    t1 = - vff2(sa,g)*vff2(sb,g)*alpha
                    matr(kk) = matr(kk) + w*t1 - kce(sa,sb)
                endif
640          continue
600      continue
1000  end do
end subroutine
