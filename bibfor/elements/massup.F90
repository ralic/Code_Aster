subroutine massup(option, ndim, dlns, nno, nnos,&
                  mate, phenom, npg, ipoids, idfde,&
                  geom, vff1, imatuu, icodre, igeom,&
                  ivf)
!
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
! aslint: disable=W1306
    implicit none
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DE LA MATRICE DE MASSE
!                          POUR ELEMENTS DONT LES NOEUDS SOMMETS
!                          ONT + DE DDL QUE LES DEPLACEMENTS
!    - ARGUMENTS:
!        DONNEES:   NDIM   -->  DIMENSION DU PROBLEME
!                   DLNS   -->  DEGRES DE LIBERTE AU NOEUD SOMMET
!                   NNO    -->  NOMBRE DE NOEUD
!                   NNOS   -->  NOMBRE DE NOEUD SOMMET
!                   MATE   -->  MATERIAU
!                   PHENOM -->  PHENOMENE
!                   NPG    -->  NOMBRE DE POIDS DE GAUSS
!                   IPOIDS -->  POSITION DES POIDS DE GAUSS DANS ZR
!                   IDFDE  -->
!                   GEOM   -->  COORDONNEES DE L ELEMENT
!                   VFF1   -->  VALEUR DES FONCTIONS DE FORME AUX PG
!                   IMATUU -->  POSITION DE LA MATRICE DE MASSE DANS ZR
!        RESULTATS: ICODRE -->  CODE RETOUR
! ......................................................................
!
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/dfdm2j.h'
    include 'asterfort/dfdm3j.h'
    include 'asterfort/lteatt.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/rccoma.h'
    include 'asterfort/rcvalb.h'
!
    integer :: i, j, k, l, kpg, ik, ijkl, dlns
    integer :: ndim, nno, nnos, npg, mate, ipoids, idfde, imatuu
    integer :: n1, n2, j2, k2, idiag
    integer :: igeom, ivf, i2, idec, spt
!
    real(kind=8) :: vff1(nno, npg), geom(ndim, nno), rho, r
    real(kind=8) :: a(ndim, ndim, nno, nno), matv(ndim*nno*(ndim*nno+1)/2)
    real(kind=8) :: poids, wgt, trace, alpha
    character(len=8) :: fami, poum
    character(len=16) :: phenom
    character(len=16) :: option
    integer :: icodre
!
!
    idec = dlns - ndim
!
    call rccoma(mate, 'ELAS', 1, phenom, icodre)
!
    call r8inir(ndim*ndim*nno*nno, 0.d0, a, 1)
    call r8inir(ndim*nno*(ndim*nno+1)/2, 0.d0, matv, 1)
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
!
    call rcvalb(fami, kpg, spt, poum, mate,&
                ' ', phenom, 0, ' ', 0.d0,&
                1, 'RHO', rho, icodre, 1)
!
    if (ndim .eq. 2) then
        do 90 kpg = 1, npg
            k = (kpg-1)*nno
            call dfdm2j(nno, kpg, idfde, geom, poids)
            poids = abs(poids) * zr(ipoids+kpg-1)
!
            if (lteatt(' ','AXIS','OUI')) then
                r = 0.0d0
                do 20 i = 1, nno
                    r = r + zr(igeom+2*(i-1))*zr(ivf+k+i-1)
20              continue
                poids = poids*r
            endif
!
            do 80 i = 1, nno
                do 70 j = 1, i
                    a(1,1,i,j) = a(1,1,i,j)+rho*poids*vff1(i,kpg)* vff1(j,kpg)
                    a(2,2,i,j) = a(1,1,i,j)
70              continue
80          continue
90      continue
    else if (ndim.eq.3) then
        do 120 kpg = 1, npg
            call dfdm3j(nno, kpg, idfde, geom, poids)
            poids = abs(poids) * zr(ipoids+kpg-1)
!
            do 110 i = 1, nno
                do 100 j = 1, i
                    a(1,1,i,j) = a(1,1,i,j)+rho*poids*vff1(i,kpg)* vff1(j,kpg)
                    a(2,2,i,j) = a(1,1,i,j)
                    a(3,3,i,j) = a(1,1,i,j)
100              continue
110          continue
120      continue
    else
! - OPTION DE CALCUL INVALIDE
        call assert(.false.)
    endif
!
! - PASSAGE DU STOCKAGE RECTANGULAIRE (A) AU STOCKAGE TRIANGULAIRE (ZR)
    do 150 k = 1, ndim
        do 140 l = 1, ndim
            do 130 i = 1, nno
                ik = ((ndim*i+k-ndim-1)* (ndim*i+k-ndim))/2
                do 125 j = 1, i
                    ijkl = ik + ndim* (j-1) + l
                    matv(ijkl) = a(k,l,i,j)
125              continue
130          continue
140      continue
150  end do
!
    if (option .eq. 'MASS_MECA') then
        do 401 k = 1, nno
            do 402 n1 = 1, ndim
                i = ndim*k+n1-ndim
                if (k .le. nnos) then
                    i2 = i+idec*(k-1)
                else
                    i2 = i+idec*nnos
                endif
                do 403 l = 1, nno
                    do 404 n2 = 1, ndim
                        j = ndim*l+n2-ndim
                        if (j .gt. i) goto 405
                        if (l .le. nnos) then
                            j2 = j+idec*(l-1)
                        else
                            j2 = j+idec*nnos
                        endif
                        zr(imatuu+i2*(i2-1)/2+j2-1) = matv(i*(i-1)/2+ j)
404                  continue
403              continue
405              continue
402          continue
401      continue
        elseif (option.eq.'MASS_MECA_DIAG' .or.&
     &        option.eq.'MASS_MECA_EXPLI' ) then
!
! - CALCUL DE LA MASSE DE L'ELEMENT
        wgt = a(1,1,1,1)
        do 170 i = 2, nno
            do 160 j = 1, i - 1
                wgt = wgt + 2*a(1,1,i,j)
160          continue
            wgt = wgt + a(1,1,i,i)
170      continue
!
! - CALCUL DE LA TRACE EN TRANSLATION SUIVANT X
        trace = 0.d0
        do 180 i = 1, nno
            trace = trace + a(1,1,i,i)
180      continue
!
! - CALCUL DU FACTEUR DE DIAGONALISATION
        alpha = wgt/trace
!
! - PASSAGE DU STOCKAGE RECTANGULAIRE (A) AU STOCKAGE TRIANGULAIRE (ZR)
        k = 0
        do 200 j = 1, nno
            do 190 i = 1, 3
                k = k + 1
                if (idec .eq. 0) then
                    idiag = k* (k+1)/2
                else
                    if (j .le. nnos) then
                        k2 = k+idec*(j-1)
                    else
                        k2 = k+idec*nnos
                    endif
                    idiag = k2* (k2+1)/2
                endif
                zr(imatuu+idiag-1) = a(i,i,j,j)*alpha
190          continue
200      continue
    else
! - OPTION DE CALCUL INVALIDE
        call assert(.false.)
    endif
!
end subroutine
