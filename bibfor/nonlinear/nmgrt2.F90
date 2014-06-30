subroutine nmgrt2(nno, poids, kpg, vff, def,&
                  pff, option, axi, r, resi,&
                  rigi, dsidep, sign, sigma, matsym,&
                  matuu, vectu)
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
!
    integer :: nno, kk, kkd, n, i, m, j, j1, kl, nmax, kpg
    character(len=16) :: option
    real(kind=8) :: pff(4, nno, nno), def(4, nno, 2), dsidep(6, 6), poids
    real(kind=8) :: vectu(*)
    real(kind=8) :: sigma(6), sign(6), matuu(*), vff(*)
    real(kind=8) :: tmp1, tmp2, sigg(6), sig(6), r
    logical(kind=1) :: matsym, axi, resi, rigi
!
!.......................................................................
!     BUT:  CALCUL DE LA MATRICE TANGENTE EN CONFIGURATION LAGRANGIENNE
!           OPTIONS RIGI_MECA_TANG ET FULL_MECA
!.......................................................................
! IN  NNO     : NOMBRE DE NOEUDS DE L'ELEMENT
! IN  POIDS   : POIDS DES POINTS DE GAUSS
! IN  KPG     : NUMERO DU POINT DE GAUSS
! IN  VFF     : VALEUR  DES FONCTIONS DE FORME
! IN  DEF     : PRODUIT DE F PAR LA DERIVEE DES FONCTIONS DE FORME
! IN  PFF     : PRODUIT DES FONCTIONS DE FORME
! IN  OPTION  : OPTION DE CALCUL
! IN  AXI     : .TRUE. SI AXIS
! IN  R       : RAYON DU POINT DE GAUSS COURANT (EN AXI)
! IN  DSIDEP  : OPERATEUR TANGENT ISSU DU COMPORTEMENT
! IN  SIGN    : CONTRAINTES PK2 A L'INSTANT PRECEDENT (AVEC RAC2)
! IN  SIGMA   : CONTRAINTES PK2 A L'INSTANT ACTUEL    (AVEC RAC2)
! IN  MATSYM  : VRAI SI LA MATRICE DE RIGIDITE EST SYMETRIQUE
! OUT MATUU   : MATRICE DE RIGIDITE PROFIL (RIGI_MECA_TANG ET FULL_MECA)
! OUT VECTU   : VECTEUR DES FORCES INTERIEURES (RAPH_MECA ET FULL_MECA)
!.......................................................................
!
!
    if (rigi) then
!
        if (option(1:4) .eq. 'RIGI') then
!
            sigg(1)=sign(1)
            sigg(2)=sign(2)
            sigg(3)=sign(3)
            sigg(4)=sign(4)
!
        else
!
            sigg(1)=sigma(1)
            sigg(2)=sigma(2)
            sigg(3)=sigma(3)
            sigg(4)=sigma(4)
!
        endif
!
        do 160 n = 1, nno
!
            do 150 i = 1, 2
!
                do 151,kl=1,4
!
                sig(kl)=0.d0
                sig(kl)=sig(kl)+def(1,n,i)*dsidep(1,kl)
                sig(kl)=sig(kl)+def(2,n,i)*dsidep(2,kl)
                sig(kl)=sig(kl)+def(3,n,i)*dsidep(3,kl)
                sig(kl)=sig(kl)+def(4,n,i)*dsidep(4,kl)
!
151              continue
!
                if (matsym) then
                    nmax = n
                else
                    nmax = nno
                endif
!
                do 140 j = 1, 2
!
                    do 130 m = 1, nmax
!
!                    RIGIDITE GEOMETRIQUE
!
                        tmp1 = 0.d0
!
                        if (i .eq. j) then
!
                            tmp1 = pff(1,n,m)*sigg(1) + pff(2,n,m)* sigg(2) + pff(3,n,m)*sigg(3) &
                                   &+ pff(4,n,m)* sigg(4)
!
!                       TERME DE CORRECTION AXISYMETRIQUE
!
                            if (axi .and. i .eq. 1) then
                                tmp1=tmp1+vff(n+(kpg-1)*nno)* vff(m+(&
                                kpg-1)*nno)/(r*r)*sigg(3)
                            endif
!
                        endif
!
!                    RIGIDITE DE COMPORTEMENT
!
                        tmp2=0.d0
                        tmp2=tmp2+sig(1)*def(1,m,j)
                        tmp2=tmp2+sig(2)*def(2,m,j)
                        tmp2=tmp2+sig(3)*def(3,m,j)
                        tmp2=tmp2+sig(4)*def(4,m,j)
!
                        if (matsym) then
!
!                        STOCKAGE EN TENANT COMPTE DE LA SYMETRIE
!
                            if (m .eq. n) then
                                j1 = i
                            else
                                j1 = 2
                            endif
!
                            if (j .le. j1) then
                                kkd = (2*(n-1)+i-1) * (2*(n-1)+i)/2
                                kk = kkd + 2*(m-1)+j
                                matuu(kk) = matuu(kk) + (tmp1+tmp2)* poids
                            endif
!
                        else
!
!                       STOCKAGE SANS SYMETRIE
!
                            kk = 2*nno*(2*(n-1)+i-1) + 2*(m-1)+j
                            matuu(kk) = matuu(kk) + (tmp1+tmp2)*poids
!
                        endif
!
130                  continue
!
140              continue
!
150          continue
!
160      continue
!
    endif
!
    if (resi) then
!
        do 230 n = 1, nno
            do 220 i = 1, 2
                do 210 kl = 1, 4
                    vectu(2*(n-1)+i)=vectu(2*(n-1)+i)+ def(kl,n,i)*&
                    sigma(kl)*poids
210              continue
220          continue
230      continue
!
    endif
!
end subroutine
