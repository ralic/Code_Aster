subroutine mmnewd(alias, nno, ndim, coorma, coorpt,&
                  itemax, epsmax, dir, ksi1, ksi2,&
                  tau1, tau2, niverr)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "asterfort/assert.h"
#include "asterfort/matini.h"
#include "asterfort/mgauss.h"
#include "asterfort/mmfonf.h"
#include "asterfort/mmtang.h"
    character(len=8) :: alias
    integer :: nno
    integer :: ndim
    real(kind=8) :: coorma(27)
    real(kind=8) :: coorpt(3)
    real(kind=8) :: dir(3)
    real(kind=8) :: ksi1, ksi2
    real(kind=8) :: tau1(3), tau2(3)
    integer :: niverr
    integer :: itemax
    real(kind=8) :: epsmax
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (TOUTES METHODES - APPARIEMENT)
!
! ALGORITHME DE NEWTON POUR CALCULER LA PROJECTION D'UN POINT SUR UNE
! MAILLE - VERSION AVEC DIRECTION DE RECHERCHE IMPOSEE
!
! ----------------------------------------------------------------------
!
! IN  ALIAS  : TYPE DE MAILLE
! IN  NNO    : NOMBRE DE NOEUD SUR LA MAILLE
! IN  NDIM   : DIMENSION DE LA MAILLE (2 OU 3)
! IN  COORMA : COORDONNEES DES NOEUDS DE LA MAILLE
! IN  COORPT : COORDONNEES DU NOEUD A PROJETER SUR LA MAILLE
! IN  ITEMAX : NOMBRE MAXI D'ITERATIONS DE NEWTON POUR LA PROJECTION
! IN  EPSMAX : RESIDU POUR CONVERGENCE DE NEWTON POUR LA PROJECTION
! IN  DIR    : DIRECTION D'APPARIEMENT
! OUT KSI1   : PREMIERE COORDONNEE PARAMETRIQUE DU POINT PROJETE
! OUT KSI2   : SECONDE COORDONNEE PARAMETRIQUE DU POINT PROJETE
! OUT TAU1   : PREMIER VECTEUR TANGENT EN KSI1,KSI2
! OUT TAU2   : SECOND VECTEUR TANGENT EN KSI1,KSI2
! OUT NIVERR : RETOURNE UN CODE ERREUR
!                0  TOUT VA BIEN
!                1  ECHEC NEWTON
!
! ----------------------------------------------------------------------
!
    real(kind=8) :: ff(9), dff(2, 9), ddff(3, 9)
    real(kind=8) :: vec1(3)
    real(kind=8) :: matri3(3, 3), matri2(2, 2)
    real(kind=8) :: test, epsrel, eps, alpha
    real(kind=8) :: dksi(3), r8bid
    integer :: ino, idim, iter, iret
    real(kind=8) :: zero
    parameter    (zero=0.d0)
!
! ----------------------------------------------------------------------
!
! --- VERIF CARACTERISTIQUES DE LA MAILLE
!
    if (nno .gt. 9) ASSERT(.false.)
    if (ndim .gt. 3) ASSERT(.false.)
    if (ndim .le. 1) ASSERT(.false.)
!
! --- POINT DE DEPART
!
    niverr = 0
    ksi1 = zero
    ksi2 = zero
    iter = 0
    alpha = 1.d0
    epsrel = epsmax
!
! --- DEBUT DE LA BOUCLE
!
20  continue
!
! --- INITIALISATIONS
!
    do 10 idim = 1, 3
        vec1(idim) = zero
        tau1(idim) = zero
        tau2(idim) = zero
        dksi(idim) = zero
10  continue
    call matini(2, 2, zero, matri2)
    call matini(3, 3, zero, matri3)
!
! --- CALCUL DES FONCTIONS DE FORME ET DE LEUR DERIVEES EN UN POINT
! --- DANS LA MAILLE
!
    call mmfonf(ndim, nno, alias, ksi1, ksi2,&
                ff, dff, ddff)
!
! --- CALCUL DU VECTEUR POSITION DU POINT COURANT SUR LA MAILLE
!
    do 40 idim = 1, ndim
        do 30 ino = 1, nno
            vec1(idim) = coorma(3*(ino-1)+idim)*ff(ino) + vec1(idim)
30      continue
40  continue
!
! --- CALCUL DES TANGENTES
!
    call mmtang(ndim, nno, coorma, dff, tau1,&
                tau2)
!
! --- CALCUL DE LA QUANTITE A MINIMISER
!
    do 35 idim = 1, ndim
        vec1(idim) = coorpt(idim) - vec1(idim)
35  continue
!
! --- CALCUL DU RESIDU
!
    dksi(1) = vec1(1) - alpha*dir(1)
    dksi(2) = vec1(2) - alpha*dir(2)
    if (ndim .eq. 3) then
        dksi(3) = vec1(3) - alpha*dir(3)
    endif
!
! --- CALCUL DE LA MATRICE TANGENTE
!
    if (ndim .eq. 2) then
        do 23 idim = 1, ndim
            matri2(idim,1)= tau1(idim)
            matri2(idim,2)= dir(idim)
23      continue
    else if (ndim.eq.3) then
        do 21 idim = 1, ndim
            matri3(idim,1)= tau1(idim)
            matri3(idim,2)= tau2(idim)
            matri3(idim,3)= dir(idim)
21      continue
    else
        ASSERT(.false.)
    endif
!
! --- RESOLUTION K.dU=RESIDU
!
    if (ndim .eq. 2) then
        call mgauss('NCVP', matri2, dksi, 2, 2,&
                    1, r8bid, iret)
        if (iret .gt. 0) then
            niverr = 1
            goto 999
        endif
    else if (ndim.eq.3) then
        call mgauss('NCVP', matri3, dksi, 3, 3,&
                    1, r8bid, iret)
        if (iret .gt. 0) then
            niverr = 1
            goto 999
        endif
    else
        ASSERT(.false.)
    endif
!
! --- ACTUALISATION
!
    if (ndim .eq. 2) then
        ksi1 = ksi1 + dksi(1)
        ksi2 = zero
        alpha = alpha + dksi(2)
    else if (ndim.eq.3) then
        ksi1 = ksi1 + dksi(1)
        ksi2 = ksi2 + dksi(2)
        alpha = alpha + dksi(3)
    else
        ASSERT(.false.)
    endif
    iter = iter + 1
!
! --- CALCUL POUR LE TEST DE CONVERGENCE
!
    eps = epsrel
    if (ndim .eq. 2) then
        test = sqrt(dksi(1)*dksi(1)+dksi(2)*dksi(2))
    else if (ndim.eq.3) then
        test = sqrt(dksi(1)*dksi(1)+dksi(2)*dksi(2)+dksi(3)*dksi(3))
    endif
!
! --- EVALUATION DE LA CONVERGENCE
!
    if ((test.gt.eps) .and. (iter.lt.itemax)) then
        goto 20
    else if ((iter.ge.itemax).and.(test.gt.eps)) then
        niverr = 1
    endif
!
! --- FIN DE LA BOUCLE
!
!
!
999  continue
!
end subroutine
