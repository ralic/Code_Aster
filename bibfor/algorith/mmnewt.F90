subroutine mmnewt(alias, nno, ndim, coorma, coorpt,&
                  itemax, epsmax, ksi1, ksi2, tau1,&
                  tau2, niverr)
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
    include 'asterc/r8gaem.h'
    include 'asterfort/assert.h'
    include 'asterfort/mmfonf.h'
    include 'asterfort/mmreli.h'
    include 'asterfort/mmtang.h'
    character(len=8) :: alias
    integer :: nno
    integer :: ndim
    real(kind=8) :: coorma(27)
    real(kind=8) :: coorpt(3)
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
! MAILLE - VERSION GENERALE
!
! ----------------------------------------------------------------------
!
!
! IN  ALIAS  : TYPE DE MAILLE
! IN  NNO    : NOMBRE DE NOEUD SUR LA MAILLE
! IN  NDIM   : DIMENSION DE LA MAILLE (2 OU 3)
! IN  COORMA : COORDONNEES DES NOEUDS DE LA MAILLE
! IN  COORPT : COORDONNEES DU NOEUD A PROJETER SUR LA MAILLE
! IN  ITEMAX : NOMBRE MAXI D'ITERATIONS DE NEWTON POUR LA PROJECTION
! IN  EPSMAX : RESIDU POUR CONVERGENCE DE NEWTON POUR LA PROJECTION
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
    integer :: ino, idim, iter
    real(kind=8) :: ff(9), dff(2, 9), ddff(3, 9)
    real(kind=8) :: vec1(3)
    real(kind=8) :: matrix(2, 2), par11(3), par12(3), par22(3)
    real(kind=8) :: residu(2), eps
    real(kind=8) :: dksi1, dksi2
    real(kind=8) :: det, test, epsrel, epsabs, refe
    real(kind=8) :: alpha
    real(kind=8) :: zero
    parameter   (zero=0.d0)
    real(kind=8) :: dist, dmin, ksi1m, ksi2m
!
! ----------------------------------------------------------------------
!
!
! --- VERIF CARACTERISTIQUES DE LA MAILLE
!
    if (nno .gt. 9) call assert(.false.)
    if (ndim .gt. 3) call assert(.false.)
    if (ndim .le. 1) call assert(.false.)
!
! --- POINT DE DEPART
!
    niverr = 0
    ksi1 = zero
    ksi2 = zero
    iter = 0
    epsabs = epsmax/100.d0
    epsrel = epsmax
    alpha = 1.d0
    dmin = r8gaem()
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
        par11(idim) = zero
        par12(idim) = zero
        par22(idim) = zero
10  continue
    residu(1) = zero
    residu(2) = zero
    matrix(1,1) = zero
    matrix(1,2) = zero
    matrix(2,1) = zero
    matrix(2,2) = zero
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
    dist = sqrt(vec1(1)*vec1(1)+ vec1(2)*vec1(2)+ vec1(3)*vec1(3))
!
! --- CALCUL DU RESIDU
!
    residu(1) = vec1(1)*tau1(1) + vec1(2)*tau1(2) + vec1(3)*tau1(3)
    if (ndim .eq. 3) then
        residu(2) = vec1(1)*tau2(1) + vec1(2)*tau2(2) + vec1(3)*tau2( 3)
    endif
!
! --- CALCUL DES COURBURES LOCALES
!
    do 42 idim = 1, ndim
        do 32 ino = 1, nno
            par11(idim) = coorma(3*(ino-1)+idim)*ddff(1,ino) + par11(idim)
            if (ndim .eq. 3) then
                par22(idim) = coorma(3*(ino-1)+idim)*ddff(2,ino) + par22(idim)
                par12(idim) = coorma(3*(ino-1)+idim)*ddff(3,ino) + par12(idim)
            endif
32      continue
42  continue
!
! --- CALCUL DE LA MATRICE TANGENTE
!
    do 60 idim = 1, ndim
        matrix(1,1) = -tau1(idim)*tau1(idim) + par11(idim)*vec1(idim) + matrix(1,1)
        if (ndim .eq. 3) then
            matrix(1,2) = -tau2(idim)*tau1(idim) + par12(idim)*vec1( idim) + matrix(1,2)
            matrix(2,1) = -tau1(idim)*tau2(idim) + par12(idim)*vec1( idim) + matrix(2,1)
            matrix(2,2) = -tau2(idim)*tau2(idim) + par22(idim)*vec1( idim) + matrix(2,2)
        endif
60  continue
!
! --- RESOLUTION K.dU=RESIDU
!
    if (ndim .eq. 2) then
        det = matrix(1,1)
    else if (ndim.eq.3) then
        det = matrix(1,1)*matrix(2,2) - matrix(1,2)*matrix(2,1)
    endif
!
    if (det .eq. 0.d0) then
        niverr = 1
        goto 999
    endif
!
    if (ndim .eq. 2) then
        dksi1 = -residu(1)/matrix(1,1)
        dksi2 = 0.d0
    else if (ndim.eq.3) then
        dksi1 = (matrix(2,2)* (-residu(1))-matrix(1,2)* (-residu(2)))/ det
        dksi2 = (matrix(1,1)* (-residu(2))-matrix(2,1)* (-residu(1)))/ det
    endif
!
! --- RECHERCHE LINEAIRE
!
    call mmreli(alias, nno, ndim, coorma, coorpt,&
                ksi1, ksi2, dksi1, dksi2, alpha)
!
! --- ACTUALISATION
!
    ksi1 = ksi1 + alpha*dksi1
    ksi2 = ksi2 + alpha*dksi2
!
    iter = iter + 1
    if (dist .le. dmin) then
        dmin = dist
        ksi1m = ksi1
        ksi2m = ksi2
    endif
!
! --- CALCUL DE LA REFERENCE POUR TEST DEPLACEMENTS
!
    refe = (ksi1*ksi1+ksi2*ksi2)
    if (refe .le. epsrel) then
        refe = 1.d0
        eps = epsabs
    else
        eps = epsrel
    endif
!
! --- CALCUL POUR LE TEST DE CONVERGENCE
!
    test = sqrt(dksi1*dksi1+dksi2*dksi2)/sqrt(refe)
!
! --- EVALUATION DE LA CONVERGENCE
!
    if ((test.gt.eps) .and. (iter.lt.itemax)) then
        goto 20
    else if ((iter.ge.itemax).and.(test.gt.eps)) then
        ksi1 = ksi1m
        ksi2 = ksi2m
        call mmfonf(ndim, nno, alias, ksi1, ksi2,&
                    ff, dff, ddff)
        call mmtang(ndim, nno, coorma, dff, tau1,&
                    tau2)
    endif
!
! --- FIN DE LA BOUCLE
!
!
999  continue
!
    if (niverr .eq. 1) then
        write(6,*) 'POINT A PROJETER : ',coorpt(1),coorpt(2),coorpt(3)
        write(6,*) 'MAILLE             ',alias,nno
!
        do 70 ino = 1, nno
            write(6,*) '  NOEUD ',ino
            write(6,*) '   (X,Y,Z)',coorma(3*(ino-1)+1) ,&
     &                            coorma(3*(ino-1)+2),&
     &                            coorma(3*(ino-1)+3)
70      continue
        write(6,*) 'KSI   : ',ksi1,ksi2
        write(6,*) 'ALPHA : ',alpha
    endif
!
end subroutine
