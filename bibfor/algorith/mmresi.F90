subroutine mmresi(alias, nno, ndim, coorma, coorpt,&
                  ksi1, ksi2, valeur)
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
    implicit none
#include "asterfort/mmfonf.h"
#include "asterfort/mmtang.h"
    character(len=8) :: alias
    integer :: nno, ndim
    real(kind=8) :: coorma(27), coorpt(3)
    real(kind=8) :: ksi1, ksi2
    real(kind=8) :: valeur
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (TOUTES METHODES - APPARIEMENT)
!
! ALGORITHME DE NEWTON POUR CALCULER LA PROJECTION D'UN POINT SUR UNE
! MAILLE : RECHERCHE LINEAIRE AVEC REBROUSSEMENT
!
! EVALUATION DE LA FONCTION DE RECHERCHE LINEAIRE
!                                     __
! ==> FONCTION G(ALPHA)  ==  1/2 * || \/D(KSI+ALPHA*DKSI) ||^2
!
!
! ----------------------------------------------------------------------
!
! IN  ALIAS  : TYPE DE MAILLE
! IN  NNO    : NOMBRE DE NOEUD SUR LA MAILLE
! IN  NDIM   : DIMENSION DE LA MAILLE (2 OU 3)
! IN  COORMA : COORDONNEES DES NOEUDS DE LA MAILLE
! IN  COORPT : COORDONNEES DU NOEUD A PROJETER SUR LA MAILLE
! IN  KSI1   : PREMIERE COORDONNEE PARAMETRIQUE DU POINT PROJETE
! IN  KSI2   : SECONDE COORDONNEE PARAMETRIQUE DU POINT PROJETE
! OUT VALEUR : VALEUR DE LA FONCTION DE RECHERCHE LINEAIRE
!
! ----------------------------------------------------------------------
!
    real(kind=8) :: ff(9), dff(2, 9), ddff(3, 9)
    integer :: ino, idim
    real(kind=8) :: vec1(3), tau1(3), tau2(3)
    real(kind=8) :: residu(2)
    real(kind=8) :: zero
    parameter   (zero=0.d0)
!
! ----------------------------------------------------------------------
!
! --- INITIALISATIONS
!
    do 10 idim = 1, 3
        vec1(idim) = zero
        tau1(idim) = zero
        tau2(idim) = zero
10  end do
    residu(1) = zero
    residu(2) = zero
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
40  end do
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
35  end do
!
! --- CALCUL DU RESIDU
!
    residu(1) = vec1(1)*tau1(1) + vec1(2)*tau1(2) + vec1(3)*tau1(3)
    if (ndim .eq. 3) then
        residu(2) = vec1(1)*tau2(1) + vec1(2)*tau2(2) + vec1(3)*tau2( 3)
    endif
!
! --- VALEUR DE G
!
    valeur = 0.5d0 * (residu(1)**2+residu(2)**2)
!
end subroutine
