subroutine te0574(option, nomte)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit      none
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/elref4.h"
#include "asterfort/fointe.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/nmpr2d.h"
#include "blas/dcopy.h"
    character(len=16) :: nomte, option
!
! ----------------------------------------------------------------------
!
! ROUTINE CALCUL ELEMENTAIRE
!
! CALCUL DES OPTIONS ELEMENTAIRES EN MECANIQUE CORRESPONDANT A UN
! CHARGEMENT EN PRESSION SUIVEUSE SUR DES ARETES D'ELEMENTS
! ISOPARAMETRIQUES 2D
!
! LA PRESSION EST UNE FONCTION RELLE
!
! ----------------------------------------------------------------------
!
!
! IN  OPTION : OPTION DE CALCUL
!               CHAR_MECA_PRSU_F
!               RIGI_MECA_PRSU_F
! IN  NOMTE  : NOM DU TYPE ELEMENT
!
!
!
!
!
    integer :: mxnoeu, mxnpg, mxvect, mxmatr
    parameter     (mxnoeu=3,mxnpg=4,mxvect=2*3,mxmatr=2*3*2*3)
    integer :: mxpara
    parameter     (mxpara=3)
!
    character(len=8) :: nompar(mxpara)
    real(kind=8) :: valpar(mxpara)
    integer :: ier
    real(kind=8) :: x, y
!
    logical :: laxi
    integer :: ndim, nno, npg, nnos, nddl
    integer :: iddl, ino, ipg
    integer :: jpoids, jvf, jdf, jgano
    integer :: jvect, jmatr
    integer :: jgeom, jdepm, jdepp, jpres, jtime
    integer :: kdec, i, j, k
!
    real(kind=8) :: p(2, mxnpg)
    real(kind=8) :: vect(mxvect), matr(mxmatr)
!
! ----------------------------------------------------------------------
!
!
! --- CARACTERISTIQUES ELEMENT
!
    laxi = lteatt('AXIS','OUI')
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, jpoids, jvf, jdf, jgano)
    nddl = 2*nno
    ASSERT(nno.le.mxnoeu)
    ASSERT(npg .le.mxnpg)
!
! --- ACCES CHAMPS
!
    call jevech('PGEOMER', 'L', jgeom)
    call jevech('PDEPLMR', 'L', jdepm)
    call jevech('PDEPLPR', 'L', jdepp)
    call jevech('PPRESSF', 'L', jpres)
    call jevech('PTEMPSR', 'L', jtime)
!
! --- REACTUALISATION DE LA GEOMETRIE PAR LE DEPLACEMENT
!
    do 10 iddl = 1, nddl
        zr(jgeom+iddl-1) = zr(jgeom+iddl-1) + zr(jdepm+iddl-1) + zr(jdepp+iddl-1)
10  end do
!
! --- CALCUL DE LA PRESSION AUX POINTS DE GAUSS (FONCTION)
!
    nompar(1) = 'X'
    nompar(2) = 'Y'
    nompar(3) = 'INST'
    valpar(3) = zr(jtime)
!
    do 100 ipg = 1, npg
        kdec = (ipg-1) * nno
        x = 0.d0
        y = 0.d0
        do 105 ino = 1, nno
            x = x + zr(jgeom+2*(ino-1)+1-1) * zr(jvf+kdec+ino-1)
            y = y + zr(jgeom+2*(ino-1)+2-1) * zr(jvf+kdec+ino-1)
105      continue
        valpar(1) = x
        valpar(2) = y
        call fointe('FM', zk8(jpres), mxpara, nompar, valpar,&
                    p(1, ipg), ier)
        call fointe('FM', zk8(jpres+1), mxpara, nompar, valpar,&
                    p(2, ipg), ier)
100  end do
!
! --- CALCUL EFFECTIF DE LA RIGIDITE
!
    if (option .eq. 'CHAR_MECA_PRSU_F') then
        call nmpr2d(1, laxi, nno, npg, zr(jpoids),&
                    zr(jvf), zr(jdf), zr( jgeom), p, vect,&
                    matr)
!
! --- RECOPIE DU VECTEUR ELEMENTAIRE
!
        call jevech('PVECTUR', 'E', jvect)
        call dcopy(nddl, vect, 1, zr(jvect), 1)
!
    else if (option.eq.'RIGI_MECA_PRSU_F') then
        call nmpr2d(2, laxi, nno, npg, zr(jpoids),&
                    zr(jvf), zr(jdf), zr( jgeom), p, vect,&
                    matr)
!
! --- RECOPIE DE LA MATRICE ELEMENTAIRE (NON-SYMETRIQUE)
! --- LES MATRICES NON SYMETRIQUES SONT ENTREES EN LIGNE
!
        call jevech('PMATUNS', 'E', jmatr)
        k = 0
        do 110 i = 1, nddl
            do 120 j = 1, nddl
                k = k + 1
                zr(jmatr-1+k) = matr((j-1)*nddl+i)
120          continue
110      end do
        ASSERT(k.eq.nddl*nddl)
!
    else
        ASSERT(.false.)
    endif
!
end subroutine
