subroutine c3drep(nomte, epais, alpha, beta, coord,&
                  numnoe, pgl)
    implicit none
#include "jeveux.h"
#include "asterfort/jeveuo.h"
#include "asterfort/vdrep2.h"
#include "asterfort/vectan.h"
#include "asterfort/vectgt.h"
    integer :: numnoe
    character(len=16) :: nomte
    real(kind=8) :: epais, alpha, beta, coord(3, 9), pgl(3, 3)
!     ---------------------------------------------------
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: nicolas.sellenet at edf.fr
!     ------------------------------------------------------------------
!
!         CETTE ROUTINE REALISE LA MEME TACHE QUE COQREP MAIS POUR LES
!         COQUES 3D
!         CALCUL DE LA MATRICE DE PASSAGE DU REPERE DE L'ELEMENT A
!         LA VARIETE (LE REPERE DE LA VARIETE EST OBTENU PAR LA MATRICE
!         DE PASSAGE GLOBAL -> LOCAL) AINSI QUE SON INVERSE
!
!     ------------------------------------------------------------------
    integer :: nb1, nb2, npgsr, i, j, k, ind, intsr, lzi, lzr
!
    real(kind=8) :: vecta(9, 2, 3), vectn(9, 3), vectg(2, 3), vectt(3, 3)
    real(kind=8) :: zero, vectpt(9, 2, 3), vectmp(3, 3), pgltmp(3, 3)
    real(kind=8) :: matevn(2, 2, 10), matevg(2, 2, 10), v
!
    zero=0.D0
    call jeveuo('&INEL.'//nomte(1:8)//'.DESI', 'L', lzi)
    call jeveuo('&INEL.'//nomte(1:8)//'.DESR', 'L', lzr)
    nb1  =zi(lzi-1+1)
    nb2  =zi(lzi-1+2)
    npgsr=zi(lzi-1+3)
!
!     -- POUR REMPLIR LZR+1090+...  ET CALCULER VECTN :
    call vectan(nb1, nb2, coord, zr(lzr), vecta,&
                vectn, vectpt)
!
!     -- POUR REMPLIR LZR+2000+... :
!     -- QUELLE VALEUR POUR IND ? FICHE ???
    ind =0
    k = 0
    do 110 intsr = 1, npgsr
        call vectgt(ind, nb1, coord, zero, intsr,&
                    zr(lzr), epais, vectn, vectg, vectt)
        do 120 j = 1, 3
            do 130 i = 1, 3
                k = k + 1
                zr(lzr+2000+k-1) = vectt(i,j)
130          continue
120      continue
110  end do
!
    call vdrep2(alpha, beta, zi(lzi), zr(lzr), matevn,&
                matevg)
!
    vectmp(1,1) = matevn(1,1,numnoe)
    vectmp(1,2) = matevn(1,2,numnoe)
    vectmp(2,1) = matevn(2,1,numnoe)
    vectmp(2,2) = matevn(2,2,numnoe)
    vectmp(1,3) = 0.d0
    vectmp(2,3) = 0.d0
    vectmp(3,3) = 1.d0
    vectmp(3,1) = 0.d0
    vectmp(3,2) = 0.d0
!
    k = 0
    do 20 j = 1, 3
        do 30 i = 1, 3
            k = k + 1
            pgltmp(i,j) = zr(lzr+1090+(numnoe-1)*9+k-1)
30      continue
20  end do
    do 1 i = 1, 3
        do 2 j = 1, 3
            v = 0.d0
            do 3 k = 1, 3
                v = v + vectmp(i,k) * pgltmp(k,j)
 3          continue
            pgl(i,j) = v
 2      continue
 1  end do
!
end subroutine
