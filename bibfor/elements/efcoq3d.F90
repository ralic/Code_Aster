subroutine efcoq3d(nomte, nb1, nb2, cara, geom, lzr,& 
                   chg,matr,effg,                   &
                   nbcou,npgsn,npgsr,npge,nso,npgt)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     CALCUL DE EFGE_ELNO
!     ------------------------------------------------------------------
   implicit none

#include "asterfort/cosiro.h"
#include "asterfort/vdefgn.h"
#include "asterfort/vdefro.h"
#include "asterfort/vdrepe.h"
#include "asterfort/vectan.h"
#include "asterfort/vectgt.h"

!
    character(len=16) :: nomte
!
!-----------------------------------------------------------------------
    integer :: i, ic, icomp, ii
    integer :: inte, intsn, intsr, isom
    integer :: j, jj
    integer :: k, k1, kpgs, l
    integer ::  nbcou, ncmp
    integer :: npge, npgt
    integer :: nso
    real(kind=8) :: hic, s, zero, zic, zmin
!-----------------------------------------------------------------------
    integer :: icou
    integer :: nb1, nb2, npgsr, npgsn
    real(kind=8) :: vecta(9, 2, 3), vectn(9, 3), vectpt(9, 2, 3)
    real(kind=8) :: vectg(2, 3), vectt(3, 3)
    real(kind=8) :: epais
    real(kind=8) :: matevn(2, 2, npgt), matevg(2, 2, npgt)
    real(kind=8) :: geom(*),cara(*),chg(*),matr(*),effg(*),lzr(*)
    real(kind=8) :: sigm(6, 270), sigma(6, 120), effgc(8, 9), effgt(8, 9)


    zero=0.0d0

     epais=cara(1)
     zmin=-epais/2.d0
     hic=epais/nbcou

     call vectan(nb1, nb2, geom, lzr, vecta,&
                 vectn, vectpt)

      kpgs=0
    do 40 icou = 1, nbcou
        do 30 inte = 1, npge
            if (inte .eq. 1) then
                zic=zmin+(icou-1)*hic
            else if (inte.eq.2) then
                zic=zmin+hic/2.d0+(icou-1)*hic
            else
                zic=zmin+hic+(icou-1)*hic
            endif
!
            do 20 intsn = 1, npgsn
                kpgs=kpgs+1
                k1=6*((intsn-1)*npge*nbcou+(icou-1)*npge+inte-1)
                do 10 i = 1, 6
                    sigm(i,kpgs)=chg(k1+i)
10              continue
20          continue
30      continue
40  continue
    ncmp=6

!
! --- DETERMINATION DES REPERES  LOCAUX DE L'ELEMENT AUX POINTS
! --- D'INTEGRATION ET STOCKAGE DE CES REPERES DANS LE VECTEUR .DESR
!     --------------------------------------------------------------
    k=0
    do 90 intsr = 1, npgsr
        call vectgt(0, nb1, geom, zero, intsr,&
                    lzr, epais, vectn, vectg, vectt)
!
        do 80 j = 1, 3
            do 70 i = 1, 3
                k=k+1
                lzr(2000+k)=vectt(i,j)
70          continue
80      continue
90  continue
! !
! !--- EXTRAPOLATION VERS LES NOEUDS SOMMETS
! !
! 
!
    do 130 icou = 1, nbcou
        do 120 ic = 1, ncmp
            do 110 i = 1, npge*nso
                l=npge*npgsn*(i-1)
                s=0.d0
                do 100 j = 1, npge*npgsn
                    jj=(icou-1)*npge*npgsn+j
                    s=s+matr(l+j)*sigm(ic,jj)
100              continue
                ii=(icou-1)*npge*nso+i
                sigma(ic,ii)=s
110          continue
120      continue
130  continue
! !
! ! --- DETERMINATION DES MATRICE DE PASSAGE DES REPERES INTRINSEQUES
! ! --- AUX NOEUDS ET AUX POINTS D'INTEGRATION DE L'ELEMENT
! ! --- AU REPERE UTILISATEUR :
! !     ---------------------
     call vdrepe(nomte, matevn, matevg)
! !
    do 150 i = 1, nb2
        do 140 j = 1, 8
            effgt(j,i)=0.d0
140      continue
150  continue
    do 180 ic = 1, nbcou
        j=(ic-1)*npge*nso+1
        zic=zmin+(ic-1)*hic
        call vdefgn(nomte, nb2, hic, zic, sigma(1, j),&
                    effgc)
        do 170 isom = 1, nb2
            do 160 icomp = 1, 8
                effgt(icomp,isom)=effgt(icomp,isom)+effgc(icomp,isom)
160          continue
170      continue
180  continue
! !
! ! --- PASSAGE DU VECTEUR DES EFFORTS GENERALISES DEFINI AUX NOEUDS
! ! --- DE L'ELEMENT DU REPERE INTRINSEQUE AU REPERE UTILISATEUR :
! !     --------------------------------------------------------
! 
     call vdefro(nb2, matevn, effgt, effg)
! 


end subroutine
