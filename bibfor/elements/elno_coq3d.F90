subroutine elno_coq3d(option, nomte, nb1, nb2, npgsr,&
                      npgsn, nso, nbcou, geom, cara,&
                      valpg, outno, lzr, matr, lgreen)
!
    implicit none
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/caurtg.h"
#include "asterfort/pk2cau.h"
#include "asterfort/utmess.h"
#include "asterfort/vdrepe.h"
#include "asterfort/vdsiro.h"
#include "asterfort/vectan.h"
#include "asterfort/vectgt.h"
    character(len=16) :: option, nomte
! ----------------------------------------------------------------------
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
!     ----------------------------------------------------------------
!     CALCUL DES OPTIONS DES ELEMENTS DE COQUE 3D
!     OPTIONS : EPSI_ELNO
!               SIEF_ELNO
!               SIGM_ELNO
!          -----------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, ic, icmp, ii, ino
    integer :: inte, intsn, intsr, isp, j
    integer :: jj, k, k1, kpgs, l
    integer :: nbcou, ncmp, npge, npgt, nso
!
    real(kind=8) :: s, zero
!-----------------------------------------------------------------------
    parameter(npge=3)
    parameter(npgt=10)
!
    integer :: icou, nordo
    integer :: nb1, nb2, npgsr, npgsn
!
    real(kind=8) :: vecta(9, 2, 3), vectn(9, 3), vectpt(9, 2, 3)
    real(kind=8) :: vectg(2, 3), vectt(3, 3)
    real(kind=8) :: epais
    real(kind=8) :: matevn(2, 2, npgt), matevg(2, 2, npgt)
    real(kind=8) :: geom(*), cara(*), valpg(*), outno(*), lzr(*), matr(*)
    real(kind=8) :: matpg(6, 27*nbcou), matno(6, 12*nbcou), matgn(6, 12*nbcou)
    real(kind=8) :: pk2(6, 27*nbcou), matgnu(6, 12*nbcou), signo(6, 12*nbcou)
!
    aster_logical :: lgreen
!
! ----------------------------------------------------------------------
!
    zero=0.0d0
!
    if (nbcou .le. 0) then
        call utmess('F', 'ELEMENTS_12')
    endif
!
    epais=cara(1)
!
    call vectan(nb1, nb2, geom, lzr, vecta,&
                vectn, vectpt)
!
    kpgs=0
    do 40 icou = 1, nbcou
        do 30 inte = 1, npge
            do 20 intsn = 1, npgsn
                kpgs=kpgs+1
                k1=6*((intsn-1)*npge*nbcou+(icou-1)*npge+inte-1)
                do 10 i = 1, 6
                    matpg(i,kpgs)=valpg(k1+i)
 10             continue
 20         continue
 30     continue
 40 continue
!
    ncmp=6
!
    if (lgreen) then
!
! --- AFFECTATION DES CONTRAINTES DE PIOLA-KIRCHHOFF DE
! --- SECONDE ESPECE :
!     --------------
        do 60 i = 1, 6
            do 50 j = 1, kpgs
                pk2(i,j)=matpg(i,j)
 50         continue
 60     continue
!
! --- TRANSFORMATION DES CONTRAINTES DE PIOLA-KIRCHHOFF DE
! --- SECONDE ESPECE PK2 EN CONTRAINTES DE CAUCHY :
!     -------------------------------------------
        call pk2cau(nomte, ncmp, pk2, matpg)
    endif
!
! ---  DETERMINATION DES REPERES  LOCAUX DE L'ELEMENT AUX POINTS
! ---  D'INTEGRATION ET STOCKAGE DE CES REPERES DANS LE VECTEUR .DESR
!      --------------------------------------------------------------
    k=0
    do 90 intsr = 1, npgsr
        call vectgt(0, nb1, geom, zero, intsr,&
                    lzr, epais, vectn, vectg, vectt)
!
        do 80 j = 1, 3
            do 70 i = 1, 3
                k=k+1
                lzr(2000+k)=vectt(i,j)
 70         continue
 80     continue
 90 continue
!
!
    do 130 icou = 1, nbcou
        do 120 ic = 1, ncmp
            do 110 i = 1, npge*nso
                l=npge*npgsn*(i-1)
                s=0.d0
                do 100 j = 1, npge*npgsn
                    jj=(icou-1)*npge*npgsn+j
                    s=s+matr(l+j)*matpg(ic,jj)
100             continue
                ii=(icou-1)*npge*nso+i
                matno(ic,ii)=s
110         continue
120     continue
130 continue
!
!
! --- DETERMINATION DES MATRICE DE PASSAGE DES REPERES INTRINSEQUES
! --- AUX NOEUDS ET AUX POINTS D'INTEGRATION DE L'ELEMENT
! --- AU REPERE UTILISATEUR :
!     ---------------------
    call vdrepe(nomte, matevn, matevg)
!
! --- PASSAGE DU VECTEUR DES CONTRAINTES DEFINI AUX NOEUDS
! --- DE L'ELEMENT DU REPERE INTRINSEQUE AU REPERE UTILISATEUR :
!     --------------------------------------------------------
!
    do 210 icou = 1, nbcou
        do 200 nordo = -1, 1
!
            isp=npge*(icou-1)+nordo+2
!
            do 150 i = 1, ncmp
                do 140 j = 1, nso
                    jj=nso*(nordo+1)+nso*npge*(icou-1)+j
                    matgn(i,j)=matno(i,jj)
140             continue
                if (nomte .eq. 'MEC3QU9H') then
                    matgn(i,5)=(matgn(i,1)+matgn(i,2))/2.d0
                    matgn(i,6)=(matgn(i,2)+matgn(i,3))/2.d0
                    matgn(i,7)=(matgn(i,3)+matgn(i,4))/2.d0
                    matgn(i,8)=(matgn(i,4)+matgn(i,1))/2.d0
                    matgn(i,9)=(matgn(i,1)+matgn(i,2)+matgn(i,3)+&
                    matgn(i,4))/ 4.d0
                else if (nomte.eq.'MEC3TR7H') then
                    matgn(i,4)=(matgn(i,1)+matgn(i,2))/2.d0
                    matgn(i,5)=(matgn(i,2)+matgn(i,3))/2.d0
                    matgn(i,6)=(matgn(i,3)+matgn(i,1))/2.d0
                    matgn(i,7)=(matgn(i,1)+matgn(i,2)+matgn(i,3))/&
                    3.d0
                endif
150         continue
!
            if (lgreen) then
                call vdsiro(nb2, 1, matevn, 'IU', 'N',&
                            matgn, matgnu)
                call caurtg(nomte, ncmp, matgnu, signo)
            else
                call vdsiro(nb2, 1, matevn, 'IU', 'N',&
                            matgn, signo)
            endif
!
            if (option .eq. 'EPSI_ELNO') then
                do 170 icmp = 1, ncmp
                    do 160 ino = 1, nb2
                        outno ( (ino-1)*ncmp*nbcou*npge+(isp-1)* ncmp+icmp)= matgn(icmp,ino)
160                 continue
170             continue
                else if ((option.eq.'SIEF_ELNO') .or. (&
            option.eq.'SIGM_ELNO')) then
                do 190 icmp = 1, ncmp
                    do 180 ino = 1, nb2
                        outno((ino-1)*ncmp*nbcou*npge+(isp-1)*ncmp+icmp)= signo(icmp,ino)
180                 continue
190             continue
            else
                ASSERT(.false.)
            endif
!
200     continue
210 continue
end subroutine
