subroutine vdxedg(nomte, option, xi, nb1, npgsr,&
                  edgpg, effgt)
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
    implicit none
#include "jeveux.h"
#include "asterfort/btdfn.h"
#include "asterfort/btdmsn.h"
#include "asterfort/btdmsr.h"
#include "asterfort/hsj1f.h"
#include "asterfort/hsj1ms.h"
#include "asterfort/jevech.h"
#include "asterfort/jevete.h"
#include "asterfort/mahsf.h"
#include "asterfort/mahsms.h"
#include "asterfort/r8inir.h"
#include "asterfort/trndgl.h"
#include "asterfort/vddege.h"
#include "asterfort/vectan.h"
#include "asterfort/vectgt.h"
    real(kind=8) :: edgpg(*)
!
! CALCUL DE L'OPTION EDGE_ELGA POUR LES COQUE_3D
!
    character(len=16) :: nomte
    character(len=*) :: option
    integer :: nb1, nb2, npgsr, npgsn
!-----------------------------------------------------------------------
    integer :: i, intsn, intsr, j
    integer :: jcara, jdepg, k, kwgt, lzi
    integer :: lzr
!-----------------------------------------------------------------------
    real(kind=8) :: xi(3, 9)
    real(kind=8) :: vecta(9, 2, 3), vectn(9, 3), vectpt(9, 2, 3)
    real(kind=8) :: vectg(2, 3), vectt(3, 3)
    real(kind=8) :: hsfm(3, 9), hss(2, 9), hsj1m(3, 9), hsj1s(2, 9)
    real(kind=8) :: btdm(4, 3, 42), btds(4, 2, 42)
    real(kind=8) :: btdm1(4, 3, 42), btds1(4, 2, 42)
    real(kind=8) :: hsf(3, 9), hsj1fx(3, 9), wgt
    real(kind=8) :: btdf(3, 42), btild(5, 42)
    real(kind=8) :: btdf1(3, 42), btild1(5, 42)
    real(kind=8) :: depl(42), rotf(9)
    real(kind=8) :: effgt(8, 9)
    real(kind=8) :: epsif(5), epsim(5)
    real(kind=8) :: epais
    real(kind=8) :: zero, un
!
! --- INITIALISATION
!
    zero = 0.0d0
    un = 1.0d0
    call r8inir(4 * 3 * 42, 0.d0, btdm1, 1)
    call r8inir(4 * 2 * 42, 0.d0, btds1, 1)
    call r8inir(3 * 42, 0.d0, btdf1, 1)
!
!     RECUPERATION DES OBJETS
!
    call jevete('&INEL.'//nomte(1:8)//'.DESI', ' ', lzi)
    nb1  =zi(lzi-1+1)
    nb2  =zi(lzi-1+2)
    npgsr=zi(lzi-1+3)
    npgsn=zi(lzi-1+4)
!
    call jevete('&INEL.'//nomte(1:8)//'.DESR', ' ', lzr)
!
    call jevech('PCACOQU', 'L', jcara)
    epais = zr(jcara)
!
    call vectan(nb1, nb2, xi, zr(lzr), vecta,&
                vectn, vectpt)
!
    call jevech('PDEPLAR', 'L', jdepg)
!
    call trndgl(nb2, vectn, vectpt, zr(jdepg), depl,&
                rotf)
!
    kwgt=0
!
!  ---- MEMBRANE ET CISAILLEMENT
!
    do 150 intsr = 1, npgsr
        call mahsms(0, nb1, xi, un, intsr,&
                    zr(lzr), epais, vectn, vectg, vectt,&
                    hsfm, hss)
!
        call hsj1ms(epais, vectg, vectt, hsfm, hss,&
                    hsj1m, hsj1s)
!
        call btdmsr(nb1, nb2, un, intsr, zr(lzr),&
                    epais, vectpt, hsj1m, hsj1s, btdm,&
                    btds)
150  continue
!
!  ---- FLEXION
!
    do 200 intsn = 1, npgsn
!
        call mahsf(1, nb1, xi, un, intsn,&
                   zr(lzr), epais, vectn, vectg, vectt,&
                   hsf)
!
        call hsj1f(intsn, zr(lzr), epais, vectg, vectt,&
                   hsf, kwgt, hsj1fx, wgt)
!
        call btdfn(1, nb1, nb2, un, intsn,&
                   zr(lzr), epais, vectpt, hsj1fx, btdf)
!
!     CALCUL DE BTDMN, BTDSN : M=MEMBRANE , S=CISAILLEMENT , N=NORMAL
!     FORMATION DE BTILD
!
        call btdmsn(1, nb1, intsn, npgsr, zr(lzr),&
                    btdm, btdf1, btds1, btild1)
        call btdmsn(1, nb1, intsn, npgsr, zr(lzr),&
                    btdm1, btdf, btds, btild)
!
        do 10 i = 1, 5
            epsim(i)=zero
            do 20 k = 1, 5*nb1+2
                epsim(i)=epsim(i)+btild1(i,k)*depl(k)
20          end do
10      end do
!
        do 30 i = 1, 5
            epsif(i)=zero
            do 40 k = 1, 5*nb1+2
                epsif(i)=epsif(i)+btild(i,k)*depl(k)
40          end do
30      end do
        epsif(1)=epsif(1)/epais
        epsif(2)=epsif(2)/epais
        epsif(3)=epsif(3)/epais
        epsif(4)=epsif(4)
        epsif(5)=epsif(5)
!
! STOCKAGE DES DEFORMATIONS DE MEMBRANE , FLEXION ET DE CISAILLMEENT
!  DANS EDGPG
!
! --- DEFOMATIONS DE MEMBRANE
        edgpg((intsn-1)*8+1)=epsim(1)
        edgpg((intsn-1)*8+2)=epsim(2)
        edgpg((intsn-1)*8+3)=epsim(3)
! --- DEFORMATION DE FLEXION
        edgpg((intsn-1)*8+4)=epsif(1)
        edgpg((intsn-1)*8+5)=epsif(2)
        edgpg((intsn-1)*8+6)=epsif(3)
! --- DEFORMATION DE CISAILLEMENT
        edgpg((intsn-1)*8+7)=epsif(4)
        edgpg((intsn-1)*8+8)=epsif(5)
!
200  continue
    if (option(1:9) .eq. 'DEGE_ELNO') then
        call vddege(nomte, nb1, npgsr, zr(lzr), edgpg,&
                    effgt)
    endif
!
! --- DETERMINATION DES REPERES LOCAUX DE L'ELEMENT AUX POINTS
! --- D'INTEGRATION ET STOCKAGE DE CES REPERES DANS LE VECTEUR .DESR :
!     --------------------------------------------------------------
    k = 0
    do 110 intsr = 1, npgsr
        call vectgt(0, nb1, xi, zero, intsr,&
                    zr(lzr), epais, vectn, vectg, vectt)
!
        do 120 j = 1, 3
            do 130 i = 1, 3
                k = k + 1
                zr(lzr+2000+k-1) = vectt(i,j)
130          continue
120      continue
110  end do
!
end subroutine
