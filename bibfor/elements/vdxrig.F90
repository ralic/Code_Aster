subroutine vdxrig(nomte, xi, rig, nb1, indm,&
                  indf)
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
    implicit none
#include "jeveux.h"
#include "asterfort/btdfn.h"
#include "asterfort/btdmsn.h"
#include "asterfort/btdmsr.h"
#include "asterfort/btkb.h"
#include "asterfort/hsj1f.h"
#include "asterfort/hsj1ms.h"
#include "asterfort/jevech.h"
#include "asterfort/jevete.h"
#include "asterfort/mahsf.h"
#include "asterfort/mahsms.h"
#include "asterfort/matrc.h"
#include "asterfort/matrkb.h"
#include "asterfort/r8inir.h"
#include "asterfort/tecach.h"
#include "asterfort/vectan.h"
#include "blas/dscal.h"
    character(len=16) :: nomte
    integer :: nb1, nb2, nddle, npge, npgsr, npgsn, itab(8)
    real(kind=8) :: xi(3, 9)
    real(kind=8) :: vecta(9, 2, 3), vectn(9, 3), vectpt(9, 2, 3)
    real(kind=8) :: vectg(2, 3), vectt(3, 3)
    real(kind=8) :: hsfm(3, 9), hss(2, 9), hsj1m(3, 9), hsj1s(2, 9)
    real(kind=8) :: btdm(4, 3, 42), btds(4, 2, 42)
    real(kind=8) :: hsf(3, 9), hsj1fx(3, 9), wgt
    real(kind=8) :: btdf(3, 42), btild(5, 42), wmatcb(5, 42)
    real(kind=8) :: matc(5, 5), ktild(42, 42), rig(51, 51)
    real(kind=8) :: ctor, epais, kappa
    real(kind=8) :: ktildi(42, 42)
!-----------------------------------------------------------------------
    integer :: i, indf, indm, inte, intsn, intsr, iret
    integer :: j, jcara, jcrf, kwgt, lzi, lzr, nddlet
!
    real(kind=8) :: coef
!-----------------------------------------------------------------------
    parameter (npge=2)
    real(kind=8) :: epsval(npge), ksi3s2
    data epsval / -0.577350269189626d0,  0.577350269189626d0 /
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
    kappa = zr(jcara+3)
    ctor = zr(jcara+4)
!
    nddle = 5*nb1+2
!
    call vectan(nb1, nb2, xi, zr(lzr), vecta,&
                vectn, vectpt)
!
    do 5 i = 1, nddle
        do 6 j = 1, nddle
            ktild(i,j)=0.d0
 6      end do
 5  end do
!
    kwgt=0
    do 100 inte = 1, npge
        ksi3s2=epsval(inte)/2.d0
!
!     CALCUL DE BTDMR, BTDSR : M=MEMBRANE , S=CISAILLEMENT , R=REDUIT
!
        do 150 intsr = 1, npgsr
            call mahsms(0, nb1, xi, ksi3s2, intsr,&
                        zr(lzr), epais, vectn, vectg, vectt,&
                        hsfm, hss)
!
            call hsj1ms(epais, vectg, vectt, hsfm, hss,&
                        hsj1m, hsj1s)
!
            call btdmsr(nb1, nb2, ksi3s2, intsr, zr(lzr),&
                        epais, vectpt, hsj1m, hsj1s, btdm,&
                        btds)
150      end do
!
!
!---- POUR L ENERGIE DE DEFORMATION DE MEMBRANE PAS DE CISAILLEMENT
!
        if (indm .eq. 1) call r8inir(4 * 2 * 42, 0.d0, btds, 1)
!
!---- POUR L ENERGIE DE DEFORMATION DE FLEXION
!
        if (indf .eq. 1) then
!
            call r8inir(4 * 3 * 42, 0.d0, btdm, 1)
!
            call r8inir(4 * 2 * 42, 0.d0, btds, 1)
!
        endif
!
        do 200 intsn = 1, npgsn
!
!     CALCUL DE BTDFN : F=FLEXION , N=NORMAL
!     ET DEFINITION DE WGT=PRODUIT DES POIDS ASSOCIES AUX PTS DE GAUSS
!                          (NORMAL) ET DU DETERMINANT DU JACOBIEN
!
            call mahsf(1, nb1, xi, ksi3s2, intsn,&
                       zr(lzr), epais, vectn, vectg, vectt,&
                       hsf)
!
            call hsj1f(intsn, zr(lzr), epais, vectg, vectt,&
                       hsf, kwgt, hsj1fx, wgt)
!
            call btdfn(1, nb1, nb2, ksi3s2, intsn,&
                       zr(lzr), epais, vectpt, hsj1fx, btdf)
!
!     CALCUL DE BTDMN, BTDSN
!     ET
!     FORMATION DE BTILD
!
!
!---- POUR L ENERGIE DE DEFORMATION DE MEMBRANE
!
            if (indm .eq. 1) call r8inir(3 * 42, 0.d0, btdf, 1)
!
!
            call btdmsn(1, nb1, intsn, npgsr, zr(lzr),&
                        btdm, btdf, btds, btild)
!
            call matrc(nomte, nb2, kappa, matc, vectt)
!
            call dscal(25, wgt, matc, 1)
!
            call btkb(5, 42, nddle, matc, btild,&
                      wmatcb, ktildi)
!
            do 11 i = 1, nddle
                do 12 j = 1, nddle
                    ktild(i,j)=ktild(i,j)+ktildi(i,j)
12              end do
11          end do
!
200      end do
100  end do
!
!     EXPANSION DE LA MATRICE : AJOUTER DE LA ROTATION FICTIVE
!
!
    nddlet=6*nb1+3
    call matrkb(nb1, 42, 51, nddlet, ktild,&
                ctor, rig, coef)
!
!     AFFECTATION DU COEF POUR LA CONTRIBUTION DES ROTATIONS FICTIVES
!     POUR LE CALCUL NON LINEAIRE
!     (CETTE AFFECTATION N'A LIEU QUE DANS LE CAS OU ON PREND LA
!     MATRICE ELASTIQUE AU LIEU DE LA MATRICE TANGENTE)
!
    call tecach('NNN', 'PCACO3D', 'E', iret, nval=8,&
                itab=itab)
    jcrf = itab(1)
    if (jcrf .ne. 0) zr(jcrf) = coef
!
end subroutine
