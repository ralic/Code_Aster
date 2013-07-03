subroutine te0419(option, nomte)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
#include "jeveux.h"
!
#include "asterfort/btdfn.h"
#include "asterfort/btdmsn.h"
#include "asterfort/btdmsr.h"
#include "asterfort/btldth.h"
#include "asterfort/hsj1f.h"
#include "asterfort/hsj1ms.h"
#include "asterfort/jevech.h"
#include "asterfort/jevete.h"
#include "asterfort/mahsf.h"
#include "asterfort/mahsms.h"
#include "asterfort/matrth.h"
#include "asterfort/trnflg.h"
#include "asterfort/vectan.h"
#include "asterfort/vexpan.h"
    character(len=16) :: option, nomte
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES
!                          POUR LES ELEMENTS MEC3QU9H, MEC3TR7H
!                          OPTIONS : 'CHAR_MECA_TEMP_R'
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
    integer :: nb1, nb2, nddle, npge, npgsr, npgsn
    real(kind=8) :: vecta(9, 2, 3), vectn(9, 3), vectpt(9, 2, 3), vecpt(9, 3, 3)
    real(kind=8) :: vectg(2, 3), vectt(3, 3)
    real(kind=8) :: hsfm(3, 9), hss(2, 9), hsj1m(3, 9), hsj1s(2, 9)
    real(kind=8) :: btdm(4, 3, 42), btds(4, 2, 42)
    real(kind=8) :: hsf(3, 9), hsj1fx(3, 9), wgt
    real(kind=8) :: btdf(3, 42), btild(5, 42)
    real(kind=8) :: forthi(42), forcth(42), vecl(51)
    real(kind=8) :: young, nu, alpha
!-----------------------------------------------------------------------
    integer :: i, ib, indic, indith, inte, intsn, intsr
    integer :: j, jcara, jgeom, jvecg, kwgt, lzi, lzr
!
    real(kind=8) :: epais, temper
!-----------------------------------------------------------------------
    parameter (npge=2)
    real(kind=8) :: epsval(npge), ksi3s2, ksi3
    data epsval / -0.577350269189626d0,  0.577350269189626d0 /
!
    call jevech('PGEOMER', 'L', jgeom)
    call jevech('PVECTUR', 'E', jvecg)
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
    nddle = 5*nb1+2
!
!
!     RECUPERATION DE LA TEMPERATURE DE REFERENCE
!
!
    do 5 i = 1, nddle
        forcth(i)=0.d0
 5  end do
!
    call vectan(nb1, nb2, zr(jgeom), zr(lzr), vecta,&
                vectn, vectpt)
!
    kwgt=0
    do 100 inte = 1, npge
        ksi3s2=epsval(inte)/2.d0
!
!     CALCUL DE BTDMR, BTDSR : M=MEMBRANE , S=CISAILLEMENT , R=REDUIT
!
        do 150 intsr = 1, npgsr
            call mahsms(0, nb1, zr(jgeom), ksi3s2, intsr,&
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
        do 200 intsn = 1, npgsn
!
!     CALCUL DE BTDFN : F=FLEXION , N=NORMAL
!     ET DEFINITION DE WGT=PRODUIT DES POIDS ASSOCIES AUX PTS DE GAUSS
!                          (NORMAL) ET DU DETERMINANT DU JACOBIEN
!
            call mahsf(1, nb1, zr(jgeom), ksi3s2, intsn,&
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
            call btdmsn(1, nb1, intsn, npgsr, zr(lzr),&
                        btdm, btdf, btds, btild)
!
            call matrth('MASS', npgsn, young, nu, alpha,&
                        indith)
!
!     CALCUL DU CHAMP DE TEMPERATURE ET(OU) DES EFFORTS THERMIQUES
!     INDIC=1 : TEMPERATURE ET EFFORTS THERMIQUES
!     INDIC=0 : TEMPERATURE
!
            indic=1
            ksi3=epsval(inte)
            call btldth('MASS', ksi3, nb1, intsn, btild,&
                        wgt, indic, young, nu, alpha,&
                        temper, forthi)
!
            do 11 i = 1, nddle
                forcth(i)=forcth(i)+forthi(i)
11          end do
!
200      end do
100  end do
!
    call vexpan(nb1, forcth, vecl)
    do 90 i = 1, 3
        vecl(6*nb1+i)=0.d0
90  end do
!
    do 15 ib = 1, nb2
        do 16 i = 1, 2
            do 17 j = 1, 3
                vecpt(ib,i,j)=vectpt(ib,i,j)
17          end do
16      end do
        vecpt(ib,3,1)=vectn(ib,1)
        vecpt(ib,3,2)=vectn(ib,2)
        vecpt(ib,3,3)=vectn(ib,3)
15  end do
!
    call trnflg(nb2, vecpt, vecl, zr(jvecg))
!
end subroutine
