subroutine fornpd(option, nomte)
    implicit none
#include "jeveux.h"
#include "asterfort/btdfn.h"
#include "asterfort/btdmsn.h"
#include "asterfort/btdmsr.h"
#include "asterfort/epseff.h"
#include "asterfort/hsj1f.h"
#include "asterfort/hsj1ms.h"
#include "asterfort/jevech.h"
#include "asterfort/jevete.h"
#include "asterfort/mahsf.h"
#include "asterfort/mahsms.h"
#include "asterfort/r8inir.h"
#include "asterfort/rccoma.h"
#include "asterfort/tecach.h"
#include "asterfort/terefe.h"
#include "asterfort/trndgl.h"
#include "asterfort/trnflg.h"
#include "asterfort/utmess.h"
#include "asterfort/vectan.h"
#include "asterfort/vexpan.h"
#include "blas/daxpy.h"
    character(len=16) :: option, nomte
! ----------------------------------------------------------------------
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
!
!     CALCUL DES OPTIONS DES ELEMENTS DE COQUE 3D
!     OPTION : FORC_NODA (REPRISE)
!
!
    integer :: icodre(26)
    character(len=10) :: phenom
!
    integer :: i, ib, icou, inte, intsn, intsr, j, k1, kpgs, kwgt, itab(7), iret
    integer :: icontm, ideplm, imate, ivectu, jcara, jgeom, lzi, lzr
    integer :: nb1, nb2, npge, npgsr, npgsn, jnbspi, nbcou, nval, nbsp
!
    real(kind=8) :: vecta(9, 2, 3), vectn(9, 3), vectpt(9, 2, 3), vecpt(9, 3, 3)
    real(kind=8) :: vectg(2, 3), vectt(3, 3)
    real(kind=8) :: hsfm(3, 9), hss(2, 9), hsj1m(3, 9), hsj1s(2, 9)
    real(kind=8) :: btdm(4, 3, 42), btds(4, 2, 42)
    real(kind=8) :: hsf(3, 9), hsj1fx(3, 9), wgt
    real(kind=8) :: btdf(3, 42), btild(5, 42)
    real(kind=8) :: epais, x
    real(kind=8) :: rotfm(9)
    real(kind=8) :: deplm(42), effint(42), vecl(48), vecll(51)
    real(kind=8) :: sgmtd(5)
    real(kind=8) :: ksi3s2
    real(kind=8) :: sigtmp(5), ftemp(40), sigref
    real(kind=8) :: zero, zic, zmin, coef, hepa, hic
!
    parameter ( npge=3 )
! DEB
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
    call jevech('PNBSP_I', 'L', jnbspi)
    nbcou=zi(jnbspi-1+1)
    if (nbcou .le. 0) then
        call utmess('F', 'ELEMENTS_12')
    endif
    if (nbcou .gt. 10) then
        call utmess('F', 'ELEMENTS_13')
    endif
    call jevech('PGEOMER', 'L', jgeom)
    call jevech('PCACOQU', 'L', jcara)
    epais = zr(jcara)
    zmin = -epais/2.d0
    hic = epais/nbcou
!
    call jevech('PMATERC', 'L', imate)
!
    if (option .eq. 'FORC_NODA') then
        call tecach('OOO', 'PCONTMR', 'L', iret, nval=7,&
                    itab=itab)
        icontm=itab(1)
        nbsp=itab(7)
        if (nbsp .ne. npge*nbcou) then
            call utmess('F', 'ELEMENTS_4')
        endif
    else if (option.eq.'REFE_FORC_NODA') then
        call terefe('SIGM_REFE', 'MECA_COQUE3D', sigref)
    endif
    call jevech('PDEPLMR', 'L', ideplm)
!
    call rccoma(zi(imate), 'ELAS', 1, phenom, icodre(1))
!
    if (phenom .ne. 'ELAS') then
        call utmess('F', 'ELEMENTS_42')
    endif
!
    call vectan(nb1, nb2, zr(jgeom), zr(lzr), vecta,&
                vectn, vectpt)
!
    call trndgl(nb2, vectn, vectpt, zr(ideplm), deplm,&
                rotfm)
!
    do 35 i = 1, 5*nb1+2
        effint(i)=0.d0
35  end do
!
    if (option .eq. 'REFE_FORC_NODA') then
        call r8inir(nb1*5, 0.d0, ftemp, 1)
    endif
!
    kwgt=0
    kpgs=0
    do 100 icou = 1, nbcou
        do 100 inte = 1, npge
            if (inte .eq. 1) then
                zic = zmin + (icou-1)*hic
                coef = 1.d0/3.d0
            else if (inte.eq.2) then
                zic = zmin + hic/2.d0 + (icou-1)*hic
                coef = 4.d0/3.d0
            else
                zic = zmin + hic + (icou-1)*hic
                coef = 1.d0/3.d0
            endif
            ksi3s2=zic/hic
            hepa = hic
!
!   CALCUL DE BTDMR, BTDSR : M=MEMBRANE , S=CISAILLEMENT , R=REDUIT
!
            do 120 intsr = 1, npgsr
                call mahsms(0, nb1, zr(jgeom), ksi3s2, intsr,&
                            zr(lzr), hepa, vectn, vectg, vectt,&
                            hsfm, hss)
!
                call hsj1ms(hepa, vectg, vectt, hsfm, hss,&
                            hsj1m, hsj1s)
!
                call btdmsr(nb1, nb2, ksi3s2, intsr, zr(lzr),&
                            hepa, vectpt, hsj1m, hsj1s, btdm,&
                            btds)
120          continue
!
            do 150 intsn = 1, npgsn
!
                call mahsf(1, nb1, zr(jgeom), ksi3s2, intsn,&
                           zr(lzr), hepa, vectn, vectg, vectt,&
                           hsf)
!
                call hsj1f(intsn, zr(lzr), hepa, vectg, vectt,&
                           hsf, kwgt, hsj1fx, wgt)
!
                wgt=coef*wgt
!
                call btdfn(1, nb1, nb2, ksi3s2, intsn,&
                           zr(lzr), hepa, vectpt, hsj1fx, btdf)
!
                call btdmsn(1, nb1, intsn, npgsr, zr(lzr),&
                            btdm, btdf, btds, btild)
!
                kpgs = kpgs + 1
                k1=6*((intsn-1)*npge*nbcou + (icou-1)*npge +inte - 1)
!
                if (option .eq. 'FORC_NODA') then
                    sgmtd(1)=zr(icontm-1+k1+1)
                    sgmtd(2)=zr(icontm-1+k1+2)
                    sgmtd(3)=zr(icontm-1+k1+4)
                    sgmtd(4)=zr(icontm-1+k1+5)
                    sgmtd(5)=zr(icontm-1+k1+6)
!
                    call epseff('EFFORI', nb1, [0.d0], btild, sgmtd,&
                                [0.d0], wgt, effint)
!
                else if (option.eq.'REFE_FORC_NODA') then
!
!      CALCUL DES FORCES NODALES DE REFERENCE
!      EN AFFECTANT LA VALEUR SIGM_REFE A CHAQUE CMP SUCCESSIVEMENT
!      POUR CHAQUE POINT D'INTEGRATION
!
                    call r8inir(5, 0.d0, sigtmp, 1)
!
                    do 155 i = 1, 5
                        sigtmp(i)=sigref
                        call epseff('EFFORI', nb1, [0.d0], btild, sigtmp,&
                                    [0.d0], wgt, effint)
                        sigtmp(i)=0.d0
                        do 156 j = 1, nb1*5
                            ftemp(j) = ftemp(j)+abs(effint(j))
156                      continue
155                  continue
!
                endif
!
150          continue
100      continue
!
!      ON PREND LA VALEUR MOYENNE DES FORCES NODALES DE REFERENCE
!
    if (option .eq. 'REFE_FORC_NODA') then
        nval=nbcou*npge*npgsn*5
        call daxpy(nb1*5, 1.d0/nval, ftemp, 1, effint,&
                   1)
    endif
!
!
!-- EXPANSION DU CHAMP
    call vexpan(nb1, effint, vecl)
!
    do 17 i = 1, 6*nb1
        vecll(i)=vecl(i)
17  end do
    vecll(6*nb1+1)=effint(5*nb1+1)
    vecll(6*nb1+2)=effint(5*nb1+2)
!        VECLL(6*NB1+3)=0.D0
!
!     ICI PAS DE CONTRIBUTION DES DDL DE LA ROTATION FICTIVE DANS EFFINT
!
    zero=0.d0
    do 18 i = 1, nb1
        vecll(6*i)=zero*rotfm(i)
18  end do
    i=nb2
    vecll(6*nb1+3)=zero*rotfm(nb2)
!
!     TRANFORMATION DANS REPERE GLOBAL PUIS STOCKAGE
!
    do 105 ib = 1, nb2
        do 106 i = 1, 2
            do 107 j = 1, 3
                vecpt(ib,i,j)=vectpt(ib,i,j)
107          end do
106      end do
        vecpt(ib,3,1)=vectn(ib,1)
        vecpt(ib,3,2)=vectn(ib,2)
        vecpt(ib,3,3)=vectn(ib,3)
105  end do
!
    call jevech('PVECTUR', 'E', ivectu)
!
    call trnflg(nb2, vecpt, vecll, zr(ivectu))
!
end subroutine
