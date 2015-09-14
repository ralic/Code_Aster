subroutine te0477(option, nomte)
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
    implicit none
#include "jeveux.h"
#include "asterc/r8vide.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/idsshb.h"
#include "asterfort/jevech.h"
#include "asterfort/moytem.h"
#include "asterfort/nmcomp.h"
#include "asterfort/r8inir.h"
#include "asterfort/rcvalb.h"
#include "asterfort/sh1eps.h"
#include "asterfort/sh1for.h"
#include "asterfort/sh1mek.h"
#include "asterfort/sh1rig.h"
#include "asterfort/sh2eps.h"
#include "asterfort/sh2for.h"
#include "asterfort/sh2mek.h"
#include "asterfort/sh2rig.h"
#include "asterfort/sh6eps.h"
#include "asterfort/sh6for.h"
#include "asterfort/sh6mek.h"
#include "asterfort/sh6rig.h"
#include "asterfort/sh8eps.h"
#include "asterfort/sh8for.h"
#include "asterfort/sh8mek.h"
#include "asterfort/sh8rig.h"
#include "asterfort/shbpkc.h"
#include "asterfort/tecach.h"
#include "asterfort/utmess.h"
!
!-----------------------------------------------------------------------
    integer :: i, icarcr, icompo, icontm, icontp, ideplm, ideplp
    integer :: idfde, igeom, imate, imatuu, ipg, ipoids, ivarim
    integer :: ivarip, ivectu, ivf, j, jcret, jgano, k
    integer :: nbres, nbv, ndim, nno, nnos, npg
    real(kind=8) :: tempm
!-----------------------------------------------------------------------
    parameter (nbres=2)
    character(len=8) :: typmod(2)
    character(len=4) :: fami
    integer :: icodre(nbres)
    character(len=16) :: nomres(nbres), nomte, option, nomshb
    real(kind=8) :: re(24, 24), sigma(120), d(36)
    real(kind=8) :: dusdx(180), fstab(12), sigmm(120), sigmp(120)
    real(kind=8) :: valres(nbres), sigm(6), simp(6)
    real(kind=8) :: g, angmas(3), instm, instp, dsidep(6, 6)
    real(kind=8) :: depslo(120), eps2d(6), deps2d(6), epsloc(120)
    integer :: iinstm, iinstp, iret, codret, jtab(7), lgpg
    real(kind=8) :: nu, e, para(2), rbid(1)
    real(kind=8) :: xidepp(60), re6(18, 18), re15(45, 45), re20(60, 60)
    real(kind=8) :: duddd(180)
    real(kind=8) :: dsde(20, 6, 6)
    fami = 'RIGI'
    call elrefe_info(fami=fami,ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
! --- INITIALISATIONS :
!     -----------------
    call idsshb(ndim, nno, npg, nomshb)
    do 10 i = 1, 2
        para(i) = 0.d0
10  continue
    call r8inir(720, 0.d0, dsde, 1)
!
!  ###############################################################
!  -- ELASTOPLASTICITE
!  ###############################################################
    if (option .eq. 'FULL_MECA' .or. option .eq. 'RAPH_MECA' .or. option .eq.&
        'RIGI_MECA_TANG') then
! - PARAMETRES EN ENTREE
        call jevech('PGEOMER', 'L', igeom)
        call jevech('PMATERC', 'L', imate)
        call jevech('PCONTMR', 'L', icontm)
        call jevech('PVARIMR', 'L', ivarim)
        call tecach('OOO', 'PVARIMR', 'L', iret, nval=7,&
                    itab=jtab)
!       LGPG : nombre de variables internes par sous point
        lgpg = max(jtab(6),1)
        call jevech('PDEPLMR', 'L', ideplm)
        call jevech('PDEPLPR', 'L', ideplp)
        call jevech('PCOMPOR', 'L', icompo)
        call jevech('PCARCRI', 'L', icarcr)
        call jevech('PINSTMR', 'L', iinstm)
        call jevech('PINSTPR', 'L', iinstp)
        instm = zr(iinstm)
        instp = zr(iinstp)
! - PARAMETRES EN SORTIE
        if (option(1:16) .eq. 'RIGI_MECA_TANG' .or. option(1:9) .eq. 'FULL_MECA') then
            call jevech('PMATUUR', 'E', imatuu)
        endif
!
        if (option(1:9) .eq. 'RAPH_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
            call jevech('PVECTUR', 'E', ivectu)
            call jevech('PCONTPR', 'E', icontp)
            call jevech('PVARIPR', 'E', ivarip)
        endif
! - PARAMETRES MATERIAU
!
        nomres(1) = 'E'
        nomres(2) = 'NU'
        nbv = 2
! ----  INTERPOLATION DES COEFFICIENTS EN FONCTION DE LA TEMPERATURE
! ----  ET DU TEMPS
! -----------
        call moytem(fami, npg, 1, '+', tempm,&
                    iret)
        call rcvalb(fami, 1, 1, '+', zi(imate),&
                    ' ', 'ELAS', 1, 'TEMP', [tempm],&
                    nbv, nomres, valres, icodre, 1)
        e = valres(1)
        nu = valres(2)
        para(1) = e
        para(2) = nu
!  =============================================
!  -  ACTUALISATION : GEOM ORIG + DEPL DEBUT PAS
!  =============================================
        if (zk16(icompo+2) .ne. 'PETIT') then
            if (zk16(icompo+2) .ne. 'GROT_GDEP') then
                call utmess('F', 'COMPOR1_69', sk=zk16(icompo+2))
            endif
        endif
        if (zk16(icompo+2) .eq. 'GROT_GDEP') then
            if (option(1:16) .eq. 'RIGI_MECA_TANG' .or. option(1:9) .eq. 'RAPH_MECA' .or.&
                option(1:9) .eq. 'FULL_MECA') then
                do 20 i = 1, 3*nno
                    zr(igeom+i-1) = zr(igeom+i-1) + zr(ideplm+i-1)
20              continue
            endif
        endif
!  =============================================
!  -  CALCUL DES CONTRAINTES
!  =============================================
        if (option(1:9) .eq. 'RAPH_MECA' .or. option(1:9) .eq. 'FULL_MECA' .or.&
            option(1:14) .eq. 'RIGI_MECA_TANG') then
!
! ----   PARAMETRES MATERIAU
            d(1) = e
            d(2) = nu
            para(1) = e
            para(2) = nu
!
            call r8inir(120, 0.d0, sigmm, 1)
            call r8inir(120, 0.d0, sigma, 1)
            call r8inir(120, 0.d0, epsloc, 1)
            call r8inir(120, 0.d0, depslo, 1)
            call r8inir(180, 0.d0, dusdx, 1)
            call r8inir(180, 0.d0, duddd, 1)
!
            do 60 i = 1, npg
                do 50 j = 1, 6
                    sigmm(6*(i-1)+j)=zr(icontm+18*(i-1)+j-1)
50              continue
60          continue
            if (nomshb .eq. 'SHB8') then
                do 70 i = 1, 12
                    fstab(i) = zr(icontm+i-1+6)
70              continue
                call sh8eps(zr(igeom), zr(ideplm), epsloc, duddd)
                call sh8eps(zr(igeom), zr(ideplp), depslo, dusdx)
            else if (nomshb.eq.'SHB6') then
                call sh6eps(zr(igeom), zr(ideplm), epsloc, duddd)
                call sh6eps(zr(igeom), zr(ideplp), depslo, dusdx)
!
            else if (nomshb.eq.'SHB15') then
                call sh1eps(zr(igeom), zr(ideplm), epsloc, duddd)
                call sh1eps(zr(igeom), zr(ideplp), depslo, dusdx)
!
            else if (nomshb.eq.'SHB20') then
                call sh2eps(zr(igeom), zr(ideplm), epsloc, duddd)
                call sh2eps(zr(igeom), zr(ideplp), depslo, dusdx)
            endif
            do 80 ipg = 1, npg
                eps2d(1) = epsloc(6*(ipg-1)+1)
                eps2d(2) = epsloc(6*(ipg-1)+2)
                eps2d(3) = epsloc(6*(ipg-1)+3)
                eps2d(4) = epsloc(6*(ipg-1)+4)/sqrt(2.d0)
                eps2d(5) = epsloc(6*(ipg-1)+5)
                eps2d(6) = epsloc(6*(ipg-1)+6)
                deps2d(1) = depslo(6*(ipg-1)+1)
                deps2d(2) = depslo(6*(ipg-1)+2)
                deps2d(3) = depslo(6*(ipg-1)+3)
                deps2d(4) = depslo(6*(ipg-1)+4)/sqrt(2.d0)
                deps2d(5) = depslo(6*(ipg-1)+5)
                deps2d(6) = depslo(6*(ipg-1)+6)
!  recuperer SIG2D *SQRT(2.D0) /SQRT(2.D0)
                sigm(1) = sigmm(6*(ipg-1)+1)
                sigm(2) = sigmm(6*(ipg-1)+2)
                sigm(3) = sigmm(6*(ipg-1)+3)
                sigm(4) = sigmm(6*(ipg-1)+4)*sqrt(2.d0)
                sigm(5) = sigmm(6*(ipg-1)+5)
                sigm(6) = sigmm(6*(ipg-1)+6)
                typmod='C_PLAN'
                call r8inir(3, r8vide(), angmas, 1)
                call r8inir(36, 0.d0, dsidep, 1)
                call nmcomp('RIGI', ipg, 1, 2, typmod,&
                            zi(imate), zk16( icompo), zr(icarcr), instm, instp,&
                            6, eps2d, deps2d, 6, sigm,&
                            zr(ivarim+lgpg*(ipg-1)), option, angmas, 1, [0.d0],&
                            simp, zr(ivarip+lgpg*(ipg-1)), 36, dsidep, 1,&
                            rbid, codret)
                g = d(1)/(2.d0*(1.d0+d(2)))
                sigma(6*(ipg-1)+1) = simp(1)
                sigma(6*(ipg-1)+2) = simp(2)
                sigma(6*(ipg-1)+3) = sigm(3)+ d(1)*depslo(6*(ipg-1)+3)
                sigma(6*(ipg-1)+4) = simp(4)/sqrt(2.d0)
                sigma(6*(ipg-1)+5) = sigm(5) +g*depslo(6*(ipg-1)+5)
                sigma(6*(ipg-1)+6) = sigm(6) +g*depslo(6*(ipg-1)+6)
                do 90 i = 1, 6
                    do 95 j = 1, 6
                        dsde(ipg,i,j)=dsidep(i,j)
95                  continue
90              continue
!
80          continue
!
            call r8inir(120, 0.d0, sigmp, 1)
            call shbpkc(sigma, sigmp, dusdx, npg)
        endif
!  ===========================================
!  -  MATRICE DE RIGIDITE TANGENTE
!  ===========================================
        if (option(1:16) .eq. 'RIGI_MECA_TANG' .or. option(1:9) .eq. 'FULL_MECA') then
            para(1) = e
            para(2) = nu
            call r8inir(120, 0.d0, sigma, 1)
            if (nomshb .eq. 'SHB8') then
                call r8inir(24*24, 0.d0, re, 1)
                call sh8rig(zr(igeom), para, dsde, option, re)
                if (zk16(icompo+2) .eq. 'GROT_GDEP') then
! ----      RIGIDITE ELASTIQUE + RIGIDITE GEOMETRIQUE
                    if (option(1:16) .eq. 'RIGI_MECA_TANG') then
                        do 130 i = 1, 12
                            fstab(i) = zr(icontm+i-1+6)
130                      continue
                        do 150 i = 1, 5
                            do 140 j = 1, 6
                                sigma(6*(i-1)+j)=zr(icontm+18*(i-1)+j-&
                                1)
140                          continue
150                      continue
                        call sh8mek(zr(igeom), sigma, re)
                    else if (option(1:9).eq.'FULL_MECA') then
                        call sh8mek(zr(igeom), sigma, re)
                    endif
                endif
! ----   RECUPERATION ET AFFECTATION DU VECTEUR EN SORTIE
! ----   DEMI-MATRICE DE RIGIDITE
                call jevech('PMATUUR', 'E', imatuu)
                k = 0
                do 170 i = 1, 24
                    do 160 j = 1, i
                        k = k + 1
                        zr(imatuu+k-1) = re(i,j)
160                  continue
170              continue
!
            else if (nomshb .eq. 'SHB6') then
                call r8inir(18*18, 0.d0, re6, 1)
                call sh6rig(zr(igeom), para, dsde, option, re6)
                if (zk16(icompo+2) .eq. 'GROT_GDEP') then
! ----      RIGIDITE ELASTIQUE + RIGIDITE GEOMETRIQUE
                    if (option(1:16) .eq. 'RIGI_MECA_TANG') then
                        do 210 i = 1, 5
                            do 200 j = 1, 6
                                sigma(6*(i-1)+j)=zr(icontm+18*(i-1)+j-&
                                1)
200                          continue
210                      continue
                        call sh6mek(zr(igeom), sigma, re6)
                    else if (option(1:9).eq.'FULL_MECA') then
                        call sh6mek(zr(igeom), sigma, re6)
                    endif
                endif
                call jevech('PMATUUR', 'E', imatuu)
                k = 0
                do 230 i = 1, 18
                    do 220 j = 1, i
                        k = k + 1
                        zr(imatuu+k-1) = re6(i,j)
220                  continue
230              continue
!
            else if (nomshb.eq.'SHB15') then
                call r8inir(45*45, 0.d0, re15, 1)
                call sh1rig(zr(igeom), para, dsde, option, re15)
                if (zk16(icompo+2) .eq. 'GROT_GDEP') then
! ----      RIGIDITE ELASTIQUE + RIGIDITE GEOMETRIQUE
                    if (option(1:16) .eq. 'RIGI_MECA_TANG') then
                        do 270 i = 1, 15
                            do 260 j = 1, 6
                                sigma(6*(i-1)+j)=zr(icontm+18*(i-1)+j-&
                                1)
260                          continue
270                      continue
                        call sh1mek(zr(igeom), sigma, re15)
                    else if (option(1:9).eq.'FULL_MECA') then
                        call sh1mek(zr(igeom), sigma, re15)
                    endif
                endif
                call jevech('PMATUUR', 'E', imatuu)
                k = 0
                do 290 i = 1, 45
                    do 280 j = 1, i
                        k = k + 1
                        zr(imatuu+k-1) = re15(i,j)
280                  continue
290              continue
!
            else if (nomshb.eq.'SHB20') then
                call r8inir(60*60, 0.d0, re20, 1)
                call sh2rig(zr(igeom), para, dsde, option, re20)
                if (zk16(icompo+2) .eq. 'GROT_GDEP') then
! ----      RIGIDITE ELASTIQUE + RIGIDITE GEOMETRIQUE
                    if (option(1:16) .eq. 'RIGI_MECA_TANG') then
                        do 330 i = 1, 20
                            do 320 j = 1, 6
                                sigma(6*(i-1)+j)=zr(icontm+18*(i-1)+j-&
                                1)
320                          continue
330                      continue
                        call sh2mek(zr(igeom), sigma, re20)
                    else if (option(1:9).eq.'FULL_MECA') then
                        call sh2mek(zr(igeom), sigma, re20)
                    endif
                    call jevech('PMATUUR', 'E', imatuu)
                    k = 0
                    do 350 i = 1, 60
                        do 340 j = 1, i
                            k = k + 1
                            zr(imatuu+k-1) = re20(i,j)
340                      continue
350                  continue
                endif
            endif
! ----   RECUPERATION ET AFFECTATION DU VECTEUR EN SORTIE
! ----   DEMI-MATRICE DE RIGIDITE
        endif
!  ===============================================================
!  -  CALCUL DES FORCES INTERNES BT.SIGMA
!  ===============================================================
        if (option(1:9) .eq. 'RAPH_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
!  -  ACTUALISATION : GEOM DEBUT PAS + INCR ITER
            if (zk16(icompo+2) .eq. 'GROT_GDEP') then
                do 360 i = 1, 3*nno
                    zr(igeom+i-1) = zr(igeom+i-1) + zr(ideplp+i-1)
360              continue
            endif
! ----   PARAMETRES MATERIAU
!           ZR(IGEOM) : GEOMETRIE + DEPL DEBUT PAS + INCR DEPL
!           ZR(IDEPLP) : INCR DEPLACEMENT
!                        (PAS UTILISE CAR LAGRANGIEN ACTUALISE)
!           ZR(ICONTP) : CONTRAINTE DE CAUCHY FIN DE PAS
!           ZR(IVARIM) (DE 2 A 14) : CONTRAINTES DE STABILISATION
            para(1) = e
            para(2) = nu
            if (zk16(icompo+2) .eq. 'GROT_GDEP') then
                do 370 i = 1, 3*nno
                    xidepp(i) = zr(ideplp+i-1)
370              continue
            else
                do 380 i = 1, 3*nno
                    xidepp(i) = zr(ideplp+i-1)
380              continue
            endif
            if (nomshb .eq. 'SHB8') then
                do 390 i = 1, 12
                    fstab(i) = zr(icontm+i-1+6)
390              continue
                call sh8for(zr(igeom), para, xidepp, sigmp, fstab,&
                            zr( ivectu))
                do 400 i = 1, 12
                    zr(icontp+i-1+6)=fstab(i)
400              continue
                do 420 i = 1, npg
                    do 410 j = 1, 6
                        zr(icontp+18*(i-1)+j-1)=sigmp(6*(i-1)+j)
410                  continue
420              continue
            else if (nomshb.eq.'SHB6') then
                call sh6for(zr(igeom), sigmp, zr(ivectu))
                do 440 i = 1, npg
                    do 430 j = 1, 6
                        zr(icontp+18*(i-1)+j-1)=sigmp(6*(i-1)+j)
430                  continue
440              continue
            else if (nomshb.eq.'SHB15') then
                call sh1for(zr(igeom), sigmp, zr(ivectu))
                do 460 i = 1, npg
                    do 450 j = 1, 6
                        zr(icontp+18*(i-1)+j-1)=sigmp(6*(i-1)+j)
450                  continue
460              continue
            else if (nomshb.eq.'SHB20') then
                call sh2for(zr(igeom), sigmp, zr(ivectu))
                do 480 i = 1, npg
                    do 470 j = 1, 6
                        zr(icontp+18*(i-1)+j-1)=sigmp(6*(i-1)+j)
470                  continue
480              continue
            endif
        endif
    endif
    if (option(1:9) .eq. 'RAPH_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
        call jevech('PCODRET', 'E', jcret)
        zi(jcret) = codret
    endif
end subroutine
