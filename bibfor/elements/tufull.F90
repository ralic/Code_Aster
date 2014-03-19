subroutine tufull(option, nomte, nbrddl, deplm, deplp,&
                  b, ktild, effint, pass, vtemp,&
                  codret)
    implicit none
#include "jeveux.h"
#include "asterc/r8pi.h"
#include "asterfort/bcoudc.h"
#include "asterfort/bcoude.h"
#include "asterfort/carcou.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/epsett.h"
#include "asterfort/jevech.h"
#include "asterfort/kcoude.h"
#include "asterfort/klg.h"
#include "asterfort/klgcou.h"
#include "asterfort/matini.h"
#include "asterfort/mavec.h"
#include "asterfort/nmcomp.h"
#include "asterfort/r8inir.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvalb.h"
#include "asterfort/tecach.h"
#include "asterfort/utmess.h"
#include "asterfort/vlggl.h"
#include "asterfort/vlgglc.h"
#include "blas/dcopy.h"
    character(len=16) :: option
! ......................................................................
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
!    - FONCTION REALISEE:  CALCUL DES MATRICES ELEMENTAIRES
!                          TUYAU
!                          OPTION : RIGI_MECA_TANG, FULL_MECA
!                                   RAPH_MECA
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!
!
    integer :: nbres, nbrddl, nc, kpgs, nbcoum, nbsecm
    integer :: vali
    parameter (nbres=9)
    character(len=16) :: nomte
    character(len=8) :: nomres(nbres), typmod(2)
    character(len=10) :: phenom
    integer :: valret(nbres)
    real(kind=8) :: valres(nbres), xpg(4)
    parameter (nbsecm=32,nbcoum=10)
    real(kind=8) :: poicou(2*nbcoum+1), poisec(2*nbsecm+1)
    real(kind=8) :: h, a, l, fi, gxz, rac2, sinfi, rbid(4), pi, deuxpi
    real(kind=8) :: deplm(nbrddl), deplp(nbrddl), b(4, nbrddl), pgl4(3, 3)
    real(kind=8) :: epsi(4), depsi(4), eps2d(6), deps2d(6)
    real(kind=8) :: sign(4), sigma(4), sgmtd(4)
    real(kind=8) :: dsidep(6, 6), dtild(4, 4)
    real(kind=8) :: cisail, wgt, r, instm, instp
    real(kind=8) :: ktild(nbrddl, nbrddl), effint(nbrddl)
    real(kind=8) :: pass(nbrddl, nbrddl)
    real(kind=8) :: pgl(3, 3), omega, vtemp(nbrddl)
    real(kind=8) :: pgl1(3, 3), pgl2(3, 3), pgl3(3, 3), rayon, theta
    real(kind=8) :: angmas(3)
    integer :: nno, npg, nbcou, nbsec, m, icompo, ndimv, ivarix
    integer :: ipoids, ivf, nbvari, lgpg, jtab(7)
    integer :: imate, imatuu, icagep, igeom
    integer :: ivarip, ivarim, icontm, icontp, ivectu
    integer :: igau, icou, isect, i, lorien
    integer :: iinstm, iinstp, ideplm, ideplp, icarcr, nbv, icoude, k1, k2
    integer :: icoud2, mmt, codret, cod
    integer :: jnbspi, iret, ksp
    integer :: ndim, nnos, jcoopg, idfdk, jdfd2, jgano
    logical :: vecteu, matric
!
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg,jpoids=ipoids,jcoopg=jcoopg,jvf=ivf,jdfde=idfdk,&
  jdfd2=jdfd2,jgano=jgano)
!
    nc = nbrddl* (nbrddl+1)/2
    pi = r8pi()
    deuxpi = 2.d0*pi
    rac2 = sqrt(2.d0)
    typmod(1) = 'C_PLAN  '
    typmod(2) = '        '
    codret = 0
!
! --- ANGLE DU MOT_CLEF MASSIF (AFFE_CARA_ELEM)
! --- INITIALISE A 0.d0 (ON NE S'EN SERT PAS)
    call r8inir(3, 0.d0, angmas, 1)
!
    m = 3
    if (nomte .eq. 'MET6SEG3') m = 6
!
!=====RECUPERATION NOMBRE DE COUCHES ET DE SECTEURS ANGULAIRES
!
    call jevech('PCOMPOR', 'L', icompo)
    if (zk16(icompo+3) .eq. 'COMP_ELAS') then
        if (zk16(icompo) .ne. 'ELAS') then
            call utmess('F', 'ELEMENTS2_90')
        endif
    endif
!
    call jevech('PNBSP_I', 'L', jnbspi)
    nbcou = zi(jnbspi-1+1)
    nbsec = zi(jnbspi-1+2)
    if (nbcou*nbsec .le. 0) then
        call utmess('F', 'ELEMENTS4_46')
    endif
    if (nbcou .gt. nbcoum) then
        vali = nbcoum
        call utmess('F', 'ELEMENTS5_2', si=vali)
    endif
    if (nbsec .gt. nbsecm) then
        vali = nbsecm
        call utmess('F', 'ELEMENTS5_3', si=vali)
    endif
!
    read (zk16(icompo-1+2),'(I16)') nbvari
    call tecach('OON', 'PVARIMR', 'L', iret, nval=7,&
                itab=jtab)
    lgpg = max(jtab(6),1)*jtab(7)
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PCAGEPO', 'L', icagep)
    h = zr(icagep+1)
    a = zr(icagep) - h/2.d0
! A= RMOY, H = EPAISSEUR, L = LONGUEUR
!
!
    do i = 1, npg
        xpg(i) = zr(jcoopg-1+i)
    end do
!
!  LES POIDS POUR L'INTEGRATION DANS L'EPAISSEUR
!
    poicou(1) = 1.d0/3.d0
    do i = 1, nbcou - 1
        poicou(2*i) = 4.d0/3.d0
        poicou(2*i+1) = 2.d0/3.d0
    end do
    poicou(2*nbcou) = 4.d0/3.d0
    poicou(2*nbcou+1) = 1.d0/3.d0
!
!  LES POIDS POUR L'INTEGRATION SUR LA CIRCONFERENCE
!
    poisec(1) = 1.d0/3.d0
    do i = 1, nbsec - 1
        poisec(2*i) = 4.d0/3.d0
        poisec(2*i+1) = 2.d0/3.d0
    end do
    poisec(2*nbsec) = 4.d0/3.d0
    poisec(2*nbsec+1) = 1.d0/3.d0
!
!   FIN DES POIDS D'INTEGRATION
!
!     --- RECUPERATION DES ORIENTATIONS ---
!
    call jevech('PCAORIE', 'L', lorien)
!
    call carcou(zr(lorien), l, pgl, rayon, theta,&
                pgl1, pgl2, pgl3, pgl4, nno,&
                omega, icoud2)
    if (icoud2 .ge. 10) then
        icoude = icoud2 - 10
        mmt = 0
        if (h/a .gt. (0.25d0)) then
            call utmess('A', 'ELEMENTS4_53')
        endif
    else
        icoude = icoud2
        mmt = 1
    endif
!
    vecteu = ((option.eq.'FULL_MECA') .or. (option.eq.'RAPH_MECA'))
    matric = ((option.eq.'FULL_MECA') .or. (option.eq.'RIGI_MECA_TANG'))
!
    call jevech('PVARIMR', 'L', ivarim)
    call jevech('PINSTMR', 'L', iinstm)
    call jevech('PINSTPR', 'L', iinstp)
    instm = zr(iinstm)
    instp = zr(iinstp)
    call jevech('PDEPLMR', 'L', ideplm)
    call jevech('PDEPLPR', 'L', ideplp)
    call jevech('PCARCRI', 'L', icarcr)
    call jevech('PCONTMR', 'L', icontm)
    call jevech('PMATERC', 'L', imate)
!
    if (vecteu) then
        call jevech('PVECTUR', 'E', ivectu)
        call jevech('PCONTPR', 'E', icontp)
        call jevech('PVARIPR', 'E', ivarip)
        ndimv = npg*nbvari* (2*nbsec+1)* (2*nbcou+1)
        call jevech('PVARIMP', 'L', ivarix)
        call dcopy(ndimv, zr(ivarix), 1, zr(ivarip), 1)
    else
!       -- POUR AVOIR UN TABLEAU BIDON A DONNER A NMCOMP :
        ivarip = ivarim
    endif
!
! ===== INITIALISATION DE LA MATRICE K DE RIGIDITE===
! ===== ET DES EFFORTS INTERNES======================
!
    call matini(nbrddl, nbrddl, 0.d0, ktild)
!
    do i = 1, nbrddl
        effint(i) = 0.d0
    end do
!
    do i = 1, nbrddl
        deplm(i) = zr(ideplm-1+i)
        deplp(i) = zr(ideplp-1+i)
    end do
    if (icoude .eq. 0) then
        call vlggl(nno, nbrddl, pgl, deplm, 'GL',&
                   pass, vtemp)
        call vlggl(nno, nbrddl, pgl, deplp, 'GL',&
                   pass, vtemp)
    else
        call vlgglc(nno, nbrddl, pgl1, pgl2, pgl3,&
                    pgl4, deplm, 'GL', pass, vtemp)
        call vlgglc(nno, nbrddl, pgl1, pgl2, pgl3,&
                    pgl4, deplp, 'GL', pass, vtemp)
    endif
!
!===============================================================
!     RECUPERATION COMPORTEMENT POUR TERME DE CISAILLEMENT
!
    call rccoma(zi(imate), 'ELAS', 1, phenom, valret(1))
!
!
!==============================================================
!
! BOUCLE SUR LES POINTS DE GAUSS
!
    kpgs = 0
    do igau = 1, npg
!
! BOUCLE SUR LES POINTS DE SIMPSON DANS L'EPAISSEUR
!
        do icou = 1, 2*nbcou + 1
            if (mmt .eq. 0) then
                r = a
            else
                r = a + (icou-1)*h/ (2.d0*nbcou) - h/2.d0
            endif
!
! BOUCLE SUR LES POINTS DE SIMPSON SUR LA CIRCONFERENCE
!
            do isect = 1, 2*nbsec + 1
                kpgs = kpgs + 1
                if (icoude .eq. 0) then
!
                    wgt = zr(ipoids-1+igau)*poicou(icou)*poisec(isect) * (l/2.d0)*h*deuxpi/ (4.d0&
                          &*nbcou*nbsec)*r
!
                    call bcoude(igau, icou, isect, l, h,&
                                a, m, nno, nbcou, nbsec,&
                                zr(ivf), zr(idfdk), zr(jdfd2), mmt, b)
                else if (icoude.eq.1) then
                    fi = (isect-1)*deuxpi/ (2.d0*nbsec)
!               FI = FI - OMEGA
                    sinfi = sin(fi)
                    l = theta* (rayon+r*sinfi)
                    call bcoudc(igau, icou, isect, h, a,&
                                m, omega, xpg, nno, nbcou,&
                                nbsec, zr(ivf), zr(idfdk), zr(jdfd2), rayon,&
                                theta, mmt, b)
                    wgt = zr(ipoids-1+igau)*poicou(icou)*poisec(isect) * (l/2.d0)*h*deuxpi/ (4.d0&
                          &*nbcou*nbsec)*r
                endif
!
                k1 = 6* (kpgs-1)
                k2 = lgpg* (igau-1) + ((2*nbsec+1)* (icou-1)+ (isect- 1))* nbvari
!
! ======= CALCUL DES DEFORMATIONS ET INCREMENTS DE DEFORMATION
!
                call epsett('DEFORM', nbrddl, deplm, b, rbid,&
                            epsi, wgt, rbid)
                eps2d(1) = epsi(1)
                eps2d(2) = epsi(2)
                eps2d(3) = 0.d0
                eps2d(4) = epsi(3)/rac2
!
                call epsett('DEFORM', nbrddl, deplp, b, rbid,&
                            depsi, wgt, rbid)
                deps2d(1) = depsi(1)
                deps2d(2) = depsi(2)
                deps2d(3) = 0.d0
                deps2d(4) = depsi(3)/rac2
!
                gxz = epsi(4) + depsi(4)
!
!  RAPPEL DU VECTEUR CONTRAINTE
!
                do i = 1, 3
                    sign(i) = zr(icontm-1+k1+i)
                end do
                sign(4) = zr(icontm-1+k1+4)*rac2
!
!
! -    APPEL A LA LOI DE COMPORTEMENT
                ksp=(icou-1)*(2*nbsec+1) + isect
                call nmcomp('RIGI', igau, ksp, 2, typmod,&
                            zi(imate), zk16(icompo), zr(icarcr), instm, instp,&
                            6, eps2d, deps2d, 6, sign,&
                            zr(ivarim+k2), option, angmas, 1, [0.d0],&
                            sigma, zr( ivarip+k2), 36, dsidep, 1,&
                            rbid(1), cod)
!
                if (phenom .eq. 'ELAS') then
                    nbv = 2
                    nomres(1) = 'E'
                    nomres(2) = 'NU'
                else
                    call utmess('F', 'ELEMENTS_44', sk=phenom)
                endif
!
                call rcvalb('RIGI', igau, ksp, '+', zi(imate),&
                            ' ', phenom, 0, ' ', [0.d0],&
                            nbv, nomres, valres, valret, 1)
!
                cisail = valres(1)/ (1.d0+valres(2))
!           COD=1 : ECHEC INTEGRATION LOI DE COMPORTEMENT
!           COD=3 : C_PLAN DEBORST SIGZZ NON NUL
                if (cod .ne. 0) then
                    if (codret .ne. 1) then
                        codret = cod
                    endif
                endif
!
!    CALCULS DE LA MATRICE TANGENTE : BOUCLE SUR L'EPAISSEUR
!
                if (matric) then
!
                    dtild(1,1) = dsidep(1,1)
                    dtild(1,2) = dsidep(1,2)
                    dtild(1,3) = dsidep(1,4)/rac2
                    dtild(1,4) = 0.d0
!
                    dtild(2,1) = dsidep(2,1)
                    dtild(2,2) = dsidep(2,2)
                    dtild(2,3) = dsidep(2,4)/rac2
                    dtild(2,4) = 0.d0
!
                    dtild(3,1) = dsidep(4,1)/rac2
                    dtild(3,2) = dsidep(4,2)/rac2
                    dtild(3,3) = dsidep(4,4)/2.d0
                    dtild(3,4) = 0.d0
!
                    dtild(4,1) = 0.d0
                    dtild(4,2) = 0.d0
                    dtild(4,3) = 0.d0
                    dtild(4,4) = cisail/2.d0
!
!  LE DERNIER TERME C'EST R DU  R DFI DX DZETA
!
                    call kcoude(nbrddl, wgt, b, dtild, ktild)
                endif
!
                if (vecteu) then
!
                    do i = 1, 3
                        zr(icontp-1+k1+i) = sigma(i)
                    end do
                    zr(icontp-1+k1+4) = sigma(4)/rac2
                    zr(icontp-1+k1+5) = cisail*gxz/2.d0
                    zr(icontp-1+k1+6) = 0.d0
!
!===========    CALCULS DES EFFORTS INTERIEURS
!
                    sgmtd(1) = zr(icontp-1+k1+1)
                    sgmtd(2) = zr(icontp-1+k1+2)
                    sgmtd(3) = zr(icontp-1+k1+4)
                    sgmtd(4) = cisail*gxz/2.d0
!
                    call epsett('EFFORI', nbrddl, rbid, b, sgmtd,&
                                rbid, wgt, effint)
!
                endif
!
            end do
        end do
    end do
!
! STOCKAGE DE LA MATRICE DE RIGIDITE
    if (matric) then
!     CHANGEMENT DE REPERE : REPERE LOCAL AU REPERE GLOBAL
        if (icoude .eq. 0) then
            call klg(nno, nbrddl, pgl, ktild)
        else if (icoude.eq.1) then
            call klgcou(nno, nbrddl, pgl1, pgl2, pgl3,&
                        pgl4, ktild)
        endif
        call jevech('PMATUUR', 'E', imatuu)
        call mavec(ktild, nbrddl, zr(imatuu), nc)
    endif
!
! STOCKAGE DES EFFORTS INTERIEURS
    if (vecteu) then
!     CHANGEMENT DE REPERE : REPERE LOCAL AU REPERE GLOBAL
        if (icoude .eq. 0) then
            call vlggl(nno, nbrddl, pgl, effint, 'LG',&
                       pass, vtemp)
        else
            call vlgglc(nno, nbrddl, pgl1, pgl2, pgl3,&
                        pgl4, effint, 'LG', pass, vtemp)
        endif
        do i = 1,nbrddl
           zr(ivectu-1+i) = effint(i)
        end do
    endif
!
end subroutine
