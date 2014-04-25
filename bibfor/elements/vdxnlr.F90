subroutine vdxnlr(option, nomte, xi, rig, nb1,&
                  codret)
    implicit none
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
#include "jeveux.h"
#include "asterc/r8vide.h"
#include "asterfort/btdfn.h"
#include "asterfort/btdmsn.h"
#include "asterfort/btdmsr.h"
#include "asterfort/btkb.h"
#include "asterfort/epseff.h"
#include "asterfort/hsj1f.h"
#include "asterfort/hsj1ms.h"
#include "asterfort/jevech.h"
#include "asterfort/jevete.h"
#include "asterfort/mahsf.h"
#include "asterfort/mahsms.h"
#include "asterfort/matrkb.h"
#include "asterfort/nmcomp.h"
#include "asterfort/r8inir.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvalb.h"
#include "asterfort/tecach.h"
#include "asterfort/trndgl.h"
#include "asterfort/trnflg.h"
#include "asterfort/utmess.h"
#include "asterfort/vectan.h"
#include "asterfort/vexpan.h"
#include "blas/dcopy.h"
#include "blas/dscal.h"
    integer :: jnbspi
    integer :: valret(26)
    character(len=8) :: nomres(26), typmod(2)
    character(len=10) :: phenom
    character(len=16) :: option, nomte
    integer :: nb1, nb2, nddle, npge, npgsr, npgsn, itab(8), codret
    integer :: cod, ksp
    real(kind=8) :: xi(3, 9)
    real(kind=8) :: vecta(9, 2, 3), vectn(9, 3), vectpt(9, 2, 3), vecpt(9, 3, 3)
    real(kind=8) :: vectg(2, 3), vectt(3, 3)
    real(kind=8) :: hsfm(3, 9), hss(2, 9), hsj1m(3, 9), hsj1s(2, 9)
    real(kind=8) :: btdm(4, 3, 42), btds(4, 2, 42)
    real(kind=8) :: hsf(3, 9), hsj1fx(3, 9), wgt
    real(kind=8) :: btdf(3, 42), btild(5, 42), wmatcb(5, 42), ktildi(42, 42)
    real(kind=8) :: ktild(42, 42), rig(51, 51)
    real(kind=8) :: ctor, epais, kappa
    real(kind=8) :: valres(26)
    real(kind=8) :: rotfcm(9), rotfcp(9)
    real(kind=8) :: deplm(42), deplp(42)
    real(kind=8) :: epsi(5), depsi(5), eps2d(4), deps2d(4)
    real(kind=8) :: dtild(5, 5), sgmtd(5), effint(42), vecl(48), vecll(51)
    real(kind=8) :: sign(4), sigma(4), dsidep(6, 6), angmas(3), rbid(1)
    logical :: vecteu, matric
!-----------------------------------------------------------------------
    integer :: i, ib, icarcr, icompo, icontm, icontp, icou
    integer :: ideplm, ideplp, iinstm, iinstp, imate, inte, intsn
    integer :: intsr, iret, ivarim, ivarip, ivarix, ivectu, j
    integer :: jcara, jcrf, k1, k2, kpgs, kwgt, lgpg
    integer :: lzi, lzr, nbcou, nbv, nbvari, nddlet, ndimv
!
    real(kind=8) :: cisail, coef, crf, gxz, gyz, hic, rac2
    real(kind=8) :: x(1), zic, zmin
!-----------------------------------------------------------------------
    parameter (npge=3)
    real(kind=8) :: ksi3s2
! ----------------------------------------------------------------------
!
    rac2 = sqrt(2.d0)
    typmod(1) = 'C_PLAN  '
    typmod(2) = '        '
    codret = 0
    nbv = 2
    nomres(1) = 'E'
    nomres(2) = 'NU'
!
    call jevete('&INEL.'//nomte(1:8)//'.DESI', ' ', lzi)
    nb1 = zi(lzi-1+1)
    nb2 = zi(lzi-1+2)
    npgsr = zi(lzi-1+3)
    npgsn = zi(lzi-1+4)
!
    nddle = 5*nb1 + 2
    do i = 1, nddle
        do j = 1, nddle
            ktild(i,j) = 0.d0
        end do
        effint(i) = 0.d0
    end do
!
    call jevete('&INEL.'//nomte(1:8)//'.DESR', ' ', lzr)
!
    vecteu = ((option.eq.'FULL_MECA') .or. (option.eq.'RAPH_MECA'))
    matric = ((option.eq.'FULL_MECA') .or. (option(1:10).eq.'RIGI_MECA_'))
!
    call jevech('PMATERC', 'L', imate)
    call jevech('PVARIMR', 'L', ivarim)
    call jevech('PINSTMR', 'L', iinstm)
    call jevech('PINSTPR', 'L', iinstp)
    call jevech('PDEPLMR', 'L', ideplm)
    call jevech('PDEPLPR', 'L', ideplp)
    call jevech('PCOMPOR', 'L', icompo)
    call jevech('PNBSP_I', 'L', jnbspi)
    call jevech('PCARCRI', 'L', icarcr)
    call jevech('PCONTMR', 'L', icontm)
    call jevech('PVARIMP', 'L', ivarix)
!
    nbcou=zi(jnbspi-1+1)
    if (nbcou .le. 0) then
        call utmess('F', 'ELEMENTS_12')
    endif


    read (zk16(icompo-1+2),'(I16)') nbvari
    call tecach('OON', 'PVARIMR', 'L', iret, nval=7,&
                itab=itab)
!      LGPG = MAX(ITAB(6),1)*ITAB(7) resultats faux sur Bull avec ifort
    if (itab(6) .le. 1) then
        lgpg=itab(7)
    else
        lgpg = itab(6)*itab(7)
    endif
!
    call jevech('PCACOQU', 'L', jcara)
    epais = zr(jcara)
    kappa = zr(jcara+3)
    ctor = zr(jcara+4)
    zmin = -epais/2.d0
    hic = epais/nbcou
!
    if (option .eq. 'RAPH_MECA') then
        call jevech('PCACO3D', 'L', jcrf)
        crf = zr(jcrf)
    else
        call jevech('PCACO3D', 'E', jcrf)
    endif
!
    if (vecteu) then
        call jevech('PVECTUR', 'E', ivectu)
        call jevech('PCONTPR', 'E', icontp)
        call jevech('PVARIPR', 'E', ivarip)
    else
!       -- POUR AVOIR UN TABLEAU BIDON A DONNER A NMCOMP :
        ivarip = ivarim
    endif
!
    ndimv=lgpg*npgsn
    call dcopy(ndimv, zr(ivarix), 1, zr(ivarip), 1)
!
    call vectan(nb1, nb2, xi, zr(lzr), vecta,&
                vectn, vectpt)
!
    call rccoma(zi(imate), 'ELAS', 1, phenom, valret(1))
!
    if (phenom .ne. 'ELAS') then
        call utmess('F', 'ELEMENTS_45', sk=phenom)
    endif
!
!===============================================================
!     CALCULS DES 2 DDL INTERNES
!
    call trndgl(nb2, vectn, vectpt, zr(ideplm), deplm,&
                rotfcm)
!
    call trndgl(nb2, vectn, vectpt, zr(ideplp), deplp,&
                rotfcp)
!
    kwgt = 0
    kpgs = 0
    do icou = 1, nbcou
        do inte = 1, npge
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
            ksi3s2 = zic/hic
!
!     CALCUL DE BTDMR, BTDSR : M=MEMBRANE , S=CISAILLEMENT , R=REDUIT
!
            do intsr = 1, npgsr
                call mahsms(0, nb1, xi, ksi3s2, intsr,&
                            zr(lzr), hic, vectn, vectg, vectt,&
                            hsfm, hss)
!
                call hsj1ms(hic, vectg, vectt, hsfm, hss,&
                            hsj1m, hsj1s)
!
                call btdmsr(nb1, nb2, ksi3s2, intsr, zr(lzr),&
                            hic, vectpt, hsj1m, hsj1s, btdm,&
                            btds)
            end do
!
            do intsn = 1, npgsn
!
!     CALCUL DE BTDFN : F=FLEXION , N=NORMAL
!     ET DEFINITION DE WGT=PRODUIT DES POIDS ASSOCIES AUX PTS DE GAUSS
!                          (NORMAL) ET DU DETERMINANT DU JACOBIEN
!
                call mahsf(1, nb1, xi, ksi3s2, intsn,&
                           zr(lzr), hic, vectn, vectg, vectt,&
                           hsf)
!
                call hsj1f(intsn, zr(lzr), hic, vectg, vectt,&
                           hsf, kwgt, hsj1fx, wgt)
!
!     PRODUIT DU POIDS DES PTS DE GAUSS DANS L'EPAISSEUR ET DE WGT
!
                wgt = coef*wgt
!
                call btdfn(1, nb1, nb2, ksi3s2, intsn,&
                           zr(lzr), hic, vectpt, hsj1fx, btdf)
!
!     CALCUL DE BTDMN, BTDSN
!     ET
!     FORMATION DE BTILD
!
                call btdmsn(1, nb1, intsn, npgsr, zr(lzr),&
                            btdm, btdf, btds, btild)
!
!     CALCULS DES COMPOSANTES DE DEFORMATIONS TRIDIMENSIONNELLES :
!     EPSXX, EPSYY, EPSXY, EPSXZ, EPSYZ (CE SONT LES COMPOSANTES TILDE)
                kpgs = kpgs + 1
                call epseff('DEFORM', nb1, deplm, btild, x,&
                            epsi, wgt, x)
                eps2d(1) = epsi(1)
                eps2d(2) = epsi(2)
                eps2d(3) = 0.d0
                eps2d(4) = epsi(3)/rac2
!
                call epseff('DEFORM', nb1, deplp, btild, x,&
                            depsi, wgt, x)
                deps2d(1) = depsi(1)
                deps2d(2) = depsi(2)
                deps2d(3) = 0.d0
                deps2d(4) = depsi(3)/rac2
!
                gxz = epsi(4) + depsi(4)
                gyz = epsi(5) + depsi(5)
!
                k1=6*((intsn-1)*npge*nbcou + (icou-1)*npge +inte - 1)
                k2 = lgpg* (intsn-1) + (npge* (icou-1)+inte-1)*nbvari
                do i = 1, 3
                    sign(i) = zr(icontm-1+k1+i)
                end do
                sign(4) = zr(icontm-1+k1+4)*rac2
! - LOI DE COMPORTEMENT
! --- ANGLE DU MOT_CLEF MASSIF (AFFE_CARA_ELEM)
! --- INITIALISE A R8VIDE (ON NE S'EN SERT PAS)
                call r8inir(3, r8vide(), angmas, 1)
! -    APPEL A LA LOI DE COMPORTEMENT
                ksp= (icou-1)*npge + inte
                call nmcomp('MASS', intsn, ksp, 2, typmod,&
                            zi(imate), zk16(icompo), zr(icarcr), zr(iinstm), zr(iinstp),&
                            4, eps2d, deps2d, 4, sign,&
                            zr(ivarim+k2), option, angmas, 1, [0.d0],&
                            sigma, zr(ivarip+k2), 36, dsidep, 1,&
                            rbid, cod)
!
                if (phenom .eq. 'ELAS') then
                    nbv = 2
                    nomres(1) = 'E'
                    nomres(2) = 'NU'
                else
                    call utmess('F', 'ELEMENTS_45', sk=phenom)
                endif
!
                call rcvalb('MASS', intsn, ksp, '+', zi(imate),&
                            ' ', phenom, 0, ' ', [0.d0],&
                            nbv, nomres, valres, valret, 1)
!
                cisail = valres(1)/ (1.d0+valres(2))
!
!           COD=1 : ECHEC INTEGRATION LOI DE COMPORTEMENT
!           COD=3 : C_PLAN DEBORST SIGZZ NON NUL
                if (cod .ne. 0) then
                    if (codret .ne. 1) then
                        codret=cod
                    endif
                    if (cod .eq. 1) goto 999
                endif
!
!    CALCULS DE LA MATRICE TANGENTE : BOUCLE SUR L'EPAISSEUR
                if (matric) then
!
                    dtild(1,1) = dsidep(1,1)
                    dtild(1,2) = dsidep(1,2)
                    dtild(1,3) = dsidep(1,4)/rac2
                    dtild(1,4) = 0.d0
                    dtild(1,5) = 0.d0
!
                    dtild(2,1) = dsidep(2,1)
                    dtild(2,2) = dsidep(2,2)
                    dtild(2,3) = dsidep(2,4)/rac2
                    dtild(2,4) = 0.d0
                    dtild(2,5) = 0.d0
!
                    dtild(3,1) = dsidep(4,1)/rac2
                    dtild(3,2) = dsidep(4,2)/rac2
                    dtild(3,3) = dsidep(4,4)/2.d0
                    dtild(3,4) = 0.d0
                    dtild(3,5) = 0.d0
!
                    dtild(4,1) = 0.d0
                    dtild(4,2) = 0.d0
                    dtild(4,3) = 0.d0
                    dtild(4,4) = cisail*kappa/2.d0
                    dtild(4,5) = 0.d0
!
                    dtild(5,1) = 0.d0
                    dtild(5,2) = 0.d0
                    dtild(5,3) = 0.d0
                    dtild(5,4) = 0.d0
                    dtild(5,5) = cisail*kappa/2.d0
!
                    call dscal(25, wgt, dtild, 1)
!
                    call btkb(5, 42, nddle, dtild, btild,&
                              wmatcb, ktildi)
!
                    do i = 1, nddle
                        do j = 1, nddle
                            ktild(i,j) = ktild(i,j) + ktildi(i,j)
                        end do
                    end do
                endif
!
                if (vecteu) then
!
                    do i = 1, 3
                        zr(icontp-1+k1+i) = sigma(i)
                    end do
                    zr(icontp-1+k1+4) = sigma(4)/rac2
                    zr(icontp-1+k1+5) = cisail*kappa*gxz/2.d0
                    zr(icontp-1+k1+6) = cisail*kappa*gyz/2.d0
!
!    CALCULS DES EFFORTS INTERIEURS
                    sgmtd(1) = zr(icontp-1+k1+1)
                    sgmtd(2) = zr(icontp-1+k1+2)
                    sgmtd(3) = zr(icontp-1+k1+4)
                    sgmtd(4) = cisail*kappa*gxz/2.d0
                    sgmtd(5) = cisail*kappa*gyz/2.d0
!
                    call epseff('EFFORI', nb1, x, btild, sgmtd,&
                                x, wgt, effint)
!
                endif
!
             end do
         end do
     end do
!
    if (matric) then
!
!     EXPANSION DE LA MATRICE : AJOUTER DE LA ROTATION FICTIVE
!
        nddlet = 6*nb1 + 3
        call matrkb(nb1, 42, 51, nddlet, ktild,&
                    ctor, rig, crf)
        zr(jcrf) = crf
!
!     AJOUTER DES 3 TRANSLATIONS FICTIVES ASSOCIEES AU NOEUD INTERNE
!     LES 3 TERMES DE RAIDEUR (FICTIVE) ASSOCIEES ONT POUR VALEUR CELLE
!     DES ROTATION FICTIVE
!
    endif
!
    if (vecteu) then
!
        call vexpan(nb1, effint, vecl)
!
        do i = 1, 6*nb1
            vecll(i) = vecl(i)
        end do
        vecll(6*nb1+1) = effint(5*nb1+1)
        vecll(6*nb1+2) = effint(5*nb1+2)
        vecll(6*nb1+3) = 0.d0
!
!     CONTRIBUTION DES DDL DE LA ROTATION FICTIVE DANS EFFINT
!
        do i = 1, nb1
            vecll(6*i) = crf* (rotfcm(i)+rotfcp(i))
        end do
        i = nb2
        vecll(6*nb1+3) = crf* (rotfcm(nb2)+rotfcp(nb2))
!     TRANFORMATION DANS REPERE GLOBAL PUIS STOCKAGE
        do ib = 1, nb2
            do i = 1, 2
                do j = 1, 3
                    vecpt(ib,i,j) = vectpt(ib,i,j)
                end do
            end do
            vecpt(ib,3,1) = vectn(ib,1)
            vecpt(ib,3,2) = vectn(ib,2)
            vecpt(ib,3,3) = vectn(ib,3)
        end do
!
        call trnflg(nb2, vecpt, vecll, zr(ivectu))
    endif
!
999 continue
end subroutine
