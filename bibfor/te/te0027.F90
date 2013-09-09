subroutine te0027(option, nomte)
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
! FONCTION REALISEE:
!
!      CALCUL DU TAUX DE RESTITUTION D'ENERGIE ELEMENTAIRE
!      EN ELASTICITE LINEAIRE ET NON LINEAIRE
!      ELEMENTS ISOPARAMETRIQUES 3D
!
!      OPTION : 'CALC_G'          (LOCAL,CHARGES REELLES)
!               'CALC_G_F'        (LOCAL,CHARGES FONCTIONS)
!               'CALC_G_GLOB'     (GLOBAL,CHARGES REELLES)
!               'CALC_G_GLOB_F'   (GLOBAL,CHARGES FONCTIONS)
!
! ENTREES  ---> OPTION : OPTION DE CALCUL
!          ---> NOMTE  : NOM DU TYPE ELEMENT
!
! ----------------------------------------------------------------------
! CORPS DU PROGRAMME
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/elref4.h"
#include "asterfort/fointe.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jevech.h"
#include "asterfort/nmelnl.h"
#include "asterfort/nmgeom.h"
#include "asterfort/nmplru.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvala.h"
#include "asterfort/rcvalb.h"
#include "asterfort/rcvarc.h"
#include "asterfort/tecach.h"
#include "asterfort/utmess.h"
!
    character(len=16) :: option, nomte
!
!
! DECLARATION VARIABLES LOCALES
    integer :: ipoids, ivf, idfde
    integer :: icomp, igeom, itemps, idepl, imate
    integer :: iepsr, iepsf, isigi, isigm, iepsp, ivari
    integer :: iforc, iforf, ithet, igthet, irota, ipesa, ier
    integer :: jgano, nno, nnos, npg, ncmp
    integer :: i, j, k, kk, l, m, kp, ndim, compt, nbvari, iret
    integer :: ivites, iaccel, j1, j2, ireth, matcod
!
    real(kind=8) :: epsref(6), epsi, rac2, crit(3)
    real(kind=8) :: dfdi(81), f(3, 3), sr(3, 3)
    real(kind=8) :: eps(6), epsin(6), depsin(6, 3), epsp(6), depsp(6, 3)
    real(kind=8) :: epsino(162), fno(81)
    real(kind=8) :: sigl(6), sigin(6), dsigin(6, 3)
    real(kind=8) :: thet, tgdm(3), tgd(20)
    real(kind=8) :: prod, prod1, prod2, divt, valpar(4)
    real(kind=8) :: tcla, tthe, tfor, tplas, tini, poids, rbid,dsidep(6,6)
    real(kind=8) :: dudm(3, 4), dfdm(3, 4), dtdm(3, 4), der(4), dvdm(3, 4)
    real(kind=8) :: p, ppg, dpdm(3), rp, energi(2), rho(1), om, omo
    real(kind=8) :: ecin, prod3, prod4, accele(3), e(1), nu(1), mu
!
    logical :: grand, fonc, incr, epsini
!
    integer :: icodre(1)
    character(len=4) :: fami
    character(len=8) :: nompar(4), typmod(2)
    character(len=16) :: compor(4), oprupt, phenom
!
! DEB ------------------------------------------------------------------
!
    call jemarq()
!
    epsi = r8prem()
    rac2 = sqrt(2.d0)
    oprupt = 'RUPTURE'
    epsini = .false.
    typmod(1) = '3D      '
    typmod(2) = ' '
!
    fami = 'RIGI'
    call elref4(' ', fami, ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
!
    ncmp = 2*ndim
!
! - PAS DE CALCUL DE G POUR LES ELEMENTS OU LA VALEUR DE THETA EST NULLE
    call jevech('PTHETAR', 'L', ithet)
    call jevech('PGTHETA', 'E', igthet)
    tcla = 0.d0
    tthe = 0.d0
    tfor = 0.d0
    tplas = 0.d0
    tini = 0.d0
    compt = 0
    do 40 i = 1, nno
        thet = 0.d0
        do 30 j = 1, ndim
            thet = thet + abs(zr(ithet+ndim* (i-1)+j-1))
30      continue
        if (thet .lt. epsi) compt = compt + 1
40  end do
!
    if (compt .eq. nno) goto 9999
!
    ivites = 0
    iaccel = 0
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PDEPLAR', 'L', idepl)
    call jevech('PMATERC', 'L', imate)
    call jevech('PCOMPOR', 'L', icomp)
    matcod = zi(imate)
! RECUPERATION DU CHAMP LOCAL (CARTE) ASSOCIE AU PRE-EPSI
! CE CHAMP EST ISSU D UN CHARGEMENT PRE-EPSI
    if (option .eq. 'CALC_G_F' .or. option .eq. 'CALC_G_GLOB_F') then
        fonc = .true.
        call jevech('PFFVOLU', 'L', iforf)
        call jevech('PTEMPSR', 'L', itemps)
        nompar(1) = 'X'
        nompar(2) = 'Y'
        nompar(3) = 'Z'
        nompar(4) = 'INST'
        valpar(4) = zr(itemps)
        call tecach('ONN', 'PEPSINF', 'L', 1, iepsf,&
                    iret)
        if (iepsf .ne. 0) epsini = .true.
    else
        fonc = .false.
        call jevech('PFRVOLU', 'L', iforc)
        call tecach('ONN', 'PEPSINR', 'L', 1, iepsr,&
                    iret)
        if (iepsr .ne. 0) epsini = .true.
    endif
!
! LOI DE COMPORTEMENT
    do 50 i = 1, 4
        compor(i) = zk16(icomp+i-1)
50  end do
    grand = compor(3).eq. 'GROT_GDEP'
    incr = compor(4) (1:9) .eq. 'COMP_INCR'
    read (zk16(icomp+1),'(I16)') nbvari
    if (incr) then
        call jevech('PCONTRR', 'L', isigm)
        call jevech('PDEFOPL', 'L', iepsp)
        call jevech('PVARIPR', 'L', ivari)
    endif
    call tecach('ONN', 'PPESANR', 'L', 1, ipesa,&
                iret)
    call tecach('ONN', 'PROTATR', 'L', 1, irota,&
                iret)
    call tecach('ONN', 'PSIGINR', 'L', 1, isigi,&
                iret)
    if (option .eq. 'CALC_G' .or. option .eq. 'CALC_G_F' .or. option .eq. 'CALC_G_GLOB'&
        .or. option .eq. 'CALC_G_GLOB_F') then
        call tecach('ONN', 'PVITESS', 'L', 1, ivites,&
                    iret)
        call tecach('ONN', 'PACCELE', 'L', 1, iaccel,&
                    iret)
    endif
!
    do 60 i = 1, ncmp*nno
        epsino(i) = 0.d0
60  end do
!
! =====================================================================
! MESSAGES D'ERREURS
! =====================================================================
!
! ON NE PEUT AVOIR SIMULTANEMENT PRE-DEFORMATIONS ET CONTRAINTES INIT.
    if ((isigi.ne.0) .and. epsini) then
        call utmess('F', 'RUPTURE1_20')
    endif
!
! =====================================================================
! RECUPERATION DES CHARGES ET PRE-DEFORMATIONS (CHARGEMENT PRE-EPSI)
! =====================================================================
    if (fonc) then
        do 100 i = 1, nno
            do 70 j = 1, ndim
                valpar(j) = zr(igeom+ndim* (i-1)+j-1)
70          continue
            do 80 j = 1, ndim
                kk = ndim* (i-1) + j
                call fointe('FM', zk8(iforf+j-1), 4, nompar, valpar,&
                            fno( kk), ier)
80          continue
            if (epsini) then
                do 90 j = 1, ncmp
                    kk = ncmp* (i-1) + j
                    call fointe('FM', zk8(iepsf+j-1), 4, nompar, valpar,&
                                epsino(kk), ier)
90              continue
            endif
100      continue
    else
        do 130 i = 1, nno
            do 110 j = 1, ndim
                fno(ndim* (i-1)+j) = zr(iforc+ndim* (i-1)+j-1)
110          continue
            if (epsini) then
                do 120 j = 1, 3
                    epsino(ncmp* (i-1)+j) = zr(iepsr+ncmp* (i-1)+j-1)
                    epsino(ncmp* (i-1)+j+3) = zr(iepsr+ncmp* (i-1)+j- 1+3)*rac2
120              continue
            endif
130      continue
    endif
!
    if (ivites .ne. 0) then
        call rccoma(matcod, 'ELAS', 1, phenom, icodre(1))
        call rcvalb('RIGI', 1, 1, '+', matcod,&
                    ' ', phenom, 1, ' ', [rbid],&
                    1, 'RHO', rho, icodre(1), 1)
    endif
!
!
! CORRECTION DES FORCES VOLUMIQUES
    if ((ipesa.ne.0) .or. (irota.ne.0)) then
        call rccoma(matcod, 'ELAS', 1, phenom, icodre(1))
        call rcvalb('RIGI', 1, 1, '+', matcod,&
                    ' ', phenom, 1, ' ', [rbid],&
                    1, 'RHO', rho, icodre(1), 1)
        if (ipesa .ne. 0) then
            do 150 i = 1, nno
                do 140 j = 1, ndim
                    kk = ndim* (i-1) + j
                    fno(kk) = fno(kk) + rho(1)*zr(ipesa)*zr(ipesa+j)
140              continue
150          continue
        endif
        if (irota .ne. 0) then
            om = zr(irota)
            do 180 i = 1, nno
                omo = 0.d0
                do 160 j = 1, ndim
                    omo = omo + zr(irota+j)*zr(igeom+ndim* (i-1)+j-1)
160              continue
                do 170 j = 1, ndim
                    kk = ndim* (i-1) + j
                    fno(kk) = fno(kk) + rho(1)*om*om* (zr(igeom+kk-1)- omo*zr(irota+j))
170              continue
180          continue
        endif
    endif
!
!
!
! ======================================================================
!
! CALCUL DU GRADIENT DE TEMPERATURE :
! SI LA TEMPERATURE N'EXISTE PAS, ON LUI IMPOSE UNE VALEUR NULLE
    do 645 kp = 1, nno
        call rcvarc(' ', 'TEMP', '+', 'NOEU', kp,&
                    1, tgd(kp), ireth)
        if (ireth .eq. 1) tgd(kp) = 0.d0
645  end do
!
!
! ======================================================================
! BOUCLE PRINCIPALE SUR LES POINTS DE GAUSS
! ======================================================================
!
    do 800 kp = 1, npg
!
! INITIALISATIONS
        l = (kp-1)*nno
        ppg = 0.d0
        do 240 i = 1, 3
            tgdm(i) = 0.d0
            dpdm(i) = 0.d0
            accele(i) = 0.d0
            do 220 j = 1, 3
                sr(i,j) = 0.d0
220          continue
            do 230 j = 1, 4
                dudm(i,j) = 0.d0
                dvdm(i,j) = 0.d0
                dtdm(i,j) = 0.d0
                dfdm(i,j) = 0.d0
230          continue
240      continue
        do 260 i = 1, 6
            sigl(i) = 0.d0
            sigin(i) = 0.d0
            epsin(i) = 0.d0
            epsp(i) = 0.d0
            eps(i) = 0.d0
            epsref(i) =0.d0
            do 250 j = 1, 3
                dsigin(i,j) = 0.d0
                depsin(i,j) = 0.d0
                depsp(i,j) = 0.d0
250          continue
260      continue
!
! - CALCUL DES ELEMENTS GEOMETRIQUES
!
        call nmgeom(ndim, nno, .false., grand, zr(igeom),&
                    kp, ipoids, ivf, idfde, zr(idepl),&
                    .true., poids, dfdi, f, eps,&
                    rbid)
!
! - CALCULS DES GRADIENTS DE U (DUDM),THETA (DTDM) ET FORCE(DFDM)
!   DU GRADIENT (TGDM) DE LA TEMPERATURE AUX POINTS DE GAUSS (TG)
!
        do 290 i = 1, nno
            der(1) = dfdi(i)
            der(2) = dfdi(i+nno)
            der(3) = dfdi(i+2*nno)
            der(4) = zr(ivf+l+i-1)
            do 280 j = 1, ndim
                tgdm(j) = tgdm(j) + tgd(i)*der(j)
                do 270 k = 1, ndim
                    dudm(j,k) = dudm(j,k) + zr(idepl+ndim* (i-1)+j-1)* der(k)
                    dtdm(j,k) = dtdm(j,k) + zr(ithet+ndim* (i-1)+j-1)* der(k)
                    dfdm(j,k) = dfdm(j,k) + fno(ndim* (i-1)+j)*der(k)
270              continue
                if (ivites .ne. 0) then
                    do 305 k = 1, ndim
                        dvdm(j,k) = dvdm(j,k) + zr(ivites+ndim*(i-1)+ j-1)*der(k)
305                  continue
                    dvdm(j,4) = dvdm(j,4) + zr(ivites+ndim*(i-1)+j-1)* der(4)
                    accele(j) = accele(j) + zr(iaccel+ndim*(i-1)+j-1)* der(4)
                endif
                dudm(j,4) = dudm(j,4) + zr(idepl+ndim* (i-1)+j-1)*der( 4)
                dtdm(j,4) = dtdm(j,4) + zr(ithet+ndim* (i-1)+j-1)*der( 4)
                dfdm(j,4) = dfdm(j,4) + fno(ndim* (i-1)+j)*der(4)
280          continue
290      continue
! =======================================================
! PLASTICITE
! =======================================================
! - CALCULS DES GRADIENTS DE P (DPDM) ET EPSP (DEPSP) EN PLASTICITE
!
        if (incr) then
            do 340 i = 1, nno
                der(1) = dfdi(i)
                der(2) = dfdi(i+nno)
                der(3) = dfdi(i+2*nno)
                der(4) = zr(ivf+l+i-1)
                p = zr(ivari+ (i-1)*nbvari)
                ppg = ppg + p*der(4)
                do 300 j = 1, ncmp
                    epsp(j) = epsp(j) + zr(iepsp+ncmp* (i-1)+j-1)*der( 4)
300              continue
                if (p .ge. epsi) then
                    do 310 j = 1, ndim
                        dpdm(j) = dpdm(j) + zr(ivari+ (i-1)*nbvari)* der(j)
310                  continue
                    do 330 k = 1, ndim
                        do 320 j = 1, ncmp
                            depsp(j,k) = depsp(j,k) + zr(iepsp+ncmp* ( i-1)+j-1)*der(k)
320                      continue
330                  continue
                endif
340          continue
            do 360 i = 4, ncmp
                epsp(i) = epsp(i)*rac2
                do 350 j = 1, ndim
                    depsp(i,j) = depsp(i,j)*rac2
350              continue
360          continue
            if (ppg .lt. epsi) then
                ppg = 0.d0
                do 370 j = 1, ncmp
                    epsp(j) = 0.d0
370              continue
            endif
        endif
!
! =======================================================
! PRE DEFORMATIONS ET LEUR GRADIENT DEPSIN
! (seule intervenant dans le calcul de G)
! =======================================================
!
        if (epsini) then
            do 410 i = 1, nno
                der(1) = dfdi(i)
                der(2) = dfdi(i+nno)
                der(3) = dfdi(i+2*nno)
                der(4) = zr(ivf+l+i-1)
                do 380 j = 1, ncmp
                    epsin(j) = epsin(j) + epsino(ncmp* (i-1)+j)*der(4)
380              continue
                do 400 j = 1, ncmp
                    do 390 k = 1, ndim
                        depsin(j,k) = depsin(j,k) + epsino(ncmp* (i-1) +j)*der(k)
390                  continue
400              continue
410          continue
            do 420 i = 1, ncmp
                eps(i) = eps(i) - epsin(i)
420          continue
        endif
!
! =======================================================
! CALCUL DES CONTRAINTES LAGRANDIENNES SIGL ET DE L'ENERGIE LIBRE
! =======================================================
!
        if (incr) then
! EN PLASTICITE
            call nmplru(fami, kp, 1, '+', ndim,&
                        typmod, matcod, compor, ppg, eps,&
                        epsp, rp, energi)
            do 430 i = 1, 3
                sigl(i) = zr(isigm+ncmp* (kp-1)+i-1)
                sigl(i+3) = zr(isigm+ncmp* (kp-1)+i-1+3)*rac2
430          continue
!
        else
!
            crit(1) = 300
            crit(2) = 0.d0
            crit(3) = 1.d-3
            call nmelnl(fami, kp, 1, '+', ndim,&
                        typmod, matcod, compor, crit, oprupt,&
                        eps, sigl, rbid, dsidep, energi)
            call tecach('NNN', 'PCONTGR', 'L', 1, isigm,&
                        iret)
            if (iret .eq. 0) then
                call jevech('PCONTGR', 'L', isigm)
                do 401 i = 1, 3
                    sigl(i) = zr(isigm+ncmp* (kp-1)+i-1)
                    sigl(i+3) = zr(isigm+ncmp* (kp-1)+i-1+3)*rac2
401              continue
            endif
        endif
        divt = dtdm(1,1) + dtdm(2,2) + dtdm(3,3)
!
! =======================================================
! CORRECTIONS LIEES A LA CONTRAINTE INITIALE (SIGM_INIT DE CALC_G)
! CONTRAINTE, DEFORMATION DE REFERENCE, ENERGIE ELASTIQUE
! =======================================================
!
        if (isigi .ne. 0) then
            do 470 i = 1, nno
                der(1) = dfdi(i)
                der(2) = dfdi(i+nno)
                der(3) = dfdi(i+2*nno)
                der(4) = zr(ivf+l+i-1)
! CALCUL DE SIGMA INITIAL
                do 440 j = 1, ncmp
                    sigin(j) = sigin(j) + zr(isigi+ncmp* (i-1)+j-1)* der(4)
440              continue
! CALCUL DU GRADIENT DE SIGMA INITIAL
                do 460 j = 1, ncmp
                    do 450 k = 1, ndim
                        dsigin(j,k) = dsigin(j,k) + zr(isigi+ncmp* (i- 1)+j-1)*der(k)
450                  continue
460              continue
470          continue
!
! TRAITEMENTS PARTICULIERS DES TERMES CROISES
            do 490 i = 4, ncmp
                sigin(i) = sigin(i)*rac2
                do 480 j = 1, ndim
                    dsigin(i,j) = dsigin(4,1)*rac2
480              continue
490          continue
!
! CALCUL DE LA DEFORMATION DE REFERENCE
            call rccoma(matcod, 'ELAS', 1, phenom, icodre(1))
            call rcvala(matcod, ' ', phenom, 1, ' ',&
                        [rbid], 1, 'NU', nu(1), icodre(1),&
                        1)
            call rcvala(matcod, ' ', phenom, 1, ' ',&
                        [rbid], 1, 'E', e(1), icodre(1),&
                        1)
!
            mu = e(1)/(2.d0*(1.d0+nu(1)))
!
            epsref(1)=-(1.d0/e(1))*(sigin(1)-(nu(1)*(sigin(2)+sigin(3))))
            epsref(2)=-(1.d0/e(1))*(sigin(2)-(nu(1)*(sigin(3)+sigin(1))))
            epsref(3)=-(1.d0/e(1))*(sigin(3)-(nu(1)*(sigin(1)+sigin(2))))
            epsref(4)=-(1.d0/mu)*sigin(4)
            epsref(5)=-(1.d0/mu)*sigin(5)
            epsref(6)=-(1.d0/mu)*sigin(6)
!
! ENERGIE ELASTIQUE (expression WADIER)
            do 465 i = 1, ncmp
                energi(1) = energi(1) + (eps(i)-0.5d0*epsref(i))* sigin(i)
465          continue
        endif
!
! =======================================================
! STOCKAGE DE SIGMA ET TRAITEMENTS DES TERMES CROISES
! =======================================================
        sr(1,1) = sigl(1)
        sr(2,2) = sigl(2)
        sr(3,3) = sigl(3)
        sr(1,2) = sigl(4)/rac2
        sr(1,3) = sigl(5)/rac2
        sr(2,3) = sigl(6)/rac2
        sr(2,1) = sr(1,2)
        sr(3,1) = sr(1,3)
        sr(3,2) = sr(2,3)
!
! - CALCUL DE G
!
! =======================================================
! - TERME THERMOELASTIQUE CLASSIQUE F.SIG:(GRAD(U).GRAD(THET))-ENER*DIVT
! =======================================================
        ecin = 0.d0
        prod3 = 0.d0
        prod4 = 0.d0
        if (ivites .ne. 0) then
            do 487 j1 = 1, ndim
                ecin = ecin + dvdm(j1,4)*dvdm(j1,4)
487          continue
            do 496 j1 = 1, ndim
                do 497 j2 = 1, ndim
                    prod3 = prod3 + accele(j1)*dudm(j1,j2)*dtdm(j2,4)
                    prod4 = prod4 + dvdm(j1,4)*dvdm(j1,j2)*dtdm(j2,4)
497              continue
496          continue
            ecin = 0.5d0*rho(1)*ecin
            prod3 = rho(1)*prod3
            prod4 = rho(1)*prod4
        endif
!
        prod = 0.d0
        do 550 i = 1, 3
            do 540 j = 1, 3
                do 530 k = 1, 3
                    do 520 m = 1, 3
                        prod = prod + f(i,j)*sr(j,k)*dudm(i,m)*dtdm(m, k)
520                  continue
530              continue
540          continue
550      continue
        prod = prod - ecin*divt + prod3 - prod4
        tcla = tcla + poids* (prod-energi(1)*divt)
!
! =======================================================
! - TERME THERMIQUE :   -(D(ENER)/DT)(GRAD(T).THETA)
! =======================================================
        if (ireth .eq. 0) then
            prod = 0.d0
            do 560 i = 1, ndim
                prod = prod + tgdm(i)*dtdm(i,4)
560          continue
            tthe = tthe - poids*prod*energi(2)
        endif
! =======================================================
! - TERME FORCE VOLUMIQUE
! =======================================================
        do 580 i = 1, ndim
            prod = 0.d0
            do 570 j = 1, ndim
                prod = prod + dfdm(i,j)*dtdm(j,4)
570          continue
            tfor = tfor + dudm(i,4)* (prod+dfdm(i,4)*divt)*poids
580      continue
!
! =======================================================
! - TERME PLASTIQUE :   SIG:(GRAD(EPSP).THETA)- R(P).GRAD(P).THETA
! =======================================================
        if (incr) then
            prod1 = 0.d0
            prod2 = 0.d0
            do 600 i = 1, ncmp
                do 590 j = 1, ndim
                    prod1 = prod1 + sigl(i)*depsp(i,j)*dtdm(j,4)
590              continue
600          continue
            do 610 i = 1, ndim
                prod2 = prod2 + rp*dpdm(i)*dtdm(i,4)
610          continue
            tplas = tplas + (prod1-prod2)*poids
        endif
!
! =======================================================
! TERME INITIAL:PROD1 LIE A LA CONTRAINTE (EPS-EPSREF):GRAD(SIGIN).THETA
!               PROD2 LIE A LA PREDEFORMATION SIG:GRAD(EPSIN).THETA
! =======================================================
!
        if ((isigi.ne.0) .or. epsini) then
            prod1 = 0.d0
            prod2 = 0.d0
            if (isigi .ne. 0) then
                do 630 i = 1, ncmp
                    do 620 j = 1, ndim
                        prod1=prod1-(eps(i)-epsref(i))*dsigin(i,j)*&
                        dtdm(j,4)
620                  continue
630              continue
            else if (epsini) then
                do 631 i = 1, ncmp
                    do 621 j = 1, ndim
                        prod2=prod2+sigl(i)*depsin(i,j)*dtdm(j,4)
621                  continue
631              continue
            endif
            tini = tini + (prod1+prod2)*poids
        endif
! ==================================================================
! FIN DE BOUCLE PRINCIPALE SUR LES POINTS DE GAUSS
! ==================================================================
800  end do
! EXIT EN CAS DE THETA FISSURE NUL PARTOUT
9999  continue
! ASSEMBLAGE FINAL DES TERMES DE G OU DG
    zr(igthet) = tthe + tcla + tfor + tplas + tini
    call jedema()
end subroutine
