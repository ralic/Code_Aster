subroutine te0516(option, nomte)
!
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
! --------------------------------------------------------------------------------------------------
!
!           ELEMENTS DE POUTRE MULTI-FIBRES DE TIMOSHENKO AVEC GAUCHISSEMENT.
!
!       OPTION       RAPH_MECA FULL_MECA RIGI_MECA_TANG
!       NOMTE        MECA_POU_D_TGM
!
! --------------------------------------------------------------------------------------------------
!
    implicit none
    character(len=16) :: option, nomte
!
#include "jeveux.h"
#include "asterf_types.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/assert.h"
#include "asterfort/bsigma.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jeexin.h"
#include "asterfort/jevech.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jsd1ff.h"
#include "asterfort/lcsovn.h"
#include "asterfort/lonele.h"
#include "asterfort/matela.h"
#include "asterfort/matrot.h"
#include "asterfort/mavec.h"
#include "asterfort/moytem.h"
#include "asterfort/pmfdef.h"
#include "asterfort/pmfinfo.h"
#include "asterfort/pmfite.h"
#include "asterfort/pmfitg.h"
#include "asterfort/pmfits.h"
#include "asterfort/pmfmats.h"
#include "asterfort/pmfmcf.h"
#include "asterfort/porea1.h"
#include "asterfort/pouex7.h"
#include "asterfort/poutre_modloc.h"
#include "asterfort/tecach.h"
#include "asterfort/utbtab.h"
#include "asterfort/utmess.h"
#include "asterfort/utpslg.h"
#include "asterfort/utpvgl.h"
#include "asterfort/utpvlg.h"
#include "blas/ddot.h"
#include "blas/dscal.h"
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nc, nno, dimklv, npg, iret, codrep
    parameter (nc=7 , dimklv= 2*nc*(2*nc+1)/2 , nno=2 , npg=3)
    real(kind=8) :: hoel(nc), fl(2*nc), hota(nc, nc), d1b(nc, 2*nc)
    real(kind=8) :: rg0(2*nc, 2*nc), eps(nc), deps(nc), u(2*nc), du(2*nc)
    real(kind=8) :: klv(dimklv), work(nc, 2*nc), co(npg)
    real(kind=8) :: rigge0(2*nc, 2*nc), ddu(2*nc), effgep(nc), d1bsig(4, 2*nc)
!
    integer :: ne, cara, idepla, iiter, iterat, ifgp
    integer :: i, jcret, npge
    integer :: igeom, imate, icontm, iorien, icompo, ivarim, iinstp, ipoids
    integer :: icarcr, ideplm, ideplp, iinstm, ivectu, icontp, ivarip, imat
    integer :: jacf, jtab(7), ivarmp, codret
    integer :: ncomp, nbvalc, isdcom
    integer :: kp, j, k, kk, istrxm, istrxp, istrmp, ncomp2
    real(kind=8) :: ey, ez, gamma, xl, xls2, Nx, My, Mz
    real(kind=8) :: aa, xiy, xiz, alfay, alfaz, xjx, xjg
    real(kind=8) :: e, g, nu, temp, temm, phiy, phiz
    real(kind=8) :: defam(6), defap(6), angp(3)
    real(kind=8) :: pgl(3, 3), ffp(3), matsct(6)
    real(kind=8) :: ang1(3)
    real(kind=8) :: xiyr2, xizr2, hotage(4, 4), epsm
    real(kind=8) :: ksi1, d1b3(2, 3), sigfib, mflex(4)
    real(kind=8) :: carsec(6)
!
    aster_logical :: vecteu, matric, reactu
!
    character(len=4) :: fami
    character(len=8) :: mator
    character(len=24) :: valk(2)
!
    integer :: nbfibr, nbgrfi, tygrfi, nbcarm, nug(10)
!
    real(kind=8), pointer :: defmfib(:) => null()
    real(kind=8), pointer :: defpfib(:) => null()
    real(kind=8), pointer :: modufib(:) => null()
    real(kind=8), pointer :: vsigfib(:) => null()
    real(kind=8), pointer :: varfib(:) => null()
! --------------------------------------------------------------------------------------------------
    integer, parameter :: nb_cara = 8
    real(kind=8) :: vale_cara(nb_cara)
    character(len=8) :: noms_cara(nb_cara)
    data noms_cara /'AY1','AZ1','EY1','EZ1','JX1','JG1','IYR21','IZR21'/
! --------------------------------------------------------------------------------------------------
!
    fami = 'RIGI'
    call elrefe_info(fami=fami, npg=npge, jpoids=ipoids)
    ASSERT( npg.ge.npge )
    co(1:npg)=zr(ipoids:ipoids+npg-1)
!
    hoel(1:nc) = 0.0d0
    fl(1:2*nc) = 0.0d0
    rg0(1:2*nc,1:2*nc) = 0.0d0
    rigge0(1:2*nc,1:2*nc) = 0.0d0
    mflex(:) = 0.d0
    codret=0
    codrep=0
!   booleens pratiques
    matric = option .eq. 'FULL_MECA' .or. option .eq. 'RIGI_MECA_TANG'
    vecteu = option .eq. 'FULL_MECA' .or. option .eq. 'RAPH_MECA'
!
!   Récupération des caractéristiques des fibres
    call pmfinfo(nbfibr,nbgrfi,tygrfi,nbcarm,nug)
    call jevech('PFIBRES', 'L', jacf)
!
!   Nombre de composantes du champs PSTRX?? par points de gauss
!   La 15eme composante ne concerne pas les POU_D_TGM
    ncomp = 18
!   Nombre de composantes d'efforts et de déformations généralisés
    ncomp2 = 7
!   Longueur de l'élément et pointeur sur le géométrie
    xl = lonele(igeom=igeom)
    call jevech('PMATERC', 'L', imate)
!
    call tecach('OON', 'PCONTMR', 'L', iret, nval=7, itab=jtab)
    icontm = jtab(1)
!
    call jevech('PSTRXMR', 'L', istrxm)
!
    call tecach('OON', 'PVARIMR', 'L', iret, nval=7, itab=jtab)
    ivarim = jtab(1)
!   Pour matric=(FULL_MECA|RIGI_MECA_TANG) : valeurs "+" égalent valeurs "-"
    icontp = icontm
    istrxp = istrxm
    ivarmp = ivarim
!
    call jevech('PCAORIE', 'L', iorien)
    call jevech('PCOMPOR', 'L', icompo)
    call jevech('PINSTMR', 'L', iinstm)
    call jevech('PINSTPR', 'L', iinstp)
    call jevech('PDEPLMR', 'L', ideplm)
    call jevech('PCARCRI', 'L', icarcr)
!   Recuperation de la SD_COMPOR ou le comportement des groupes de fibres est stocke
!   pour chaque groupe : (nom, mater, loi, algo1d, deformation nbfig) dans
!   l'ordre croissant des numeros de groupes
    call jeveuo(zk16(icompo-1+7), 'L', isdcom)
    read (zk16(icompo-1+2),'(I16)') nbvalc
!
!   la presence du champ de deplacement a l instant t+ devrait etre conditionne  par l'option
!   (avec rigi_meca_tang ca n a pas de sens). Ce champ est initialise a 0 par la routine nmmatr.
    call jevech('PDEPLPR', 'L', ideplp)
    call jevech('PDDEPLA', 'L', idepla)
!   on recupere le no de l'iteration de newton pour initialiser deps
    call jevech('PITERAT', 'L', iiter)
    iterat = nint(zr(iiter))
!
    if (matric) call jevech('PMATUUR', 'E', imat)
    if (vecteu) then
        call jevech('PVECTUR', 'E', ivectu)
        call jevech('PCONTPR', 'E', icontp)
        call jevech('PVARIPR', 'E', ivarip)
        call jevech('PSTRXPR', 'E', istrxp)
        call tecach('OON', 'PVARIMP', 'L', iret, nval=7, itab=jtab)
        ivarmp = jtab(1)
        call jevech('PSTRXMP', 'L', istrmp)
    endif
!   deformations anelastiques
    defam(:) = 0.0d0
    defap(:) = 0.0d0
!
    if (zk16(icompo+3) .eq. 'COMP_ELAS') then
        call utmess('F', 'ELEMENTS2_90')
    else if ((zk16(icompo+2) .ne. 'PETIT').and.(zk16(icompo+2).ne.'GROT_GDEP')) then
        valk(1) = zk16(icompo+2)
        valk(2) = nomte
        call utmess('F', 'ELEMENTS3_40', nk=2, valk=valk)
    endif
!   verification que c'est bien des multifibres
    call jeexin(zk16(icompo-1+7), iret)
    if (iret .eq. 0) then
        call utmess('F', 'ELEMENTS4_14', sk=nomte)
    endif
!
!   geometrie eventuellement reactualisee :
    reactu = zk16(icompo+2).eq.'GROT_GDEP'
    if (reactu) then
!       recuperation du 3eme angle nautique au temps t-
        gamma = zr(istrxm+18-1)
!       calcul de PGL,XL et ANGP
        call porea1(nno, nc, zr(ideplm), zr(ideplp), zr(igeom+1),&
                    gamma, vecteu, pgl, xl, angp)
!       sauvegarde des angles nautiques
        if (vecteu) then
            zr(istrxp+16-1) = angp(1)
            zr(istrxp+17-1) = angp(2)
            zr(istrxp+18-1) = angp(3)
        endif
    else
        ang1(1) = zr(iorien-1+1)
        ang1(2) = zr(iorien-1+2)
        ang1(3) = zr(iorien-1+3)
        call matrot(ang1, pgl)
    endif
    xls2 = xl / 2.d0
!   recuperation des caracteristiques de la section
    call pmfitg(tygrfi, nbfibr, nbcarm, zr(jacf), carsec)
    aa  = carsec(1)
    xiy = carsec(5)
    xiz = carsec(4)
!
    call poutre_modloc('CAGNP2', noms_cara, nb_cara, lvaleur=vale_cara)
!
    alfay = vale_cara(1)
    alfaz = vale_cara(2)
    ey    = vale_cara(3)
    ez    = vale_cara(4)
    xjx   = vale_cara(5)
    xjg   = vale_cara(6)
    xiyr2 = vale_cara(7)
    xizr2 = vale_cara(8)
!   calcul des deplacements et de leurs increments passage dans le repere local
    call utpvgl(nno, nc, pgl, zr(ideplm), u)
    call utpvgl(nno, nc, pgl, zr(ideplp), du)
    call utpvgl(nno, nc, pgl, zr(idepla), ddu)
!   prise en compte de la position du centre de torsion
    do i = 1, 2
        u(7*(i-1)+2) = u(7*(i-1)+2) - ez* u(7*(i-1)+4)
        u(7*(i-1)+3) = u(7*(i-1)+3) + ey* u(7*(i-1)+4)
        du(7*(i-1)+2) = du(7*(i-1)+2) - ez*du(7*(i-1)+4)
        du(7*(i-1)+3) = du(7*(i-1)+3) + ey*du(7*(i-1)+4)
        ddu(7*(i-1)+2) = ddu(7*(i-1)+2) - ez*ddu(7*(i-1)+4)
        ddu(7*(i-1)+3) = ddu(7*(i-1)+3) + ey*ddu(7*(i-1)+4)
    enddo
!   coefficient dependant de la temperature moyenne
    call moytem(fami, npg, 1, '+', temp, iret)
    call moytem(fami, npg, 1, '-', temm, iret)
!   caracteristiques elastiques (pas de temperature pour l'instant)
!   on prend le E et NU du materiau torsion (voir op0059)
    call pmfmats(imate, mator)
    ASSERT( mator.ne.' ' )
    call matela(zi(imate), mator, 1, temp, e, nu)
    g = e / (2.d0*(1.d0+nu))
!   matrice de raideur elastique : materiau integre sur la section
    hoel(1) = e*aa
    hoel(2) = g*aa/alfay
    hoel(3) = g*aa/alfaz
    hoel(4) = g*xjx
    hoel(5) = e*xiy
    hoel(6) = e*xiz
    hoel(7) = e*xjg
    phiy = e*xiz*12.d0*alfay/ (xl*xl*g*aa)
    phiz = e*xiy*12.d0*alfaz/ (xl*xl*g*aa)
!   deformatiions moins et increment de deformation pour chaque fibre
    AS_ALLOCATE(vr=defmfib, size=nbfibr)
    AS_ALLOCATE(vr=defpfib, size=nbfibr)
!   nombre de variable interne de la loi de comportement
    read (zk16(icompo-1+2),'(I16)') nbvalc
!   module et contraintes sur chaque fibre (comportement)
    AS_ALLOCATE(vr=modufib, size=nbfibr)
    AS_ALLOCATE(vr=vsigfib, size=nbfibr)
    AS_ALLOCATE(vr=varfib, size=nbfibr*nbvalc*npg)
!   boucle sur les points de gauss
    do kp = 1, 3
!       calcul de EPS, DEPS, D1B ( EPSI = D1B * U ) :
        call jsd1ff(kp, xl, phiy, phiz, d1b)
        eps(1:nc) = 0.0d0
        deps(1:nc) = 0.0d0
!       calcul de l'increment de deformation sur un pas en grands deplacements il est cumulatif.
!           - dans strxmp, on trouve l'increment de deformation jusqu'a l'iteration de newton
!             precedente (si iterat=1, c'est 0)
!           - dans ddu, on trouve l'increment de deplacement depuis l'iteration de newton
!             precedente (si iterat=1, c'est 0)
!           - apres calcul de deps, on le stocke dans strxpr
!       les deformations sont stockes a partir de la 8eme position
        kk=ncomp*(kp-1)+ncomp2
        if (.not. reactu) then
!           calcul classique des deformations à partir de DU
            do i = 1, nc
                do j = 1, 2*nc
                    eps(i) = eps(i) + d1b(i,j)* u(j)
                    deps(i) = deps(i) + d1b(i,j)*du(j)
                enddo
            enddo
        else
!           calcul ameliore tenant compte de la reactualisation
!           on cumule les increments de def de chaque iteration
            if (.not. vecteu) then
                do i = 1, nc
                    do j = 1, 2*nc
                        eps(i) = eps(i) + d1b(i,j)* u(j)
                    enddo
                    deps(i) = 0.d0
                enddo
            else if (iterat .ge. 2) then
                do i = 1, nc
                    deps(i) = zr(istrmp+kk+i)
                    do j = 1, 2*nc
                        eps(i) = eps(i) + d1b(i,j)* u(j)
                        deps(i) = deps(i) + d1b(i,j)* ddu(j)
                    enddo
                    zr(istrxp+kk+i) = deps(i)
                enddo
            else
                do i = 1, nc
                    do j = 1, 2*nc
                        eps(i) = eps(i) + d1b(i,j)* u(j)
                        deps(i) = deps(i) + d1b(i,j)* ddu(j)
                    enddo
                    zr(istrxp+kk+i) = deps(i)
                enddo
            endif
        endif
!       calcul des deformations et des increments de def sur les fibres
        call pmfdef(tygrfi, nbfibr, nbcarm, zr(jacf), eps, defmfib)
        call pmfdef(tygrfi, nbfibr, nbcarm, zr(jacf), deps, defpfib)
        epsm = (u(8)-u(1))/xl
!       module et contraintes sur chaque fibre (comportement)
        call pmfmcf(kp, nbgrfi, nbfibr, nug, zk24(isdcom), &
                    zr(icarcr), option, zr(iinstm), zr(iinstp), zi(imate), &
                    nbvalc, defam, defap, zr(ivarim), zr(ivarmp), &
                    zr(icontm), defmfib, defpfib, epsm, modufib, &
                    vsigfib, varfib, codrep)
!
        if (codrep .ne. 0) then
            codret=codrep
!           code 3: on continue et on le renvoie a la fin. autres codes: sortie immediate
            if (codrep .ne. 3) goto 900
        endif
!       calcul de BT*H*B :
        if (matric) then
            hota(1:nc,1:nc)= 0.0d0
!           calcul de la matrice tangente au comportement global
!           seuls 3 efforts sont concernés, les autres ==> élastique
!              effort normal X : composante 1
!              moment autour Y : composante 5
!              moment autour Z : composante 6
!           calcul de la raideur tangente au comportement par fibre
            call pmfite(tygrfi, nbfibr, nbcarm, zr(jacf), modufib, matsct)
!           MATSCT(1:3) : INT(E.DS)     INT(E.Y.DS)   INT(E.Z.DS)
!           MATSCT(4:6) : INT(E.Y.Y.DS) INT(E.Z.Z.DS) INT(E.Y.Z.DS)
            hota(2,2) = hoel(2)
            hota(3,3) = hoel(3)
            hota(4,4) = hoel(4)
            hota(7,7) = hoel(7)
!
            hota(1,1) =  matsct(1)
            hota(1,5) =  matsct(3)
            hota(1,6) = -matsct(2)
            hota(5,1) =  matsct(3)
            hota(5,5) =  matsct(5)
            hota(5,6) = -matsct(6)
            hota(6,1) = -matsct(2)
            hota(6,5) = -matsct(6)
            hota(6,6) =  matsct(4)
            call dscal(nc*nc, xls2, hota, 1)
            call dscal(nc*nc, co(kp), hota, 1)
            call utbtab('CUMU', nc, 2*nc, hota, d1b,&
                        work, rg0)
        endif
!       On stocke a "+" : contraintes, fl, vari
        if (vecteu) then
!           Contraintes
            do i = 1, nbfibr
                zr(icontp-1+nbfibr*(kp-1)+i) = vsigfib(i)
            enddo
!           Variables internes
            do i = 1, nbfibr*nbvalc*npg
                zr(ivarip-1+i) = varfib(i)
            enddo
!           Efforts généralisés à "+" :
!               ffp : < int(sig.ds) int(sig.y.ds)  int(sig.z.ds) >
!               ffp : <     Nx          -Mz             My       >
            call pmfits(tygrfi, nbfibr, nbcarm, zr(jacf), vsigfib, ffp)
            Nx =  ffp(1)
            My =  ffp(3)
            Mz = -ffp(2)
!           Calcul des efforts généralisés
            ifgp=ncomp*(kp-1)-1
            zr(istrxp+ifgp+1) = Nx
            zr(istrxp+ifgp+2) = zr(istrxm+ifgp+2) + hoel(2)*deps(2)
            zr(istrxp+ifgp+3) = zr(istrxm+ifgp+3) + hoel(3)*deps(3)
!           On rajoute l'effet WAGNER dû au gauchissement
            zr(istrxp+ifgp+4) = zr(istrxm+ifgp+4) + hoel(4)*deps(4) + &
                               (Nx*((xiy+xiz)/aa+ey**2+ez**2))*deps(4) + &
                               (My*(xizr2/xiy-2.0d0*ez))*deps(4) - &
                               (Mz*(xiyr2/xiz-2.0d0*ey))*deps(4)
!
            zr(istrxp+ifgp+5) = My
            zr(istrxp+ifgp+6) = Mz
            zr(istrxp+ifgp+7) = zr(istrxm+ifgp+7) + hoel(7)*deps(7)
!
            do k = 1, 2*nc
                do i = 1, nc
                    fl(k)=fl(k) + xls2*zr(istrxp+ifgp+i)*d1b(i,k)*co(kp)
                enddo
            enddo
        endif
!       Calcul de la matrice de rigidite geometrique
        if (matric .and. reactu) then
            hotage(:,:) = 0.0d0
            ifgp=ncomp*(kp-1)-1
            do i = 1, ncomp2
                effgep(i) = zr(istrxp+ifgp+i)
            enddo
            hotage(1,2) = -effgep(3)
            hotage(1,3) = effgep(2)
            hotage(1,4) = -(ey*effgep(2)+ez*effgep(3)) + (0.5d0*(xiyr2/ xiz)*effgep(2)) + &
                           (0.5d0*(xizr2/xiy)*effgep(3))
!           Terme non calcule exactement (on fait l'hypothese d'une torsion de saint-venant)
            hotage(2,1) = hotage(1,2)
            hotage(2,2) = effgep(1)
            hotage(2,4) = (ez*effgep(1)-effgep(5))
!
            hotage(3,1) = hotage(1,3)
            hotage(3,3) = effgep(1)
            hotage(3,4) =-(ey*effgep(1)+effgep(6))
!
            hotage(4,1) = hotage(1,4)
            hotage(4,2) = hotage(2,4)
            hotage(4,3) = hotage(3,4)
!           Moment de WAGNER
            hotage(4,4) = (effgep(1)*((xiy+xiz)/aa+ey**2+ez**2)) + &
                          (effgep(5)*(xizr2/xiy-2.0d0*ez)) - (effgep(6)*(xiyr2/xiz-2.0d0*ey))
!           Terme non calcule actuellement car xiwr2 n'est pas fourni par l'utilisateur
!               XIWR2 = INT(W*(Y*Y+Z*Z)*DS) + (EFFGEP(7)*(XIWR2/XJG))
            call dscal(4*4, xls2, hotage, 1)
            call dscal(4*4, co(kp), hotage, 1)
!           Recuperation de la matrice des fonctions de forme d1bsig
!           Le dernier argument permet de choisir l'interpolation :
!               lineaire (0)
!               cubique flexion-torsion(1)
            call bsigma(kp, xl, phiy, phiz, d1bsig, 1)
            call utbtab('CUMU', 4, 2*nc, hotage, d1bsig, work, rigge0)
        endif
    enddo
!
    if (matric) then
!       Calcul de la matrice de correction des GR
        if (reactu) then
!           rappel :
!               le calcul de la matrice de correction kc est fait a part, on tient compte à
!               posteriori des rotations modérées entre deux iterations
!
!           Les MFY et MFZ intervenant ici sont ceux aux extremites et on ne les connait
!           qu'aux points de gauss il faut donc utiliser des fonctions de forme pour les
!           transporter aux noeuds (on prend l'interpolation polynomiale d'ordre 2)
!
!           On projette avec des fcts de forme sur les noeuds debut et fin de l'élément
!           pour le point 1
            ksi1 = -sqrt( 5.d0 / 3.d0 )
            d1b3(1,1) = ksi1*(ksi1-1.d0)/2.0d0
            d1b3(1,2) = 1.d0-ksi1*ksi1
            d1b3(1,3) = ksi1*(ksi1+1.d0)/2.0d0
!           pour le point 2
            ksi1 = sqrt( 5.d0 / 3.d0 )
            d1b3(2,1) = ksi1*(ksi1-1.d0)/2.0d0
            d1b3(2,2) = 1.d0-ksi1*ksi1
            d1b3(2,3) = ksi1*(ksi1+1.d0)/2.0d0
!
!           pour les noeuds 1 et 2 :
!               calcul des contraintes
!               calcul des efforts generalises a partir des contraintes
            do ne = 1, 2
                do i = 1, nbfibr
                    sigfib = 0.d0
                    do kp = 1, 3
                        kk = icontp+nbfibr*(kp-1) + i - 1
                        sigfib = sigfib + zr(kk)*d1b3(ne,kp)
                    enddo
                    kk = 2*(ne-1)
                    cara = jacf+(i-1)*nbcarm
                    mflex(1+kk)=mflex(1+kk)+sigfib*zr(cara+2)*zr(cara+1)
                    mflex(2+kk)=mflex(2+kk)-sigfib*zr(cara+2)*zr(cara)
                enddo
            enddo
!           on calcule la matrice tangente en sommant les termes de :
!               rigidité matérielle RG0
!               rigidité géométrique RIGGE0
!               matrice de correction pour la prise en compte de rotations modérées
            rigge0(4,5) = rigge0(4,5) + mflex(2)*0.5d0
            rigge0(4,6) = rigge0(4,6) - mflex(1)*0.5d0
            rigge0(5,4) = rigge0(5,4) + mflex(2)*0.5d0
            rigge0(6,4) = rigge0(6,4) - mflex(1)*0.5d0
!
            rigge0(11,12) = rigge0(11,12) - mflex(4)*0.5d0
            rigge0(11,13) = rigge0(11,13) + mflex(3)*0.5d0
            rigge0(12,11) = rigge0(12,11) - mflex(4)*0.5d0
            rigge0(13,11) = rigge0(13,11) + mflex(3)*0.5d0
!           On remet tout dans rg0
            call lcsovn(2*nc*2*nc, rg0, rigge0, rg0)
        endif
        call mavec(rg0, 2*nc, klv, dimklv)
    endif
!
!   On rend le FL dans le repere global
    if (vecteu) then
!       Prise en compte du centre de torsion
        do i = 1, 2
            fl(7*(i-1)+4) = fl(7*(i-1)+4) - ez*fl(7*(i-1)+2) + ey*fl( 7*(i-1)+3 )
        enddo
!        passage local -> global
        call utpvlg(nno, nc, pgl, fl, zr(ivectu))
    endif
!   On rend la matrice tangente
    if (matric) then
!       Prise en compte du centre de torsion
        call pouex7(klv, ey, ez)
!       Passage local -> global
        call utpslg(nno, nc, pgl, klv, zr(imat))
    endif
!
900 continue
    if (vecteu) then
        call jevech('PCODRET', 'E', jcret)
        zi(jcret) = codret
    endif
    AS_DEALLOCATE(vr=defmfib)
    AS_DEALLOCATE(vr=defpfib)
    AS_DEALLOCATE(vr=modufib)
    AS_DEALLOCATE(vr=vsigfib)
    AS_DEALLOCATE(vr=varfib)
end subroutine
