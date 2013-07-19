subroutine te0516(option, nomte)
! aslint: disable=W1501
    implicit none
#include "jeveux.h"
#include "asterfort/bsigma.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jevech.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jsd1ff.h"
#include "asterfort/lcsovn.h"
#include "asterfort/matela.h"
#include "asterfort/matrot.h"
#include "asterfort/mavec.h"
#include "asterfort/moytem.h"
#include "asterfort/pmfdef.h"
#include "asterfort/pmffor.h"
#include "asterfort/pmfite.h"
#include "asterfort/pmfitg.h"
#include "asterfort/pmfmcf.h"
#include "asterfort/pmftgt.h"
#include "asterfort/porea1.h"
#include "asterfort/pouex7.h"
#include "asterfort/r8inir.h"
#include "asterfort/tecach.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/utbtab.h"
#include "asterfort/utpslg.h"
#include "asterfort/utpvgl.h"
#include "asterfort/utpvlg.h"
#include "asterfort/vdiff.h"
#include "asterfort/wkvect.h"
#include "blas/ddot.h"
#include "blas/dscal.h"
    character(len=16) :: option, nomte
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
!
!
! ======================================================================
!-----------------------------------------------------------------------
!     ELEMENTS DE POUTRE MULTI-FIBRES DE TIMOSHENKO AVEC GAUCHISSEMENT.
!
!    - FONCTION REALISEE:  CALCUL DES OPTIONS NON-LINEAIRES MECANIQUES
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  RAPH_MECA ET FULL_MECA
!                                        OU RIGI_MECA_TANG
!                      NOMTE        -->  MECA_POU_D_TGM
!
!     NC  : NOMBRE DE COMPOSANTES DE CONTRAINTES
!     NNO : NOMBRE DE NOEUDS
!     NPG : NOMBRE DE POINTS DE GAUSS
    integer :: nc, nno, dimklv, npg, iret, codrep
    parameter ( nc = 7 , dimklv = 2*nc*(2*nc+1)/2 ,nno = 2 , npg = 3)
    real(kind=8) :: hoel(nc), fl(2*nc), hota(nc, nc), d1b(nc, 2*nc)
    real(kind=8) :: rg0(2*nc*2*nc), eps(nc), deps(nc), u(2*nc), du(2*nc)
    real(kind=8) :: klv(dimklv), work(nc, 2*nc)
    real(kind=8) :: co(npg), epsm
!
!
    real(kind=8) :: pgl(3, 3), ffp(3), matsct(6)
    real(kind=8) :: xd(3), ang1(3)
    real(kind=8) :: ey, ez, gamma, xl, xl2, xls2
    logical :: vecteu, matric, reactu
    integer :: i, jcret, isecan
    integer :: igeom, imate, icontm, isect, iorien, icompo, ivarim, iinstp
    integer :: icarcr, ideplm, ideplp, iinstm, ivectu, icontp, ivarip, imat
    integer :: inbfib, ncarfi, nbfib, jacf, jtab(7), ivarmp, codret
!
    integer :: ncomp, jdefm, jdefp, jmodfb, jsigfb, nbvalc, jvarfb, isdcom
    integer :: kp, j, k, kk, istrxm, istrxp, istrmp, ncomp2
    real(kind=8) :: aa, xiy, xiz, alfay, alfaz, xjx, xjg
    real(kind=8) :: e, g, nu, temp, temm, phiy, phiz
    real(kind=8) :: defam(6), defap(6), angp(3)
!
    real(kind=8) :: xiyr2, xizr2, effgep(nc), hotage(4, 4), d1bsig(4, 2*nc)
    real(kind=8) :: rigge0(2*nc, 2*nc), ksi1, d1b3(2, 3), sigfib, mflex(4)
    real(kind=8) :: ddu(2*nc), carsec(6)
    integer :: ne, cara, idepla, iiter, iterat, nint, ifgp
    integer :: nbgf, isicom, nbgfmx
    character(len=8) :: mator
    character(len=4) :: fami
    character(len=24) :: valk(2)
!     ------------------------------------------------------------------
!
    fami = 'RIGI'
!
    call r8inir(nc, 0.d0, hoel, 1)
    call r8inir(2*nc, 0.d0, fl, 1)
    call r8inir(nc*nc, 0.d0, hota, 1)
    call r8inir(2*nc*2*nc, 0.d0, rg0, 1)
    call r8inir(4*4, 0.d0, hotage, 1)
    call r8inir(2*nc*2*nc, 0.d0, rigge0, 1)
    mflex(1) = 0.d0
    mflex(2) = 0.d0
    mflex(3) = 0.d0
    mflex(4) = 0.d0
    codret=0
    codrep=0
!
! --- POIDS DES POINTS DE GAUSS
    co(1) = 5.d0/9.d0
    co(2) = 8.d0/9.d0
    co(3) = 5.d0/9.d0
! --- BOOLEENS PRATIQUES :
    matric = option .eq. 'FULL_MECA' .or. option .eq. 'RIGI_MECA_TANG'
    vecteu = option .eq. 'FULL_MECA' .or. option .eq. 'RAPH_MECA'
!
!     --- RECUPERATION DES CARACTERISTIQUES DES FIBRES
    call jevech('PNBSP_I', 'L', inbfib)
    nbfib = zi(inbfib)
    nbgf = zi(inbfib+1)
    call jevech('PFIBRES', 'L', jacf)
    ncarfi = 3
!
!     NOMBRE DE COMPOSANTES DU CHAMPS PSTRX?? PAR POINTS DE GAUSS
!     LA 15EME COMPOSANTE NE CONCERNE PAS LES POU_D_TGM
    ncomp = 18
!     NOMBRE DE COMPOSANTES D'EFFORTS GENERALISES
!     NOMBRE DE COMPOSANTES DE DEFORMATIONS GENERALISEES
    ncomp2 = 7
!
! --- RECUPERATION DES PARAMETRES "IN"/"OUT":
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
!
    call tecach('OON', 'PCONTMR', 'L', 7, jtab,&
                iret)
    icontm = jtab(1)
!
    call jevech('PSTRXMR', 'L', istrxm)
!
    call tecach('OON', 'PVARIMR', 'L', 7, jtab,&
                iret)
    ivarim = jtab(1)
!
    if (vecteu) then
        call tecach('OON', 'PVARIMP', 'L', 7, jtab,&
                    iret)
        ivarmp = jtab(1)
        call jevech('PSTRXMP', 'L', istrmp)
    endif
!
    call jevech('PCAGNPO', 'L', isect)
    call jevech('PCAORIE', 'L', iorien)
    call jevech('PCOMPOR', 'L', icompo)
    call jevech('PINSTMR', 'L', iinstm)
    call jevech('PINSTPR', 'L', iinstp)
    call jevech('PDEPLMR', 'L', ideplm)
    call jevech('PCARCRI', 'L', icarcr)
!
! --- RECUPERATION DE LA SD_COMPOR OU LE COMPORTEMENT DES GROUPES DE
!     FIBRES DE CET ELEMENT EST STOCKE
!     (NOM, MATER, LOI, ALGO1D, DEFORMATION NBFIG) POUR CHAQUE GROUPE
!     DANS L'ORDRE CROISSANT DE NUMEROS DE GROUPES
    call jeveuo(zk16(icompo-1+7), 'L', isdcom)
    read (zk16(icompo-1+2),'(I16)') nbvalc
!
!
! ---- LA PRESENCE DU CHAMP DE DEPLACEMENT A L INSTANT T+
! ---- DEVRAIT ETRE CONDITIONNE  PAR L OPTION (AVEC RIGI_MECA_TANG
! ---- CA N A PAS DE SENS).
! ---- CEPENDANT CE CHAMP EST INITIALISE A 0 PAR LA ROUTINE NMMATR.
    call jevech('PDEPLPR', 'L', ideplp)
    call jevech('PDDEPLA', 'L', idepla)
! --- ON RECUPERE LE NO DE L'ITERATION DE NEWTON POUR INITIALISER DEPS
    call jevech('PITERAT', 'L', iiter)
    iterat = nint(zr(iiter))
!
    if (matric) call jevech('PMATUUR', 'E', imat)
    if (vecteu) then
        call jevech('PVECTUR', 'E', ivectu)
        call jevech('PCONTPR', 'E', icontp)
        call jevech('PVARIPR', 'E', ivarip)
        call jevech('PSTRXPR', 'E', istrxp)
! ---    POUR LE CAS OU OPTION=RIGI_MECA_TANG
! ---    ON PREND LES VALEURS A 'MOINS' POUR ICONTP ET IVARMP
    else
        icontp = icontm
        istrxp = istrxm
        ivarmp = ivarim
    endif
! --- DEFORMATIONS ANELASTIQUES
    call r8inir(6, 0.d0, defam, 1)
    call r8inir(6, 0.d0, defap, 1)
!
    if (zk16(icompo+3) .eq. 'COMP_ELAS') then
        call u2mess('F', 'ELEMENTS2_90')
        else if ( (zk16(icompo+2) .ne. 'PETIT') .and. (zk16(icompo+2)&
    .ne. 'GROT_GDEP') ) then
        valk(1) = zk16(icompo+2)
        valk(2) = nomte
        call u2mesk('F', 'ELEMENTS3_40', 2, valk)
    endif
! --- VERIFICATION QUE C'EST BIEN DES MULTIFIBRES
    call jeexin(zk16(icompo-1+7), iret)
    if (iret .eq. 0) then
        call u2mesk('F', 'ELEMENTS4_14', 1, nomte)
    endif
!
! --- GEOMETRIE EVENTUELLEMENT REACTUALISEE :
!
    reactu = zk16(icompo+2).eq.'GROT_GDEP'
    if (reactu) then
!        RECUPERATION DU 3EME ANGLE NAUTIQUE AU TEMPS T-
        gamma = zr(istrxm+18-1)
!
!        CALCUL DE PGL,XL ET ANGP
        call porea1(nno, nc, zr(ideplm), zr(ideplp), zr(igeom),&
                    gamma, vecteu, pgl, xl, angp)
!
!        SAUVEGARDE DES ANGLES NAUTIQUES
        if (vecteu) then
            zr(istrxp+16-1) = angp(1)
            zr(istrxp+17-1) = angp(2)
            zr(istrxp+18-1) = angp(3)
        endif
!
    else
        call vdiff(3, zr(igeom-1+4), zr(igeom), xd)
        xl2 = ddot(3,xd,1,xd,1)
        xl = sqrt(xl2)
        ang1(1) = zr(iorien-1+1)
        ang1(2) = zr(iorien-1+2)
        ang1(3) = zr(iorien-1+3)
        call matrot(ang1, pgl)
    endif
    xls2 = xl / 2.d0
!---  RECUPERATION DES CARACTERISTIQUES DE LA SECTION
!
    call pmfitg(nbfib, 3, zr(jacf), carsec)
    aa = carsec(1)
    xiy = carsec(5)
    xiz = carsec(4)
    alfay = zr(isect + 3)
    alfaz = zr(isect + 4)
    xjx = zr(isect + 7)
    xjg = zr(isect + 11)
    xiyr2 = zr(isect + 12)
    xizr2 = zr(isect + 13)
!
!---  PASSAGE DE G (CENTRE DE GRAVITE) A C (CENTRE DE TORSION)
    ey = -zr(isect + 5)
    ez = -zr(isect + 6)
!---  CALCUL DES DEPLACEMENTS ET DE LEURS INCREMENTS
!     PASSAGE DANS LE REPERE LOCAL:
    call utpvgl(nno, nc, pgl, zr(ideplm), u)
    call utpvgl(nno, nc, pgl, zr(ideplp), du)
    call utpvgl(nno, nc, pgl, zr(idepla), ddu)
!     PRISE EN COMPTE DE LA POSITION DU CENTRE DE TORSION
    do 200 i = 1, 2
        u(7*(i-1)+2) = u(7*(i-1)+2) - ez* u(7*(i-1)+4)
        u(7*(i-1)+3) = u(7*(i-1)+3) + ey* u(7*(i-1)+4)
        du(7*(i-1)+2) = du(7*(i-1)+2) - ez*du(7*(i-1)+4)
        du(7*(i-1)+3) = du(7*(i-1)+3) + ey*du(7*(i-1)+4)
        ddu(7*(i-1)+2) = ddu(7*(i-1)+2) - ez*ddu(7*(i-1)+4)
        ddu(7*(i-1)+3) = ddu(7*(i-1)+3) + ey*ddu(7*(i-1)+4)
200  end do
!
!     COEFFICIENT DEPENDANT DE LA TEMPERATURE MOYENNE
    call moytem(fami, npg, 1, '+', temp,&
                iret)
    call moytem(fami, npg, 1, '-', temm,&
                iret)
!
! --- CARACTERISTIQUES ELASTIQUES (PAS DE TEMPERATURE POUR L'INSTANT)
!     ON PREND LE E ET NU DU MATERIAU TORSION (VOIR OP0059)
    call jeveuo(zk16(icompo-1+7)(1:8)//'.CPRI', 'L', isicom)
    nbgfmx = zi(isicom+2)
    mator = zk24(isdcom-1+nbgfmx*6+1)(1:8)
    call matela(zi(imate), mator, 1, temp, e,&
                nu)
    g = e / (2.d0*(1.d0+nu))
!
!     MATRICE DE RAIDEUR ELASTIQUE : MATERIAU INTEGRE SUR LA SECTION
    hoel(1) = e*aa
    hoel(2) = g*aa/alfay
    hoel(3) = g*aa/alfaz
    hoel(4) = g*xjx
    hoel(5) = e*xiy
    hoel(6) = e*xiz
    hoel(7) = e*xjg
    phiy = e*xiz*12.d0*alfay/ (xl*xl*g*aa)
    phiz = e*xiy*12.d0*alfaz/ (xl*xl*g*aa)
!
! --- DEFORMATIIONS MOINS ET INCREMENT DE DEFORMATION POUR CHAQUE FIBRE
    call wkvect('&&TE0516.DEFMFIB', 'V V R8', nbfib, jdefm)
    call wkvect('&&TE0516.DEFPFIB', 'V V R8', nbfib, jdefp)
!
! --- NOMBRE DE VARIABLE INTERNE DE LA LOI DE COMPORTEMENT
    read (zk16(icompo-1+2),'(I16)') nbvalc
! --- MODULE ET CONTRAINTES SUR CHAQUE FIBRE (COMPORTEMENT)
    call wkvect('&&TE0516.MODUFIB', 'V V R8', nbfib, jmodfb)
    call wkvect('&&TE0516.SIGFIB', 'V V R8', nbfib, jsigfb)
    call wkvect('&&TE0516.VARFIB', 'V V R8', nbfib*nbvalc*npg, jvarfb)
!
!     BOUCLE SUR LES POINTS DE GAUSS
    do 300 kp = 1, 3
!        CALCUL DE EPS ET DEPS
!        CALCUL DE D1B ( EPSI = D1B * U ) :
        call jsd1ff(kp, xl, phiy, phiz, d1b)
        call r8inir(nc, 0.d0, eps, 1)
        call r8inir(nc, 0.d0, deps, 1)
! ---    CALCUL DE L'INCREMENT DE DEFORMATION SUR UN PAS
!        EN GRANDS DEPLACEMENTS IL EST CUMULATIF.
!        -  DANS STRXMP, ON TROUVE L'INCREMENT DE DEFORMATION JUSQU'A
!           L'ITERATION DE NEWTON PRECEDENTE (SI ITERAT=1, C'EST 0)
!        -  DANS DDU, ON TROUVE L'INCREMENT DE DEPLACEMENT DEPUIS
!           L'ITERATION DE NEWTON PRECEDENTE (SI ITERAT=1, C'EST 0)
!        -  APRES CALCUL DE DEPS, ON LE STOCKE DANS STRXPR
!        LES DEFORMATIONS SONT STOCKES A PARTIR DE LA 8EME POSITION
        kk=ncomp*(kp-1)+ncomp2
        if (.not. reactu) then
! ---       CALCUL CLASSIQUE DES DEFORMATIONS A PARTIR DE DU
            do 310 i = 1, nc
                do 311 j = 1, 2*nc
                    eps(i) = eps(i) + d1b(i,j)* u(j)
                    deps(i) = deps(i) + d1b(i,j)*du(j)
311              continue
310          continue
        else
! ---       CALCUL AMELIORE TENANT COMPTE DE LA REACTUALISATION
! ---       ON CUMULE LES INCREMENTS DE DEF DE CHAQUE ITERATION
            if (.not. vecteu) then
                do 315 i = 1, nc
                    do 316 j = 1, 2*nc
                        eps(i) = eps(i) + d1b(i,j)* u(j)
316                  continue
                    deps(i) = 0.d0
315              continue
            else if (iterat .ge. 2) then
                do 320 i = 1, nc
                    deps(i) = zr(istrmp+kk+i)
                    do 321 j = 1, 2*nc
                        eps(i) = eps(i) + d1b(i,j)* u(j)
                        deps(i) = deps(i) + d1b(i,j)* ddu(j)
321                  continue
                    zr(istrxp+kk+i) = deps(i)
320              continue
            else
                do 325 i = 1, nc
                    do 326 j = 1, 2*nc
                        eps(i) = eps(i) + d1b(i,j)* u(j)
                        deps(i) = deps(i) + d1b(i,j)* ddu(j)
326                  continue
                    zr(istrxp+kk+i) = deps(i)
325              continue
            endif
        endif
!        CALCUL DES DEFORMATIONS ET DES INCREMENTS DE DEF SUR LES FIBRES
        call pmfdef(nbfib, ncarfi, zr(jacf), eps, zr(jdefm))
        call pmfdef(nbfib, ncarfi, zr(jacf), deps, zr(jdefp))
        epsm = (u(8)-u(1))/xl
!
! ---   MODULE ET CONTRAINTES SUR CHAQUE FIBRE (COMPORTEMENT)
        call pmfmcf(kp, nbgf, nbfib, zi(inbfib+2), zk24(isdcom),&
                    zr( icarcr), option, zr(iinstm), zr(iinstp),&
                    zi(imate), nbvalc, defam, defap, zr(ivarim),&
                    zr(ivarmp), zr(icontm), zr(jdefm), zr( jdefp), epsm,&
                    zr(jmodfb), zr(jsigfb), zr(jvarfb), isecan, codrep)
!
        if (codrep .ne. 0) then
            codret=codrep
!           CODE 3: ON CONTINUE ET ON LE RENVOIE A LA FIN
!           AUTRE CODES: SORTIE IMMEDIATE
            if (codrep .ne. 3) goto 900
        endif
!
        if (vecteu) then
!---        CALCUL DES EFFORTS GENERALISES A "+"
!              FFP(1) = +INT(SE.DS)   = N
!              FFP(2) = +INT(SE.Z.DS) = MY
!              FFP(3) = -INT(SE.Y.DS) = MZ
            call pmffor(nbfib, ncarfi, zr(jacf), zr(jsigfb), ffp)
        endif
!        CALCUL DE BT*H*B :
        if (matric) then
!           CALCUL DE LA MATRICE TANGENTE AU COMPORTEMENT GLOBAL
!           SEULS 3 EFFORTS SONT CONCERNES, LES AUTRES ==> ELASTIQUE
!              EFFORT NORMAL   : COMPOSANTE 1
!              MOMENT AUTOUR Y : COMPOSANTE 5
!              MOMENT AUTOUR Z : COMPOSANTE 6
!   ----    CALCUL DE LA RAIDEUR TANGENTE AU COMPORTEMENT PAR FIBRE
            if (isecan .eq. 1) then
                call pmftgt(nbfib, e, zr(icontm+nbfib*(kp-1)), zr( jsigfb), zr(jdefp),&
                            zr(jmodfb))
            endif
            call pmfite(nbfib, ncarfi, zr(jacf), zr(jmodfb), matsct)
!           MATSCT(1) : INT(E.DS)
!           MATSCT(2) : INT(E.Y.DS)
!           MATSCT(3) : INT(E.Z.DS)
!           MATSCT(4) : INT(E.Y.Y.DS)
!           MATSCT(5) : INT(E.Z.Z.DS)
!           MATSCT(6) : INT(E.Y.Z.DS)
            hota(2,2) = hoel(2)
            hota(3,3) = hoel(3)
            hota(4,4) = hoel(4)
            hota(7,7) = hoel(7)
!
            hota(1,1) = matsct(1)
            hota(1,5) = matsct(3)
            hota(1,6) = -matsct(2)
            hota(5,1) = matsct(3)
            hota(5,5) = matsct(5)
            hota(5,6) = -matsct(6)
            hota(6,1) = -matsct(2)
            hota(6,5) = -matsct(6)
            hota(6,6) = matsct(4)
            call dscal(nc*nc, xls2, hota, 1)
            call dscal(nc*nc, co(kp), hota, 1)
            call utbtab('CUMU', nc, 2*nc, hota, d1b,&
                        work, rg0)
            call r8inir(nc*nc, 0.d0, hota, 1)
        endif
!        ON STOCKE A "+" : CONTRAINTES, FL, VARI
        if (vecteu) then
            do 332 i = 1, nbfib
                zr(icontp-1+nbfib*(kp-1)+i) = zr(jsigfb-1+i)
332          continue
!
!
! ---       CALCUL DES EFFORTS GENERALISES
            ifgp=ncomp*(kp-1)-1
            zr(istrxp+ifgp+1) = ffp(1)
            zr(istrxp+ifgp+2) = zr(istrxm+ifgp+2) + hoel(2)*deps(2)
            zr(istrxp+ifgp+3) = zr(istrxm+ifgp+3) + hoel(3)*deps(3)
! ------    ON RAJOUTE L'EFFET WAGNER DU AU GAUCHISSEMENT --------------
            zr(istrxp+ifgp+4) = zr(istrxm+ifgp+4) + hoel(4)*deps(4) + (ffp(1)*((xiy+xiz)/aa+ey**2&
                                &+ez**2))*deps(4) + (ffp(2)*( xizr2/xiy-2*ez))*deps(4) - (ffp(3)*&
                                &(xiyr2/xiz-2*ey))*deps( 4)
! ------    ------------------------------------------------------------
            zr(istrxp+ifgp+5) = ffp(2)
            zr(istrxp+ifgp+6) = ffp(3)
            zr(istrxp+ifgp+7) = zr(istrxm+ifgp+7) + hoel(7)*deps(7)
!
            do 360 k = 1, 2*nc
                do 361 i = 1, nc
                    fl(k)=fl(k) + xls2*zr(istrxp+ifgp+i)*d1b(i,k)*co(&
                    kp)
!
361              continue
360          continue
        endif
!
!-----   CALCUL DE LA MATRICE DE RIGIDITE GEOMETRIQUE---------
        if (matric .and. reactu) then
            ifgp=ncomp*(kp-1)-1
            do 370 i = 1, ncomp2
                effgep(i) = zr(istrxp+ifgp+i)
370          continue
            hotage(1,2) = -effgep(3)
            hotage(1,3) = effgep(2)
            hotage(1,4) = -(&
                          ey*effgep(2)+ez*effgep(3)) +(0.5d0*(xiyr2/ xiz)*effgep(2)) +(0.5d0*(xiz&
                          &r2/xiy)*effgep(3)&
                          )
!           TERME NON CALCULE EXACTEMENT (ON FAIT L'HYPOTHESE D'UNE
!           TORSION DE SAINT-VENANT)
!
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
!           MOMENT DE WAGNER : IL PEUT ETRE INTERESSANT A L'AVENIR
!           DE LE CALCULER A PARTIR DE LA DISCRETISATION EN FIBRES
            hotage(4,4) = (&
                          effgep(1)*((xiy+xiz)/aa+ey**2+ez**2)) +(effgep(5)*(xizr2/xiy-2*ez)) -(e&
                          &ffgep(6)*(xiyr2/xiz-2*ey)&
                          )
!           TERME NON CALCULE ACTUELLEMENT CAR XIWR2 N'EST PAS FOURNI
!           PAR L'UTILISATEUR : XIWR2 = INT(W*(Y*Y+Z*Z)*DS)
!     &                               + (EFFGEP(7)*(XIWR2/XJG))
!
            call dscal(4*4, xls2, hotage, 1)
            call dscal(4*4, co(kp), hotage, 1)
! ---       RECUPERATION DE LA MATRICE DES FONCTIONS DE FORME D1BSIG
!           LE DERNIER ARGUMENT PERMET DE CHOISIR L'INTERPOLATION :
!           LINEAIRE (0) OU CUBIQUE FLEXION-TORSION(1)
            call bsigma(kp, xl, phiy, phiz, d1bsig,&
                        1)
            call utbtab('CUMU', 4, 2*nc, hotage, d1bsig,&
                        work, rigge0)
            call r8inir(4*4, 0.d0, hotage, 1)
        endif
!
300  end do
!
    if (vecteu) then
        do 330 i = 1, nbfib*nbvalc*npg
            zr(ivarip-1+i) = zr(jvarfb-1+i)
330      continue
    endif
!
    if (matric) then
!-----   CALCUL DE LA MATRICE DE CORRECTION DES GR -----------
        if (reactu) then
!
!           RAPPEL : LE CALCUL DE LA MATRICE DE CORRECTION KC EST
!                 FAIT A PART, ON TIENT COMPTE A POSTERIORI
!                 DES ROTATIONS MODEREES ENTRE DEUX ITERATIONS
!                 (PAS BESOIN D'INTEGRATION NUMERIQUE)
!
! ---       LES MFY ET MFZ INTERVENANT ICI SONT CEUX AUX EXTREMITES
!           ET ON NE LES CONNAIT QU'AUX POINTS DE GAUSS
!           IL FAUT DONC UTILISER DES FONCTIONS DE FORME POUR LES
!           TRANSPORTER AUX NOEUDS (ON PREND L'INTERPOLATION
!           POLYNOMIALE D'ORDRE 2)
!
!           ON PROJETTE AVEC DES FCTS DE FORME
!           SUR LES NOEUDS DEBUT ET FIN DE L'ELEMENT
!           POUR LE POINT 1
            ksi1 = -sqrt( 5.d0 / 3.d0 )
            d1b3(1,1) = ksi1*(ksi1-1.d0)/2.0d0
            d1b3(1,2) = 1.d0-ksi1*ksi1
            d1b3(1,3) = ksi1*(ksi1+1.d0)/2.0d0
!           POUR LE POINT 2
            ksi1 = sqrt( 5.d0 / 3.d0 )
            d1b3(2,1) = ksi1*(ksi1-1.d0)/2.0d0
            d1b3(2,2) = 1.d0-ksi1*ksi1
            d1b3(2,3) = ksi1*(ksi1+1.d0)/2.0d0
!
!           POUR LES NOEUDS 1 ET 2
!           CALCUL DES CONTRAINTES
!           CALCUL DES EFFORTS GENERALISES A PARTIR DES CONTRAINTES
            do 400 ne = 1, 2
                do 410 i = 1, nbfib
                    sigfib = 0.d0
                    do 420 kp = 1, 3
                        kk = icontp+nbfib*(kp-1) + i - 1
                        sigfib = sigfib + zr(kk)*d1b3(ne,kp)
420                  continue
                    kk = 2*(ne-1)
                    cara = jacf+(i-1)*ncarfi
                    mflex(1+kk)=mflex(1+kk)+sigfib*zr(cara+2)*zr(cara+&
                    1)
                    mflex(2+kk)=mflex(2+kk)-sigfib*zr(cara+2)*zr(cara)
410              continue
400          continue
!           ON CALCULE LA MATRICE TANGENTE EN SOMMANT LES TERMES DE
!           -  RIGIDITE MATERIELLE RG0
!           -  RIGIDITE GEOMETRIQUE RIGGE0
!           -  MATRICE DE CORRECTION POUR LA PRISE EN COMPTE
!              DE ROTATIONS MODEREES (VIENT CORRIGER RIGGE0)
            rigge0(4,5) = rigge0(4,5) + mflex(2)*0.5d0
            rigge0(4,6) = rigge0(4,6) - mflex(1)*0.5d0
            rigge0(5,4) = rigge0(5,4) + mflex(2)*0.5d0
            rigge0(6,4) = rigge0(6,4) - mflex(1)*0.5d0
!
            rigge0(11,12) = rigge0(11,12) - mflex(4)*0.5d0
            rigge0(11,13) = rigge0(11,13) + mflex(3)*0.5d0
            rigge0(12,11) = rigge0(12,11) - mflex(4)*0.5d0
            rigge0(13,11) = rigge0(13,11) + mflex(3)*0.5d0
!           ON REMET TOUT DANS RG0
            call lcsovn(2*nc*2*nc, rg0, rigge0, rg0)
        endif
!
        call mavec(rg0, 2*nc, klv, dimklv)
    endif
!
!     ON REND LE FL DANS LE REPERE GLOBAL :
    if (vecteu) then
!        PRISE EN COMPTE DU CENTRE DE TORSION
        do 216 i = 1, 2
            fl(7*(i-1)+4) = fl(7*(i-1)+4) - ez*fl(7*(i-1)+2) + ey*fl( 7*(i-1)+3 )
216      continue
!        PASSAGE LOCAL -> GLOBAL
        call utpvlg(nno, nc, pgl, fl, zr(ivectu))
    endif
!     ON REND LA MATRICE TANGENTE :
    if (matric) then
!       PRISE EN COMPTE DU CENTRE DE TORSION
        call pouex7(klv, ey, ez)
!        PASSAGE LOCAL -> GLOBAL
        call utpslg(nno, nc, pgl, klv, zr(imat))
    endif
!
900  continue
    if (vecteu) then
        call jevech('PCODRET', 'E', jcret)
        zi(jcret) = codret
    endif
    call jedetr('&&TE0516.DEFMFIB')
    call jedetr('&&TE0516.DEFPFIB')
    call jedetr('&&TE0516.MODUFIB')
    call jedetr('&&TE0516.SIGFIB')
    call jedetr('&&TE0516.VARFIB')
end subroutine
