subroutine te0039(option, nomte)
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
    implicit       none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dichoc.h"
#include "asterfort/disief.h"
#include "asterfort/elref4.h"
#include "asterfort/infdis.h"
#include "asterfort/infted.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jevech.h"
#include "asterfort/matro2.h"
#include "asterfort/matrot.h"
#include "asterfort/pmfitg.h"
#include "asterfort/pmfitx.h"
#include "asterfort/ptka10.h"
#include "asterfort/r8inir.h"
#include "asterfort/rcvalb.h"
#include "asterfort/tecael.h"
#include "asterfort/terefe.h"
#include "asterfort/trigom.h"
#include "asterfort/u2mesk.h"
#include "asterfort/ut2vgl.h"
#include "asterfort/ut2vlg.h"
#include "asterfort/utpsgl.h"
#include "asterfort/utpvgl.h"
#include "asterfort/utpvlg.h"
#include "blas/dcopy.h"
    character(len=16) :: option, nomte
! ----------------------------------------------------------------------
! IN OPTION    : K16 :  OPTION DE CALCUL
!      'SIEQ_ELNO'       'SIEQ_ELGA'
!     'FORC_NODA'  'REFE_FORC_NODA'  'CHAR_MECA_EPSI_R'
!     'FONL_NOEU'
! IN NOMTE     : K16 : NOM DU TYPE ELEMENT
!     DISCRETS :
!        'MECA_DIS_T_N'      'MECA_DIS_T_L'     'MECA_DIS_TR_N'
!        'MECA_DIS_TR_L'     'MECA_2D_DIS_T_N'  'MECA_2D_DIS_T_L'
!        'MECA_2D_DIS_TR_N'  'MECA_2D_DIS_TR_L'
!     POUTRES DROITE D'EULER
!        'MECA_POU_D_E'   : SECTION VARIABLE
!        'MECA_POU_D_EM'  : SECTION MULTIFIBRES
!     POUTRE DROITE DE TIMOSHENKO
!        'MECA_POU_D_T'   : SECTION VARIABLE
!        'MECA_POU_D_TG'  : AVEC GAUCHISSEMENT
!        'MECA_POU_D_TGM' : AVEC GAUCHISSEMENT, SECTION MULTIFIBRES
!     POUTRE COURBE DE TIMOSHENKO
!        'MECA_POU_C_T'   : SECTION CONSTANTE
!     ------------------------------------------------------------------
    integer :: nbres
    parameter     (nbres=4)
!
    integer :: codres(nbres), kpg, spt
    character(len=8) :: nomres(nbres), nompar, nomail, k8bid, fami, poum
    character(len=16) :: ch16, kmess(5)
!
    real(kind=8) :: valres(nbres), matk(78), alfay, alfaz, xnu, xl, xiz, xiy2
    real(kind=8) :: xiy
    real(kind=8) :: xflz, xfly, xfl, un, zero, deux, xjx, a, a2, xiz2
    real(kind=8) :: valpar, e, g, epx, xky, xkz, ang, along
    real(kind=8) :: pgl(3, 3), pgl1(3, 3), pgl2(3, 3)
    real(kind=8) :: bsm(12, 12), de(12), fs(14)
    real(kind=8) :: rad, angarc, angs2
    real(kind=8) :: carsec(6), r8bid
    real(kind=8) :: ugp(12), dug(12), klv(78), duly, force(3), plouf
    real(kind=8) :: ulp(12), dul(12), dvl(12), dpe(12), dve(12)
    real(kind=8) :: sim(12), sip(12), fono(12), varmo(8), varpl(8)
    real(kind=8) :: forref, momref
!
    integer :: ncc, nnoc, lorien, j, ind, lrcou, lx, idefi, nbpar, lmater, in
    integer :: lsect2, lsect, i, ivectu, icontg, neq, nc, nno
    integer :: ielem, irepe, ndim, iadzi, iazk24
    integer :: iplouf, npg, infodi, itype, ibid
    integer :: igeom, ideplm, ideplp, icompo, nbt, jdc, irep, ifono, ilogic
    integer :: inbfib, nbfib, jacf
!
    parameter (zero=0.0d0, deux=2.0d0, un=1.0d0)
!     ------------------------------------------------------------------
    call jemarq()
    infodi = 1
    irepe = 0
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
    if ((nomte(1:9) .eq.'MECA_DIS_') .or. (nomte(1:12) .eq.'MECA_2D_DIS_')) then
!        ON VERIFIE QUE LES CARACTERISTIQUES ONT ETE AFFECTEES
!        LE CODE DU DISCRET
        call infdis('CODE', ibid, r8bid, nomte)
!        LE CODE STOKE DANS LA CARTE
        call infdis('TYDI', infodi, r8bid, k8bid)
        if (infodi .ne. ibid) then
            call u2mesk('F+', 'DISCRETS_25', 1, nomte)
            call infdis('DUMP', ibid, r8bid, 'F+')
        endif
!        DISCRET DE TYPE RAIDEUR
        call infdis('DISK', infodi, r8bid, k8bid)
        if (infodi .eq. 0) then
            call u2mesk('A+', 'DISCRETS_27', 1, nomte)
            call infdis('DUMP', ibid, r8bid, 'A+')
        endif
!        MATRICE DE RAIDEUR SYMETRIQUE OU PAS, POUR LES DISCRETS
        call infdis('SYMK', infodi, r8bid, k8bid)
    endif
!     RECUPERE LES INFORMATIONS SUR LES ELEMENTS
    call infted(nomte, infodi, nbt, nno, nc,&
                ndim, itype)
    neq = nno*nc
!     ELEMENT A 1 NOEUD OU 3PTS DE GAUSS : IELEM=0
    ielem = 1
    if ((nomte.eq.'MECA_POU_D_TG') .or. (nomte.eq.'MECA_POU_D_TGM') .or. (nno.eq.1)) then
        ielem = 0
    endif
!     NOMBRE DE POINTS DE GAUSS DE L'ELEMENT
    call elref4(' ', 'RIGI', iplouf, iplouf, iplouf,&
                npg, iplouf, iplouf, iplouf, iplouf)
!
! --- ------------------------------------------------------------------
    if (option(1:14) .eq. 'REFE_FORC_NODA') then
        call jevech('PVECTUR', 'E', ivectu)
        if (nomte .eq. 'MECA_POU_C_T' .or. nomte(1:11) .eq. 'MECA_DIS_TR') then
            call terefe('EFFORT_REFE', 'MECA_DISCRET', forref)
            call terefe('MOMENT_REFE', 'MECA_DISCRET', momref)
!
            do 200 in = 1, nno
                do 203 i = 1, 3
                    zr(ivectu+(in-1)*nc+i-1)=forref
203              continue
                do 202 i = 4, nc
                    zr(ivectu+(in-1)*nc+i-1)=momref
202              continue
200          continue
        else if (nomte(1:14).eq.'MECA_2D_DIS_T_') then
            call terefe('EFFORT_REFE', 'MECA_DISCRET', forref)
            do 204 in = 1, nno
                zr(ivectu+(in-1)*nc)=forref
                zr(ivectu+(in-1)*nc+1)=forref
204          continue
        else if (nomte(1:14).eq.'MECA_2D_DIS_TR') then
            call terefe('EFFORT_REFE', 'MECA_DISCRET', forref)
            call terefe('MOMENT_REFE', 'MECA_DISCRET', momref)
            do 205 in = 1, nno
                zr(ivectu+(in-1)*nc)=forref
                zr(ivectu+(in-1)*nc+1)=forref
                zr(ivectu+(in-1)*nc+2)=momref
205          continue
        else if (nomte(1:11).eq.'MECA_DIS_T_') then
            call terefe('EFFORT_REFE', 'MECA_DISCRET', forref)
            do 206 in = 1, nno
                zr(ivectu+(in-1)*nc)=forref
                zr(ivectu+(in-1)*nc+1)=forref
                zr(ivectu+(in-1)*nc+2)=forref
206          continue
        else
            kmess(1) = option
            kmess(2) = nomte
            kmess(3) = 'TE0039'
            call u2mesk('F', 'DISCRETS_15', 2, kmess)
        endif
! --- ------------------------------------------------------------------
    else if (option.eq.'SIEF_ELNO') then
        call jevech('PSIEFNOR', 'E', ivectu)
        call jevech('PCONTRR', 'L', icontg)
        do 10 i = 1, neq
            zr(ivectu-1+i) = zr(icontg-1+i)
10      continue
! --- ------------------------------------------------------------------
    else if (option.eq.'FONL_NOEU') then
        call jevech('PGEOMER', 'L', igeom)
        call jevech('PDEPLMR', 'L', ideplm)
        call jevech('PDEPLPR', 'L', ideplp)
        call jevech('PCOMPOR', 'L', icompo)
        call jevech('PMATERC', 'L', lmater)
        if (nomte(1:10) .eq. 'MECA_DIS_T') then
!           PARAMETRES EN ENTREE
            call jevech('PCAORIE', 'L', lorien)
            call matrot(zr(lorien), pgl)
!           DEPLACEMENTS DANS LE REPERE GLOBAL
!                 UGM = DEPLACEMENT PRECEDENT
!                 DUG = INCREMENT DE DEPLACEMENT
!                 UGP = DEPLACEMENT COURANT
            do 300 i = 1, neq
                dug(i) = zr(ideplp+i-1)
                ugp(i) = zr(ideplm+i-1) + dug(i)
300          continue
!           DEPLACEMENTS DANS LE REPERE LOCAL
!              ULM = DEPLACEMENT PRECEDENT    = PLG * UGM
!              DUL = INCREMENT DE DEPLACEMENT = PLG * DUG
!              ULP = DEPLACEMENT COURANT      = PLG * UGP
            if (ndim .eq. 3) then
                call utpvgl(nno, nc, pgl, dug, dul)
                call utpvgl(nno, nc, pgl, ugp, ulp)
            else if (ndim.eq.2) then
                call ut2vgl(nno, nc, pgl, dug, dul)
                call ut2vgl(nno, nc, pgl, ugp, ulp)
            endif
!           SEUL LE CAS SYMETRIQUE EST TRAITE
            call infdis('SYMK', iplouf, r8bid, k8bid)
            if (iplouf .ne. 1) then
                kmess(1) = option
                kmess(2) = nomte
                kmess(3) = 'TE0039'
                kmess(4) = ' '
                call u2mesk('F', 'DISCRETS_12', 4, kmess)
            endif
!
            call jevech('PCADISK', 'L', jdc)
            call infdis('REPK', irep, r8bid, k8bid)
            call dcopy(nbt, zr(jdc), 1, klv, 1)
            if (irep .eq. 1) then
                call utpsgl(nno, nc, pgl, zr(jdc), klv)
            endif
            if (zk16(icompo) .eq. 'DIS_CHOC') then
                call r8inir(8, zero, varmo, 1)
                call r8inir(12, zero, dvl, 1)
                call r8inir(12, zero, dpe, 1)
                call r8inir(12, zero, dve, 1)
!              RELATION DE COMPORTEMENT DE CHOC : FORCES NODALES
                call jevech('PVECTUR', 'E', ifono)
                do 501 i = 1, neq
                    zr(ifono+i-1) = 0.d0
                    sim(i) = 0.d0
501              continue
!
                ilogic = 0
                plouf = 0.d0
                call r8inir(3, zero, force, 1)
                call disief(nbt, neq, nno, nc, pgl,&
                            klv, ulp, sim, ilogic, plouf,&
                            sip, fono, force, ndim)
                call dichoc(nbt, neq, nno, nc, zi(lmater),&
                            dul, ulp, zr( igeom), pgl, klv,&
                            duly, dvl, dpe, dve, force,&
                            varmo, varpl, ndim)
                ilogic = 2
                call disief(nbt, neq, nno, nc, pgl,&
                            klv, ulp, sim, ilogic, duly,&
                            sip, zr(ifono), force, ndim)
                do 601 i = 1, neq
                    zr(ifono+i-1) = zr(ifono+i-1)-fono(i)
601              continue
                if (nno .eq. 2) then
                    do 602 i = 1, nc
                        zr(ifono+i-1) = 0.d0
602                  continue
                endif
            endif
        endif
    else
! --- ------------------------------------------------------------------
        if (option .eq. 'FORC_NODA') then
            call jevech('PCONTMR', 'L', icontg)
            call jevech('PVECTUR', 'E', ivectu)
            if (ielem .eq. 0) then
                do 50 in = 1, neq
                    fs(in) = zr(icontg+in-1)
50              continue
            else
!              VERIF DU NOMBRE DE POINT DE GAUUS
                ASSERT((npg.eq.2).or.(npg.eq.3))
                if (npg .eq. 2) then
                    do 60 in = 1, nc
                        fs(in) = -zr(icontg+in-1)
                        fs(in+nc) = zr(icontg+in+nc-1)
60                  continue
                else
!                 3 POINTS DE GAUSS : C'EST LE 1 ET LE 3
                    do 65 in = 1, nc
                        fs(in) = -zr(icontg+in-1)
                        fs(in+nc) = zr(icontg+in+nc+nc-1)
65                  continue
                endif
            endif
! --- ------------------------------------------------------------------
        else if (option.eq.'CHAR_MECA_EPSI_R') then
!           --- CARACTERISTIQUES MATERIAUX ---
            call jevech('PMATERC', 'L', lmater)
            nbpar = 0
            nompar = '  '
            valpar = zero
            do 70 i = 1, nbres
                valres(i) = zero
70          continue
            nomres(1) = 'E'
            nomres(2) = 'NU'
            nomres(3) = 'ALPHA'
            nomres(4) = 'RHO'
            call rcvalb(fami, kpg, spt, poum, zi(lmater),&
                        ' ', 'ELAS', nbpar, nompar, valpar,&
                        2, nomres, valres, codres, 1)
            call rcvalb(fami, kpg, spt, poum, zi(lmater),&
                        ' ', 'ELAS', nbpar, nompar, valpar,&
                        2, nomres(3), valres(3), codres(3), 0)
            if (codres(3) .ne. 0) valres(3) = zero
            if (codres(4) .ne. 0) valres(4) = zero
            e = valres(1)
            xnu = valres(2)
            g = e/ (deux* (un+xnu))
!           --- CARACTERISTIQUES GENERALES DES SECTIONS ---
            call jevech('PCAGNPO', 'L', lsect)
            lsect = lsect - 1
!           --- SECTION INITIALE ---
            if (nomte .eq. 'MECA_POU_D_TGM') then
                call jevech('PNBSP_I', 'L', inbfib)
                nbfib = zi(inbfib)
                call jevech('PFIBRES', 'L', jacf)
                call pmfitg(nbfib, 3, zr(jacf), carsec)
                a = carsec(1)
                xiy = carsec(5)
                xiz = carsec(4)
            else
                a = zr(lsect+1)
                xiy = zr(lsect+2)
                xiz = zr(lsect+3)
            endif
            alfay = zr(lsect+4)
            alfaz = zr(lsect+5)
            xjx = zr(lsect+8)
!
            if ((nomte.ne.'MECA_POU_D_TG') .and. ( nomte.ne.'MECA_POU_D_TGM')) then
!              --- SECTION FINALE ---
                lsect2 = lsect + 11
                a2 = zr(lsect2+1)
                xiy2 = zr(lsect2+2)
                xiz2 = zr(lsect2+3)
            endif
            call jevech('PEPSINR', 'L', idefi)
            call jevech('PVECTUR', 'E', ivectu)
            epx = zr(idefi)
            xky = zr(idefi+1)
            xkz = zr(idefi+2)
            if (nomte .ne. 'MECA_POU_C_T') then
                fs(1) = e*a*epx
                fs(2) = 0.d0
                fs(3) = 0.d0
                fs(4) = 0.d0
                fs(5) = e*xiy*xky
                fs(6) = e*xiz*xkz
                if ((nomte.eq.'MECA_POU_D_TG')) then
                    fs( 7) = 0.d0
                    fs( 8) = e*a*epx
                    fs( 9) = 0.d0
                    fs(10) = 0.d0
                    fs(11) = 0.d0
                    fs(12) = e*xiy*xky
                    fs(13) = e*xiz*xkz
                    fs(14) = 0.d0
                else if (nomte.eq.'MECA_POU_D_TGM') then
!                 RECUPERATION DES CARACTERISTIQUES DES FIBRES
                    call pmfitx(zi(lmater), 1, carsec, r8bid)
                    fs(1) = carsec(1)*epx
                    fs(5) = carsec(5)*xky
                    fs(6) = carsec(4)*xkz
                    fs( 7) = 0.d0
                    fs( 8) = fs(1)
                    fs( 9) = 0.d0
                    fs(10) = 0.d0
                    fs(11) = 0.d0
                    fs(12) = fs(5)
                    fs(13) = fs(6)
                    fs(14) = 0.d0
                else if (nomte.eq.'MECA_POU_D_EM') then
!                 RECUPERATION DES CARACTERISTIQUES DES FIBRES
                    call pmfitx(zi(lmater), 1, carsec, r8bid)
                    fs(1) = carsec(1)*epx
                    fs(5) = carsec(5)*xky
                    fs(6) = carsec(4)*xkz
                    fs(7) = fs(1)
                    fs(8) = 0.d0
                    fs(9) = 0.d0
                    fs(10) = 0.d0
                    fs(11) = fs(5)
                    fs(12) = fs(6)
                else
                    fs( 7) = e*a2*epx
                    fs( 8) = 0.d0
                    fs( 9) = 0.d0
                    fs(10) = 0.d0
                    fs(11) = e*xiy2*xky
                    fs(12) = e*xiz2*xkz
                endif
                fs(1) = -fs(1)
                fs(2) = -fs(2)
                fs(3) = -fs(3)
                fs(4) = -fs(4)
                fs(5) = -fs(5)
                fs(6) = -fs(6)
            else if (nomte.eq.'MECA_POU_C_T') then
!              COORDONNEES DES NOEUDS
                call jevech('PGEOMER', 'L', lx)
                lx = lx - 1
                xl = sqrt(&
                     ( zr(lx+4)-zr(lx+1))**2 + (zr(lx+5)-zr(lx+2) )**2 + (zr(lx+6)-zr(lx+3) )**2)
                if (xl .eq. zero) then
                    call tecael(iadzi, iazk24)
                    nomail = zk24(iazk24-1+3)(1:8)
                    call u2mesk('F', 'ELEMENTS2_43', 1, nomail)
                endif
                call jevech('PCAARPO', 'L', lrcou)
                rad = zr(lrcou)
                angarc = zr(lrcou+1)
                xfl = zr(lrcou+2)
                xfly = xfl
                xflz = xfl
                if (xfl .eq. zero) then
                    xfly = zr(lrcou+4)
                    xflz = zr(lrcou+6)
                endif
                angs2 = trigom('ASIN',xl/ (deux*rad))
                ang = angs2*deux
                xl = rad*ang
                xiy = xiy/xfly
                xiz = xiz/xflz
                xiy2 = xiy2/xfly
                xiz2 = xiz2/xflz
                call ptka10(matk, e, a, xiy, xiz,&
                            xjx, g, alfay, alfaz, rad,&
                            ang, 1)
!              REMPLISSAGE DE LA MATRICE CARREE
                ind = 0
                do 90 i = 1, 12
                    fs(i) = zero
                    de(i) = zero
                    do 80 j = 1, i - 1
                        ind = ind + 1
                        bsm(i,j) = matk(ind)
                        bsm(j,i) = matk(ind)
80                  continue
                    ind = ind + 1
                    bsm(i,i) = matk(ind)
90              continue
                along = deux*rad*epx*sin(angs2)
                de(1) = -along*cos(angs2)
                de(2) = along*sin(angs2)
                de(7) = -de(1)
                de(8) = de(2)
!              CALCUL DES FORCES INDUITES ---
                do 110 i = 1, 6
                    fs(i) = 0.d0
                    fs(i+6) = 0.d0
                    do 100 j = 1, 6
                        fs(i) = fs(i) + bsm(i,j)*de(j)
                        fs(i+6) = fs(i+6) + bsm(i+6,j+6)*de(j+6)
100                  continue
110              continue
            endif
        else
            ch16 = option
            call u2mesk('F', 'ELEMENTS2_47', 1, ch16)
        endif
!        RECUPERATION DES ORIENTATIONS ALPHA,BETA,GAMMA
        call jevech('PCAORIE', 'L', lorien)
!        MATRICE DE ROTATION MGL
        if (nomte .eq. 'MECA_POU_C_T') then
!           POUTRE COURBE DE TIMOSKENKO A 6 DDL: COORDONNEES DES NOEUDS
            call jevech('PGEOMER', 'L', lx)
            lx = lx - 1
            xl = sqrt(( zr(lx+4)-zr(lx+1))**2 + (zr(lx+5)-zr(lx+2))** 2 + (zr(lx+6)-zr(lx+3) )**2&
                 )
            if (xl .eq. zero) then
                ch16 = ' ?????????'
                call u2mesk('F', 'ELEMENTS2_43', 1, ch16(:8))
            endif
            call jevech('PCAARPO', 'L', lrcou)
            rad = zr(lrcou)
            angarc = zr(lrcou+1)
            angs2 = trigom('ASIN',xl/ (deux*rad))
            call matro2(zr(lorien), angarc, angs2, pgl1, pgl2)
            nnoc = 1
            ncc = 6
            call utpvlg(nnoc, ncc, pgl1, fs, zr(ivectu))
            call utpvlg(nnoc, ncc, pgl2, fs(7), zr(ivectu+6))
            if (irepe .ne. 0) then
                call matro2(zr(irepe), angarc, angs2, pgl1, pgl2)
                call utpvlg(nnoc, ncc, pgl1, zr(ivectu), zr(ivectu))
                call utpvlg(nnoc, ncc, pgl2, zr(ivectu+6), zr(ivectu+6))
            endif
        else
            call matrot(zr(lorien), pgl)
            if (ndim .eq. 3) then
                call utpvlg(nno, nc, pgl, fs, zr(ivectu))
                if (irepe .ne. 0) then
                    call matrot(zr(irepe), pgl)
                    call utpvlg(nno, nc, pgl, zr(ivectu), zr(ivectu))
                endif
            else if (ndim.eq.2) then
                call ut2vlg(nno, nc, pgl, fs, zr(ivectu))
                if (irepe .ne. 0) then
                    call matrot(zr(irepe), pgl)
                    call ut2vlg(nno, nc, pgl, zr(ivectu), zr(ivectu))
                endif
            endif
        endif
    endif
    call jedema()
end subroutine
