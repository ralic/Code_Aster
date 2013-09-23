subroutine te0150(option, nomte)
    implicit none
#include "jeveux.h"
#include "asterfort/elref4.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jevech.h"
#include "asterfort/jeveuo.h"
#include "asterfort/matro2.h"
#include "asterfort/matrot.h"
#include "asterfort/moytem.h"
#include "asterfort/pmfitg.h"
#include "asterfort/pmfitx.h"
#include "asterfort/pmfk01.h"
#include "asterfort/ptfocp.h"
#include "asterfort/ptforp.h"
#include "asterfort/ptka01.h"
#include "asterfort/ptka02.h"
#include "asterfort/ptka10.h"
#include "asterfort/ptka21.h"
#include "asterfort/rcvalb.h"
#include "asterfort/rcvarc.h"
#include "asterfort/tecach.h"
#include "asterfort/tecael.h"
#include "asterfort/trigom.h"
#include "asterfort/utmess.h"
#include "asterfort/utpvlg.h"
#include "asterfort/verifm.h"
    character(len=*) :: option, nomte
!     ------------------------------------------------------------------
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
!     CALCULE LE CHARGEMENT INDUIT PAR UNE ELEVATION UNIFORME DE
!     TEMPERATURE DANS LES POUTRES D'EULER ET DE TIMOSHENKO
!     ------------------------------------------------------------------
! IN  OPTION : K16 : NOM DE L'OPTION A CALCULER
!       'FC1D1D_MECA'       : FORCES LINEIQUES (COMP)
!       'FR1D1D_MECA'       : FORCES LINEIQUES (REEL)
!       'FF1D1D_MECA'       : FORCES LINEIQUES (FONCTION)
!       'SR1D1D_MECA'       : FORCES LINEIQUES SUIVEUSES (REEL)
!       'SF1D1D_MECA'       : FORCES LINEIQUES SUIVEUSES (FONCTION)
!       'CHAR_MECA_PESA_R'  : CHARGES DE PESANTEUR
!       'CHAR_MECA_ROTA_R'  : CHARGES DE ROTATION
!       'CHAR_MECA_TEMP_R'  : DEFORMATIONS THERMIQUES
!       'CHAR_MECA_SECH_R'  : DEFORMATIONS DUES AU SECHAGE
!       'CHAR_MECA_HYDR_R'  : DEFORMATIONS HYDRIQUES
! IN  NOMTE  : K16 : NOM DU TYPE ELEMENT
!       'MECA_POU_D_E'  : POUTRE DROITE D'EULER       (SECTION VARIABLE)
!       'MECA_POU_D_T'  : POUTRE DROITE DE TIMOSHENKO (SECTION VARIABLE)
!       'MECA_POU_C_T'  : POUTRE COURBE DE TIMOSHENKO(SECTION CONSTANTE)
!       'MECA_POU_D_EM' : POUTRE DROITE MULTIFIBRE D EULER (SECT. CONST)
!       'MECA_POU_D_TG' : POUTRE DROITE DE TIMOSHENKO (GAUCHISSEMENT)
!       'MECA_POU_D_TGM': POUTRE DROITE DE TIMOSHENKO (GAUCHISSEMENT)
!                         MULTI-FIBRES SECTION CONSTANTE
!     ------------------------------------------------------------------
    integer :: nbres, nbpar, lmater, itemps, iret, lsect, lsect2
    integer :: istruc, lorien, lrcou, lvect, lx
    integer :: itype, nc, ind, i, j, igau
    parameter                 (nbres=2)
    real(kind=8) :: valpar(3), valres(nbres)
    integer :: codres(nbres)
    character(len=4) :: fami
    character(len=8) :: nompar(3), nomres(nbres), materi, nomail
    character(len=16) :: ch16
    real(kind=8) :: e, nu, g
    real(kind=8) :: a, xiy, xiz, alfay, alfaz, xjx, ez, ey
    real(kind=8) :: a2, xiy2, xiz2, alfay2, alfaz2, xjx2, xl
    real(kind=8) :: ang, rad, angarc, angs2, along, xfly, xflz
    real(kind=8) :: pgl(3, 3), pgl1(3, 3), pgl2(3, 3), de(14), ffe(14)
    real(kind=8) :: bsm(14, 14), matk(105), carsec(6), cars1(6)
    real(kind=8) :: xfl, temp, f, zero, xjg
    real(kind=8) :: fr(14), fi(14), fgr(14), fgi(14)
    real(kind=8) :: fer(12), fei(12)
    real(kind=8) :: ea
    real(kind=8) :: kendog(1), kdessi(1), sech, hydr, instan, sechg(3), hydrg(3), sref
    integer :: ndim, nno, nnos, npg, ipoids
    integer :: ivf, idfdx, jgano
    integer :: icompo, isdcom, nbgfmx, iadzi, iazk24, isicom
    integer :: inbfib, nbfib, jacf
    logical :: lrho
!
    data nomres / 'E', 'NU' /
!     ------------------------------------------------------------------
    call jemarq()
!
    zero = 0.d0
    fami = 'RIGI'
!
    call elref4(' ', fami, ndim, nno, nnos,&
                npg, ipoids, ivf, idfdx, jgano)
!
!     -- POUR LA PESANTEUR ET LA ROTATION, ON N'A BESOIN QUE DE RHO
!        QUI EST FORCEMENT CONSTANT DANS LA MAILLE
    lrho=(option.eq.'CHAR_MECA_PESA_R'.or.&
     &      option.eq.'CHAR_MECA_ROTA_R')
!
!     --- RECUPERATION DES CARACTERISTIQUES MATERIAUX (MOYENNE)
    if (option(13:16) .ne. '1D1D' .and. .not.lrho) then
        call jevech('PMATERC', 'L', lmater)
        call moytem(fami, npg, 1, '+', valpar(1),&
                    iret)
    endif
    do 10 i = 1, nbres
        valres(i) = 0.d0
10  end do
!
    nbpar = 1
    nompar(1) = 'TEMP'
!
    e = 0.d0
!
    materi=' '
    if ((nomte.eq.'MECA_POU_D_EM') .or. (nomte.eq.'MECA_POU_D_TGM')) then
!       -- POUTRES MULTIFIBRES
!    --- APPEL INTEGRATION SUR SECTION
        if (option(13:16) .ne. '1D1D' .and. .not.lrho) then
!
!    --- RECUPERATION DU MATERIAU TORSION POUR ALPHA
            call jevech('PCOMPOR', 'L', icompo)
            call jeveuo(zk16(icompo-1+7), 'L', isdcom)
            call jeveuo(zk16(icompo-1+7)(1:8)//'.CPRI', 'L', isicom)
            nbgfmx=zi(isicom+2)
            materi=zk24(isdcom+6*nbgfmx)(1:8)
            call pmfitx(zi(lmater), 1, carsec, g)
            if (nomte .eq. 'MECA_POU_D_TGM') then
                ea = carsec(1)
                call jevech('PNBSP_I', 'L', inbfib)
                nbfib = zi(inbfib)
                call jevech('PFIBRES', 'L', jacf)
                call pmfitg(nbfib, 3, zr(jacf), cars1)
                a = cars1(1)
                xiy = cars1(5)
                xiz = cars1(4)
                e = ea/a
                call jevech('PCAGNPO', 'L', lsect)
                lsect = lsect-1
                alfay = zr(lsect+4)
                alfaz = zr(lsect+5)
                xjx = zr(lsect+8)
                xjg = zr(lsect+12)
            endif
        else
            if (nomte .eq. 'MECA_POU_D_TGM') then
                call jevech('PCAGNPO', 'L', lsect)
                itype=30
            else
                itype=0
            endif
            a = zero
            a2 = zero
        endif
    else
!       -- POUTRES CLASSIQUES
        if (option(13:16) .ne. '1D1D' .and. .not.lrho) then
            call rcvalb(fami, 1, 1, '+', zi(lmater),&
                        materi, 'ELAS', nbpar, nompar, valpar,&
                        nbres, nomres, valres, codres, 1)
!
            e = valres(1)
            nu = valres(2)
            g = e / (2.d0*(1.d0+nu))
        endif
!       -- RECUPERATION DES CARACTERISTIQUES GENERALES DES SECTIONS
        call jevech('PCAGNPO', 'L', lsect)
        lsect = lsect-1
        itype = nint(zr(lsect+23))
!
!       --- SECTION INITIALE ---
        a = zr(lsect+1)
        xiy = zr(lsect+2)
        xiz = zr(lsect+3)
        alfay = zr(lsect+4)
        alfaz = zr(lsect+5)
!        EY    = -ZR(LSECT+6)
!        EZ    = -ZR(LSECT+7)
        xjx = zr(lsect+8)
!
!       --- SECTION FINALE ---
        lsect2 = lsect + 11
        a2 = zr(lsect2+1)
        xiy2 = zr(lsect2+2)
        xiz2 = zr(lsect2+3)
        alfay2 = zr(lsect2+4)
        alfaz2 = zr(lsect2+5)
        ey = -(zr(lsect+6)+zr(lsect2+6))/2.d0
        ez = -(zr(lsect+7)+zr(lsect2+7))/2.d0
        xjx2 = zr(lsect2+8)
    endif
!
!     --- RECUPERATION DES COORDONNEES DES NOEUDS ---
    call jevech('PGEOMER', 'L', lx)
    lx = lx - 1
    xl = sqrt( (zr(lx+4)-zr(lx+1))**2 + (zr(lx+5)-zr(lx+2))**2 + (zr(lx+6)-zr(lx+3) )**2 )
    if (xl .eq. zero) then
        call tecael(iadzi, iazk24)
        nomail = zk24(iazk24-1+3)(1:8)
        call utmess('F', 'ELEMENTS2_43', sk=nomail)
    endif
!
!     --- RECUPERATION DES ORIENTATIONS ---
    call jevech('PCAORIE', 'L', lorien)
!
    if (nomte .eq. 'MECA_POU_D_E') then
!        --- POUTRE DROITE D'EULER A 6 DDL ---
        istruc = 1
        nno = 2
        nc = 6
        alfay = 0.d0
        alfaz = 0.d0
        alfay2 = 0.d0
        alfaz2 = 0.d0
        call matrot(zr(lorien), pgl)
    else if (nomte .eq. 'MECA_POU_D_T') then
!        --- POUTRE DROITE DE TIMOSKENKO A 6 DDL ---
        istruc = 1
        nno = 2
        nc = 6
        call matrot(zr(lorien), pgl)
!
    else if (nomte .eq. 'MECA_POU_C_T') then
!        --- POUTRE COURBE DE TIMOSKENKO A 6 DDL ---
        istruc = 1
        nno = 1
        nc = 6
        call jevech('PCAARPO', 'L', lrcou)
        rad = zr(lrcou)
        angarc = zr(lrcou+1)
        xfl = zr(lrcou+2)
        xfly = xfl
        xflz = xfl
        if (xfl .eq. 0.d0) then
            xfly = zr(lrcou+4)
            xflz = zr(lrcou+6)
        endif
        angs2 = trigom('ASIN', xl / ( 2.d0 * rad ) )
        ang = angs2 * 2.d0
        xl = rad * ang
        xiy = xiy / xfly
        xiz = xiz / xflz
        xiy2 = xiy2 / xfly
        xiz2 = xiz2 / xflz
        call matro2(zr(lorien), angarc, angs2, pgl1, pgl2)
!
    else if (nomte .eq. 'MECA_POU_D_EM') then
!        --- POUTRE MULTIFIBRE DROITE D'EULER A 6 DDL ---
        if (lrho) then
            itype=0
        else
            itype = 20
        endif
        nno = 2
        nc = 6
        call matrot(zr(lorien), pgl)
!
    else if (nomte .eq. 'MECA_POU_D_TG') then
!        --- POUTRE DROITE DE TIMOSKENKO A 7 DDL (GAUCHISSEMENT)---
        itype = 30
        nno = 2
        nc = 7
        call matrot(zr(lorien), pgl)
        a2 = a
        ey = -zr(lsect+6)
        ez = -zr(lsect+7)
        xjg = zr(lsect+12)
!
    else if (nomte .eq. 'MECA_POU_D_TGM') then
!        --- POUTRE DROITE DE TIMOSKENKO A 7 DDL ---
!           (GAUCHISSEMENT, MULTIFIBRES)---
        itype = 30
        nno = 2
        nc = 7
        call matrot(zr(lorien), pgl)
        a2 = a
        ey = -zr(lsect+6)
        ez = -zr(lsect+7)
        xjg = zr(lsect+12)
!
    else
        ch16 = nomte
        call utmess('F', 'ELEMENTS2_42', sk=ch16)
    endif
!
!     --- PASSAGE DU REPERE LOCAL AU REPERE GLOBAL ---
    if (option .eq. 'CHAR_MECA_FC1D1D') then
        call jevech('PVECTUC', 'E', lvect)
        if (nomte .eq. 'MECA_POU_D_TG' .or. nomte .eq. 'MECA_POU_D_TGM') then
            call ptfocp(itype, option, nomte, xl, rad,&
                        angs2, nno, 6, pgl, pgl1,&
                        pgl2, fr, fi)
            call utpvlg(nno, 6, pgl, fr, fgr)
            call utpvlg(nno, 6, pgl, fi, fgi)
            do 25 i = 1, 6
                zc(lvect+i-1) = dcmplx(fgr(i),fgi(i))
                zc(lvect+i-1+7) = dcmplx(fgr(i+6),fgi(i+6))
25          continue
            zc(lvect+7-1) = dcmplx(0.d0,0.d0)
            zc(lvect+14-1) = dcmplx(0.d0,0.d0)
        else
            call ptfocp(itype, option, nomte, xl, rad,&
                        angs2, nno, nc, pgl, pgl1,&
                        pgl2, fr, fi)
            if (nomte .eq. 'MECA_POU_C_T') then
                call utpvlg(nno, nc, pgl1, fr, fgr)
                call utpvlg(nno, nc, pgl2, fr(7), fgr(7))
                call utpvlg(nno, nc, pgl1, fi, fgi)
                call utpvlg(nno, nc, pgl2, fi(7), fgi(7))
            else
                call utpvlg(nno, nc, pgl, fr, fgr)
                call utpvlg(nno, nc, pgl, fi, fgi)
            endif
            do 15 i = 1, 12
                zc(lvect+i-1) = dcmplx(fgr(i),fgi(i))
15          continue
        endif
        else if( option.eq.'CHAR_MECA_FR1D1D' .or.&
     &         option.eq.'CHAR_MECA_FF1D1D' .or.&
     &         option.eq.'CHAR_MECA_SR1D1D' .or.&
     &         option.eq.'CHAR_MECA_SF1D1D' .or.&
     &         option.eq.'CHAR_MECA_ROTA_R' .or.&
     &         option.eq.'CHAR_MECA_PESA_R' ) then
        if (nomte .eq. 'MECA_POU_D_TG' .or. nomte .eq. 'MECA_POU_D_TGM') then
            call ptforp(0, option, nomte, a, a2,&
                        xl, rad, angs2, 1, nno,&
                        6, pgl, pgl1, pgl2, fer,&
                        fei)
        else
            call ptforp(itype, option, nomte, a, a2,&
                        xl, rad, angs2, 1, nno,&
                        nc, pgl, pgl1, pgl2, fer,&
                        fei)
        endif
        do 20 i = 1, 6
            ffe(i) = fer(i)
            ffe(i+nc) = fer(i+6)
20      continue
        if (nc .eq. 7) then
            ffe(7) = 0.d0
            ffe(14) = 0.d0
        endif
    else
        if (itype .eq. 0) then
!        --- POUTRE DROITE A SECTION CONSTANTE ---
            call ptka01(matk, e, a, xl, xiy,&
                        xiz, xjx, g, alfay, alfaz,&
                        ey, ez, istruc)
        else if (itype .eq. 1 .or. itype .eq. 2) then
!        --- POUTRE DROITE A SECTION VARIABLE (TYPE 1 OU 2) ---
            call ptka02(itype, matk, e, a, a2,&
                        xl, xiy, xiy2, xiz, xiz2,&
                        xjx, xjx2, g, alfay, alfay2,&
                        alfaz, alfaz2, ey, ez, istruc)
        else if (itype .eq. 10) then
!        --- POUTRE COURBE A SECTION CONSTANTE ---
            call ptka10(matk, e, a, xiy, xiz,&
                        xjx, g, alfay, alfaz, rad,&
                        ang, istruc)
        else if (itype .eq. 20) then
!        --- POUTRE DROITE MULTIFIBRE A SECTION CONSTANTE ---
            call pmfk01(carsec, 0.d0, xl, matk)
        else if (itype .eq. 30) then
!        --- POUTRE DROITE DE TIMOSHENKO (GAUCHISSEMENT, MULTIFIBRES) --
            call ptka21(matk, e, a, xl, xiy,&
                        xiz, xjx, xjg, g, alfay,&
                        alfaz, ey, ez)
        endif
!
!     --- REMPLISSAGE DE LA MATRICE CARREE ---
        ind = 0
        do 30 i = 1, nc*2
            de(i) = 0.d0
            do 40 j = 1, i-1
                ind = ind + 1
                bsm(i,j) = matk(ind)
                bsm(j,i) = matk(ind)
40          continue
            ind = ind + 1
            bsm(i,i) = matk(ind)
30      continue
!
        if (option .eq. 'CHAR_MECA_TEMP_R') then
!
!        --- CALCUL DU DEPLACEMENT LOCAL INDUIT PAR L'ELEVATION DE TEMP.
            call verifm(fami, npg, 1, '+', zi(lmater),&
                        'ELAS', 1, f, iret)
!
        else if (option.eq.'CHAR_MECA_SECH_R') then
!
!        --- CALCUL DU DEPLACEMENT LOCAL INDUIT PAR L'ELEVATION DE TEMP.
!        TEMPERATURE EFFECTIVE
            call moytem(fami, npg, 1, '+', temp,&
                        iret)
! ----   RECUPERATION DU CHAMP DU SECHAGE SUR L'ELEMENT
            sech=0.d0
            do 50 igau = 1, npg
                call rcvarc(' ', 'SECH', '+', 'RIGI', igau,&
                            1, sechg(igau), iret)
                if (iret .eq. 1) sechg(igau)=0.d0
                sech=sech+sechg(igau)/npg
50          continue
! ----   RECUPERATION DU SECHAGE DE REFERENCE
            call rcvarc(' ', 'SECH', 'REF', 'RIGI', 1,&
                        1, sref, iret)
            if (iret .eq. 1) sref=0.d0
! ----   RECUPERATION DE L'INSTANT
            call tecach('ONN', 'PTEMPSR', 'L', iret, iad=itemps)
            if (itemps .ne. 0) then
                instan = zr(itemps)
            else
                instan = 0.d0
            endif
            nompar(1) = 'TEMP'
            valpar(1) = temp
            nompar(2) = 'INST'
            valpar(2) = instan
            nompar(3) = 'SECH'
            valpar(3) = sech
! ----   INTERPOLATION DE K_DESSIC EN FONCTION DE LA TEMPERATURE
!        DE L HYDRATATION OU DU SECHAGE
!        ----------------------------------------------------------
            call rcvalb(fami, 1, 1, '+', zi(lmater),&
                        materi, 'ELAS', 3, nompar, valpar,&
                        1, 'K_DESSIC', kdessi, codres, 0)
            if (codres(1) .ne. 0) kdessi(1)=0.d0
            f = -kdessi(1)*(sref-sech)
!
        else if (option.eq.'CHAR_MECA_HYDR_R') then
!
!        TEMPERATURE EFFECTIVE
            call moytem(fami, npg, 1, '+', temp,&
                        iret)
! ----    RECUPERATION DU CHAMP D HYDRATATION SUR L'ELEMENT
            hydr=0.d0
            do 60 igau = 1, npg
                call rcvarc(' ', 'HYDR', '+', 'RIGI', igau,&
                            1, hydrg(igau), iret)
                if (iret .eq. 1) hydrg(igau)=0.d0
                hydr=hydr+hydrg(igau)/npg
60          continue
! ----   RECUPERATION DE L'INSTANT
            call tecach('ONN', 'PTEMPSR', 'L', iret, iad=itemps)
            if (itemps .ne. 0) then
                instan = zr(itemps)
            else
                instan = 0.d0
            endif
            nompar(1) = 'TEMP'
            valpar(1) = temp
            nompar(2) = 'INST'
            valpar(2) = instan
            nompar(3) = 'HYDR'
            valpar(3) = hydr
! ----   INTERPOLATION DE B_ENDOGE EN FONCTION DE LA TEMPERATURE
!        ET DE L HYDRATATION
            call rcvalb(fami, 1, 1, '+', zi(lmater),&
                        materi, 'ELAS', 3, nompar, valpar,&
                        1, 'B_ENDOGE', kendog, codres, 0)
            if (codres(1) .ne. 0) kendog(1)=0.d0
!        DEPLACEMENT INDUIT PAR L'HYDRATATION
            f = -kendog(1)*hydr
        else
            ch16 = option
            call utmess('F', 'ELEMENTS2_47', sk=ch16)
        endif
!
        if (itype .eq. 10) then
            along = 2.d0 * rad * f * sin(angs2)
            de(1) = -along * cos(angs2)
            de(2) = along * sin(angs2)
            de(7) = -de(1)
            de(8) = de(2)
        else if (itype .eq.30) then
            de(1) = -f * xl
            de(8) = -de(1)
        else
            de(1) = -f * xl
            de(7) = -de(1)
        endif
!
!        --- CALCUL DES FORCES INDUITES ---
        do 70 i = 1, nc
            ffe(i) = 0.d0
            ffe(i+nc) = 0.d0
            do 80 j = 1, nc
                ffe(i) = ffe(i) + bsm(i,j) * de(j)
                ffe(i+nc) = ffe(i+nc) + bsm(i+nc,j+nc) * de(j+nc)
80          continue
70      continue
    endif
!
    if (option .ne. 'CHAR_MECA_FC1D1D') then
        call jevech('PVECTUR', 'E', lvect)
!      --- MATRICE DE PASSAGE DU REPERE GLOBAL AU REPERE LOCAL: PGL
        if (itype .eq. 10) then
            call utpvlg(nno, nc, pgl1, ffe, zr(lvect))
            call utpvlg(nno, nc, pgl2, ffe(7), zr(lvect+6))
        else
            call utpvlg(nno, nc, pgl, ffe, zr(lvect))
        endif
    endif
!
    call jedema()
end subroutine
