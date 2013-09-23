subroutine te0247(option, nomte)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/carapo.h"
#include "asterfort/chgrep.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/lcsovn.h"
#include "asterfort/matela.h"
#include "asterfort/matro2.h"
#include "asterfort/moytem.h"
#include "asterfort/nmfgas.h"
#include "asterfort/nmlmab.h"
#include "asterfort/nmpoel.h"
#include "asterfort/porea1.h"
#include "asterfort/ptka01.h"
#include "asterfort/ptka02.h"
#include "asterfort/ptka10.h"
#include "asterfort/ptkg00.h"
#include "asterfort/r8inir.h"
#include "asterfort/rcvalb.h"
#include "asterfort/rcvarc.h"
#include "asterfort/tecach.h"
#include "asterfort/trigom.h"
#include "asterfort/utmess.h"
#include "asterfort/utpslg.h"
#include "asterfort/utpvlg.h"
#include "asterfort/verifm.h"
!
    character(len=*) :: option, nomte
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
!     CALCUL DES OPTIONS FULL_MECA OU RAPH_MECA POUR
!     LES ELEMENTS DE POUTRE 'MECA_POU_D_E/D_T'
!
!     ------------------------------------------------------------------
! IN  OPTION : K16 : NOM DE L'OPTION A CALCULER
!
! IN  NOMTE  : K16 : NOM DU TYPE ELEMENT
!        'MECA_POU_D_E' : POUTRE DROITE D'EULER       (SECTION VARIABLE)
!        'MECA_POU_D_T' : POUTRE DROITE DE TIMOSHENKO (SECTION VARIABLE)
!        'MECA_POU_C_T' : POUTRE COURBE DE TIMOSHENKO
!     ------------------------------------------------------------------
!
!
    integer :: igeom, icompo, imate, isect, iorien, nd, nk
    integer :: iinstm, iinstp, icarcr, icontm, ideplm, ideplp, imatuu
    integer :: ivectu, icontp, itype, nno, nc, ivarim, ivarip, itemp, i
    integer :: jtab(7), jcret, kk, lgpg, iret, iretm, iretp, iret2
    integer :: npg, ndimel, nnoel, nnosel, iplouf
    integer :: lx, lrcou, istrxm, istrxp, ldep
    parameter    (nno=2,nc=6,nd=nc*nno,nk=nd*(nd+1)/2)
    real(kind=8) :: e, nu, g, em, num
    real(kind=8) :: a, xiy, xiz, alfay, alfaz, xjx, ez, ey
    real(kind=8) :: a2, xiy2, xiz2, alfay2, alfaz2, xjx2, xl
! AUTRES
    integer :: nbclma, nbclem
    parameter (nbclma=12,nbclem=7)
    real(kind=8) :: coelma(nbclma), coelem(nbclem)
    integer :: codlma(nbclma), codlem(nbclem)
    character(len=8) :: nomlma(nbclma), nomlem(nbclem)
    character(len=24) :: valk(2)
!
    real(kind=8) :: pgl(3, 3), fl(nd), klv(nk), kls(nk), flc, effnoc
    real(kind=8) :: pgl1(3, 3), pgl2(3, 3), rad, angarc, angs2
    real(kind=8) :: zero, deux
    real(kind=8) :: xfl, xfly, xflz, ang
    real(kind=8) :: effnom, tempm, tempp
    real(kind=8) :: irram, irrap, epsthe
    real(kind=8) :: sigma(nd), rgeom(nk), gamma, angp(3)
    logical :: reactu, matric, vecteu
!
    data nomlma / 'K','N','DE_0','P','P1','P2','M','RM',&
     &              'A_0','Y_0','Y_I','B'/
    data nomlem / 'N', 'UN_SUR_K', 'UN_SUR_M', 'QSR_K',&
     &              'BETA','PHI_ZERO','L'/
!     ------------------------------------------------------------------
!
!
    zero = 0.d0
    deux = 2.d0
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PCOMPOR', 'L', icompo)
    call jevech('PMATERC', 'L', imate)
    call jevech('PCAGNPO', 'L', isect)
    call jevech('PCAORIE', 'L', iorien)
    call jevech('PINSTMR', 'L', iinstm)
    call jevech('PINSTPR', 'L', iinstp)
    call jevech('PCARCRI', 'L', icarcr)
    call jevech('PVARIMR', 'L', ivarim)
    call jevech('PCONTMR', 'L', icontm)
    call jevech('PDEPLMR', 'L', ideplm)
!
    if (nomte .eq. 'MECA_POU_C_T') then
        if (zk16(icompo) .ne. 'ELAS' .or. zk16(icompo+2) .ne. 'PETIT') then
            call utmess('F', 'ELEMENTS5_43')
        endif
    endif
!
! ---- LA PRESENCE DU CHAMP DE DEPLACEMENT A L INSTANT T+
! ---- DEVRAIT ETRE CONDITIONNE  PAR L OPTION (AVEC RIGI_MECA_TANG
! ---- CA N A PAS DE SENS).
! ---- CEPENDANT CE CHAMP EST INITIALISE A 0 PAR LA ROUTINE NMMATR.
    call jevech('PDEPLPR', 'L', ideplp)
!
! --- POINT DE GAUSS DE L'ELEMENT
!
    call elref4(' ', 'RIGI', ndimel, nnoel, nnosel,&
                npg, iplouf, iplouf, iplouf, iplouf)
    ASSERT((npg.eq.2).or.(npg.eq.3))
!
!     -- BOOLEENS PRATIQUES :
    matric = option .eq. 'FULL_MECA' .or. option .eq. 'RIGI_MECA_TANG'
    vecteu = option .eq. 'FULL_MECA' .or. option .eq. 'RAPH_MECA'
!
! --- PARAMETRES EN SORTIE
!
    if (matric) then
        call jevech('PMATUUR', 'E', imatuu)
    endif
!
    if (vecteu) then
        call jevech('PVECTUR', 'E', ivectu)
        call jevech('PCONTPR', 'E', icontp)
        call jevech('PVARIPR', 'E', ivarip)
    endif
!
    call carapo(zr(isect), zr(igeom), zr(iorien), xl, pgl,&
                itype, a, xiy, xiz, xjx,&
                alfay, alfaz, ey, ez, a2,&
                xiy2, xiz2, xjx2, alfay2, alfaz2)
!
!
    if (zk16(icompo+2) .ne. 'PETIT' .and. zk16(icompo+2) .ne. 'GROT_GDEP') then
        valk(1) = zk16(icompo+2)
        valk(2) = nomte
        call utmess('F', 'ELEMENTS3_40', nk=2, valk=valk)
    endif
    reactu = zk16(icompo+2) .eq. 'GROT_GDEP'
!
    if (reactu .and. (zk16(icompo).eq.'ELAS')) then
!
!        RECUPERATION DU 3EME ANGLE NAUTIQUE AU TEMPS T-
        call jevech('PSTRXMR', 'L', istrxm)
        gamma = zr(istrxm+3-1)
!
!        CALCUL DE PGL,XL ET ANGP
        call porea1(nno, nc, zr(ideplm), zr(ideplp), zr(igeom),&
                    gamma, vecteu, pgl, xl, angp)
!
!        SAUVEGARDE DES ANGLES NAUTIQUES
        if (vecteu) then
            call jevech('PSTRXPR', 'E', istrxp)
            zr(istrxp+1-1) = angp(1)
            zr(istrxp+2-1) = angp(2)
            zr(istrxp+3-1) = angp(3)
        endif
!
    endif
!
    if (zk16(icompo) .eq. 'ELAS') then
!
        call moytem('RIGI', npg, 1, '+', tempp,&
                    iretp)
        call moytem('RIGI', npg, 1, '-', tempm,&
                    iretm)
        itemp = 0
        if ((iretp+iretm) .eq. 0) itemp=1
        call matela(zi(imate), ' ', itemp, tempp, e,&
                    nu)
        call matela(zi(imate), ' ', itemp, tempm, em,&
                    num)
        call verifm('RIGI', npg, 1, 'T', zi(imate),&
                    'ELAS', 1, epsthe, iret)
        g = e / (2.d0*(1.d0+nu))
!
!        --- CALCUL DES MATRICES ELEMENTAIRES ----
        if (itype .eq. 0) then
            if (nomte .eq. 'MECA_POU_D_E') then
                alfay = 0.d0
                alfaz = 0.d0
            endif
            call ptka01(klv, e, a, xl, xiy,&
                        xiz, xjx, g, alfay, alfaz,&
                        ey, ez, 1)
        else if (itype .eq. 1 .or. itype .eq. 2) then
            if (nomte .eq. 'MECA_POU_D_E') then
                alfay2 = 0.d0
                alfaz2 = 0.d0
                alfay = 0.d0
                alfaz = 0.d0
            endif
            call ptka02(itype, klv, e, a, a2,&
                        xl, xiy, xiy2, xiz, xiz2,&
                        xjx, xjx2, g, alfay, alfay2,&
                        alfaz, alfaz2, ey, ez, 1)
!
        else if (itype .eq. 10) then
            if (nomte .eq. 'MECA_POU_C_T') then
!        --- POUTRE COURBE DE TIMOSKENKO A 6 DDL ---
                call jevech('PCAARPO', 'L', lrcou)
                rad = zr(lrcou)
                xfl = zr(lrcou+2)
                xfly = xfl
                xflz = xfl
                if (xfl .eq. zero) then
                    xfly = zr(lrcou+4)
                    xflz = zr(lrcou+6)
                endif
                angs2 = trigom('ASIN', xl/(deux*rad) )
                ang = angs2*deux
                xl = rad*ang
                xiy = xiy/xfly
                xiz = xiz/xflz
                lx = igeom - 1
                xl = sqrt(&
                     ( zr(lx+4)-zr(lx+1))**2 + (zr(lx+5)-zr(lx+2) )**2 + (zr(lx+6)-zr(lx+3) )**2)
                angarc = zr(lrcou+1)
                angs2 = trigom('ASIN', xl/(deux*rad) )
                call matro2(zr(iorien), angarc, angs2, pgl1, pgl2)
                call ptka10(klv, e, a, xiy, xiz,&
                            xjx, g, alfay, alfaz, rad,&
                            ang, 1)
            endif
        endif
!
        if (option .eq. 'RAPH_MECA' .or. option .eq. 'FULL_MECA') then
            if ((itemp.ne.0) .and. (nu.ne.num)) then
                call utmess('A', 'ELEMENTS3_59')
            endif
            call nmpoel(nomte, npg, klv, xl, nno,&
                        nc, pgl, pgl1, pgl2, zr(ideplp),&
                        epsthe, e, em, zr(icontm), fl,&
                        zr( icontp), angs2, rad)
        endif
!
        elseif ((zk16(icompo).eq.'LMARC_IRRA').or. (zk16(icompo)&
    .eq.'LEMAITRE_IRRA')) then
!
        call tecach('OON', 'PVARIMR', 'L', iret, nval=7,&
                    itab=jtab)
        lgpg = max(jtab(6),1)*jtab(7)
!
!-- RECUPERATION DES CARACTERISTIQUES ELASTIQUES
        call moytem('RIGI', npg, 1, '+', tempp,&
                    iretp)
        call moytem('RIGI', npg, 1, '-', tempm,&
                    iretm)
        itemp = 0
        if ((iretp+iretm) .eq. 0) itemp=1
        call matela(zi(imate), ' ', itemp, tempp, e,&
                    nu)
        call matela(zi(imate), ' ', itemp, tempm, em,&
                    num)
        call verifm('RIGI', npg, 1, 'T', zi(imate),&
                    'ELAS', 1, epsthe, iret)
        g = e / (2.d0*(1.d0+nu))
!
!        --- CALCUL DES MATRICES ELEMENTAIRES ELASTIQUES ----
        if (itype .eq. 0) then
            if (nomte .eq. 'MECA_POU_D_E') then
                alfay = 0.d0
                alfaz = 0.d0
            endif
            call ptka01(klv, e, a, xl, xiy,&
                        xiz, xjx, g, alfay, alfaz,&
                        ey, ez, 1)
        else if (itype .eq. 1 .or. itype .eq. 2) then
            if (nomte .eq. 'MECA_POU_D_E') then
                alfay2 = 0.d0
                alfaz2 = 0.d0
                alfay = 0.d0
                alfaz = 0.d0
            endif
            call ptka02(itype, klv, e, a, a2,&
                        xl, xiy, xiy2, xiz, xiz2,&
                        xjx, xjx2, g, alfay, alfay2,&
                        alfaz, alfaz2, ey, ez, 1)
        endif
        if ((itemp.ne.0) .and. (nu.ne.num)) then
            call utmess('A', 'ELEMENTS3_59')
        endif
!
        call nmpoel(nomte, npg, klv, xl, nno,&
                    nc, pgl, pgl1, pgl2, zr(ideplp),&
                    epsthe, e, em, zr(icontm), fl,&
                    zr(icontp), angs2, rad)
!
!-- CALCUL DE LA MATRICE TANGENTE ET CORRECTION DES TERMES D'ALLONGEMENT
!-- DES VECTEURS FORCES NODALES ET EFFORTS
        if (vecteu) then
            if (xl .eq. 0.d0) then
                call utmess('F', 'ELEMENTS3_60')
            endif
            effnom = zr(icontm)
!
            if (zk16(icompo) .eq. 'LMARC_IRRA') then
                call rcvalb('RIGI', 1, 1, '+', zi(imate),&
                            ' ', 'LMARC_IRRA', 0, ' ', [0.d0],&
                            12, nomlma(1), coelma(1), codlma(1), 1)
                call rcvarc(' ', 'IRRA', '-', 'RIGI', 1,&
                            1, irram, iret2)
                if (iret2 .gt. 0) irram=0.d0
                call rcvarc(' ', 'IRRA', '+', 'RIGI', 1,&
                            1, irrap, iret2)
                if (iret2 .gt. 0) irrap=0.d0
                call nmlmab(pgl, nno, npg, nc, zr(ideplp),&
                            effnom, tempm, tempp, zi(imate), zr(icarcr),&
                            zr(iinstm), zr(iinstp), xl, e, a,&
                            coelma, irram, irrap, zr(ivarim), zr(ivarip),&
                            kls, flc, effnoc, em, iret)
                do 52 kk = 1, 4
                    zr(ivarip+lgpg+kk-1) = zr(ivarip+kk-1)
                    zr(ivarip+2*lgpg+kk-1) = zr(ivarip+kk-1)
52              continue
                call rcvarc(' ', 'IRRA', '+', 'RIGI', 1,&
                            1, irrap, iret2)
                if (iret2 .gt. 0) irrap=0.d0
                zr(ivarip+5-1) = irrap
                call rcvarc(' ', 'IRRA', '+', 'RIGI', 2,&
                            1, irrap, iret2)
                if (iret2 .gt. 0) irrap=0.d0
                zr(ivarip+lgpg+5-1) = irrap
                call rcvarc(' ', 'IRRA', '+', 'RIGI', 3,&
                            1, irrap, iret2)
                if (iret2 .gt. 0) irrap=0.d0
                zr(ivarip+2*lgpg+5-1) = irrap
!
            else if (zk16(icompo).eq.'LEMAITRE_IRRA') then
                call rcvalb('RIGI', 1, 1, '+', zi(imate),&
                            ' ', 'LEMAITRE_IRRA', 0, ' ', [0.d0],&
                            7, nomlem(1), coelem(1), codlem(1), 1)
                call rcvarc(' ', 'IRRA', '-', 'RIGI', 1,&
                            1, irram, iret2)
                if (iret2 .gt. 0) irram=0.d0
                call rcvarc(' ', 'IRRA', '+', 'RIGI', 1,&
                            1, irrap, iret2)
                if (iret2 .gt. 0) irrap=0.d0
                call nmfgas('RIGI', npg, zi(imate), pgl, nno,&
                            nc, zr( ideplp), effnom, zr(ivarim), zr(icarcr),&
                            zr(iinstm), zr(iinstp), xl, a, coelem,&
                            irram, irrap, kls, flc, effnoc,&
                            zr(ivarip))
                zr(ivarip+1) = irrap
                zr(ivarip+lgpg) = zr(ivarip)
                call rcvarc(' ', 'IRRA', '+', 'RIGI', 2,&
                            1, irrap, iret2)
                if (iret2 .gt. 0) irrap=0.d0
                zr(ivarip+lgpg+1) = irrap
                zr(ivarip+2*lgpg) = zr(ivarip)
                call rcvarc(' ', 'IRRA', '+', 'RIGI', 3,&
                            1, irrap, iret2)
                if (iret2 .gt. 0) irrap=0.d0
                zr(ivarip+2*lgpg+1) = irrap
            endif
!
!           MODIFICATION DE L'EFFORT NORMAL
            if (npg .eq. 2) then
                zr(icontp) = effnoc
                zr(icontp+6) = effnoc
            else
                zr(icontp) = effnoc
                zr(icontp+6) = effnoc
                zr(icontp+12) = effnoc
            endif
!
            fl(1) = -flc
            fl(7) = flc
!
            if (option .eq. 'FULL_MECA') then
                klv(1) = kls(1)
                klv(22) = kls(22)
                klv(28) = kls(28)
            endif
!
        endif
    else
        call utmess('F', 'ELEMENTS3_61', sk=zk16(icompo))
    endif
    if (matric) then
!       CALCUL DE LA MATRICE DE RIGIDITE GEOMETRIQUE
        if (reactu .and. (zk16(icompo).eq.'ELAS')) then
            if (option .eq. 'FULL_MECA') then
                ldep = icontp
            else
                ldep = icontm
            endif
            if (npg .eq. 2) then
                do 15 i = 1, nc
                    sigma(i) = zr(ldep+i-1)
                    sigma(i+nc) = zr(ldep+nc+i-1)
15              continue
            else
                do 17 i = 1, nc
                    sigma(i) = zr(ldep+i-1)
                    sigma(i+nc) = zr(ldep+nc+nc+i-1)
17              continue
            endif
            if (itype .ne. 10) then
                call r8inir(nk, 0.0d0, rgeom, 1)
                call ptkg00(sigma, a, a2, xiz, xiz2,&
                            xiy, xiy2, xl, ey, ez,&
                            rgeom)
                call lcsovn(nk, klv, rgeom, klv)
            else
                call utmess('A', 'ELEMENTS3_28')
            endif
        endif
!
    endif
!
!
!
!        --- PASSAGE DU REPERE LOCAL AU REPERE GLOBAL ---
!
    if (nomte .eq. 'MECA_POU_C_T') then
        call jevech('PGEOMER', 'L', lx)
        lx = lx - 1
        xl = sqrt( ( zr(lx+4)-zr(lx+1))**2 + (zr(lx+5)-zr(lx+2))**2 + (zr(lx+6)-zr(lx+3) )**2 )
        call jevech('PCAARPO', 'L', lrcou)
        rad = zr(lrcou)
        angarc = zr(lrcou+1)
        angs2 = trigom('ASIN',xl/ (deux*rad))
        call matro2(zr(iorien), angarc, angs2, pgl1, pgl2)
    endif
    if (matric) then
        if (nomte .eq. 'MECA_POU_C_T') then
            call chgrep('LG', pgl1, pgl2, klv, zr(imatuu))
        else
            call utpslg(nno, nc, pgl, klv, zr(imatuu))
        endif
    endif
    if (vecteu) then
        if (nomte .eq. 'MECA_POU_C_T') then
            call utpvlg(1, 6, pgl1, fl, zr(ivectu))
            call utpvlg(1, 6, pgl2, fl(7), zr(ivectu+6))
        else
            call utpvlg(nno, nc, pgl, fl, zr(ivectu))
        endif
        call jevech('PCODRET', 'E', jcret)
        zi(jcret) = 0
    endif
!
end subroutine
