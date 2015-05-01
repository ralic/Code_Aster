subroutine te0141(option, nomte)
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
!     CALCULE LA MATRICE DE MASSE ELEMENTAIRE DES ELEMENTS DE POUTRE
!     D'EULER ET DE TIMOSHENKO
!
! --------------------------------------------------------------------------------------------------
!
! IN  OPTION : K16 : NOM DE L'OPTION A CALCULER
!       'MASS_MECA'      : CALCUL DE LA MATRICE DE MASSE COHERENTE
!       'MASS_MECA_DIAG' : CALCUL DE LA MATRICE DE MASSE CONCENTREE
!       'MASS_MECA_EXPLI': ......
!       'MASS_FLUI_STRU' : CALCUL DE LA MATRICE DE MASSE AJOUTEE
!       'M_GAMMA'        : CALCUL DU VECTEUR M_GAMMA
!
! IN  NOMTE  : K16 : NOM DU TYPE ELEMENT
!       'MECA_POU_D_E'  : POUTRE DROITE D'EULER       (SECTION VARIABLE)
!       'MECA_POU_D_T'  : POUTRE DROITE DE TIMOSHENKO (SECTION VARIABLE)
!       'MECA_POU_C_T'  : POUTRE COURBE DE TIMOSHENKO(SECTION CONSTANTE)
!       'MECA_POU_D_EM' : POUTRE DROITE MULTIFIBRE D EULER (SECT. CONST)
!       'MECA_POU_D_TG' : POUTRE DROITE DE TIMOSHENKO (GAUCHISSEMENT)
!       'MECA_POU_D_TGM': POUTRE DROITE DE TIMOSHENKO (GAUCHISSEMENT)
!                         MULTI-FIBRES SECTION CONSTANTE
!
! --------------------------------------------------------------------------------------------------
!
    implicit none
#include "jeveux.h"
#include "asterfort/chgrep.h"
#include "asterfort/jevech.h"
#include "asterfort/lonele.h"
#include "asterfort/matro2.h"
#include "asterfort/matrot.h"
#include "asterfort/moytem.h"
#include "asterfort/pmavec.h"
#include "asterfort/pmfmas.h"
#include "asterfort/pomass.h"
#include "asterfort/poutre_modloc.h"
#include "asterfort/rcvalb.h"
#include "asterfort/rhoequ.h"
#include "asterfort/trigom.h"
#include "asterfort/utmess.h"
#include "asterfort/utpslg.h"
#include "asterfort/vecma.h"
!
    character(len=*) :: option, nomte
!
! --------------------------------------------------------------------------------------------------
!
    integer :: lmater, iret, nbpar, labsc
    integer :: lorien, iacce, ivect, lrcou, lmat
    integer :: nno, nc, ntc, nbv, kanl, kpg, spt
    integer :: inbf, nbgf
    real(kind=8) :: valpar
    real(kind=8) :: xl, rad, angs2
    real(kind=8) :: absmoy, angarc
    real(kind=8) :: e, g, xnu, rho, rhos, rhofi, rhofe, cm, phie, phii
    real(kind=8) :: pgl(3, 3), pgl1(3, 3), pgl2(3, 3), mlv(105)
    real(kind=8) :: matv(105), matp(14, 14)
    character(len=8) :: nompar, fami, poum
    character(len=16) :: ch16
    character(len=24) :: mator
!
! --------------------------------------------------------------------------------------------------
!
    integer, parameter :: nbres=6
    integer :: codres(nbres)
    real(kind=8) :: valres(nbres)
    character(len=16) :: nomres(nbres)
    data nomres/'E','NU','RHO','PROF_RHO_F_INT','PROF_RHO_F_EXT','COEF_MASS_AJOU'/
!
! --------------------------------------------------------------------------------------------------
!
    integer, parameter :: nb_cara1 = 2
    real(kind=8) :: vale_cara1(nb_cara1)
    character(len=8) :: noms_cara1(nb_cara1)
    data noms_cara1 /'R1','EP1'/
!
! --------------------------------------------------------------------------------------------------
!
!   caracteristiques des elements
    nno = 2
    nc = 6
    fami='FPG1'
    kpg =1
    spt =1
    poum='+'
    mator = ' '
    if ((nomte.eq.'MECA_POU_D_TG').or.(nomte .eq. 'MECA_POU_D_TGM')) then
        nno = 2
        nc = 7
    endif
    ntc = nc*nno
    nbv = ntc*(ntc+1)/2
!   recuperation des caracteristiques materiaux
    rho = 0.d0
    valres(:) = 0.0d0
    call jevech('PMATERC', 'L', lmater)
    call moytem('RIGI', 2, 1, '+', valpar, iret)
    nompar = 'TEMP'
    nbpar = 1
!
    if (option .eq. 'MASS_FLUI_STRU') then
        call poutre_modloc('CAGEP1', noms_cara1, nb_cara1, lvaleur=vale_cara1)
        call jevech('PABSCUR', 'L', labsc)
        absmoy = (zr(labsc-1+1)+zr(labsc-1+2))/2.0d0
        if (nomte .eq. 'MECA_POU_D_TGM') then
            call jevech('PNBSP_I', 'L', inbf)
            nbgf=zi(inbf+1)
            if (nbgf.ne.1) call utmess('F', 'ELEMENTS3_3')
        endif
        call rcvalb(fami, kpg, spt, poum, zi(lmater), mator, 'ELAS_FLUI', 1, 'ABSC', [absmoy],&
                    nbres, nomres, valres, codres, 1)
        e = valres(1)
        xnu = valres(2)
        rhos = valres(3)
        rhofi = valres(4)
        rhofe = valres(5)
        cm = valres(6)
        phie = vale_cara1(1)*2.0d0
        g = e / ( 2.0d0 * ( 1.0d0 + xnu ) )
        if (phie .eq. 0.d0) then
            call utmess('F', 'ELEMENTS3_26')
        endif
        phii = (phie-2.0d0*vale_cara1(2))
        call rhoequ(rho, rhos, rhofi, rhofe, cm, phii, phie)
!
    else if ( (option.eq.'MASS_MECA').or.(option.eq.'MASS_MECA_DIAG').or.&
              (option.eq.'MASS_MECA_EXPLI').or.(option.eq.'M_GAMMA') ) then
        if ((nomte.ne.'MECA_POU_D_EM') .and. (nomte.ne.'MECA_POU_D_TGM')) then
            call rcvalb(fami, kpg, spt, poum, zi(lmater), ' ', 'ELAS', nbpar, nompar, [valpar],&
                        3, nomres, valres, codres, 1)
            e = valres(1)
            xnu = valres(2)
            rho = valres(3)
            g = e / ( 2.0d0 * ( 1.0d0 + xnu ) )
        endif
    else
        ch16 = option
        call utmess('F', 'ELEMENTS2_47', sk=ch16)
    endif
!   coordonnees des noeuds
    xl = lonele()
!   recuperation des orientations
    call jevech('PCAORIE', 'L', lorien)
!   calcul de la matrice de masse locale
    kanl = 1
    if (option .eq. 'MASS_MECA_DIAG' .or. option .eq. 'MASS_MECA_EXPLI') then
        kanl = 0
    endif
    if ((nomte.eq.'MECA_POU_D_EM') .or. (nomte.eq.'MECA_POU_D_TGM')) then
        call pmfmas(nomte, option, rho, zi(lmater), kanl, mlv)
    else
        call pomass(nomte, e, xnu, rho, kanl, mlv)
    endif
!
    if (option .eq. 'M_GAMMA') then
        call jevech('PACCELR', 'L', iacce)
        call jevech('PVECTUR', 'E', ivect)
        if (nomte.eq.'MECA_POU_C_T') then
            call jevech('PCAARPO', 'L', lrcou)
            rad = zr(lrcou)
            angarc = zr(lrcou+1)
            angs2 = trigom('ASIN',xl/ (2.0d0*rad))
            call matro2(zr(lorien), angarc, angs2, pgl1, pgl2)
            call chgrep('LG', pgl1, pgl2, mlv, matv)
        else
            call matrot(zr(lorien), pgl)
            call utpslg(nno, nc, pgl, mlv, matv)
        endif
        call vecma(matv, nbv, matp, ntc)
        call pmavec('ZERO', ntc, matp, zr(iacce), zr(ivect))
    else
        call jevech('PMATUUR', 'E', lmat)
!
        if (nomte.eq.'MECA_POU_C_T') then
            call jevech('PCAARPO', 'L', lrcou)
            rad = zr(lrcou)
            angarc = zr(lrcou+1)
            angs2 = trigom('ASIN',xl/ (2.0d0*rad))
            call matro2(zr(lorien), angarc, angs2, pgl1, pgl2)
            call chgrep('LG', pgl1, pgl2, mlv, zr(lmat))
        else
            call matrot(zr(lorien), pgl)
            call utpslg(nno, nc, pgl, mlv, zr(lmat))
        endif
    endif
!
end subroutine
