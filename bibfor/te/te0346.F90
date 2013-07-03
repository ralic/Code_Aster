subroutine te0346(option, nomte)
    implicit none
#include "jeveux.h"
#include "asterfort/jevech.h"
#include "asterfort/jspgno.h"
#include "asterfort/lcsovn.h"
#include "asterfort/matrot.h"
#include "asterfort/nmvmpo.h"
#include "asterfort/porea1.h"
#include "asterfort/pouex7.h"
#include "asterfort/ptkg20.h"
#include "asterfort/r8inir.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/utpslg.h"
#include "asterfort/utpvgl.h"
#include "asterfort/utpvlg.h"
#include "asterfort/vdiff.h"
#include "blas/ddot.h"
    character(len=16) :: option, nomte
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
!
!    - FONCTION REALISEE:  CALCUL DES OPTIONS NON-LINEAIRES MECANIQUES
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  RAPH_MECA ET FULL_MECA
!                                        OU RIGI_MECA_TANG
!                      NOMTE        -->  MECA_POU_D_TG
!
    real(kind=8) :: u(14), du(14), fl(14), pgl(3, 3), klv(105)
    real(kind=8) :: xd(3)
    real(kind=8) :: hoel(7*7), hota(7*7), d1b(7*14), work(14*7), rg0(14*14)
    real(kind=8) :: ey, ez, gamma, xl, xl2
    real(kind=8) :: b(14), rgeom(105), angp(3)
    real(kind=8) :: a, xiy, xiz, iyr2, izr2
    logical :: vecteu, matric, reactu
    integer :: nno, nc, i, j, jcret, npg
    integer :: igeom, imate, icontm, isect, iorien, icompo, iinstp
    integer :: icarcr, ideplm, ideplp, iinstm, ivectu, icontp, imat
    integer :: istrxm, istrxp, ldep
    character(len=4) :: fami
    character(len=24) :: valk(2)
!
    nno = 2
    nc = 7
!
!     -- BOOLEENS PRATIQUES :
!
    matric = option .eq. 'FULL_MECA' .or. option .eq. 'RIGI_MECA_TANG'
    vecteu = option .eq. 'FULL_MECA' .or. option .eq. 'RAPH_MECA'
!
!     -- RECUPERATION DES PARAMETRES "IN"/"OUT":
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call jevech('PCONTMR', 'L', icontm)
    call jevech('PCAGNPO', 'L', isect)
    call jevech('PCAORIE', 'L', iorien)
    call jevech('PCOMPOR', 'L', icompo)
    call jevech('PINSTMR', 'L', iinstm)
    call jevech('PINSTPR', 'L', iinstp)
    call jevech('PDEPLMR', 'L', ideplm)
    call jevech('PCARCRI', 'L', icarcr)
!
!     SEULEMENT EN COMP_INCR
    if (zk16(icompo+3) .eq. 'COMP_ELAS') then
        call u2mess('F', 'ELEMENTS2_90')
    endif
!
    if (zk16(icompo+2) .ne. 'PETIT' .and. zk16(icompo+2) .ne. 'GROT_GDEP') then
        valk(1) = zk16(icompo+2)
        valk(2) = nomte
        call u2mesk('F', 'ELEMENTS3_40', 2, valk)
    endif
!
    npg = 3
    fami = 'RIGI'
!
! ---- LA PRESENCE DU CHAMP DE DEPLACEMENT A L INSTANT T+
! ---- DEVRAIT ETRE CONDITIONNE  PAR L OPTION (AVEC RIGI_MECA_TANG
! ---- CA N A PAS DE SENS).
! ---- CEPENDANT CE CHAMP EST INITIALISE A 0 PAR LA ROUTINE NMMATR.
    call jevech('PDEPLPR', 'L', ideplp)
    icontp = 1
    if (vecteu) then
        call jevech('PVECTUR', 'E', ivectu)
        call jevech('PCONTPR', 'E', icontp)
    endif
    if (matric) call jevech('PMATUUR', 'E', imat)
!
!
!     GEOMETRIE EVENTUELLEMENT  REACTUALISEE :
!
    reactu = zk16(icompo+2) .eq. 'GROT_GDEP'
    if (reactu) then
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
    else
        call vdiff(3, zr(igeom-1+4), zr(igeom), xd)
        xl2=ddot(3,xd,1,xd,1)
        xl = sqrt(xl2)
        call matrot(zr(iorien), pgl)
    endif
!
!     PASSAGE DES DEPLACEMENTS DANS LE REPERE LOCAL:
!
    call utpvgl(nno, nc, pgl, zr(ideplp), du)
    call utpvgl(nno, nc, pgl, zr(ideplm), u)
!
!     -- PASSAGE DE G (CENTRE DE GRAVITE) A C (CENTRE DE TORSION)
!
    ey = -zr(isect-1+6)
    ez = -zr(isect-1+7)
    do 217 i = 1, 2
        j=7*(i-1)
        u(j+2) = u(j+2) - ez* u(j+4)
        u(j+3) = u(j+3) + ey* u(j+4)
        du(j+2) = du(j+2) - ez*du(j+4)
        du(j+3) = du(j+3) + ey*du(j+4)
217  end do
!
    call nmvmpo(fami, npg, option, nomte, nc,&
                xl, zi(imate), zr(isect), zr(icarcr), zk16(icompo),&
                u, du, zr(icontm), hoel, hota,&
                d1b, work, rg0, zr(icontp), fl,&
                klv)
!
!     ON REND LE FL DANS LE REPERE GLOBAL :
!
    if (vecteu) then
        fl( 4)=fl( 4)-ez*fl(2)+ey*fl( 3)
        fl(11)=fl(11)-ez*fl(9)+ey*fl(10)
!
        call utpvlg(nno, nc, pgl, fl, zr(ivectu))
    endif
!
    if (matric) then
!       CALCUL DE LA MATRICE DE RIGIDITE GEOMETRIQUE
        if (reactu) then
            if (option .eq. 'FULL_MECA') then
                ldep = icontp
            else
                ldep = icontm
            endif
            call jspgno(xl, zr(ldep), b)
            do 20 i = 1, 7
                b(i) = -b(i)
20          continue
            a = zr(isect+1)
            xiy = zr(isect+2)
            xiz = zr(isect+3)
            iyr2= zr(isect+10)
            izr2= zr(isect+11)
            call r8inir(105, 0.0d0, rgeom, 1)
            call ptkg20(b, a, xiz, xiy, iyr2,&
                        izr2, xl, ey, ez, rgeom)
!
            call lcsovn(105, klv, rgeom, klv)
        endif
    endif
!
!     ON REND LA MATRICE TANGENTE :
    if (matric) then
        call pouex7(klv, ey, ez)
        call utpslg(nno, nc, pgl, klv, zr(imat))
    endif
!
    if (option(1:9) .eq. 'FULL_MECA' .or. option(1:9) .eq. 'RAPH_MECA') then
        call jevech('PCODRET', 'E', jcret)
        zi(jcret) = 0
    endif
!
end subroutine
