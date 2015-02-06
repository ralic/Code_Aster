subroutine te0346(option, nomte)
!
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
    implicit none
    character(len=16) :: option, nomte
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/jspgno.h"
#include "asterfort/lcsovn.h"
#include "asterfort/lonele.h"
#include "asterfort/matrot.h"
#include "asterfort/nmvmpo.h"
#include "asterfort/porea1.h"
#include "asterfort/pouex7.h"
#include "asterfort/poutre_modloc.h"
#include "asterfort/ptkg20.h"
#include "asterfort/utmess.h"
#include "asterfort/utpslg.h"
#include "asterfort/utpvgl.h"
#include "asterfort/utpvlg.h"
#include "blas/ddot.h"
! --------------------------------------------------------------------------------------------------
!
!       OPTION       -->  RAPH_MECA  FULL_MECA RIGI_MECA_TANG
!       NOMTE        -->  MECA_POU_D_TG
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nno, nc, i, j, jcret, npg, ipoids
    integer :: igeom, imate, icontm, iorien, icompo, iinstp
    integer :: ideplm, ideplp, iinstm, ivectu, icontp, imat
    integer :: istrxm, istrxp, ldep
    character(len=4) :: fami
    character(len=24) :: valk(3)
    aster_logical :: vecteu, matric, reactu
    real(kind=8) :: u(14), du(14), fl(14), pgl(3, 3), klv(105)
    real(kind=8) :: ey, ez, gamma, xl
    real(kind=8) :: b(14), rgeom(105), angp(3)
    real(kind=8) :: a, xiy, xiz, iyr2, izr2
!-----------------------------------------------------------------------
    integer, parameter :: nb_cara = 11
    real(kind=8) :: vale_cara(nb_cara)
    character(len=8) :: noms_cara(nb_cara)
    data noms_cara /'A1','IY1','IZ1','AY1','AZ1','EY1','EZ1','JX1','JG1','IYR21','IZR21'/
!
! --------------------------------------------------------------------------------------------------
!   booléens pratiques
    matric = option .eq. 'FULL_MECA' .or. option .eq. 'RIGI_MECA_TANG'
    vecteu = option .eq. 'FULL_MECA' .or. option .eq. 'RAPH_MECA'
!
!   Longueur de l'élément et pointeur sur la géométrie
    xl = lonele(igeom=igeom)
!   Récupération des parametres "in"/"out"
    call jevech('PMATERC', 'L', imate)
    call jevech('PCONTMR', 'L', icontm)
    call jevech('PCAORIE', 'L', iorien)
    call jevech('PCOMPOR', 'L', icompo)
    call jevech('PINSTMR', 'L', iinstm)
    call jevech('PINSTPR', 'L', iinstp)
    call jevech('PDEPLMR', 'L', ideplm)
!
    call poutre_modloc('CAGNP1', noms_cara, nb_cara, lvaleur=vale_cara)
!
!
    if (zk16(icompo+2) .ne. 'PETIT' .and. zk16(icompo+2) .ne. 'GROT_GDEP') then
        valk(1) = zk16(icompo+2)
        valk(2) = nomte
        call utmess('F', 'ELEMENTS3_40', nk=2, valk=valk)
    endif
!
    if (zk16(icompo) .ne. 'ELAS') then
        valk(1) = option
        valk(2) = zk16(icompo)
        valk(3) = nomte
        call utmess('F', 'ELEMENTS3_2', nk=3, valk=valk)
    endif
!
    fami = 'RIGI'
    call elrefe_info(fami=fami, nno=nno, npg=npg, jpoids=ipoids)
    nc = 7
!
!   la présence du champ de déplacement à l'instant T+
!   devrait être conditionné  par l'option (avec RIGI_MECA_TANG cela n'a pas de sens).
!   cependant ce champ est initialisé à 0 par la routine nmmatr.
    call jevech('PDEPLPR', 'L', ideplp)
    icontp = 1
    if (vecteu) then
        call jevech('PVECTUR', 'E', ivectu)
        call jevech('PCONTPR', 'E', icontp)
    endif
    if (matric) call jevech('PMATUUR', 'E', imat)
!
!   géométrie éventuellement  réactualisée
    reactu = zk16(icompo+2) .eq. 'GROT_GDEP'
    if (reactu) then
!       récupération du 3ème angle nautique au temps T-
        call jevech('PSTRXMR', 'L', istrxm)
        gamma = zr(istrxm+3-1)
!       calcul de pgl,xl et angp
        call porea1(nno, nc, zr(ideplm), zr(ideplp), zr(igeom+1), gamma, vecteu, pgl, xl, angp)
!       sauvegarde des angles nautiques
        if (vecteu) then
            call jevech('PSTRXPR', 'E', istrxp)
            zr(istrxp+1-1) = angp(1)
            zr(istrxp+2-1) = angp(2)
            zr(istrxp+3-1) = angp(3)
        endif
    else
        call matrot(zr(iorien), pgl)
    endif
!
!   passage des déplacements dans le repère local:
    call utpvgl(nno, nc, pgl, zr(ideplp), du)
    call utpvgl(nno, nc, pgl, zr(ideplm), u)
!
!   passage de G (centre de gravite) à C (centre de torsion)
    ey = vale_cara(6)
    ez = vale_cara(7)
    do i = 1, nno
        j=nc*(i-1)
        u(j+2) = u(j+2) - ez* u(j+4)
        u(j+3) = u(j+3) + ey* u(j+4)
        du(j+2) = du(j+2) - ez*du(j+4)
        du(j+3) = du(j+3) + ey*du(j+4)
    enddo
!
    call nmvmpo(fami, npg, nno, option, nc,&
                xl, zr(ipoids), zi(imate), vale_cara, u,&
                du, zr(icontm), zr(icontp), fl, klv)
!   FL dans le repère global
    if (vecteu) then
        fl( 4)=fl( 4)-ez*fl(2)+ey*fl( 3)
        fl(11)=fl(11)-ez*fl(9)+ey*fl(10)
        call utpvlg(nno, nc, pgl, fl, zr(ivectu))
    endif
!
    if (matric) then
!       calcul de la matrice de rigidité géométrique
        if (reactu) then
            if (option .eq. 'FULL_MECA') then
                ldep = icontp
            else
                ldep = icontm
            endif
            call jspgno(xl, zr(ldep), b)
            do i = 1, 7
                b(i) = -b(i)
            enddo
            a = vale_cara(1)
            xiy = vale_cara(2)
            xiz = vale_cara(3)
            iyr2= vale_cara(10)
            izr2= vale_cara(11)
            rgeom(:)=0.0d0
            call ptkg20(b, a, xiz, xiy, iyr2, izr2, xl, ey, ez, rgeom)
            call lcsovn(105, klv, rgeom, klv)
        endif
    endif
!
!   Matrice tangente
    if (matric) then
        call pouex7(klv, ey, ez)
        call utpslg(nno, nc, pgl, klv, zr(imat))
    endif
!
    if (vecteu) then
        call jevech('PCODRET', 'E', jcret)
        zi(jcret) = 0
    endif
!
end subroutine
