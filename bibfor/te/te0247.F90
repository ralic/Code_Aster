subroutine te0247(option, nomte)
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
!     CALCUL DES OPTIONS FULL_MECA OU RAPH_MECA POUR
!     LES ELEMENTS DE POUTRE 'MECA_POU_D_E/D_T'
!
! --------------------------------------------------------------------------------------------------
!
! IN  OPTION : K16 : NOM DE L'OPTION A CALCULER
!
! IN  NOMTE  : K16 : NOM DU TYPE ELEMENT
!        'MECA_POU_D_E' : POUTRE DROITE D'EULER       (SECTION VARIABLE)
!        'MECA_POU_D_T' : POUTRE DROITE DE TIMOSHENKO (SECTION VARIABLE)
!        'MECA_POU_C_T' : POUTRE COURBE DE TIMOSHENKO
!
! --------------------------------------------------------------------------------------------------
!
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/chgrep.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/lcsovn.h"
#include "asterfort/lonele.h"
#include "asterfort/matela.h"
#include "asterfort/matrot.h"
#include "asterfort/matro2.h"
#include "asterfort/moytem.h"
#include "asterfort/nmpoel.h"
#include "asterfort/porea1.h"
#include "asterfort/porigi.h"
#include "asterfort/poutre_modloc.h"
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
! --------------------------------------------------------------------------------------------------
!
    integer :: igeom, icompo, imate, iorien, nd, nk
    integer :: iinstm, iinstp, icarcr, icontm, ideplm, ideplp, imatuu
    integer :: ivectu, icontp, itype, nno, nc, ivarim, ivarip, itemp, i
    integer :: jcret, iret, iretm, iretp
    integer :: npg, ndimel, nnoel, nnosel
    integer :: lrcou, istrxm, istrxp, ldep
    parameter    (nno=2,nc=6,nd=nc*nno,nk=nd*(nd+1)/2)
    real(kind=8) :: e, nu, em, num
    real(kind=8) :: a, xiy, xiz, alfay, alfaz, xjx, ez, ey
    real(kind=8) :: a2, xiy2, xiz2, alfay2, alfaz2, xjx2, xl
!
    character(len=24) :: valk(2)
!
    real(kind=8) :: pgl(3, 3), fl(nd), klv(nk)
    real(kind=8) :: pgl1(3, 3), pgl2(3, 3), rad, angarc, angs2, ang
    real(kind=8) :: tempm, tempp
    real(kind=8) :: epsthe(1)
    real(kind=8) :: sigma(nd), rgeom(nk), gamma, angp(3)
    aster_logical :: reactu, matric, vecteu
! --------------------------------------------------------------------------------------------------
    integer, parameter :: nb_cara = 17
    real(kind=8) :: vale_cara(nb_cara)
    character(len=8) :: noms_cara(nb_cara)
    data noms_cara /'A1','IY1','IZ1','AY1','AZ1','EY1','EZ1','JX1',&
                    'A2','IY2','IZ2','AY2','AZ2','EY2','EZ2','JX2','TVAR'/
! --------------------------------------------------------------------------------------------------
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PCOMPOR', 'L', icompo)
    call jevech('PMATERC', 'L', imate)
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
!   la presence du champ de deplacement a l instant t+ devrait etre conditionne par l'option
!   (avec rigi_meca_tang ca n a pas de sens).
!   cependant ce champ est initialise a 0 par la routine nmmatr.
    call jevech('PDEPLPR', 'L', ideplp)
!   POINT DE GAUSS DE L'ELEMENT
    call elrefe_info(fami='RIGI', ndim=ndimel, nno=nnoel, nnos=nnosel, npg=npg)
    ASSERT((npg.eq.2).or.(npg.eq.3))
!
!   booleens pratiques
    matric = option .eq. 'FULL_MECA' .or. option .eq. 'RIGI_MECA_TANG'
    vecteu = option .eq. 'FULL_MECA' .or. option .eq. 'RAPH_MECA'
!
!   parametres en sortie
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
    call matrot(zr(iorien), pgl)
    xl = lonele()
    call poutre_modloc('CAGNPO', noms_cara, nb_cara, lvaleur=vale_cara)
!
    a      = vale_cara(1)
    xiy    = vale_cara(2)
    xiz    = vale_cara(3)
    alfay  = vale_cara(4)
    alfaz  = vale_cara(5)
    xjx    = vale_cara(8)
    a2     = vale_cara(9)
    xiy2   = vale_cara(10)
    xiz2   = vale_cara(11)
    alfay2 = vale_cara(12)
    alfaz2 = vale_cara(13)
    xjx2   = vale_cara(16)
    ey = (vale_cara(6) +vale_cara(14))/2.d0
    ez = (vale_cara(7) +vale_cara(15))/2.d0
    itype = nint(vale_cara(17))
!
    if (nomte .eq. 'MECA_POU_C_T') then
        call jevech('PCAARPO', 'L', lrcou)
        rad = zr(lrcou)
        angarc = zr(lrcou+1)
        angs2 = trigom('ASIN',xl/ (2.0d0*rad))
        ang = angs2 * 2.0d0
        xl = rad * ang
        call matro2(zr(iorien), angarc, angs2, pgl1, pgl2)
    endif
!
    if (zk16(icompo+2) .ne. 'PETIT' .and. zk16(icompo+2) .ne. 'GROT_GDEP') then
        valk(1) = zk16(icompo+2)
        valk(2) = nomte
        call utmess('F', 'ELEMENTS3_40', nk=2, valk=valk)
    endif
    reactu = zk16(icompo+2) .eq. 'GROT_GDEP'
    if (reactu .and. (zk16(icompo).eq.'ELAS')) then
!       recuperation du 3eme angle nautique au temps t-
        call jevech('PSTRXMR', 'L', istrxm)
        gamma = zr(istrxm+3-1)
!       calcul de pgl,xl et angp
        call porea1(nno, nc, zr(ideplm), zr(ideplp), zr(igeom),&
                    gamma, vecteu, pgl, xl, angp)
!       sauvegarde des angles nautiques
        if (vecteu) then
            call jevech('PSTRXPR', 'E', istrxp)
            zr(istrxp+1-1) = angp(1)
            zr(istrxp+2-1) = angp(2)
            zr(istrxp+3-1) = angp(3)
        endif
!
    endif
!
!   recuperation des caracteristiques elastiques
    call moytem('RIGI', npg, 1, '+', tempp, iretp)
    call moytem('RIGI', npg, 1, '-', tempm, iretm)
    itemp = 0
    if ((iretp+iretm) .eq. 0) itemp=1
    call matela(zi(imate), ' ', itemp, tempp, e, nu)
    call matela(zi(imate), ' ', itemp, tempm, em, num)
    call verifm('RIGI', npg, 1, 'T', zi(imate), 'ELAS', 1, epsthe, iret)
!
    if (zk16(icompo) .eq. 'ELAS') then
!       calcul des matrices elementaires
        call porigi(nomte, e, nu, xl, klv)
!
        if (option .eq. 'RAPH_MECA' .or. option .eq. 'FULL_MECA') then
            if ((itemp.ne.0) .and. (nu.ne.num)) then
                call utmess('A', 'ELEMENTS3_59')
            endif
            call nmpoel(nomte, npg, klv, xl, nno,&
                        nc, pgl, pgl1, pgl2, zr(ideplp),&
                        epsthe(1), e, em, zr(icontm), fl,&
                        zr( icontp), angs2, rad)
        endif
!
    else
        call utmess('F', 'ELEMENTS3_61', sk=zk16(icompo))
    endif
    if (matric) then
!       calcul de la matrice de rigidite geometrique
        if (reactu .and. (zk16(icompo).eq.'ELAS')) then
            if (option .eq. 'FULL_MECA') then
                ldep = icontp
            else
                ldep = icontm
            endif
            if (npg .eq. 2) then
                do i = 1, nc
                    sigma(i) = zr(ldep+i-1)
                    sigma(i+nc) = zr(ldep+nc+i-1)
                enddo
            else
                do i = 1, nc
                    sigma(i) = zr(ldep+i-1)
                    sigma(i+nc) = zr(ldep+nc+nc+i-1)
                enddo
            endif
            if (itype .ne. 10) then
                call r8inir(nk, 0.0d0, rgeom, 1)
                call ptkg00(sigma, a, a2, xiz, xiz2,&
                            xiy, xiy2, xl, ey, ez, rgeom)
                call lcsovn(nk, klv, rgeom, klv)
            else
                call utmess('A', 'ELEMENTS3_28')
            endif
        endif
    endif
!
!   passage du repere local au repere global
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
