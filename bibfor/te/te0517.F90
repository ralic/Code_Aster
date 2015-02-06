subroutine te0517(option, nomte)
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
!
! ======================================================================
!
! --------------------------------------------------------------------------------------------------
!
!           Calcul de l'option FORC_NODA pour les éléments POU_D_EM POU_D_TGM
!
! --------------------------------------------------------------------------------------------------
!
    implicit none
    character(len=16) :: option, nomte
!
#include "jeveux.h"
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/jevech.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jsd1ff.h"
#include "asterfort/lonele.h"
#include "asterfort/matela.h"
#include "asterfort/matrot.h"
#include "asterfort/moytem.h"
#include "asterfort/pmfinfo.h"
#include "asterfort/pmfitg.h"
#include "asterfort/porea2.h"
#include "asterfort/poutre_modloc.h"
#include "asterfort/r8inir.h"
#include "asterfort/tecach.h"
#include "asterfort/terefe.h"
#include "asterfort/utmess.h"
#include "asterfort/utpvlg.h"
#include "blas/ddot.h"
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nc, nno, kp, ncomp, i, jacf, icompo, iorien, ivectu
    integer :: jtab(7), ino, istrxm, nbsp
    integer :: nbgfmx, isdcom, igeom, iret, imate, k, npg, ifgm, iretc
!
    real(kind=8) :: pgl(3, 3), fl(14), xiy, xiz
    real(kind=8) :: ey, ez, temp, xl, gamma
    real(kind=8) :: xls2, d1b(7, 14), co(3), aa, e, nu, g, alfay, alfaz, phiy
    real(kind=8) :: phiz, forref, momref, carsec(6)
    aster_logical :: reactu
    character(len=24) :: mator
!
    integer, pointer :: cpri(:) => null()
!
! --------------------------------------------------------------------------------------------------
    integer :: nbfibr, nbgrfi, tygrfi, nbcarm, nug(10)
    integer, parameter :: nb_cara = 4
    real(kind=8) :: vale_cara(nb_cara)
    character(len=8) :: noms_cara(nb_cara)
    data noms_cara /'AY1','AZ1','EY1','EZ1'/
! --------------------------------------------------------------------------------------------------
!
    nno = 2
    ncomp = 18
!
    if (nomte .eq. 'MECA_POU_D_EM') then
        nc = 6
        npg = 2
    else if (nomte.eq.'MECA_POU_D_TGM') then
        nc = 7
        npg = 3
    endif
!
    if (option .eq. 'REFE_FORC_NODA  ') then
        call jevech('PVECTUR', 'E', ivectu)
        call terefe('EFFORT_REFE', 'MECA_POUTRE', forref)
        call terefe('MOMENT_REFE', 'MECA_POUTRE', momref)
        do ino = 1, nno
            do i = 1, 3
                zr(ivectu+(ino-1)*nc+i-1)=forref
            enddo
            do i = 4, nc
                zr(ivectu+(ino-1)*nc+i-1)=momref
            enddo
        enddo
    else if (option .eq. 'FORC_NODA') then
!       Récupération des caractéristiques des fibres
        call pmfinfo(nbfibr,nbgrfi,tygrfi,nbcarm,nug)
        call jevech('PFIBRES', 'L', jacf)
!
        call jevech('PCAORIE', 'L', iorien)
        call jevech('PGEOMER', 'L', igeom)
        call tecach('OON', 'PCONTMR', 'L', iret, nval=7, itab=jtab)
        nbsp=jtab(7)
        if (nbsp .ne. nbfibr) then
            call utmess('F', 'ELEMENTS_4')
        endif
        call jevech('PSTRXMR', 'L', istrxm)
!
        reactu = .false.
        call tecach('ONN', 'PCOMPOR', 'L', iretc, iad=icompo)
        if (iretc .eq. 0) reactu = (zk16(icompo+2).eq.'GROT_GDEP')
!
        call jevech('PVECTUR', 'E', ivectu)
        call r8inir(2*nc, 0.d0, fl, 1)
!       Calcul de la matrice de passage global/local
        if (reactu) then
            gamma = zr(istrxm+18-1)
            call porea2(nno, nc, zr(igeom), gamma, pgl, xl)
        else
            xl = lonele()
            call matrot(zr(iorien), pgl)
        endif
!
        if (nomte .eq. 'MECA_POU_D_EM') then
            do kp = 1, npg
                do k = 1, nc
                    fl(nc*(kp-1)+k) = zr(istrxm-1+ncomp*(kp-1)+k)
                enddo
            enddo
        else if (nomte.eq.'MECA_POU_D_TGM') then
!           Caractéristiques de la section
            call pmfitg(tygrfi, nbfibr, nbcarm, zr(jacf), carsec)
            aa    = carsec(1)
            xiy   = carsec(5)
            xiz   = carsec(4)

            call poutre_modloc('CAGNPO', noms_cara, nb_cara, lvaleur=vale_cara)
            alfay = vale_cara(1)
            alfaz = vale_cara(2)
!           Passage de G (centre de gravite) à C (centre de torsion)
            ey = vale_cara(3)
            ez = vale_cara(4)
!
            call jevech('PMATERC', 'L', imate)
            call moytem('RIGI', npg, 1, '+', temp, iret)
!
            call jeveuo(zk16(icompo-1+7)(1:8)//'.CPRI', 'L', vi=cpri)
            call jeveuo(zk16(icompo-1+7), 'L', isdcom)
            nbgfmx = cpri(3)
            mator = zk24(isdcom-1+nbgfmx*6+1)(1:8)
            call matela(zi(imate), mator, 1, temp, e, nu)
            g = e / (2.d0*(1.d0+nu))
            phiy = e*xiz*12.d0*alfay/ (xl*xl*g*aa)
            phiz = e*xiy*12.d0*alfaz/ (xl*xl*g*aa)
            xls2 = 0.5d0 * xl
!           Poids des points de gauss
            co(1) = 5.d0/9.d0
            co(2) = 8.d0/9.d0
            co(3) = 5.d0/9.d0
            do kp = 1, 3
                call jsd1ff(kp, xl, phiy, phiz, d1b)
                ifgm=ncomp*(kp-1)-1
                do k = 1, 2*nc
                    do i = 1, nc
                        fl(k)=fl(k) + xls2*zr(istrxm+ifgm+i)*d1b(i,k)*co(kp)
                    enddo
                enddo
            enddo
            do i = 1, 2
                fl(7*(i-1)+4) = fl( 7*(i-1)+4) - ez*fl(7*(i-1)+2) + ey* fl(7*(i-1)+3 )
            enddo
        else
            ASSERT(.false.)
        endif
!       Passage du repère local au repère global
        call utpvlg(nno, nc, pgl, fl, zr(ivectu))
    else
        ASSERT(.false.)
    endif
!
end subroutine
