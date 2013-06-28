subroutine te0517(option, nomte)
    implicit none
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/jevech.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jsd1ff.h"
#include "asterfort/matela.h"
#include "asterfort/matrot.h"
#include "asterfort/moytem.h"
#include "asterfort/pmfitg.h"
#include "asterfort/porea2.h"
#include "asterfort/r8inir.h"
#include "asterfort/tecach.h"
#include "asterfort/terefe.h"
#include "asterfort/u2mess.h"
#include "asterfort/utpvlg.h"
#include "asterfort/vdiff.h"
#include "blas/ddot.h"
    character(len=16) :: option, nomte
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
!
! ======================================================================
!     CALCUL DE L'OPTION FORC_NODA POUR LES ELEMENTS :
!
!                         POU_D_EM (MULTI-FIBRES)
!                         POU_D_TGM (MULTI-FIBRES)
!
! IN  OPTION : OPTION DE CALCUL
! IN  NOMTE  : NOM DU TYPE ELEMENT
! ----------------------------------------------------------------------
!
    integer :: nc, nno
    character(len=24) :: mator
!
    real(kind=8) :: pgl(3, 3), fl(14), xiy, xiz
!
    integer :: nbfib, kp, ncomp, i, jacf
    integer :: icompo, iorien, ivectu
    integer :: jtab(7), ino, istrxm, nbsp
    integer :: isicom, nbgfmx, isdcom
!
    integer :: igeom, iret, isect, imate, k, npg, ifgm, iretc
    real(kind=8) :: xd(3), ey, ez, temp
    real(kind=8) :: xl, xl2, gamma
    real(kind=8) :: xls2, d1b(7, 14), co(3), aa, e, nu, g, alfay, alfaz, phiy
    real(kind=8) :: phiz
    real(kind=8) :: forref, momref, carsec(6)
    logical :: reactu
!
! ----------------------------------------------------------------------
    nno = 2

    if (nomte .eq. 'MECA_POU_D_EM') then
        nc = 6
        npg = 2
        ncomp = 15
    else if (nomte.eq.'MECA_POU_D_TGM') then
        nc = 7
        npg = 3
        ncomp = 18
    endif
!
    if (option .eq. 'REFE_FORC_NODA  ') then
!
        call jevech('PVECTUR', 'E', ivectu)
!
        call terefe('EFFORT_REFE', 'MECA_POUTRE', forref)
        call terefe('MOMENT_REFE', 'MECA_POUTRE', momref)
!
        do 501 ino = 1, nno
            do 503 i = 1, 3
                zr(ivectu+(ino-1)*nc+i-1)=forref
503          continue
            do 502 i = 4, nc
                zr(ivectu+(ino-1)*nc+i-1)=momref
502          continue
501      continue
    else if (option .eq. 'FORC_NODA') then
!
!       --- RECUPERATION DES CARACTERISTIQUES DES FIBRES
        call jevech('PNBSP_I', 'L', i)
        nbfib = zi(i)
        call jevech('PFIBRES', 'L', jacf)
!
        call jevech('PCAORIE', 'L', iorien)
        call jevech('PGEOMER', 'L', igeom)
        call tecach('OON', 'PCONTMR', 'L', 7, jtab,&
                    iret)
        nbsp=jtab(7)
        if (nbsp .ne. nbfib) call u2mess('F', 'ELEMENTS_4')
        call jevech('PSTRXMR', 'L', istrxm)
!
!
        reactu = .false.
        if (nomte .eq. 'MECA_POU_D_TGM') then
            call tecach('ONN', 'PCOMPOR', 'L', 1, icompo,&
                        iretc)
            if (iretc .eq. 0) reactu = (zk16(icompo+2).eq.'GROT_GDEP')
        endif
!
        call jevech('PVECTUR', 'E', ivectu)
        call r8inir(2*nc, 0.d0, fl, 1)
!
!        CALCUL DE LA MATRICE DE PASSAGE GLOBAL/LOCAL
        if (reactu) then
            gamma = zr(istrxm+18-1)
            call porea2(nno, nc, zr(igeom), gamma, pgl,&
                        xl)
!
        else
            call vdiff(3, zr(igeom-1+4), zr(igeom), xd)
            xl2=ddot(3,xd,1,xd,1)
            xl = sqrt(xl2)
            call matrot(zr(iorien), pgl)
!
        endif
!
        if (nomte .eq. 'MECA_POU_D_EM') then
!        
            do kp = 1,npg
                do k = 1,nc
                    fl(nc*(kp-1)+k) = zr(istrxm-1+ncomp*(kp-1)+k)
                end do
            end do

!
        else if (nomte.eq.'MECA_POU_D_TGM') then
!
            call jevech('PCAGNPO', 'L', isect)
!
!           -- CARACTERISTIQUES DE LA SECTION
            call pmfitg(nbfib, 3, zr(jacf), carsec)
            aa = carsec(1)
            xiy = carsec(5)
            xiz = carsec(4)
            alfay = zr(isect + 3)
            alfaz = zr(isect + 4)
!           -- PASSAGE DE G (CENTRE DE GRAVITE) A C (CENTRE DE TORSION)
            ey = -zr(isect + 5)
            ez = -zr(isect + 6)
!
            call jevech('PMATERC', 'L', imate)
            call moytem('RIGI', npg, 1, '+', temp,&
                        iret)
!
            call jeveuo(zk16(icompo-1+7)(1:8)//'.CPRI', 'L', isicom)
            call jeveuo(zk16(icompo-1+7), 'L', isdcom)
            nbgfmx = zi(isicom+2)
            mator = zk24(isdcom-1+nbgfmx*6+1)(1:8)
            call matela(zi(imate), mator, 1, temp, e,&
                        nu)
!
            g = e / (2.d0*(1.d0+nu))
            phiy = e*xiz*12.d0*alfay/ (xl*xl*g*aa)
            phiz = e*xiy*12.d0*alfaz/ (xl*xl*g*aa)
            xls2 = 0.5d0 * xl
!           POIDS DES POINTS DE GAUSS
            co(1) = 5.d0/9.d0
            co(2) = 8.d0/9.d0
            co(3) = 5.d0/9.d0
!
            do 400 kp = 1, 3
                call jsd1ff(kp, xl, phiy, phiz, d1b)
                ifgm=ncomp*(kp-1)-1
                do 410 k = 1, 2*nc
                    do 420 i = 1, nc
                        fl(k)=fl(k) + xls2*zr(istrxm+ifgm+i)*d1b(i,k)*&
                        co(kp)
420                  continue
410              continue
400          continue
            do 430 i = 1, 2
                fl(7*(i-1)+4) = fl( 7*(i-1)+4) - ez*fl(7*(i-1)+2) + ey* fl(7*(i-1)+3 )
430          continue
        else
            call assert(.false.)
        endif
!
!        PASSAGE DU REPERE LOCAL AU REPERE GLOBAL ---
        call utpvlg(nno, nc, pgl, fl, zr(ivectu))
    else
        call assert(.false.)
    endif
!
end subroutine
