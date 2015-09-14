subroutine te0156(option, nomte)
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
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/angvx.h"
#include "asterfort/jevech.h"
#include "asterfort/matrot.h"
#include "asterfort/tecach.h"
#include "asterfort/terefe.h"
#include "asterfort/utpvlg.h"
    character(len=16) :: option, nomte
!-----------------------------------------------------------------------
! REALISE LES OPTIONS :
!     SIEF_ELNO
!                POUR  LES CONTRAINTES DE L'ELEMENT MECA_BARRE
!     FORC_NODA      : FORCES NODALE DE L'ELEMENT MECA_BARRE
!
! ----------------------------------------------------------------------
! IN OPTION    : K16 :  OPTION DE CALCUL
!                       'FORC_NODA' OU  'SIEF_ELNO'
!                       OU 'REFE_FORC_NODA'
! IN NOMTE     : K16 : NOM DU TYPE ELEMENT
!                      'MECA_BARRE'
!                      'MECA_2D_BARRE'
!
!
!
    integer :: ivectu, icontg, lorien, nno, nc, ino, i
    integer :: icompo, ideplm, ideplp, igeom, iretc
    real(kind=8) :: fs(6), pgl(3, 3), vect(6), forref
    real(kind=8) :: w(6), ang1(3), xd(3)
    aster_logical :: reactu
!
!     ------------------------------------------------------------------
!
    if (option .eq. 'REFE_FORC_NODA') then
        nno = 2
        if (nomte .eq. 'MECA_2D_BARRE') then
            nc = 2
        else if (nomte .eq. 'MECA_BARRE') then
            nc = 3
        endif
        call jevech('PVECTUR', 'E', ivectu)
        call terefe('EFFORT_REFE', 'MECA_BARRE', forref)
        do 101 ino = 1, nno
            do 102 i = 1, nc
                zr(ivectu+(ino-1)*nc+i-1)=forref
102         continue
101     continue
!
    else if (option .eq. 'FORC_NODA') then
        call jevech('PCONTMR', 'L', icontg)
        call jevech('PCAORIE', 'L', lorien)
        call tecach('ONO', 'PCOMPOR', 'L', iretc, iad=icompo)
        reactu = .false.
        if (iretc .eq. 0) reactu = (zk16(icompo+2).eq.'PETIT_REAC')
!
!        PARAMETRES EN SORTIE
        call jevech('PVECTUR', 'E', ivectu)
        nno=2
        nc=3
        do 13 i = 1, nno*nc
            fs(i)=0.d0
 13     continue
        fs(1) = -zr(icontg)
        fs(4) = zr(icontg)
!
!
        if (reactu) then
            call jevech('PGEOMER', 'L', igeom)
            call jevech('PDEPLMR', 'L', ideplm)
            call jevech('PDEPLPR', 'L', ideplp)
            if (nomte .eq. 'MECA_BARRE') then
                do 10 i = 1, 3
                    w(i) = zr(igeom-1+i) + zr(ideplm-1+i) + zr(ideplp- 1+i)
                    w(i+3) = zr(igeom+2+i) + zr(ideplm+2+i) + zr( ideplp+2+i)
                    xd(i) = w(i+3) - w(i)
 10             continue
            else if (nomte.eq.'MECA_2D_BARRE') then
                w(1) = zr(igeom-1+1) + zr(ideplm-1+1) + zr(ideplp-1+1)
                w(2) = zr(igeom-1+2) + zr(ideplm-1+2) + zr(ideplp-1+2)
                w(3) = 0.d0
                w(4) = zr(igeom-1+3) + zr(ideplm-1+3) + zr(ideplp-1+3)
                w(5) = zr(igeom-1+4) + zr(ideplm-1+4) + zr(ideplp-1+4)
                w(6) = 0.d0
                xd(1) = w(4) - w(1)
                xd(2) = w(5) - w(2)
                xd(3) = 0.d0
            endif
            call angvx(xd, ang1(1), ang1(2))
            ang1(3) = zr(lorien+2)
            call matrot(ang1, pgl)
        else
            call matrot(zr(lorien), pgl)
        endif
        call utpvlg(nno, nc, pgl, fs, vect)
!
!        ECRITURE DANS LE VECTEUR VECTU SUIVANT L'ELEMENT
!
        if (nomte .eq. 'MECA_BARRE') then
            do 30 i = 1, 6
                zr(ivectu+i-1)=vect(i)
 30         continue
        else if (nomte.eq.'MECA_2D_BARRE') then
            zr(ivectu) = vect(1)
            zr(ivectu +1) = vect(2)
            zr(ivectu +2) = vect(4)
            zr(ivectu +3) = vect(5)
        endif
    endif
end subroutine
