subroutine te0409(option, nomte)
!     ----------------------------------------------------------------
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dxglrc.h"
#include "asterfort/dxqpgl.h"
#include "asterfort/dxtpgl.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/utmess.h"
#include "asterfort/utpslg.h"
#include "asterfort/utpvgl.h"
#include "asterfort/utpvlg.h"
    character(len=16) :: option, nomte
!
!   CALCUL DES OPTIONS DES ELEMENTS DE PLAQUE POUR LA MODELISATION DKTG
!   ET LA MODELISATION Q4GG
!     -----------------------------------------------------------------
!                            TRIANGLE  QUADRANGLE
!        KIRCHOFF  (MINCE)      DKT       DKQ
!
!                  (EPAIS)      Q4G       T3G
!
!        OPTIONS     RIGI_MECA       RIGI_MECA_TANG
!                    FULL_MECA       RAPH_MECA
!
! person_in_charge: sebastien.fayolle at edf.fr
!
    logical :: lrgm
!
    integer :: nnos, ipoids, ivf, idfdx, jgano
    integer :: codret, ideplm, ideplp
    integer :: icompo, i, i1, i2, ivectu, npg
    integer :: jcret
    integer :: nno, igeom, imatuu
    integer :: ndim, iret, icarcr
!
    real(kind=8) :: pgl(3, 3), xyzl(3, 4)
    real(kind=8) :: vecloc(24)
!
!     ---> POUR DKT MATELEM = 3 * 6 DDL = 171 TERMES STOCKAGE SYME
!     ---> POUR DKQ MATELEM = 4 * 6 DDL = 300 TERMES STOCKAGE SYME
    real(kind=8) :: matloc(300)
!
!     --->   UML : DEPLACEMENT A L'INSTANT T- (REPERE LOCAL)
!     --->   DUL : INCREMENT DE DEPLACEMENT   (REPERE LOCAL)
    real(kind=8) :: uml(6, 4), dul(6, 4)
!
    character(len=16) :: comp3, compor
!
! ---   RECUPERATION DES ADRESSES DANS ZR DES POIDS DES PG
!       DES FONCTIONS DE FORME DES VALEURS DES DERIVEES DES FONCTIONS
!       DE FORME ET DE LA MATRICE DE PASSAGE GAUSS -> NOEUDS
    call elref4(' ', 'RIGI', ndim, nno, nnos, npg, ipoids, ivf, idfdx, jgano)
!
    call jevech('PGEOMER', 'L', igeom)
!
    if (nno .eq. 3) then
        call dxtpgl(zr(igeom), pgl)
    else if (nno.eq.4) then
        call dxqpgl(zr(igeom), pgl, 'S', iret)
    endif
!
    call utpvgl(nno, 3, pgl, zr(igeom), xyzl)
!
    if (option.eq.'FULL_MECA'      .or. option.eq.'RAPH_MECA' .or.&
        option.eq.'RIGI_MECA_TANG' .or. option.eq.'RIGI_MECA') then
!
        lrgm = option.eq.'RIGI_MECA       '
!
        if (.not. lrgm) then
            call jevech('PDEPLMR', 'L', ideplm)
            call jevech('PDEPLPR', 'L', ideplp)
            call jevech('PCOMPOR', 'L', icompo)
            comp3 = zk16(icompo+3)
!
            if (zk16(icompo+2)(6:10) .eq. '_REAC' .or. zk16(icompo+2) .eq. 'GROT_GDEP') then
                if (zk16(icompo+2)(6:10) .eq. '_REAC') call utmess('A', 'ELEMENTS2_72')
!
                do i = 1, nno
                    i1 = 3* (i-1)
                    i2 = 6* (i-1)
                    zr(igeom+i1)   = zr(igeom+i1)   + zr(ideplm+i2)   + zr(ideplp+i2)
                    zr(igeom+i1+1) = zr(igeom+i1+1) + zr(ideplm+i2+1) + zr(ideplp+i2+1)
                    zr(igeom+i1+2) = zr(igeom+i1+2) + zr(ideplm+i2+2) + zr(ideplp+i2+2)
                end do
!
                if (nno .eq. 3) then
                    call dxtpgl(zr(igeom), pgl)
                else if (nno.eq.4) then
                    call dxqpgl(zr(igeom), pgl, 'S', iret)
                endif
!
                call utpvgl(nno, 3, pgl, zr(igeom), xyzl)
            endif
!
            call utpvgl(nno, 6, pgl, zr(ideplm), uml)
            call utpvgl(nno, 6, pgl, zr(ideplp), dul)
            call jevech('PCARCRI', 'L', icarcr)
        else
            comp3 = 'COMP_INCR       '
            compor= 'GLRC_DM         '
            if (nomte .eq. 'MEQ4GG4' .or. nomte .eq. 'MET3GG3') then
                compor = 'ELAS            '
            endif
            icarcr=1
        endif
!
        if (nomte .eq. 'MEDKTG3' .or. nomte .eq. 'MET3GG3'&
       .or. nomte .eq. 'MEDKQG4' .or. nomte .eq. 'MEQ4GG4') then
            if (lrgm) then
                call dxglrc(nomte, option, compor, xyzl, uml, dul, vecloc, matloc, pgl,&
                            zr(icarcr), codret)
            else
                call dxglrc(nomte, option, zk16(icompo), xyzl, uml, dul, vecloc, matloc, pgl,&
                            zr(icarcr), codret)
            endif
        else
            call utmess('F', 'ELEMENTS2_74', sk=nomte)
        endif
!
        if (option .eq. 'FULL_MECA') then
            call jevech('PMATUUR', 'E', imatuu)
            call jevech('PVECTUR', 'E', ivectu)
            call utpslg(nno, 6, pgl, matloc, zr(imatuu))
            call utpvlg(nno, 6, pgl, vecloc, zr(ivectu))
        else if (option.eq.'RAPH_MECA') then
            call jevech('PVECTUR', 'E', ivectu)
            call utpvlg(nno, 6, pgl, vecloc, zr(ivectu))
        else if (option.eq.'RIGI_MECA_TANG' .or. option.eq.'RIGI_MECA') then
            call jevech('PMATUUR', 'E', imatuu)
            call utpslg(nno, 6, pgl, matloc, zr(imatuu))
        endif
    else
        ASSERT(.false.)
    endif
!
    if (option(1:9) .eq. 'FULL_MECA' .or. option(1:9) .eq. 'RAPH_MECA') then
        call jevech('PCODRET', 'E', jcret)
        zi(jcret) = codret
    endif
!
end subroutine
