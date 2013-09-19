subroutine te0444(option, nomte)
    implicit none
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/dkqmas.h"
#include "asterfort/dkqrig.h"
#include "asterfort/dktmas.h"
#include "asterfort/dktrig.h"
#include "asterfort/dxiner.h"
#include "asterfort/dxqpgl.h"
#include "asterfort/dxroep.h"
#include "asterfort/dxtpgl.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/pmavec.h"
#include "asterfort/q4grig.h"
#include "asterfort/t3grig.h"
#include "asterfort/utmess.h"
#include "asterfort/utpslg.h"
#include "asterfort/utpvgl.h"
#include "asterfort/vecma.h"
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
!        OPTIONS     MASS_MECA       MASS_INER
!                    EPOT_ELEM       ECIN_ELEM
!
! person_in_charge: sebastien.fayolle at edf.fr
!
    integer :: nnos, ipoids, ivf, idfdx, jgano
    integer :: multic
    integer :: i, j, ivectu, npg
    integer :: nno, igeom, imatuu, jener, jfreq, iacce
    integer :: nddl, nvec, ndim, iret, n1, ni, n2
    real(kind=8) :: rho, epais
    real(kind=8) :: pgl(3, 3), xyzl(3, 4)
    real(kind=8) :: ener(3), matp(24, 24), matv(300)
!
!     ---> POUR DKT MATELEM = 3 * 6 DDL = 171 TERMES STOCKAGE SYME
!     ---> POUR DKQ MATELEM = 4 * 6 DDL = 300 TERMES STOCKAGE SYME
    real(kind=8) :: matloc(300)
!
! ---   RECUPERATION DES ADRESSES DANS ZR DES POIDS DES PG
!       DES FONCTIONS DE FORME DES VALEURS DES DERIVEES DES FONCTIONS
!       DE FORME ET DE LA MATRICE DE PASSAGE GAUSS -> NOEUDS
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfdx, jgano)
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
    if (option .eq. 'EPOT_ELEM') then
        if (nomte .eq. 'MEDKTG3') then
            call dktrig(nomte, xyzl, option, pgl, matloc,&
                        ener, multic)
        else if (nomte.eq.'MEDKQG4') then
            call dkqrig(nomte, xyzl, option, pgl, matloc,&
                        ener)
        else if (nomte.eq.'MET3GG3') then
            call t3grig(nomte, xyzl, option, pgl, matloc,&
                        ener)
        else if (nomte.eq.'MEQ4GG4') then
            call q4grig(nomte, xyzl, option, pgl, matloc,&
                        ener)
        endif
!
        call jevech('PENERDR', 'E', jener)
!
        do i = 1, 3
            zr(jener-1+i) = ener(i)
        end do
!
        else if (option.eq.'MASS_MECA'       .or. option.eq.'MASS_MECA_DIAG'&
        .or. option.eq.'MASS_MECA_EXPLI' .or. option.eq.'M_GAMMA'&
        .or. option.eq.'ECIN_ELEM') then
!
        if (nomte .eq. 'MEDKTG3' .or. nomte .eq. 'MET3GG3') then
            call dktmas(xyzl, option, pgl, matloc, ener,&
                        multic)
        else if (nomte.eq.'MEDKQG4'.or. nomte.eq.'MEQ4GG4') then
            call dkqmas(xyzl, option, pgl, matloc, ener)
        endif
!
        if (option .eq. 'MASS_MECA') then
            call jevech('PMATUUR', 'E', imatuu)
            call utpslg(nno, 6, pgl, matloc, zr(imatuu))
        else if (option.eq.'ECIN_ELEM') then
            call jevech('PENERCR', 'E', jener)
            call jevech('POMEGA2', 'L', jfreq)
!
            do i = 1, 3
                zr(jener-1+i) = zr(jfreq)*ener(i)
            end do
!
        else if (option.eq.'M_GAMMA') then
            call jevech('PACCELR', 'L', iacce)
            call jevech('PVECTUR', 'E', ivectu)
!
            nddl = 6*nno
            nvec = nddl* (nddl+1)/2
!
            call utpslg(nno, 6, pgl, matloc, matv)
            call vecma(matv, nvec, matp, nddl)
            call pmavec('ZERO', nddl, matp, zr(iacce), zr(ivectu))
!
        else if (option.eq.'MASS_MECA_DIAG' .or. option.eq.'MASS_MECA_EXPLI') then
            call jevech('PMATUUR', 'E', imatuu)
!
            nddl = 6*nno
            ndim = nddl* (nddl+1)/2
!
            do i = 1, ndim
                zr(imatuu-1+i) = matloc(i)
            end do
!
            if (option .eq. 'MASS_MECA_EXPLI') then
!     CORRECTION DES TERMES CORRESPONDANT AU DDL 6
!     NON PREVU PAR LA THEORIE DKT. ON RAJOUTE
!     UN TERME DIAGONAL NON ZERO EGAL A CELUI DU DDL 5.
!     CETTE CORRECTION A ETE INSPIRE PAR LA DEMARCHE DANS EUROPLEXUS
                do j = 1, nno
                    n1 = 6*(j-1) + 5
                    n2 = 6*(j-1) + 4
                    ni = 6*j
                    ndim = (ni + 1)*ni/2
                    n1 = (n1 + 1)*n1/2
                    n2 = (n2 + 1)*n2/2
                    zr(imatuu-1+ndim)=(zr(imatuu-1+n1)+zr(imatuu-1+n2))*0.5d0
                end do
            endif
        endif
    else if (option.eq.'MASS_INER') then
        call jevech('PMASSINE', 'E', imatuu)
        call dxroep(rho, epais)
!
        if (rho .le. r8prem()) then
            call utmess('F', 'ELEMENTS5_45')
        endif
!
        call dxiner(nno, zr(igeom), rho, epais, zr(imatuu),&
                    zr(imatuu+1), zr(imatuu+4))
    else
        ASSERT(.false.)
    endif
!
end subroutine
