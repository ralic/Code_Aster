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
#include "asterc/r8dgrd.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/coqrep.h"
#include "asterfort/dkqmas.h"
#include "asterfort/dkqrig.h"
#include "asterfort/dktmas.h"
#include "asterfort/dktrig.h"
#include "asterfort/dxbsig.h"
#include "asterfort/dxefro.h"
#include "asterfort/dxglrc.h"
#include "asterfort/dxiner.h"
#include "asterfort/dxqpgl.h"
#include "asterfort/dxroep.h"
#include "asterfort/dxtpgl.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/pmavec.h"
#include "asterfort/q4grig.h"
#include "asterfort/rccoma.h"
#include "asterfort/t3grig.h"
#include "asterfort/tecach.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/utpslg.h"
#include "asterfort/utpvgl.h"
#include "asterfort/utpvlg.h"
#include "asterfort/vecma.h"
#include "blas/dcopy.h"
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
!                    MASS_MECA       MASS_INER
!                    EPOT_ELEM       ECIN_ELEM
!                    FORC_NODA
!
! person_in_charge: sebastien.fayolle at edf.fr
!
    logical :: lrgm
!
    integer :: nnos, ipoids, ivf, idfdx, jgano
    integer :: multic, jtab(7), codret, ideplm, ideplp
    integer :: icompo, i, i1, i2, j, k, ivectu, ipg, npg
    integer :: icontm, jcret, iretc
    integer :: nno, igeom, imatuu, jener, jfreq, iacce
    integer :: nddl, nvec, ndim, iret, n1, ni, n2, icarcr
    integer :: jcara
!
    real(kind=8) :: rho, epais
    real(kind=8) :: pgl(3, 3), xyzl(3, 4), bsigma(24)
    real(kind=8) :: effgt(32), effort(32)
    real(kind=8) :: vecloc(24), ener(3), matp(24, 24), matv(300)
    real(kind=8) :: alpha, beta, t2ev(4), t2ve(4), c, s
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
    logical :: reactu
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
    if (option .eq. 'EPOT_ELEM') then
        if (nomte .eq. 'MEDKTG3') then
            call dktrig(nomte, xyzl, option, pgl, matloc, ener, multic)
        else if (nomte.eq.'MEDKQG4') then
            call dkqrig(nomte, xyzl, option, pgl, matloc, ener)
        else if (nomte.eq.'MET3GG3') then
            call t3grig(nomte, xyzl, option, pgl, matloc, ener)
        else if (nomte.eq.'MEQ4GG4') then
            call q4grig(nomte, xyzl, option, pgl, matloc, ener)
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
            call dktmas(xyzl, option, pgl, matloc, ener, multic)
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
            call u2mess('F', 'ELEMENTS5_45')
        endif
!
        call dxiner(nno, zr(igeom), rho, epais, zr(imatuu), zr(imatuu+1), zr(imatuu+4))
!
!     -- OPTIONS NON-LINEAIRES
    else if (option.eq.'FULL_MECA'      .or. option.eq.'RAPH_MECA' .or.&
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
            if (comp3 .eq. 'COMP_ELAS') then
                call u2mess('F', 'ELEMENTS3_92')
            endif
!
            if (zk16(icompo+2)(6:10) .eq. '_REAC' .or. zk16(icompo+2) .eq. 'GROT_GDEP') then
                if (zk16(icompo+2)(6:10) .eq. '_REAC') call u2mess('A', 'ELEMENTS2_72')
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
            call u2mesk('F', 'ELEMENTS2_74', 1, nomte)
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
    else if (option.eq.'FORC_NODA') then
        call tecach('ONN', 'PCOMPOR', 'L', 1, icompo, iretc)
!
! --- CALCUL DES MATRICES DE CHANGEMENT DE REPERES
!
!     T2EV : LA MATRICE DE PASSAGE (2X2) : UTILISATEUR -> INTRINSEQUE
!     T2VE : LA MATRICE DE PASSAGE (2X2) : INTRINSEQUE -> UTILISATEUR
!
        call jevech('PCACOQU', 'L', jcara)
        alpha = zr(jcara+1) * r8dgrd()
        beta  = zr(jcara+2) * r8dgrd()
        call coqrep(pgl, alpha, beta, t2ev, t2ve, c, s)
!
! --- VECTEUR DES EFFORTS GENERALISES AUX POINTS
! --- D'INTEGRATION DU REPERE LOCAL
        call tecach('OON', 'PCONTMR', 'L', 7, jtab, iret)
!
        do ipg=1, npg
            icontm=jtab(1)+8*(ipg-1)
            call dcopy(8, zr(icontm), 1, effort(8*(ipg-1)+1), 1)
        end do
!
! --- PASSAGE DU VECTEUR DES EFFORTS GENERALISES AUX POINTS
! --- D'INTEGRATION DU REPERE LOCAL AU REPERE INTRINSEQUE
!
        if (zk16(icompo)(1:7) .eq. 'GLRC_DM') then
            do ipg=1, npg
                icontm=jtab(1)+8*(ipg-1)
                call dcopy(8, zr(icontm), 1, effgt(8*(ipg-1)+1), 1)
            end do
        else
            call dxefro(npg, t2ev, effort, effgt)
        endif
!
        reactu = .false.
        if (iretc .eq. 0) then
            if (zk16(icompo+2)(6:10) .eq. '_REAC') call u2mess('A', 'ELEMENTS2_72')
            reactu = ( zk16(icompo+2) .eq. 'PETIT_REAC' .or. zk16(icompo+ 2) .eq. 'GROT_GDEP' )
        endif
!
        if (reactu) then
            call jevech('PDEPLMR', 'L', ideplm)
            call jevech('PDEPLPR', 'L', ideplp)
            do i = 1, nno
                i1 = 3* (i-1)
                i2 = 6* (i-1)
                zr(igeom+i1) = zr(igeom+i1) + zr(ideplm+i2) + zr( ideplp+i2)
                zr(igeom+i1+1) = zr(igeom+i1+1) + zr(ideplm+i2+1) + zr(ideplp+i2+1)
                zr(igeom+i1+2) = zr(igeom+i1+2) + zr(ideplm+i2+2) + zr(ideplp+i2+2)
            end do
            if (nno .eq. 3) then
                call dxtpgl(zr(igeom), pgl)
            else if (nno.eq.4) then
                call dxqpgl(zr(igeom), pgl, 'S', iret)
            endif
!
            call utpvgl(nno, 3, pgl, zr(igeom), xyzl)
        endif
!
! --- CALCUL DES EFFORTS INTERNES (I.E. SOMME_VOL(BT_SIG))
        call dxbsig(nomte, xyzl, pgl, effgt, bsigma)
!
! --- AFFECTATION DES VALEURS DE BSIGMA AU VECTEUR EN SORTIE
        call jevech('PVECTUR', 'E', ivectu)
!
        k = 0
        do i = 1, nno
            do j = 1, 6
                k = k + 1
                zr(ivectu+k-1) = bsigma(k)
            end do
        end do
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
