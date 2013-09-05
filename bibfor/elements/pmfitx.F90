subroutine pmfitx(icdmat, isw, casect, gto)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "jeveux.h"
!
#include "asterfort/jevech.h"
#include "asterfort/jeveuo.h"
#include "asterfort/pmfitg.h"
#include "asterfort/rcvala.h"
#include "asterfort/rcvalb.h"
#include "asterfort/rhoequ.h"
#include "asterfort/u2mess.h"
    integer :: icdmat, isw
    real(kind=8) :: casect(6), gto
!     ------------------------------------------------------------------
!     CALCULE INTEGRALE(E.DS) ou INTEGRALE(RHO.DS)
!     AVEC E OU RHO CONSTANT PAR GROUPE
!     IN : ICDMAT : MATERIAU CODE
!     IN : ISW  1-> 'E' ou 2 -> 'RHO' ou 3 -> 'RHO' facultatif
!     OUT : CASECT
!     OUT : GTO : G DE MATER TORSION SI ISW=1
!     ------------------------------------------------------------------
!
    integer :: ncarfi, jacf, isicom
    real(kind=8) :: casec1(6)
    real(kind=8) :: zero, un, deux
    parameter (zero=0.0d+0,un=1.0d0,deux=2.d0)
    integer :: nbgf, inbf, icompo, isdcom, i, ipos, icp, nugf, ig, nbfig, nbgfmx
    integer :: lcage, labsc
    real(kind=8) :: rho, rhos, rhofi, rhofe, cm, phie, phii
    real(kind=8) :: val(1), e, nu, valres(4), absmoy
    character(len=8) :: materi, nomre4(4)
    integer :: codres(4)
    character(len=2) :: nomres(2)
!     ------------------------------------------------------------------
    data nomre4/'RHO','RHO_F_IN','RHO_F_EX','CM'/
!     ------------------------------------------------------------------
!
!     --- RECUPERATION DES CARACTERISTIQUES DES FIBRES :
    call jevech('PNBSP_I', 'L', inbf)
    nbgf=zi(inbf+1)
    call jevech('PFIBRES', 'L', jacf)
    ncarfi = 3
!
! --- RECUPERATION DES DIFFERENTS MATERIAUX DANS SDCOMP DANS COMPOR
    call jevech('PCOMPOR', 'L', icompo)
!
    call jeveuo(zk16(icompo-1+7), 'L', isdcom)
!
    do 10 i = 1, 6
        casect(i) = zero
10  end do
! --- BOUCLE SUR LES GROUPES DE FIBRE
    ipos=jacf
    do 100 ig = 1, nbgf
        nugf=zi(inbf+1+ig)
        icp=isdcom-1+(nugf-1)*6
        read(zk24(icp+6),'(I24)')nbfig
        materi=zk24(icp+2)(1:8)
! ---    CALCUL DES CARACTERISTIQUES DU GROUPE ---
        call pmfitg(nbfig, ncarfi, zr(ipos), casec1)
! ---    ON MULTIPLIE PAR RHO OU E (CONSTANT SUR LE GROUPE)
        if (isw .eq. 1) then
            call rcvalb('RIGI', 1, 1, '+', icdmat,&
                        materi, 'ELAS', 0, ' ', zero,&
                        1, 'E', val, codres, 0)
            if (codres(1) .eq. 1) then
                call rcvalb('RIGI', 1, 1, '+', icdmat,&
                            materi, 'ELAS_FLUI', 0, ' ', zero,&
                            1, 'E', val, codres, 1)
            endif
        else if (isw.eq.2) then
            call rcvala(icdmat, materi, 'ELAS', 0, ' ',&
                        [zero], 1, 'RHO', val, codres,0)
            if (codres(1) .eq. 1) then
                call jevech('PCAGEPO', 'L', lcage)
                call jevech('PABSCUR', 'L', labsc)
                absmoy = (zr(labsc-1+1)+zr(labsc-1+2))/deux
                call rcvala(icdmat, materi, 'ELAS_FLUI', 1, 'ABSC',&
                            [absmoy], 4, nomre4, valres, codres,1)
                rhos = valres(1)
                rhofi = valres(2)
                rhofe = valres(3)
                cm = valres(4)
                phie = zr(lcage-1+1)*deux
                if (phie .eq. 0.d0) then
                    call u2mess('F', 'ELEMENTS3_26')
                endif
                phii = (phie-deux*zr(lcage-1+2))
                call rhoequ(rho, rhos, rhofi, rhofe, cm,&
                            phii, phie)
                val(1) = rho
            endif
        else if (isw.eq.3) then
            call rcvala(icdmat, materi, 'ELAS', 0, ' ',&
                        [zero], 1, 'RHO', val, codres,0)
            if (codres(1) .ne. 0) val(1) = zero
        endif
        do 20 i = 1, 6
            casect(i) = casect(i) + val(1)*casec1(i)
20      continue
        ipos=ipos+nbfig*ncarfi
100  end do
!
! ---  SI ITO=1 ON RECUPERE LE MATERIAU DE TORSION
    if (isw .eq. 1) then
!
        call jeveuo(zk16(icompo-1+7)(1:8)//'.CPRI', 'L', isicom)
        nbgfmx=zi(isicom+2)
        materi=zk24(isdcom-1+nbgfmx*6+1)
        nomres(1) = 'E'
        nomres(2) = 'NU'
        call rcvalb('RIGI', 1, 1, '+', icdmat,&
                    materi, 'ELAS', 0, ' ', zero,&
                    2, nomres, valres, codres, 0)
!
        if (codres(1) .eq. 1) then
            call rcvalb('RIGI', 1, 1, '+', icdmat,&
                        materi, 'ELAS_FLUI', 0, ' ', zero,&
                        2, nomres, valres, codres, 1)
        endif
        e = valres(1)
        nu = valres(2)
        gto = e/ (deux* (un+nu))
    endif
end subroutine
