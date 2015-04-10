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
!
! --------------------------------------------------------------------------------------------------
!
!     CALCULE INTEGRALE(E.DS) ou INTEGRALE(RHO.DS) AVEC E OU RHO CONSTANT PAR GROUPE
!
! --------------------------------------------------------------------------------------------------
!
!   IN
!       ICDMAT  : MATERIAU CODE
!       ISW     : 1-> 'E' ou 2 -> 'RHO' ou 3 -> 'RHO' facultatif
!
!     OUT
!       CASECT  :
!       GTO     : G DE MATER TORSION SI ISW=1
!
! --------------------------------------------------------------------------------------------------
!
    implicit none
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/jevech.h"
#include "asterfort/jeveuo.h"
#include "asterfort/pmfinfo.h"
#include "asterfort/pmfitg.h"
#include "asterfort/pmfmats.h"
#include "asterfort/poutre_modloc.h"
#include "asterfort/rcvala.h"
#include "asterfort/rcvalb.h"
#include "asterfort/rhoequ.h"
#include "asterfort/utmess.h"
!
    integer :: icdmat, isw
    real(kind=8) :: casect(6), gto
!
! --------------------------------------------------------------------------------------------------
!
    integer :: jacf, labsc
    integer :: icompo, isdcom, i, ipos, icp, ig, nbfig
    real(kind=8) :: carsec(6)
    real(kind=8) :: rho, rhos, rhofi, rhofe, cm, phie, phii
    real(kind=8) :: val(1), e, nu, absmoy
    character(len=32) :: materi
!
    integer :: codres(4)
    real(kind=8) :: valres(4)
    character(len=2) ::  nomres2(2)
    character(len=16) :: nomres4(4)
    data nomres2 /'E','NU'/
    data nomres4 /'RHO','PROF_RHO_F_INT','PROF_RHO_F_EXT','COEF_MASS_AJOU'/
!
! --------------------------------------------------------------------------------------------------
    integer, parameter :: nb_cara1 = 2
    real(kind=8) :: vale_cara1(nb_cara1)
    character(len=8) :: noms_cara1(nb_cara1)
    data noms_cara1 /'R1','EP1'/
!-----------------------------------------------------------------------
!
    integer :: nbfibr, nbgrfi, tygrfi, nbcarm, nug(10)
!
! --------------------------------------------------------------------------------------------------
!   Récupération des caractéristiques des fibres
    call pmfinfo(nbfibr,nbgrfi,tygrfi,nbcarm,nug)
    call jevech('PFIBRES', 'L', jacf)
!
!   Récupération des différents matériaux dans SDCOMP dans COMPOR
    call jevech('PCOMPOR', 'L', icompo)
    call jeveuo(zk16(icompo-1+7), 'L', isdcom)
!
! --------------------------------------------------------------------------------------------------
!   boucle sur les groupes de fibre
    casect(:) = 0.0d+0
    ipos=jacf
    do ig = 1, nbgrfi
        icp=isdcom-1+(nug(ig)-1)*6
        read(zk24(icp+6),'(I24)') nbfig
        materi=zk24(icp+2)(1:8)
!       calcul des caractéristiques du groupe
        call pmfitg(tygrfi, nbfig, nbcarm, zr(ipos), carsec)
!       multiplie par RHO ou E (constant sur le groupe)
        if (isw .eq. 1) then
            call rcvalb('RIGI', 1, 1, '+', icdmat, materi, 'ELAS', 0, ' ', [0.0d+0],&
                        1, 'E', val, codres, 0)
            if (codres(1) .eq. 1) then
                call rcvalb('RIGI', 1, 1, '+', icdmat, materi, 'ELAS_FLUI', 0, ' ', [0.0d+0],&
                            1, 'E', val, codres, 1)
            endif
        else if (isw.eq.2) then
            call rcvala(icdmat, materi, 'ELAS', 0, ' ', [0.0d+0], 1, 'RHO', val, codres, 0)
            if (codres(1) .eq. 1) then
                call poutre_modloc('CAGEP1', noms_cara1, nb_cara1, lvaleur=vale_cara1)
                call jevech('PABSCUR', 'L', labsc)
                absmoy = (zr(labsc-1+1)+zr(labsc-1+2))/2.0d0
                call rcvala(icdmat, materi, 'ELAS_FLUI', 1, 'ABSC',&
                            [absmoy], 4, nomres4, valres, codres, 1)
                rhos  = valres(1)
                rhofi = valres(2)
                rhofe = valres(3)
                cm    = valres(4)
                phie  = vale_cara1(1)*2.0d0
                if ( phie .le. r8prem() ) then
                    call utmess('F', 'ELEMENTS3_26')
                endif
                phii = (phie-2.0d0*vale_cara1(2))
                call rhoequ(rho, rhos, rhofi, rhofe, cm, phii, phie)
                val(1) = rho
            endif
        else if (isw.eq.3) then
            call rcvala(icdmat, materi, 'ELAS', 0, ' ', [0.0d+0], 1, 'RHO', val, codres, 0)
            if (codres(1) .ne. 0) val(1) = 0.0d+0
        endif
        do i = 1, 6
            casect(i) = casect(i) + val(1)*carsec(i)
        enddo
        ipos=ipos+nbfig*nbcarm
    enddo
! --------------------------------------------------------------------------------------------------
!   si ito=1 on récupère le matériau de torsion
    if (isw .eq. 1) then
        call pmfmats(icdmat, materi)
        call rcvalb('RIGI', 1, 1, '+', icdmat, materi, 'ELAS', 0, ' ', [0.0d+0],&
                    2, nomres2, valres, codres, 0)
!
        if (codres(1) .eq. 1) then
            call rcvalb('RIGI', 1, 1, '+', icdmat, materi, 'ELAS_FLUI', 0, ' ', [0.0d+0],&
                        2, nomres2, valres, codres, 1)
        endif
        e   = valres(1)
        nu  = valres(2)
        gto = e/(2.0d0*(1.0d0+nu))
    endif
end subroutine
