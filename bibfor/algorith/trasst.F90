subroutine trasst(modgen, numsst, isst1, lisint, nbeq1,&
                  nbmod, nbint)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!-------------------------------------------------------------C
!--       ROUTINE XXXXX2           M. CORUS - AOUT 2011     --C
!--       CALCUL DES TRAVAUX DANS LES SOUS STRUCTURES       --C
!--                                                         --C
!-------------------------------------------------------------C
!--   VARIABLES E/S  :
!--   MODGEN   /IN/  : NOM DU MODELE GENERALISE
!--   NUMSST   /IN/  : NUMERO DE LA SOUS STRUCTURE TRAITEE
!--   ISST1    /IN/  : NUMERO DE LA SOUS STRUCTURE
!--   LISINT   /IN/  : LISTE DES NOMS D'INTERFACES
!--   NBEQ1    /IN/  : NB DE DDL DE LA SST
!--   NBMOD    /IN/  : NOMBRE DE MODE DU MODELE REDUIT
!--   NBINT    /IN/  : NB D'INTERFACES ASSOCIEES A LA SST
!
    implicit none
!
!
!
!
#include "jeveux.h"
#include "asterfort/codent.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/jelira.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/lceqvn.h"
#include "asterfort/mrmult.h"
#include "asterfort/mtcmbl.h"
#include "asterfort/mtdefs.h"
#include "asterfort/mtdscr.h"
#include "asterfort/preres.h"
#include "asterfort/utmess.h"
#include "asterfort/zerlag.h"
#include "blas/daxpy.h"
#include "blas/ddot.h"
!
    character(len=1) :: listyp(2)
    character(len=4) :: k4bid
    character(len=8) :: modgen, rest1, mraid, mmass
    character(len=19) :: imped, lismat(2), nume91, solveu
    character(len=24) :: indin1
    integer :: i1, ibid, iret, j1, k1, l1, nbeq1, nbmod, isst1, llint1, nbddl1
    integer :: tach1, lomeg, lmod1, lmass, lbid, ltrsst, lraid, leff1, leff2
    integer :: lintf, nbint, ideeq, lcopy1, lsecme, limped, unit, numsst
    real(kind=8) :: travm, travk, traint, comlin(2), shift
    character(len=24) :: lisint
!
    call getvis(' ', 'UNITE', scal=unit, nbret=ibid)
    i1=numsst
!
!-- RECHERCHE DU MACRO ELEMENT ASSOCIE A LA SST
    call jeveuo(jexnum(modgen//'      .MODG.SSME', isst1), 'L', ibid)
    call jeveuo(zk8(ibid)//'      .NUME.DEEQ', 'L', ideeq)
!
!------------------------------------------------------------C
!--                                                        --C
!-- CONSTRUCTION DES MATRICES D'IMPEDANCE DYNAMIQUE K+MU*M --C
!--                   POUR L'ENRICHISSEMENT                --C
!--                                                        --C
!------------------------------------------------------------C
!
    call codent(numsst, 'D0', k4bid)
    imped='&&OP0091.IMPED'//k4bid
!
    call jeveuo(jexnum(modgen//'      .MODG.SSME', isst1), 'L', ibid)
!
    call jeveuo(zk8(ibid)//'.MAEL_MASS_REFE', 'L', lbid)
    mmass=zk24(lbid+1)(1:8)
    call jeveuo(zk8(ibid)//'.MAEL_RAID_REFE', 'L', lbid)
    mraid=zk24(lbid+1)(1:8)
    call mtdefs(imped, mmass, 'V', ' ')
    lismat(1)=mraid
    lismat(2)=mmass
!
    call dismoi('NOM_NUME_DDL', mraid, 'MATR_ASSE', repk=nume91)
!
    call getvr8(' ', 'SHIFT', scal=shift, nbret=ibid)
    comlin(1)=1.d0
    comlin(2)=-((shift*2.d0*3.1415927d0)**2)
    listyp(1)='R'
    listyp(2)='R'
    call mtcmbl(2, listyp, comlin, lismat, imped,&
                ' ', nume91, 'ELIM1')
    call mtdscr(imped)
    call jeveuo(imped(1:19)//'.&INT', 'E', limped)
!
    call dismoi('SOLVEUR', mraid, 'MATR_ASSE', repk=solveu)
!
    call preres(solveu, 'V', iret, '&&OP0091.MATPRE', imped,&
                ibid, -9999)
    if (iret .eq. 2) then
        call utmess('F', 'ALGELINE4_37', sk=imped)
    endif
!
    rest1='&&91'//k4bid
    call jeveuo(jexnum(rest1//'           .TACH', 1), 'L', tach1)
    call jeveuo('&&OP0091.MODE_SST1', 'E', lmod1)
    call jeveuo('&&OP0091.MODE_SST1_EFF1', 'E', leff1)
    call jeveuo('&&OP0091.MODE_SST1_EFF2', 'E', leff2)
    call jeveuo('&&OP0091.MODE_SST1_COPY', 'E', lcopy1)
    call jeveuo(lisint, 'L', lintf)
!
    call jeveuo('&&OP0091.MATRICE_MASS', 'L', lmass)
    call jeveuo('&&OP0091.MATRICE_RAID', 'L', lraid)
    call jeveuo('&&OP0091.TRAV_SST', 'E', ltrsst)
    call jeveuo('&&OP0091.PULSA_PROPRES', 'L', lomeg)
    call jeveuo('&&OP0091.MODE_INTF_DEPL', 'E', lsecme)
!
!-- BOUCLE SUR LES MODES
    do 80 j1 = 1, nbmod
        call jeveuo(zk24(tach1+j1-1)(1:19)//'.VALE', 'L', ibid)
!
!-- RECOPIE DANS UN VECTEUR DE TRAVAIL
        call lceqvn(nbeq1, zr(ibid), zr(lcopy1))
!
!-- ANNULATION DES DDL DE LAGRANGE
        call zerlag(nbeq1, zi(ideeq), vectr=zr(lcopy1))
!
!-- NOUVELLE COPIE
        call lceqvn(nbeq1, zr(lcopy1), zr(lmod1))
!
!-- ANNULATION DES COMPOSANTES ASSOCIEES AUX INTERFACES
        do 90 k1 = 1, nbint
            indin1='&&VEC_DDL_INTF_'//zk8(lintf+k1-1)
            call jeveuo(indin1, 'L', llint1)
            call jelira(indin1, 'LONMAX', nbddl1)
            do 140 l1 = 1, nbddl1
                if (zi(llint1+l1-1) .gt. 0) then
                    zr(lmod1+zi(llint1+l1-1)-1)=0
                endif
140         continue
 90     continue
!
!-- CALCUL DES TRAVAUX
        call mrmult('ZERO', zi(lmass+isst1-1), zr(lcopy1), zr(leff1), 1,&
                    .true.)
!
        travm=ddot(nbeq1,zr(lmod1),1,zr(leff1),1)
        call mrmult('ZERO', zi(lraid+isst1-1), zr(lcopy1), zr(leff2), 1,&
                    .true.)
        travk=ddot(nbeq1,zr(lmod1),1,zr(leff2),1)
        traint=travk-(zr(lomeg+j1-1)**2)*travm
        if (zr(lomeg+j1-1) .gt. 1) traint=traint/zr(lomeg+j1-1)
        write(unit,*)'MODE ',j1,' -  TRAVAIL SST =',traint
        zr(ltrsst+nbmod*(i1-1)+j1-1)=traint
!
!--
!-- CALCUL DU SECOND MEMBRE ET DES ENRICHISSEMENTS
!--
        call daxpy(nbeq1, -(zr(lomeg+j1-1)**2), zr(leff1), 1, zr(leff2),&
                   1)
        call zerlag(nbeq1, zi(ideeq), vectr=zr(leff1))
        lbid=lsecme
        call lceqvn(nbeq1, zr(leff1), zr(lsecme+nbeq1*(j1-1)))
!
!-- DIFFERENTIATION DES SECONDS MEMBRES : INTERFACE / INTERIEUR
!
        do 160 k1 = 1, nbint
            indin1='&&VEC_DDL_INTF_'//zk8(lintf+k1-1)
            call jeveuo(indin1, 'L', llint1)
            call jelira(indin1, 'LONMAX', nbddl1)
            do 170 l1 = 1, nbddl1
                ibid=zi(llint1+l1-1)
                if (ibid .gt. 0) then
                    zr(lsecme+nbeq1*(j1-1)+ibid-1)=0
                    zr(lsecme+nbeq1*(nbmod+j1-1)+ibid-1)=zr(leff1+&
                    ibid-1)
                endif
170         continue
160     continue
!
!
 80 continue
!
end subroutine
