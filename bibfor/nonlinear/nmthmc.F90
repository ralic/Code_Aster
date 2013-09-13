subroutine nmthmc(comp, modelz, moclef, k, comel,&
                  ncomel, nbnvi)
!
! ======================================================================
! ======================================================================
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: sylvie.granet at edf.fr
! =====================================================================
! --- BUT : DETERMINER LA COHERENCE DE LA RELATION DE COUPLAGE THM ----
! =====================================================================
    implicit none
#include "jeveux.h"
#include "asterc/lccree.h"
#include "asterc/lcinfo.h"
#include "asterfort/dismoi.h"
#include "asterfort/jelira.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/reliem.h"
#include "asterfort/utmess.h"
!
    integer :: ncomel, k
    integer :: nbnvi(4)
    character(len=16) :: comp, moclef, comel(*)
    character(len=*) :: modelz
! ----------------------------------------------------------------------
!
!
! =====================================================================
! --- DEFINITION DES DIMENSIONS DES VECTEURS DE POSSIBILITE DES LOIS --
! =====================================================================
    logical :: lthmc, lhydr, lmeca, tout
    integer :: dmthmc, dmhydr, dmmeca, jmail, itypel
    integer :: nbma, ierd, ibid, jnoma, jmesm, numlc
    parameter   ( dmthmc = 8  )
    parameter   ( dmhydr = 5  )
    parameter   ( dmmeca = 18 )
    character(len=16) :: pothmc(dmthmc), modeli, nomte
    character(len=16) :: pohydr(dmhydr), pomeca(dmmeca)
    character(len=16) :: comcod
    character(len=16) :: thmc, ther, hydr, meca, mocles(2)
    character(len=8) :: noma, typmcl(2), modele
    character(len=24) :: mesmai
    character(len=24) :: valk(2)
!
    integer :: jj, ii, im, ima
! *********************************************************************
! --- DEBUT INITIALISATION ------------------------------------------ *
! *********************************************************************
    thmc = ' '
    ther = ' '
    hydr = ' '
    meca = ' '
    modele=modelz
! =====================================================================
! --- PARTIE THMC -----------------------------------------------------
! =====================================================================
    data pothmc / 'LIQU_SATU'     ,&
     &              'LIQU_GAZ'      ,&
     &              'GAZ'           ,&
     &              'LIQU_GAZ_ATM'  ,&
     &              'LIQU_VAPE_GAZ' ,&
     &              'LIQU_VAPE'     ,&
     &              'LIQU_AD_GAZ_VAPE',&
     &              'LIQU_AD_GAZ' /
! =====================================================================
! --- PARTIE HYDR -----------------------------------------------------
! =====================================================================
    data pohydr / 'HYDR'      ,&
     &              'HYDR_UTIL' ,&
     &              'HYDR_VGM' ,&
     &              'HYDR_VGC' ,&
     &              'HYDR_ENDO' /
! =====================================================================
! --- PARTIE MECA -----------------------------------------------------
! =====================================================================
    data pomeca / 'ELAS'            ,&
     &              'CJS'             ,&
     &              'HUJEUX'          ,&
     &              'CAM_CLAY'        ,&
     &              'BARCELONE'       ,&
     &              'LAIGLE'          ,&
     &              'LETK'            ,&
     &              'VISC_DRUC_PRAG'  ,&
     &              'HOEK_BROWN_EFF'  ,&
     &              'HOEK_BROWN_TOT'  ,&
     &              'MAZARS'          ,&
     &              'ENDO_ISOT_BETON' ,&
     &              'ELAS_GONF'       ,&
     &              'DRUCK_PRAGER'    ,&
     &              'DRUCK_PRAG_N_A'  ,&
     &              'JOINT_BANDIS'    ,&
     &              'CZM_LIN_REG'     ,&
     &              'CZM_EXP_REG'    /
! *********************************************************************
! --- FIN INITIALISATION -------------------------------------------- *
! *********************************************************************
    call jeveuo(modele//'.MAILLE', 'L', jmail)
    call jeveuo(modele//'.MODELE    .LGRF', 'L', jnoma)
    noma = zk8(jnoma)
! =====================================================================
! --- LE COMPORTEMENT DEFINI EST-IL COHERENT ? ------------------------
! =====================================================================
    lthmc = .false.
    lhydr = .false.
    lmeca = .false.
    tout = .false.
    mocles(1) = 'GROUP_MA'
    mocles(2) = 'MAILLE'
    typmcl(1) = 'GROUP_MA'
    typmcl(2) = 'MAILLE'
    mesmai = '&&NMTHMC.MES_MAILLES'
!
    call reliem(modele, noma, 'NU_MAILLE', moclef, k,&
                2, mocles, typmcl, mesmai, nbma)
    if (nbma .eq. 0) then
        call jelira(modele//'.MAILLE', 'LONUTI', ival=nbma)
        tout=.true.
    else
        call jeveuo(mesmai, 'L', jmesm)
    endif
!
    do 1 im = 1, nbma
! =====================================================================
! --- COHERENCE DE LA LOI DE COUPLAGE ---------------------------------
! =====================================================================
        if (tout) then
            ima = im
        else
            ima = zi(jmesm+im-1)
        endif
        itypel = zi(jmail-1+ima)
        if (itypel .ne. 0) then
            call jenuno(jexnum('&CATA.TE.NOMTE', itypel), nomte)
            call dismoi('F', 'MODELISATION', nomte, 'TYPE_ELEM', ibid,&
                        modeli, ierd)
            do 5 jj = 1, ncomel
                if ((comel(jj)(1:3).eq.'GAZ') .or. (comel(jj)(1:9) .eq.'LIQU_SATU') .or.&
                    (comel(jj)(1:12) .eq.'LIQU_GAZ_ATM')) then
!
                    if ((modeli(1:6).ne.'3D_THM') .and. (modeli(1:5) .ne.'3D_HM') .and.&
                        (modeli(1:5).ne.'3D_HS') .and. (modeli(1:8).ne.'AXIS_THM') .and.&
                        (modeli(1:7) .ne.'AXIS_HM') .and. (modeli(1:10).ne.'D_PLAN_THM')&
                        .and. (modeli(1:9).ne.'D_PLAN_HS') .and.&
                        (modeli(1: 9).ne.'D_PLAN_HM') .and. (modeli(1:8) .ne.'PLAN_JHM')&
                        .and. (modeli(1:8).ne.'AXIS_JHM') .and. (modeli.ne.'#PLUSIEURS')) then
!
                        valk(1) = comel(jj)
                        valk(2) = modeli
                        call utmess('F', 'ALGORITH8_35', nk=2, valk=valk)
                    endif
!
                    elseif ((comel(jj)(1:13).eq.'LIQU_VAPE_GAZ').or.&
                (comel(jj)(1:8).eq.'LIQU_GAZ')) then
!
                    if ((modeli(1:6).ne.'3D_THH') .and. (modeli(1:6) .ne.'3D_HHM') .and.&
                        (modeli(1:5).ne.'3D_HH') .and. (modeli(1:8).ne.'AXIS_THH') .and.&
                        (modeli(1:8) .ne.'AXIS_HHM') .and. (modeli(1:7).ne.'AXIS_HH') .and.&
                        (modeli(1:10).ne.'D_PLAN_THH') .and.&
                        (modeli( 1:10).ne.'D_PLAN_HHM') .and. (modeli(1:9) .ne.'D_PLAN_HH')&
                        .and. (modeli.ne.'#PLUSIEURS')) then
!
                        valk(1) = comel(jj)
                        valk(2) = modeli
                        call utmess('F', 'ALGORITH8_35', nk=2, valk=valk)
                    endif
                else if (comel(jj)(1:9).eq.'LIQU_VAPE') then
!
                    if ((modeli(1:6).ne.'3D_THV') .and. (modeli(1:8) .ne.'AXIS_THV') .and.&
                        (modeli(1:10) .ne.'D_PLAN_THV') .and. (modeli.ne.'#PLUSIEURS')) then
!
                        valk(1) = comel(jj)
                        valk(2) = modeli
                        call utmess('F', 'ALGORITH8_35', nk=2, valk=valk)
                    endif
                else if (comel(jj)(1:16).eq.'LIQU_AD_GAZ_VAPE') then
!
                    if ((modeli(1:9).ne.'AXIS_HH2M') .and. (modeli(1:9) .ne.'AXIS_THH2')&
                        .and. (modeli(1:8).ne.'AXIS_HH2') .and.&
                        (modeli(1:11).ne.'D_PLAN_HH2M') .and.&
                        (modeli(1:11).ne.'D_PLAN_THH2') .and.&
                        (modeli(1:11) .ne.'D_PLAN_THH2') .and.&
                        (modeli(1:10) .ne.'D_PLAN_HH2') .and. (modeli(1:7).ne.'3D_HH2M')&
                        .and. (modeli(1:7).ne.'3D_THH2') .and. (modeli(1:6) .ne.'3D_HH2')&
                        .and. (modeli.ne.'#PLUSIEURS')) then
                        valk(1) = comel(jj)
                        valk(2) = modeli
                        call utmess('F', 'ALGORITH8_35', nk=2, valk=valk)
                    endif
                endif
 5          continue
        endif
 1  end do
    do 10 jj = 1, ncomel
! =====================================================================
! --- DEFINITION DE LA LOI DE COUPLAGE --------------------------------
! =====================================================================
        do 20 ii = 1, dmthmc
            if (comel(jj) .eq. pothmc(ii)) then
                thmc = comel(jj)
                if (lthmc) then
                    call utmess('F', 'ALGORITH8_36')
                endif
                lthmc = .true.
                goto 10
            endif
20      continue
! =====================================================================
! --- DEFINITION DE LA LOI HYDRAULIQUE --------------------------------
! =====================================================================
        do 40 ii = 1, dmhydr
            if (comel(jj) .eq. pohydr(ii)) then
                hydr = comel(jj)
                if (lhydr) then
                    call utmess('F', 'ALGORITH8_37')
                endif
                lhydr = .true.
                goto 10
            endif
40      continue
! =====================================================================
! --- DEFINITION DE LA LOI MECANIQUE ----------------------------------
! =====================================================================
        do 50 ii = 1, dmmeca
            if (comel(jj) .eq. pomeca(ii)) then
                meca = comel(jj)
                if (lmeca) then
                    call utmess('F', 'ALGORITH8_38')
                endif
                lmeca = .true.
                goto 10
            endif
50      continue
10  end do
! =====================================================================
! --- VERIFICATION DE LA COHERENCE AVEC LA RELATION DEMANDEE ----------
! =====================================================================
    if (.not.lthmc) then
        call utmess('F', 'ALGORITH8_39')
    endif
    if (.not.lhydr) then
        call utmess('F', 'ALGORITH8_40')
    endif
! =====================================================================
! --- PARTIE KIT_HM ---------------------------------------------------
! =====================================================================
    if (comp .eq. 'KIT_HM') then
        if (.not.lmeca) then
            call utmess('F', 'ALGORITH8_41')
        endif
        if (thmc .ne. 'LIQU_SATU' .and. thmc .ne. 'GAZ' .and. thmc .ne. 'LIQU_GAZ_ATM') then
            valk(1) = 'HM'
            call utmess('F', 'ALGORITH8_42', sk=valk(1))
        endif
        if (hydr .eq. 'HYDR_ENDO' .and.&
            ( meca.ne.'MAZARS' .and. meca.ne.'ENDO_ISOT_BETON' )) then
            call utmess('F', 'ALGORITH8_43')
        endif
        if (meca .eq. 'BARCELONE') then
            valk(1) = 'HM'
            call utmess('F', 'ALGORITH8_44', sk=valk(1))
        endif
! =====================================================================
! --- PARTIE KIT_HHM --------------------------------------------------
! =====================================================================
    else if (comp.eq.'KIT_HHM') then
        if (.not.lmeca) then
            call utmess('F', 'ALGORITH8_41')
        endif
        if (thmc .ne. 'LIQU_GAZ' .and. thmc .ne. 'LIQU_VAPE_GAZ' .and. thmc .ne.&
            'LIQU_AD_GAZ_VAPE' .and. thmc .ne. 'LIQU_AD_GAZ') then
            valk(1) = 'HHM'
            call utmess('F', 'ALGORITH8_42', sk=valk(1))
        endif
        if (hydr .eq. 'HYDR_ENDO' .and.&
            ( meca.ne.'MAZARS' .and. meca.ne.'ENDO_ISOT_BETON' )) then
            call utmess('F', 'ALGORITH8_43')
        endif
        if (meca .eq. 'BARCELONE' .and. (thmc.ne.'LIQU_GAZ' .and. thmc.ne.'LIQU_VAPE_GAZ')) then
            valk(1) = 'HHM'
            call utmess('F', 'ALGORITH8_44', sk=valk(1))
        endif
! =====================================================================
! --- PARTIE KIT_H ----------------------------------------------------
! =====================================================================
    else if (comp.eq.'KIT_H') then
        if (lmeca) then
            valk(1) = 'H'
            call utmess('F', 'ALGORITH8_46', sk=valk(1))
        endif
        if (thmc .ne. 'LIQU_SATU' .and. thmc .ne. 'GAZ') then
            call utmess('F', 'ALGORITH8_59')
        endif
! =====================================================================
! --- PARTIE KIT_THH --------------------------------------------------
! =====================================================================
    else if (comp.eq.'KIT_THH') then
        ther = 'THER'
        if (lmeca) then
            valk(1) = 'THH'
            call utmess('F', 'ALGORITH8_46', sk=valk(1))
        endif
        if (thmc .ne. 'LIQU_GAZ' .and. thmc .ne. 'LIQU_VAPE_GAZ' .and. thmc .ne.&
            'LIQU_AD_GAZ_VAPE' .and. thmc .ne. 'LIQU_AD_GAZ') then
            valk(1) = 'THH'
            call utmess('F', 'ALGORITH8_42', sk=valk(1))
        endif
! =====================================================================
! --- PARTIE KIT_HH --------------------------------------------------
! =====================================================================
    else if (comp.eq.'KIT_HH') then
        if (lmeca) then
            valk(1) = 'HH'
            call utmess('F', 'ALGORITH8_46', sk=valk(1))
        endif
        if (thmc .ne. 'LIQU_GAZ' .and. thmc .ne. 'LIQU_VAPE_GAZ' .and. thmc .ne.&
            'LIQU_AD_GAZ_VAPE' .and. thmc .ne. 'LIQU_AD_GAZ') then
            valk(1) = 'HH'
            call utmess('F', 'ALGORITH8_42', sk=valk(1))
        endif
! =====================================================================
! --- PARTIE KIT_THV --------------------------------------------------
! =====================================================================
    else if (comp.eq.'KIT_THV') then
        ther = 'THER'
        if (lmeca) then
            valk(1) = 'THV'
            call utmess('F', 'ALGORITH8_46', sk=valk(1))
        endif
        if (thmc .ne. 'LIQU_VAPE') then
            valk(1) = 'THV'
            call utmess('F', 'ALGORITH8_42', sk=valk(1))
        endif
! =====================================================================
! --- PARTIE KIT_THM --------------------------------------------------
! =====================================================================
    else if (comp.eq.'KIT_THM') then
        ther = 'THER'
        if (.not.lmeca) then
            call utmess('F', 'ALGORITH8_41')
        endif
        if (thmc .ne. 'LIQU_SATU' .and. thmc .ne. 'LIQU_GAZ_ATM' .and. thmc .ne. 'GAZ') then
            valk(1) = 'THM'
            call utmess('F', 'ALGORITH8_42', sk=valk(1))
        endif
        if (hydr .eq. 'HYDR_ENDO' .and.&
            ( meca.ne.'MAZARS' .and. meca.ne.'ENDO_ISOT_BETON' )) then
            call utmess('F', 'ALGORITH8_43')
        endif
        if (meca .eq. 'BARCELONE') then
            valk(1) = 'THM'
            call utmess('F', 'ALGORITH8_44', sk=valk(1))
        endif
! =====================================================================
! --- PARTIE KIT_THHM -------------------------------------------------
! =====================================================================
    else if (comp.eq.'KIT_THHM') then
        ther = 'THER'
        if (.not.lmeca) then
            call utmess('F', 'ALGORITH8_41')
        endif
        if (thmc .ne. 'LIQU_VAPE_GAZ' .and. thmc .ne. 'LIQU_AD_GAZ_VAPE' .and. thmc .ne.&
            'LIQU_AD_GAZ' .and. thmc .ne. 'LIQU_GAZ') then
            valk(1) = 'THHM'
            call utmess('F', 'ALGORITH8_42', sk=valk(1))
        endif
        if (hydr .eq. 'HYDR_ENDO' .and.&
            ( meca.ne.'MAZARS' .and. meca.ne.'ENDO_ISOT_BETON' )) then
            call utmess('F', 'ALGORITH8_43')
        endif
        if (meca .eq. 'BARCELONE' .and. (thmc.ne.'LIQU_GAZ' .and. thmc.ne.'LIQU_VAPE_GAZ')) then
            valk(1) = 'THHM'
            call utmess('F', 'ALGORITH8_42', sk=valk(1))
        endif
    endif
! =========================================================
! MISE A JOUR DES RELATIONS DE COMPORTEMENTS --------------
! =========================================================
    comel(1) = thmc
    comel(2) = ther
    comel(3) = hydr
    comel(4) = meca
!  RECUPERATION DES NOMBRES DE VARIABLES INTERNES
!
! ======================================================================
! --- POUR CHAQUE RELATION DE COMPORTEMENT PRESENTE ON RECUPERE --------
! --- LE NOMBRE DE VARIABLES INTERNES ASSOCIE A CETTE LOI --------------
! ======================================================================
! --- LOI DE COUPLAGE --------------------------------------------------
! ======================================================================
    if (comel(1) .ne. ' ') then
        call lccree(1, comel(1), comcod)
        call lcinfo(comcod, numlc, nbnvi(1))
    else
        nbnvi(1)=0
    endif
! ======================================================================
! --- LOI DE THERMIQUE -------------------------------------------------
! ======================================================================
    if (comel(2) .ne. ' ') then
        call lccree(1, comel(2), comcod)
        call lcinfo(comcod, numlc, nbnvi(2))
    else
        nbnvi(2)=0
    endif
! ======================================================================
! --- LOI HYDRAULIQUE --------------------------------------------------
! ======================================================================
    if (comel(3) .ne. ' ') then
        call lccree(1, comel(3), comcod)
        call lcinfo(comcod, numlc, nbnvi(3))
    else
        nbnvi(3)=0
    endif
! ======================================================================
! --- LOI DE MECANIQUE -------------------------------------------------
! ======================================================================
    if (comel(4) .ne. ' ') then
        call lccree(1, comel(4), comcod)
        call lcinfo(comcod, numlc, nbnvi(4))
    else
        nbnvi(4)=0
    endif
! =====================================================================
end subroutine
