subroutine rc32r1(nomres, lefat)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/getvtx.h"
#include "asterfort/jelira.h"
#include "asterfort/jeveuo.h"
#include "asterfort/tbajpa.h"
#include "asterfort/tbajli.h"
#include "asterfort/jexnum.h"
#include "asterfort/getvr8.h"
#include "asterc/r8vide.h"
!
    character(len=8) :: nomres
    aster_logical :: lefat
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     ------------------------------------------------------------------
!     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3200
!     STOCKAGE DES RESULTATS DANS LA TABLE DE SORTIE
!     TROIS CHOIX POSSIBLES:
!                 - DETAILS : toutes les valeurs  
!                 - SYSTUS : les valeurs max et les valeurs qui
!                           interviennent dans le facteur d'usage
!                 - VALE_MAX : que les valeurs maximales des grandeurs  
!
!     ------------------------------------------------------------------
!
    character(len=4) :: lieu(2)
    integer :: npar0, npar1, npar2, npar3, npar4, npar5, npar6, npar8, npar7
    parameter    ( npar0 = 38, npar1 = 24, npar2 = 11, npar3 = 4,&
     &             npar4 = 14, npar5 = 12, npar6 = 21, npar7 = 7, npar8 = 9)
    character(len=16) :: nopar0(npar0), nopar1(npar1), nopar2(npar2)
    character(len=16) :: nopar3(npar3), nopar4(npar4), nopar5(npar5)
    character(len=16) :: nopar6(npar6), nopar7(npar7), nopar8(npar8)
    character(len=8) :: typar0(npar0), typar1(npar1), typar2(npar2) 
    character(len=8) :: typtab
    character(len=24) :: valek(4)
    integer :: ibid, n1, nbgr, jcombi, im, jvale, ig, numgr, iocs
    complex(kind=8) :: c16b
    integer, pointer :: situ_nume_group(:) => null()
    integer, pointer :: situ_seisme(:) => null()
    integer, pointer :: situ_numero(:) => null()
    integer :: valei(7), nbsigr, jnsg, jreas, jress, is, ioc, ii, is1
    character(len=24) :: k24b, k24c, k24t, k24t2
    integer :: ioc1, is2, ioc2, jfact, jfact2, i3, n5
    real(kind=8) :: valer(16), utot, utotenv, fenint
!
!     ------------------------------------------------------------------
    data lieu   / 'ORIG' , 'EXTR' /
!
! --- TABLE AVEC TYPE_RESU='DETAILS' 
!
    data nopar0 / 'TYPE', 'SEISME', 'LIEU',&
     &              'PM_MAX', 'PB_MAX', 'PMPB_MAX', 'SN_MAX', 'SN*_MAX',&
     &              'SP_MAX', 'SALT_MAX', 'FU_CUMU','FU_EN_CUMU',&
     &              'NOM_SIT1', 'SIT1', 'N_OCC_SIT1', 'GROUPE_1', 'NOM_SIT2', 'SIT2',&
     &              'N_OCC_SIT2', 'GROUPE_2', 'PM', 'PB', 'PMPB', 'INST_SN_1',&
     &              'INST_SN_2', 'SN', 'SN*', 'KE_MECA', 'KE_THER',&
     &              'INST_SP_1', 'INST_SP_2', 'SALT',&
     &              'FU_UNIT', 'N_OCC_pris',  'FU_PARTIEL',&
     &              'FEN', 'FEN_INTEGRE', 'FU_EN_PARTIEL'/
!
    data typar0 / 'K8', 'K8', 'K8',&
     &              'R', 'R', 'R', 'R', 'R',&
     &              'R', 'R', 'R', 'R',&
     &              'K24', 'I', 'I', 'I', 'K24', 'I',&
     &              'I', 'I', 'R', 'R', 'R', 'R',&
     &              'R', 'R', 'R', 'R', 'R',&
     &              'R', 'R', 'R',&
     &              'R', 'I', 'R',&
     &              'R', 'R', 'R' /
!
! --- TABLE AVEC TYPE_RESU='SYSTUS' (FACTEUR d'USAGE)
!
    data nopar1 / 'TYPE', 'LIEU',&
     &              'NOM_SIT1', 'SIT1', 'N_OCC_SIT1', 'GROUPE_1', 'NOM_SIT2', 'SIT2',&
     &              'N_OCC_SIT2', 'GROUPE_2', 'INST_SN_1', 'INST_SN_2', 'SN', 'KE_MECA', 'KE_THER',&
     &              'INST_SP_1', 'INST_SP_2',&
     &              'SALT', 'FU_UNIT', 'N_OCC_pris',  'FU_PARTIEL',&
     &              'FEN', 'FEN_INTEGRE', 'FU_EN_PARTIEL'/
!
    data typar1 / 'K8', 'K8',&
     &              'K24', 'I', 'I', 'I', 'K24', 'I',&
     &              'I', 'I', 'R', 'R', 'R', 'R', 'R',&
     &              'R', 'R',&
     &              'R', 'R', 'I', 'R',&
     &              'R', 'R', 'R' /
!
! --- TABLE AVEC TYPE_RESU='VALE_MAX' 
!
    data nopar2 / 'TYPE', 'LIEU', 'PM_MAX', 'PB_MAX', 'PMPB_MAX', 'SN_MAX',&
     &              'SN*_MAX' , 'SP_MAX', 'SALT_MAX', 'FU_CUMU', 'FU_EN_CUMU' /
!
    data typar2 / 'K8', 'K8', 'R', 'R', 'R', 'R',&
     &              'R', 'R', 'R', 'R', 'R'  /
!
! --- TABLE AVEC TYPE_RESU='SYSTUS' dernière ligne avec le total
!
    data nopar3 / 'TYPE', 'LIEU', 'FU_PARTIEL', 'FU_EN_PARTIEL'/

! --- PARAMETRES POUR CHAQUE SITUATION
!
    data nopar4 / 'TYPE', 'SEISME', 'LIEU' , 'SIT1', 'GROUPE_1',&
     &              'PM', 'PB' , 'PMPB', 'SN', 'SN*',&
     &              'KE_MECA', 'KE_THER', 'SALT', 'FU_PARTIEL'  /
!
! --- PARAMETRES POUR LES COMBINAISONS : transitoires fictifs
!
    data nopar5 / 'TYPE', 'SEISME', 'LIEU', 'SIT1',&
     &              'GROUPE_1','SIT2', 'INST_SN_1', 'INST_SN_2',  'SN' ,&
     &              'KE_MECA', 'KE_THER', 'FU_UNIT' /
!
    data nopar8 / 'TYPE', 'SEISME', 'LIEU',&
     &              'SIT1', 'GROUPE_1', 'SIT2', 'INST_SP_1', 'INST_SP_2',&
     &              'SALT' /
!
! --- FACTEUR d'USAGE : on fait apparaitre les transitoires fictifs
!
    data nopar6 / 'TYPE',  'LIEU', 'NOM_SIT1', 'SIT1', 'N_OCC_SIT1',&
     &              'GROUPE_1', 'NOM_SIT2', 'SIT2', 'N_OCC_SIT2',&
     &              'GROUPE_2', 'INST_SN_1', 'INST_SN_2', 'SN', 'KE_MECA', 'KE_THER',&
     &              'FU_UNIT', 'N_OCC_pris',  'FU_PARTIEL',&
     &              'FEN', 'FEN_INTEGRE', 'FU_EN_PARTIEL'/
!
    data nopar7 / 'TYPE', 'LIEU',&
     &              'NOM_SIT1', 'NOM_SIT2',&
     &              'INST_SP_1', 'INST_SP_2', 'SALT'/
!
! DEB ------------------------------------------------------------------
!
    ibid=0
    c16b=(0.d0,0.d0)
    call getvtx(' ', 'TYPE_RESU', scal=typtab, nbret=n1)
    call jelira('&&RC3200.SITU_NUME_GROUP', 'LONMAX', nbgr)
    call jeveuo('&&RC3200.SITU_NUME_GROUP', 'L', vi=situ_nume_group)
    call jeveuo('&&RC3200.SITU_SEISME', 'L', vi=situ_seisme)
    call jeveuo('&&RC3200.SITU_NUMERO', 'L', vi=situ_numero)
    call jeveuo('&&RC3200.SITU_COMBINABLE', 'L', jcombi)
!
!     ------------------------------------------------------------------
!
    if (typtab .eq. 'VALE_MAX') then
        call tbajpa(nomres, npar2, nopar2, typar2)
    else if (typtab .eq. 'SYSTUS') then
        call tbajpa(nomres, npar1, nopar1, typar1)
    else
        call tbajpa(nomres, npar0, nopar0, typar0)
    endif
!
! --- LIGNES POUR LES MAXIMA
!
    valek(1) = 'MAXI'
    do 110 im = 1, 2
        valek(2) = lieu(im)
        call jeveuo('&&RC3200.RESU.'//lieu(im), 'L', jvale)
!
        if (lefat) then
            utot = zr(jvale+7)
            utotenv = zr(jvale+8)
            call getvr8('ENVIRONNEMENT', 'FEN_INTEGRE', iocc=1, scal=fenint, nbret=n5)
            if (utotenv/utot .gt. fenint) then
                zr(jvale+8)=utotenv/fenint 
            endif
        else
            zr(jvale+8)=r8vide()        
        endif
!
        if (typtab .ne. 'SYSTUS') then
            call tbajli(nomres, npar2, nopar2, [ibid], zr(jvale),[c16b], valek, 0)
        endif
110  continue
!
    if (typtab .eq. 'VALE_MAX') goto 999
!
!        --------------------------------------------------------------
!        LE RESTE POUR CHAQUE GROUPE
!        --------------------------------------------------------------
!
    do 100 ig = 1, nbgr
        numgr = abs(situ_nume_group(ig))
        if (typtab .eq. 'SYSTUS') goto 888
        iocs = situ_seisme(ig)
        call jelira(jexnum('&&RC3200.LES_GROUPES', numgr), 'LONMAX', nbsigr)
        call jeveuo(jexnum('&&RC3200.LES_GROUPES', numgr), 'L', jnsg)
!
! --- LIGNES POUR LES SITUATIONS
!
        valek(1) = 'SITU'
        valei(2)=numgr
        do 102 im = 1, 2
            valek(3) = lieu(im)
            k24b = '&&RC3200.AVEC_SEISME'//lieu(im)
            call jeveuo(jexnum(k24b, numgr), 'L', jreas)
            k24c = '&&RC3200.SANS_SEISME'//lieu(im)
            call jeveuo(jexnum(k24c, numgr), 'L', jress)
            valek(2) = 'AVEC'
            do 104 is = 1, nbsigr
                ioc = zi(jnsg+is-1)
                valei(1)=situ_numero(ioc)
                call tbajli(nomres, npar4, nopar4, valei, zr(jreas- 1+9*(is-1)+1),&
                            [c16b], valek, 0)
104         continue
            valek(2) = 'SANS'
            do 106 is = 1, nbsigr
                ioc = zi(jnsg+is-1)
                valei(1)=situ_numero(ioc)
                call tbajli(nomres, npar4, nopar4, valei, zr(jress- 1+9*(is-1)+1),&
                            [c16b], valek, 0)
106         continue
102     continue
!
! --- LIGNES POUR LES COMBINAISONS
!
        valek(1) = 'COMB'
!
        valei(2)=numgr
        do 120 im = 1, 2
            valek(3) = lieu(im)
            k24b = '&&RC3200.COMBI_A_SEI'//lieu(im)
            call jeveuo(jexnum(k24b, numgr), 'L', jreas)
            k24c = '&&RC3200.COMBI_S_SEI'//lieu(im)
            call jeveuo(jexnum(k24c, numgr), 'L', jress)
!
            ii = 0
            do 122 is1 = 1, nbsigr
                ioc1 = zi(jnsg+is1-1)
                if (.not.zl(jcombi+ioc1-1)) goto 122
                if (ioc1 .eq. iocs) goto 122
                valei(1) = situ_numero(ioc1)
                do 124 is2 = is1 + 1, nbsigr
                    ioc2 = zi(jnsg+is2-1)
                    if (.not.zl(jcombi+ioc2-1)) goto 124
                    if (ioc2 .eq. iocs) goto 124
                    valei(3)=situ_numero(ioc2)
!
                    valek(2) = 'AVEC'
                    valer(1)=zr(jreas+ii)
                    valer(2)=zr(jreas+ii+1)
                    valer(3)=zr(jreas+ii+2)
                    valer(4)=zr(jreas+ii+3)
                    valer(5)=zr(jreas+ii+4)
                    valer(6)=zr(jreas+ii+11)
                    call tbajli(nomres, npar5, nopar5, valei, valer,&
                                [c16b], valek, 0)
                    valer(1)=zr(jreas+ii+5)
                    valer(2)=zr(jreas+ii+6)
                    valer(3)=zr(jreas+ii+9)
                    call tbajli(nomres, npar8, nopar8, valei, valer,&
                                [c16b], valek, 0)
                    valer(1)=zr(jreas+ii+7)
                    valer(2)=zr(jreas+ii+8)
                    valer(3)=zr(jreas+ii+10)
                    call tbajli(nomres, npar8, nopar8, valei, valer,&
                                [c16b], valek, 0)
!
                    valek(2) = 'SANS'
                    valer(1)=zr(jress+ii)
                    valer(2)=zr(jress+ii+1)
                    valer(3)=zr(jress+ii+2)
                    valer(4)=zr(jress+ii+3)
                    valer(5)=zr(jress+ii+4)
                    valer(6)=zr(jress+ii+11)
                    call tbajli(nomres, npar5, nopar5, valei, valer,&
                                [c16b], valek, 0)
                    valer(1)=zr(jress+ii+5)
                    valer(2)=zr(jress+ii+6)
                    valer(3)=zr(jress+ii+9)
                    call tbajli(nomres, npar8, nopar8, valei, valer,&
                                [c16b], valek, 0)
                    valer(1)=zr(jress+ii+7)
                    valer(2)=zr(jress+ii+8)
                    valer(3)=zr(jress+ii+10)
                    call tbajli(nomres, npar8, nopar8, valei, valer,&
                                [c16b], valek, 0)
!
                    ii = ii + 12
124             continue
122         continue
120     continue
!
! --- LIGNES POUR LE FACTEUR D'USAGE
!
888 continue
!
        valek(1) = 'FACT'
        do 112 im = 1, 2
            k24t = '&&RC3200.FACTUSAG '//lieu(im)
            k24t2 = '&&RC3200.FACTUSAG2 '//lieu(im)
            call jeveuo(jexnum(k24t, numgr), 'L', jfact)
            call jeveuo(jexnum(k24t2, numgr), 'L', jfact2)
!
            if (lefat) then
                call jeveuo('&&RC3200.RESU.'//lieu(im), 'L', jvale)
                utot = zr(jvale+7)
                utotenv = zr(jvale+8)
                call getvr8('ENVIRONNEMENT', 'FEN_INTEGRE', iocc=1, scal=fenint, nbret=n5)
            endif
!
            do 114 is = 1, 50
                i3 = int( zr(jfact-1+23*(is-1)+1) )
                if (i3 .eq. 0) goto 116
!
                valek(2) = lieu(im)
                valek(3)= zk24(jfact2-1+2*(is-1)+1)
                valek(4)= zk24(jfact2-1+2*(is-1)+2)
!
                valei(1) = int( zr(jfact-1+23*(is-1)+2) )
                valei(2)= int(zr(jfact-1+23*(is-1)+3))
                valei(3) = int( zr(jfact-1+23*(is-1)+4) )
                valei(4)= int(zr(jfact-1+23*(is-1)+5))
                valei(5)= int(zr(jfact-1+23*(is-1)+6))
                valei(6)= int(zr(jfact-1+23*(is-1)+7))
                valei(7)= int(zr(jfact-1+23*(is-1)+8))
             
!
                valer(1) = zr(jfact-1+23*(is-1)+9)
                valer(2) = zr(jfact-1+23*(is-1)+10)
                valer(3) = zr(jfact-1+23*(is-1)+11)
                valer(4) = zr(jfact-1+23*(is-1)+12)
                valer(5) = zr(jfact-1+23*(is-1)+13)
                valer(6) = zr(jfact-1+23*(is-1)+20)
                valer(7) = zr(jfact-1+23*(is-1)+21)
!
                if (lefat) then
                    valer(8) = zr(jfact-1+23*(is-1)+22)
                    valer(9) = fenint
                    if (utotenv/utot .gt. fenint) then
                        valer(10) = zr(jfact-1+23*(is-1)+23)/fenint
                    else
                        valer(10) = zr(jfact-1+23*(is-1)+23)
                    endif
!
                else
                    valer(8) = r8vide()
                    valer(9) = r8vide()
                    valer(10) = r8vide()
                endif
                call tbajli(nomres, npar6, nopar6, valei, valer,&
                            [c16b], valek, 0)
! 
                valek(2)=lieu(im)
!
                valek(3)= 'FICTIF_1'
                valek(4)= 'FICTIF_1'
                valer(1) = zr(jfact-1+23*(is-1)+14)
                valer(2) = zr(jfact-1+23*(is-1)+15)
                valer(3) = zr(jfact-1+23*(is-1)+18)
                call tbajli(nomres, npar7, nopar7, [ibid], valer,&
                            [c16b], valek, 0)
!
                valek(3)= 'FICTIF_2'
                valek(4)= 'FICTIF_2'
                valer(1) = zr(jfact-1+23*(is-1)+16)
                valer(2) = zr(jfact-1+23*(is-1)+17)
                valer(3) = zr(jfact-1+23*(is-1)+19)
                call tbajli(nomres, npar7, nopar7, [ibid], valer,&
                            [c16b], valek, 0)
!
114         continue
116         continue
112     continue
100 continue
!
    if (typtab .eq. 'SYSTUS') then
        valek(1) = 'TOTAL'
        do 132 im = 1, 2
            valek(2)=lieu(im)
            call jeveuo('&&RC3200.RESU.'//lieu(im), 'L', jvale)
            utot = zr(jvale+7)
            utotenv = zr(jvale+8)
            valer(1) = utot
            valer(2) = r8vide()
            if (lefat) then
                call getvr8('ENVIRONNEMENT', 'FEN_INTEGRE', iocc=1, scal=fenint, nbret=n5)
                if (utotenv/utot .gt. fenint) then
                    valer(2) = utotenv/fenint
                else
                    valer(2) = utotenv
                endif
            endif
            call tbajli(nomres, npar3, nopar3, [ibid], valer,[c16b], valek, 0)
132     continue
    endif
!
999 continue
!
end subroutine
