subroutine rc32r1(nomres)
    implicit   none
#include "jeveux.h"
!
#include "asterc/getvtx.h"
#include "asterfort/codent.h"
#include "asterfort/jelira.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/tbajli.h"
#include "asterfort/tbajpa.h"
    character(len=8) :: nomres
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3200
!     STOCKAGE DES RESULTATS DANS LA TABLE DE SORTIE
!
!     ------------------------------------------------------------------
!
    integer :: ibid, npar2, npar1, npar4, npar6, im, ig, is, i3, nbsigr
    integer :: valei(3), jnumgr, jnsitu, jnsg, jvale, jpmpb, nbgr, ioc, numgr
    integer :: is1, is2, jreas, jress, n1, jseigr, jcombi, ioc1, ioc2, iocs, ii
    integer :: npar0
    parameter    ( npar0 = 35, npar2 = 7, npar1 = 11, npar4 = 15,&
     &               npar6 = 13 )
    real(kind=8) :: utot, valer(2)
    complex(kind=8) :: c16b
    character(len=4) :: lieu(2)
    character(len=8) :: k8b, valek(4), typar0(npar0), typar6(npar6), typtab
    character(len=16) :: nopar2(npar2), nopar1(npar1), nopar4(npar4)
    character(len=16) :: nopar6(npar6), nopar0(npar0)
    character(len=24) :: k24b, k24c, k24t
    integer :: iarg
!     ------------------------------------------------------------------
    data lieu   / 'ORIG' , 'EXTR' /
!
    data nopar0 / 'TYPE', 'SEISME', 'NUME_GROUPE', 'LIEU' ,&
     &              'PM_MAX', 'PB_MAX', 'PMPB_MAX', 'SM' ,&
     &              'SN/3SM' , 'SN_MAX' , 'SN*_MAX' , 'SP_MAX',&
     &              'KE_MAX', 'SALT_MAX', 'FACT_USAGE_CUMU',&
     &              'NUME_SITU', 'NUME_SITU_I', 'NUME_SITU_J',&
     &              'PM' , 'PB' , 'PMPB', 'SN', 'SN*', 'SP', 'KE_MECA',&
     &              'KE_THER', 'SALT', 'NUME_SITU_K', 'NUME_SITU_L',&
     &              'FACT_USAGE',  '%_FACT_USAGE' ,&
     &              'SP1_IJ', 'SP2_IJ', 'SALT1_IJ', 'SALT2_IJ' /
    data typar0 / 'K8', 'K8', 'I', 'K8',  'R', 'R', 'R', 'R', 'R',&
     &              'R', 'R', 'R', 'R', 'R', 'R', 'I', 'I', 'I', 'R',&
     &              'R', 'R', 'R', 'R', 'R', 'R', 'R', 'R', 'K8', 'K8',&
     &              'R', 'R', 'R', 'R', 'R', 'R'  /
!
! --- PARAMETRES FACTEUR D'USAGE
!
    data nopar2 / 'TYPE', 'NUME_GROUPE', 'LIEU', 'NUME_SITU_K',&
     &              'NUME_SITU_L', 'FACT_USAGE' , '%_FACT_USAGE' /
!
! --- PARAMETRES POUR LE CALCUL DU FACTEUR D'USAGE
!
    data nopar1 / 'TYPE', 'SEISME', 'NUME_GROUPE', 'LIEU',&
     &              'NUME_SITU_I', 'NUME_SITU_J', 'SN' ,&
     &              'SP1_IJ', 'SP2_IJ', 'SALT1_IJ', 'SALT2_IJ' /
!
! --- PARAMETRES POUR CHAQUE SITUATION
!
    data nopar4 / 'TYPE', 'SEISME', 'NUME_GROUPE', 'LIEU' ,&
     &              'NUME_SITU', 'PM' , 'PB' , 'PMPB', 'SN', 'SN*',&
     &              'SP', 'KE_MECA', 'KE_THER', 'SALT', 'FACT_USAGE'  /
!
! --- PARAMETRES POUR LES MAXIMA
!
    data nopar6 / 'TYPE', 'LIEU', 'PM_MAX', 'PB_MAX', 'PMPB_MAX',&
     &              'SM' , 'SN/3SM' , 'SN_MAX' , 'SN*_MAX' , 'SP_MAX',&
     &              'KE_MAX', 'SALT_MAX', 'FACT_USAGE_CUMU' /
!
    data typar6 / 'K8', 'K8', 'R', 'R', 'R', 'R', 'R', 'R',&
     &                          'R', 'R', 'R', 'R', 'R'  /
! DEB ------------------------------------------------------------------
!
    call getvtx(' ', 'TYPE_RESU', 1, iarg, 1,&
                typtab, n1)
!
    call jelira('&&RC3200.SITU_NUME_GROUP', 'LONMAX', nbgr)
    call jeveuo('&&RC3200.SITU_NUME_GROUP', 'L', jnumgr)
    call jeveuo('&&RC3200.SITU_SEISME', 'L', jseigr)
!
    call jeveuo('&&RC3200.SITU_NUMERO', 'L', jnsitu)
    call jeveuo('&&RC3200.SITU_COMBINABLE', 'L', jcombi)
!
!     ------------------------------------------------------------------
!
    if (typtab .eq. 'VALE_MAX') then
        call tbajpa(nomres, npar6, nopar6, typar6)
    else
        call tbajpa(nomres, npar0, nopar0, typar0)
    endif
!
!     -----------------------------------------------------------------
!     LES MAXIMUM DES QUANTITES
!     -----------------------------------------------------------------
!
    valek(1) = 'MAXI'
    do 110 im = 1, 2
!
        valek(2) = lieu(im)
!
        call jeveuo('&&RC3200.RESULTAT  .'//lieu(im), 'L', jvale)
!
        call tbajli(nomres, npar6, nopar6, ibid, zr(jvale),&
                    c16b, valek, 0)
!
110  end do
!
    if (typtab .eq. 'VALE_MAX') goto 9999
!
!     -----------------------------------------------------------------
!
    do 100 ig = 1, nbgr
        numgr = abs(zi(jnumgr+ig-1))
        iocs = zi(jseigr+ig-1)
        valei(1) = numgr
        call jelira(jexnum('&&RC3200.LES_GROUPES', numgr), 'LONMAX', nbsigr)
        call jeveuo(jexnum('&&RC3200.LES_GROUPES', numgr), 'L', jnsg)
!
!        --------------------------------------------------------------
!        QUANTITE POUR CHAQUE SITUATION
!        --------------------------------------------------------------
        valek(1) = 'SITU'
        do 102 im = 1, 2
            valek(3) = lieu(im)
            k24b = '&&RC3200.AVEC_SEISME'//lieu(im)
            call jeveuo(jexnum(k24b, numgr), 'L', jreas)
            k24c = '&&RC3200.SANS_SEISME'//lieu(im)
            call jeveuo(jexnum(k24c, numgr), 'L', jress)
            valek(2) = 'AVEC'
            do 104 is = 1, nbsigr
                ioc = zi(jnsg+is-1)
                valei(2) = zi(jnsitu+ioc-1)
                call tbajli(nomres, npar4, nopar4, valei, zr(jreas- 1+10*(is-1)+1),&
                            c16b, valek, 0)
104          continue
            valek(2) = 'SANS'
            do 106 is = 1, nbsigr
                ioc = zi(jnsg+is-1)
                valei(2) = zi(jnsitu+ioc-1)
                call tbajli(nomres, npar4, nopar4, valei, zr(jress- 1+10*(is-1)+1),&
                            c16b, valek, 0)
106          continue
102      continue
!
!        --------------------------------------------------------------
!        QUANTITE POUR CHAQUE COMBINAISON
!        --------------------------------------------------------------
!
        valek(1) = 'COMB'
        do 120 im = 1, 2
!
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
                valei(2) = zi(jnsitu+ioc1-1)
                do 124 is2 = is1 + 1, nbsigr
                    ioc2 = zi(jnsg+is2-1)
                    if (.not.zl(jcombi+ioc2-1)) goto 124
                    if (ioc2 .eq. iocs) goto 124
                    valei(3) = zi(jnsitu+ioc2-1)
!
                    valek(2) = 'AVEC'
                    call tbajli(nomres, npar1, nopar1, valei, zr(jreas+ii),&
                                c16b, valek, 0)
                    valek(2) = 'SANS'
                    call tbajli(nomres, npar1, nopar1, valei, zr(jress+ii),&
                                c16b, valek, 0)
!
                    ii = ii + 5
124              continue
!
122          continue
!
120      continue
!
!        --------------------------------------------------------------
!        FACTEUR D'USAGE
!        --------------------------------------------------------------
!
        valek(1) = 'FACT'
        do 112 im = 1, 2
            valek(2) = lieu(im)
!
            call jeveuo('&&RC3200.RESULTAT  .'//lieu(im), 'L', jvale)
            utot = zr(jvale+7)
!
            k24t = '&&RC3200.FACT_USAGE '//lieu(im)
            call jeveuo(jexnum(k24t, numgr), 'L', jpmpb)
            do 114 is = 1, 50
                i3 = int( zr(jpmpb-1+4*(is-1)+1) )
                if (i3 .eq. 0) goto 116
                is1 = int( zr(jpmpb-1+4*(is-1)+2) )
                is2 = int( zr(jpmpb-1+4*(is-1)+3) )
                valer(1) = zr(jpmpb-1+4*(is-1)+4)
                if (utot .eq. 0.d0) then
                    valer(2) = 0.d0
                else
                    valer(2) = 100.d0 * valer(1) / utot
                endif
                call codent(is1, 'D', k8b)
                valek(3) = k8b
                call codent(is2, 'D', k8b)
                valek(4) = k8b
                call tbajli(nomres, npar2, nopar2, valei, valer,&
                            c16b, valek, 0)
!
114          continue
116          continue
112      continue
100  continue
!
9999  continue
!
end subroutine
