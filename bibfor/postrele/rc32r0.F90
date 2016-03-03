subroutine rc32r0(nomres, pmpb, sn, snet)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8vide.h"
#include "asterfort/getvtx.h"
#include "asterfort/jelira.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/tbajli.h"
#include "asterfort/tbajpa.h"
!
    character(len=8) :: nomres
    aster_logical :: pmpb, sn, snet
!     ------------------------------------------------------------------
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ------------------------------------------------------------------
!     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3200
!     STOCKAGE DES RESULTATS DANS LA TABLE DE SORTIE
!
!     ------------------------------------------------------------------
!
    integer :: npar0, npar1, npar2, im, ig, is, nbsigr, valei(2)
    integer :: jnsg, jpmpba, jpmpbs, nbgr, ioc, numgr, jvale, ibid, n1
    parameter    ( npar0 = 15 , npar1 = 7 , npar2 = 10 )
    real(kind=8) :: valer(5)
    complex(kind=8) :: c16b
    character(len=4) :: lieu(2)
    character(len=8) :: valek(3), typar0(npar0), typar1(npar1), typtab
    character(len=16) :: nopar0(npar0), nopar1(npar1), nopar2(npar2)
    character(len=24) :: k24a, k24s
    integer, pointer :: situ_numero(:) => null()
    integer, pointer :: situ_nume_group(:) => null()
!     ------------------------------------------------------------------
    data lieu   / 'ORIG' , 'EXTR' /
!
    data nopar0 / 'TYPE','SEISME', 'NUME_GROUPE', 'LIEU', 'NUME_SITU',&
     &              'PM', 'PB', 'PMPB', 'SN', 'SN*', 'PM_MAX',&
     &              'PB_MAX', 'PMPB_MAX', 'SN_MAX' , 'SN*_MAX' /
    data typar0 / 'K8', 'K8', 'I', 'K8', 'I', 'R', 'R', 'R', 'R' ,&
     &                               'R', 'R', 'R', 'R', 'R' , 'R'  /
!
    data nopar1 / 'TYPE', 'LIEU', 'PM_MAX', 'PB_MAX', 'PMPB_MAX',&
     &                              'SN_MAX', 'SN*_MAX' /
    data typar1 / 'K8', 'K8', 'R', 'R', 'R', 'R' , 'R'  /
!
    data nopar2 / 'TYPE', 'SEISME', 'NUME_GROUPE', 'LIEU',&
     &              'NUME_SITU', 'PM', 'PB', 'PMPB', 'SN', 'SN*'  /
! DEB ------------------------------------------------------------------
!
    ibid=0
    c16b=(0.d0,0.d0)
    call getvtx(' ', 'TYPE_RESU', scal=typtab, nbret=n1)
!
    call jelira('&&RC3200.SITU_NUME_GROUP', 'LONMAX', nbgr)
    call jeveuo('&&RC3200.SITU_NUME_GROUP', 'L', vi=situ_nume_group)
!
    call jeveuo('&&RC3200.SITU_NUMERO', 'L', vi=situ_numero)
!
!     -----------------------------------------------------------------
!
    if (typtab .eq. 'VALE_MAX') then
        call tbajpa(nomres, npar1, nopar1, typar1)
    else
        call tbajpa(nomres, npar0, nopar0, typar0)
    endif
!
!     -----------------------------------------------------------------
!
! --- STOCKAGE DES MAXIMA DANS LA TABLE
!
    valek(1) = 'MAXI'
    do 130 im = 1, 2
!
        valek(2) = lieu(im)
!
        call jeveuo('&&RC3200.RESU.'//lieu(im), 'L', jvale)
!
        if (pmpb) then
            valer(1) = zr(jvale)
            valer(2) = zr(jvale+1)
            valer(3) = zr(jvale+2)
        else
            valer(1) = r8vide()
            valer(2) = r8vide()
            valer(3) = r8vide()
        endif
        if (sn) then
            valer(4) = zr(jvale+5)
        else
            valer(4) = r8vide()
        endif
        if (snet) then
            valer(5) = zr(jvale+6)
        else
            valer(5) = r8vide()
        endif
!
        call tbajli(nomres, npar1, nopar1, [ibid], valer,&
                    [c16b], valek, 0)
130 end do
!
    if (typtab .eq. 'VALE_MAX') goto 9999
!
!     -----------------------------------------------------------------
!
! --- STOCKAGE DES GRANDEURS PAR SITUATION
!
    valek(1) = 'SITU'
    do 200 ig = 1, nbgr
        numgr = abs(situ_nume_group(ig))
        valei(1) = numgr
        call jelira(jexnum('&&RC3200.LES_GROUPES', numgr), 'LONMAX', nbsigr)
        call jeveuo(jexnum('&&RC3200.LES_GROUPES', numgr), 'L', jnsg)
!
        do 202 im = 1, 2
            valek(3) = lieu(im)
            k24a = '&&RC3200.AVEC_SEISME'//lieu(im)
            call jeveuo(jexnum(k24a, numgr), 'L', jpmpba)
            k24s = '&&RC3200.SANS_SEISME'//lieu(im)
            call jeveuo(jexnum(k24s, numgr), 'L', jpmpbs)
!
            valek(2) = 'AVEC'
            do 204 is = 1, nbsigr
                ioc = zi(jnsg+is-1)
                valei(2) = situ_numero(ioc)
!
                call tbajli(nomres, npar2, nopar2, valei, zr(jpmpba- 1+10*(is-1)+1),&
                            [c16b], valek, 0)
204         continue
!
            valek(2) = 'SANS'
            do 206 is = 1, nbsigr
                ioc = zi(jnsg+is-1)
                valei(2) = situ_numero(ioc)
!
                call tbajli(nomres, npar2, nopar2, valei, zr(jpmpbs- 1+10*(is-1)+1),&
                            [c16b], valek, 0)
206         continue
202     continue
200 continue
!
9999 continue
!
end subroutine
