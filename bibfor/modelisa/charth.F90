subroutine charth(fonree)
    implicit none
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/adalig.h"
#include "asterfort/alligr.h"
#include "asterfort/caddli.h"
#include "asterfort/caechp.h"
#include "asterfort/cagene.h"
#include "asterfort/cagrou.h"
#include "asterfort/caliag.h"
#include "asterfort/caliai.h"
#include "asterfort/calich.h"
#include "asterfort/calirc.h"
#include "asterfort/cbconv.h"
#include "asterfort/cbecha.h"
#include "asterfort/cbflnl.h"
#include "asterfort/cbflux.h"
#include "asterfort/cbgrai.h"
#include "asterfort/cbrayo.h"
#include "asterfort/cbsonl.h"
#include "asterfort/cbsour.h"
#include "asterfort/cormgi.h"
#include "asterfort/initel.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jeveuo.h"
    character(len=4) :: fonree
!---------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!      OPERATEURS :     AFFE_CHAR_THER ET AFFE_CHAR_THER_F
!
!      MOTS-CLES ACTUELLEMENT TRAITES:
!        SOURCE
!        SOUR_NL
!        ECHANGE
!        TEMP_IMPO
!        FLUX_REP
!        ECHANGE_PAROI
!        PRE_GRAD_TEMP
!        LIAISON_DDL
!        LIAISON_GROUP
!        RAYONNEMENT
!---------------------------------------------------------------------
    integer :: ibid, igrel, inema, iret, ndim, jlgrf
    character(len=8) :: char, noma
    character(len=16) :: type, oper, motfac
    character(len=19) :: ligrch, ligrmo
!     ------------------------------------------------------------------
!
!
    call getres(char, type, oper)
!
! --- NOMS DE LIGREL, MAILLAGE , DIMENSION DU PB
!
    call cagene(char, oper, ligrmo, noma, ndim)
!
! --- ALLOCATION DU LIGREL DE CHARGE (MOTS-CLE: TEMP_IMPO,
!                          ECHANGE_PAROI, LIAISON_DDL, LIAISON_GROUP )
!
    call alligr(char, oper, noma, fonree, ligrch)
!
! --- TEMP_IMPO ---
!
    igrel = 0
    inema = 0
!
! --- SOURCE ---
!
    call cbsour(char, noma, ligrmo, ndim, fonree)
!
! --- SOUR_NL---
!
    call cbsonl(char, noma, ligrmo, ndim, fonree)
!
! --- CONVECTION ---
!
    call cbconv(char)
!
! --- FLUX_REP ---
!
    call cbflux(char, noma, ligrmo, ndim, fonree)
!
! --- FLUX_NL
!
    call cbflnl(char, noma, ligrmo, fonree)
!
! --- RAYONNEMENT ---
!
    call cbrayo(char, noma, ligrmo, fonree)
!
! --- ECHANGE ---
!
    call cbecha(char, noma, ligrmo, ndim, fonree)
!
! --- ECHANGE_PAROI ---
!
    call caechp(char, ligrch, ligrmo, igrel, inema,&
                noma, fonree, ndim)
!
! --- GRADIENT INITIAL ---
!
    call cbgrai(char, noma, ligrmo, fonree)
!
! --- TEMP_IMPO ---
!
    motfac = 'TEMP_IMPO'
    call caddli(motfac, char, noma, ligrmo, fonree)
!
! --- LIAISON_DDL ---
!
    call caliai(fonree, char)
!
! --- LIAISON_GROUP ---
!
    call caliag(fonree, char)
!
! --- LIAISON_UNIF ---
!
    call cagrou(char, noma, fonree)
!
! --- LIAISON_CHAMNO ---
!
    if (fonree .eq. 'REEL') then
        call calich(char)
    endif
!
!
! --- LIAISON_MAIL ---
!
    if (fonree .eq. 'REEL') then
        call calirc(char)
    endif
!
!
! --- MISE A JOUR DU LIGREL DE CHARGE :
    call jeexin(ligrch//'.LGRF', iret)
    if (iret .ne. 0) then
        call adalig(ligrch)
        call cormgi('G', ligrch)
        call jeecra(ligrch//'.LGRF', 'DOCU', ibid, 'THER')
        call initel(ligrch)
!       -- LIEN ENTRE LE LIGREL DE CHARGE ET LE MODELE :
        call jeveuo(ligrch//'.LGRF', 'E', jlgrf)
        zk8(jlgrf-1+2)=ligrmo(1:8)
    endif
!
end subroutine
