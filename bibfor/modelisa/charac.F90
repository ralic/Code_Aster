subroutine charac(fonree)
    implicit   none
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/adalig.h"
#include "asterfort/caddli.h"
#include "asterfort/cagene.h"
#include "asterfort/cagrou.h"
#include "asterfort/cbimpe.h"
#include "asterfort/cbvite.h"
#include "asterfort/cormgi.h"
#include "asterfort/initel.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jeveuo.h"
    character(len=4) :: fonree
! ----------------------------------------------------------------------
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
!      OPERATEURS :  AFFE_CHAR_ACOU  ET  AFFE_CHAR_ACOU_F
!
!      MOTS-CLES ACTUELLEMENT TRAITES:
!         VITE_FACE
!         IMPE_FACE
!         PRES_IMPO
!         LIAISON_UNIF
! ----------------------------------------------------------------------
    integer :: ndim, iret, ibid, jlgrf
    character(len=4) :: fonr2
    character(len=8) :: char, noma
    character(len=16) :: type, oper, motfac
    character(len=19) :: ligrmo, ligrch
!     ------------------------------------------------------------------
!
    call getres(char, type, oper)
!
! --- NOMS DE LIGREL, MAILLAGE , DIMENSION DU PB
!
    call cagene(char, oper, ligrmo, noma, ndim)
!
    fonr2 = 'COMP'
    if (oper(15:16) .eq. '_F') fonr2 = 'FONC'
!
! --- VITE_FACE ---
!
    call cbvite(char, noma, ligrmo, fonree)
!
!  --- IMPE_FACE ---
!
    call cbimpe(char, noma, ligrmo, fonree)
!
! --- PRES_IMPO ---
!
    motfac = 'PRES_IMPO'
    call caddli(motfac, char, noma, ligrmo, fonr2)
!
! --- LIAISON_UNIF ---
!
    call cagrou(fonree, char)
!
!
! --- MISE A JOUR DU LIGREL DE CHARGE :
    ligrch = char//'.CHAC.LIGRE'
    call jeexin(ligrch//'.LGRF', iret)
    if (iret .ne. 0) then
        call adalig(ligrch)
        call cormgi('G', ligrch)
        call jeecra(ligrch//'.LGRF', 'DOCU', ibid, 'ACOU')
        call initel(ligrch)
!       -- LIEN ENTRE LE LIGREL DE CHARGE ET LE MODELE :
        call jeveuo(ligrch//'.LGRF', 'E', jlgrf)
        zk8(jlgrf-1+2)=ligrmo(1:8)
    endif
!
end subroutine
