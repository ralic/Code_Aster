subroutine vetyma(noma, listma, nbma, listgr, nbgr,&
                  option, ndim, codret)
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
!
! BUT : VERIFICATION DU TYPE DES MAILLES AFFECTEES SUIVANT LE CHARGEMENT
!
! ARGUMENTS D'ENTREE:
!      NOMA   : NOM DU MAILLAGE
!      LISTMA : LISTE DES MAILLES
!      NBMA   : NOMBRE DE MAILLES
!      LISTGR : LISTE DES GROUPES DE MAILLES
!      NBGR   : NOMBRE DE GROUPES DE MAILLES
!      OPTION : MOT-CLE FACTEUR DANS AFFE_CHAR_MECA OU AFFE_CHAR_THER
!      NDIM   : DIMENSION DU PROBLEME (2D OU 3D)
!
! ARGUMENT DE SORTIE:
!      CODRET : CODE RETOUR : 0 SI OK, >0 SINON
!
! MOT-CLES FACTEUR VERIFIES :   FLUX_REP  ECHANGE     SOURCE
!                               PRES_REP  FORCE_FACE  FORCE_CONTOUR
!                               VITE_FACE IMPE_FACE   FORCE_INTERNE
!
! ROUTINES APPELEES:
!
#include "jeveux.h"
!
#include "asterfort/codent.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/u2mesk.h"
    integer :: codret
    character(len=8) :: listmz
    character(len=*) :: noma, listma(1), listgr(1), option
    character(len=8) :: type, kima, noma8
    character(len=24) :: grpma, optioz, listgz
    character(len=24) :: valk(2)
    character(len=1) :: k1bid
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, iadgma, iadtyp, iatyma, ibid, ima, j
    integer :: nbgr, nbma, ndim, nerr
!-----------------------------------------------------------------------
    call jemarq()
    optioz = option
    noma8=noma
    call jeveuo(noma8//'.TYPMAIL', 'L', iatyma)
    grpma = noma8//'.GROUPEMA'
    nerr=0
!
    if (option .eq. 'FLUX_REP' .or. option .eq. 'PRES_REP' .or. option .eq. 'ECHANGE' .or.&
        option .eq. 'FORCE_FACE' .or. option .eq. 'IMPE_FACE' .or. option .eq. 'VITE_FACE'&
        .or. option .eq. 'FORCE_CONTOUR') then
!
!  MOT-CLE MAILLE
        if (nbma .gt. 0) then
            do 1 i = 1, nbma
                call jenonu(jexnom(noma8//'.NOMMAI', listma(i)), ibid)
                iadtyp=iatyma-1+ibid
                call jenuno(jexnum('&CATA.TM.NOMTM', zi(iadtyp)), type)
                listmz = listma(i)
                if (ndim .eq. 2 .and. type(1:3) .ne. 'SEG') then
                    nerr=nerr+1
                    valk(1) = listmz
                    valk(2) = optioz
                    call u2mesk('A', 'MODELISA7_86', 2, valk)
                    elseif(ndim.eq.3.and.type(1:4).ne.'QUAD' .and.type(1:&
                4).ne.'TRIA') then
                    nerr=nerr+1
                    valk(1) = listmz
                    valk(2) = optioz
                    call u2mesk('A', 'MODELISA7_87', 2, valk)
                endif
 1          continue
            if (nbma .eq. nerr) then
                call u2mesk('A', 'MODELISA7_88', 1, optioz)
            endif
        endif
!
!  MOT-CLE GROUP_MA
        if (nbgr .gt. 0) then
            do 2 i = 1, nbgr
                call jeveuo(jexnom(grpma, listgr(i)), 'L', iadgma)
                call jelira(jexnom(grpma, listgr(i)), 'LONUTI', nbma, k1bid)
                do 3 j = 1, nbma
                    ima=zi(iadgma-1+j)
                    call codent(ima, 'G', kima)
                    iadtyp=iatyma-1+ima
                    call jenuno(jexnum('&CATA.TM.NOMTM', zi(iadtyp)), type)
                    if (ndim .eq. 2 .and. type(1:3) .ne. 'SEG') then
                        nerr=nerr+1
                        valk(1) = kima
                        valk(2) = optioz
                        call u2mesk('A', 'MODELISA7_89', 2, valk)
                        elseif(ndim.eq.3.and.type(1:4).ne.'QUAD' .and.type&
                    (1:4).ne.'TRIA' .and.type(1:3).ne.'SEG') then
                        nerr=nerr+1
                        valk(1) = kima
                        valk(2) = optioz
                        call u2mesk('A', 'MODELISA7_90', 2, valk)
                    endif
 3              continue
                if (nbma .eq. nerr) then
                    listgz = listgr(i)
                    valk(1) = listgz
                    valk(2) = optioz
                    call u2mesk('A', 'MODELISA7_91', 2, valk)
                endif
 2          continue
        endif
!
!
    else if (option.eq.'SOURCE' .or.option.eq.'FORCE_INTERNE') then
!  MOT-CLE MAILLE
        if (nbma .gt. 0) then
            do 10 i = 1, nbma
                call jenonu(jexnom(noma8//'.NOMMAI', listma(i)), ibid)
                iadtyp=iatyma-1+ibid
                call jenuno(jexnum('&CATA.TM.NOMTM', zi(iadtyp)), type)
                listmz = listma(i)
                if (ndim .eq. 2 .and. type(1:4) .ne. 'QUAD' .and. type(1:4) .ne. 'TRIA') then
                    nerr=nerr+1
                    valk(1) = listmz
                    valk(2) = optioz
                    call u2mesk('A', 'MODELISA7_87', 2, valk)
                    elseif(ndim.eq.3.and.type(1:4).ne.'HEXA' .and.type(1:&
                4).ne.'PENT' .and.type(1:4).ne.'PYRA' .and.type(1:4)&
                .ne.'TETR') then
                    nerr=nerr+1
                    valk(1) = listmz
                    valk(2) = optioz
                    call u2mesk('A', 'MODELISA7_92', 2, valk)
                endif
10          continue
            if (nbma .eq. nerr) then
                call u2mesk('A', 'MODELISA7_88', 1, optioz)
            endif
        endif
!  MOT-CLE GROUP_MA
        if (nbgr .gt. 0) then
            do 20 i = 1, nbgr
                call jeveuo(jexnom(grpma, listgr(i)), 'L', iadgma)
                call jelira(jexnom(grpma, listgr(i)), 'LONUTI', nbma, k1bid)
                do 30 j = 1, nbma
                    ima=zi(iadgma-1+j)
                    call codent(ima, 'G', kima)
                    iadtyp=iatyma-1+ima
                    call jenuno(jexnum('&CATA.TM.NOMTM', zi(iadtyp)), type)
                    if (ndim .eq. 2 .and. type(1:4) .ne. 'QUAD' .and. type(1: 4) .ne.&
                        'TRIA') then
                        nerr=nerr+1
                        valk(1) = kima
                        valk(2) = optioz
                        call u2mesk('A', 'MODELISA7_90', 2, valk)
                        elseif(ndim.eq.3.and.type(1:4).ne.'HEXA' .and.type&
                    (1:4).ne.'PENT' .and.type(1:4).ne.'PYRA' .and.type&
                    (1:4).ne.'TETR') then
                        nerr=nerr+1
                        valk(1) = kima
                        valk(2) = optioz
                        call u2mesk('A', 'MODELISA7_93', 2, valk)
                    endif
30              continue
                if (nbma .eq. nerr) then
                    listgz = listgr(i)
                    valk(1) = listgz
                    valk(2) = optioz
                    call u2mesk('A', 'MODELISA7_91', 2, valk)
                endif
20          continue
        endif
    endif
    codret = nerr
    call jedema()
end subroutine
