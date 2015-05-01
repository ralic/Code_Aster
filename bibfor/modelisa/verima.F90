subroutine verima(nomz, limanz, lonlim, typz)
    implicit none
#include "jeveux.h"
#include "asterfort/jeexin.h"
#include "asterfort/jenonu.h"
#include "asterfort/jexnom.h"
#include "asterfort/utmess.h"
!
    integer :: lonlim
    character(len=*) :: nomz, limanz(lonlim), typz
! ----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     VERIFICATION DE L'APPARTENANCE DES OBJETS DE LA LISTE
!     LIMANO AU MAILLAGE NOMA
!
! IN       : NOMZ     : NOM DU MAILLAGE
! IN       : LIMANZ   : LISTE DE MAILLES OU DE NOEUDS OU DE GROUP_NO
!                       OU DE GROUP_MA
! IN       : LONLIM   : LONGUEUR DE LA LISTE LIMANO
! IN       : TYPZ     : TYPE DES OBJETS DE LA LISTE :
!                       MAILLE OU NOEUD OU GROUP_NO OU GROUP_MA
! ----------------------------------------------------------------------
!
    integer :: igr, iret, ino, ima
    character(len=8) :: noma, type
    character(len=24) :: noeuma, grnoma, mailma, grmama, limano
    character(len=24) :: valk(2)
! ----------------------------------------------------------------------
!
    noma = nomz
    type = typz
!
!
    noeuma = noma//'.NOMNOE'
    grnoma = noma//'.GROUPENO'
    mailma = noma//'.NOMMAI'
    grmama = noma//'.GROUPEMA'
!
    if (type .eq. 'GROUP_NO') then
!
!      --VERIFICATION DE L'APPARTENANCE DES GROUP_NO
!        AUX GROUP_NO DU MAILLAGE
!        -------------------------------------------------------
        call jeexin(grnoma, iret)
        if ((lonlim.ne.0) .and. (iret.eq.0)) then
            valk(1) = type
            valk(2) = noma
            call utmess('F', 'MODELISA7_12', nk=2, valk=valk)
        endif
        do 10 igr = 1, lonlim
            limano = limanz(igr)
            call jenonu(jexnom(grnoma, limano), iret)
            if (iret .eq. 0) then
                valk(1) = limano
                valk(2) = noma
                call utmess('F', 'MODELISA7_75', nk=2, valk=valk)
            endif
10      continue
!
    else if (type.eq.'NOEUD') then
!
!      --VERIFICATION DE L'APPARTENANCE DES NOEUDS
!        AUX NOEUDS DU MAILLAGE
!        -------------------------------------------------------
        call jeexin(noeuma, iret)
        if ((lonlim.ne.0) .and. (iret.eq.0)) then
            valk(1) = type
            valk(2) = noma
            call utmess('F', 'MODELISA7_12', nk=2, valk=valk)
        endif
        do 20 ino = 1, lonlim
            limano = limanz(ino)
            call jenonu(jexnom(noeuma, limano), iret)
            if (iret .eq. 0) then
                valk(1) = limano
                valk(2) = noma
                call utmess('F', 'MODELISA7_76', nk=2, valk=valk)
            endif
20      continue
!
    else if (type.eq.'GROUP_MA') then
!
!      --VERIFICATION DE L'APPARTENANCE DES GROUP_MA
!        AUX GROUP_MA DU MAILLAGE
!        -------------------------------------------------------
        call jeexin(grmama, iret)
        if ((lonlim.ne.0) .and. (iret.eq.0)) then
            valk(1) = type
            valk(2) = noma
            call utmess('F', 'MODELISA7_12', nk=2, valk=valk)
        endif
        do 30 igr = 1, lonlim
            limano = limanz(igr)
            call jenonu(jexnom(grmama, limano), iret)
            if (iret .eq. 0) then
                valk(1) = limano
                valk(2) = noma
                call utmess('F', 'MODELISA7_77', nk=2, valk=valk)
            endif
30      continue
!
    else if (type.eq.'MAILLE') then
!
!      --VERIFICATION DE L'APPARTENANCE DES MAILLES
!        AUX MAILLES DU MAILLAGE
!        -------------------------------------------------------
        call jeexin(mailma, iret)
        if ((lonlim.ne.0) .and. (iret.eq.0)) then
            valk(1) = type
            valk(2) = noma
            call utmess('F', 'MODELISA7_12', nk=2, valk=valk)
        endif
        do 40 ima = 1, lonlim
            limano = limanz(ima)
            call jenonu(jexnom(mailma, limano), iret)
            if (iret .eq. 0) then
                valk(1) = limano
                valk(2) = noma
                call utmess('F', 'MODELISA6_10', nk=2, valk=valk)
            endif
40      continue
!
    else
        call utmess('F', 'MODELISA7_79', sk=type)
    endif
end subroutine
