function nopar2(nomopt, nomgd, statut)
    implicit none
    character(len=8) :: nopar2
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
! person_in_charge: jacques.pellet at edf.fr
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/u2mesk.h"
    character(len=*) :: nomopt, nomgd, statut
! ----------------------------------------------------------------------
!     ENTREES:
!     NOMOPT   : NOM D'1 OPTION
!     NOMGD    : NOM D'1 GRANDEUR
!     STATUT : 'IN'/'OUT'/'INOUT'
!
!     SORTIES:
!     NOPAR2 : NOM DU PARAMETRE DE L'OPTION NOMOPT
!              QUI CORRESPOND A LA GRANDEUR NOMGD ET AU STATUT STATUT
!              RQUE : ERREUR <F> SI ON EN TROUVE : 0,2,3,...
!              RQUE : SI INOUT, ON CHERCHE D'ABORD DANS OUT ET SI ON NE
!                     TROUVE PAS, ON CHERCHE DANS IN
!
!     SI NOMGD=' ' ET SI STATUT='OUT' :
!         - SI L'OPTION N'A QU'UN PARAMETRE 'OUT', ON LE REND
!         - SI L'OPTION A PLUSIEURS PARAMETRES 'OUT' => ERREUR <F>
!
! ----------------------------------------------------------------------
!
!
!     VARIABLES LOCALES:
!     ------------------
    integer :: opt, nbin, nbout, nbtrou, itrou, gd, gd2, iadesc, iaoppa, kk
    character(len=16) :: nomop2
    character(len=8) :: nomgd2
    character(len=8) :: statu2, outrou
    character(len=24) :: valk(3)
!
! DEB-------------------------------------------------------------------
    nomop2=nomopt
    nomgd2=nomgd
    statu2=statut
!
    call jenonu(jexnom('&CATA.OP.NOMOPT', nomop2), opt)
    if (nomgd2 .ne. ' ') call jenonu(jexnom('&CATA.GD.NOMGD', nomgd2), gd)
    call jeveuo(jexnum('&CATA.OP.DESCOPT', opt), 'L', iadesc)
    call jeveuo(jexnum('&CATA.OP.OPTPARA', opt), 'L', iaoppa)
    nbin = zi(iadesc-1+2)
    nbout = zi(iadesc-1+3)
    nbtrou=0
    outrou=' '
!
!
    if (statu2 .eq. 'OUT') then
        if (nomgd2 .eq. ' ') then
            call assert(nbout.eq.1)
            nbtrou=nbtrou+1
            itrou=1
            outrou='OUT'
        else
            do 1,kk=1,nbout
            gd2 = zi(iadesc-1+4+nbin+kk)
            if (gd .eq. gd2) then
                nbtrou=nbtrou+1
                itrou=kk
                outrou='OUT'
            endif
 1          continue
        endif
!
    else if (statu2.eq.'IN') then
        do 2,kk=1,nbin
        gd2 = zi(iadesc-1+4+kk)
        if (gd .eq. gd2) then
            nbtrou=nbtrou+1
            itrou=kk
            outrou='IN'
        endif
 2      continue
!
    else if (statu2.eq.'INOUT') then
        do 11,kk=1,nbout
        gd2 = zi(iadesc-1+4+nbin+kk)
        if (gd .eq. gd2) then
            nbtrou=nbtrou+1
            itrou=kk
            outrou='OUT'
        endif
11      continue
!
        if (nbtrou .eq. 0) then
            do 12,kk=1,nbin
            gd2 = zi(iadesc-1+4+kk)
            if (gd .eq. gd2) then
                nbtrou=nbtrou+1
                itrou=kk
                outrou='IN'
            endif
12          continue
        endif
!
    else
        call assert(.false.)
    endif
!
    if (nbtrou .eq. 0) then
        valk(1) = statu2
        valk(2) = nomgd2
        valk(3) = nomop2
        call u2mesk('F', 'CALCULEL3_84', 3, valk)
    endif
    if (nbtrou .gt. 1) then
        valk(1) = statu2
        valk(2) = nomgd2
        valk(3) = nomop2
        call u2mesk('F', 'CALCULEL3_85', 3, valk)
    endif
    if (outrou .eq. 'OUT') then
        nopar2=zk8(iaoppa-1+nbin+itrou)
!
    else if (outrou.eq.'IN') then
        nopar2=zk8(iaoppa-1+itrou)
!
    else
        call assert(.false.)
    endif
!
end function
