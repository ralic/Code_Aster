subroutine as_mfdfdi(fid, ind, cha, type, comp,&
                     unit, nseqca, cret)
! person_in_charge: nicolas.sellenet at edf.fr
!
! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
    implicit none
#include "asterf_types.h"
#include "asterf.h"
#include "asterfort/utmess.h"
#include "med/mfdfdi.h"
    character(len=*) :: cha, comp, unit
    aster_int :: fid, ind, type, cret, lmail, nseqca
    character(len=64) :: nommai
    character(len=80) :: unidt
!
!     UNITE DU PAS DE TEMPS EST UN GRANDEUR PAS UTILISEE DANS ASTER
!     DE MEME QUE LE MAILLAGE QUI EST RELU AVANT LIRE_RESU
#ifdef _DISABLE_MED
    call utmess('F', 'FERMETUR_2')
#else
!
#if med_int_kind != aster_int_kind
    med_int :: fid4, ind4, type4, cret4, nseqc4, lmai4
    fid4 = fid
    ind4 = ind
    call mfdfdi(fid4, ind4, cha, nommai, lmai4,&
                type4, comp, unit, unidt, nseqc4,&
                cret4)
    type = type4
    cret = cret4
    nseqca = nseqc4
#else
    call mfdfdi(fid, ind, cha, nommai, lmail,&
                type, comp, unit, unidt, nseqca,&
                cret)
#endif
!
#endif
end subroutine
