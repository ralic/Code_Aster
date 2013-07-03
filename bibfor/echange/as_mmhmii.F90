subroutine as_mmhmii(fid, indice, maa, dim, type, desc, cret)
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
#include "aster_types.h"
#include "med/mmhmii.h"
    aster_int :: fid, dim, dimb, cret, indice, type, typtri, nbseq, typrep
    character(len=64) :: maa
    character(len=200) :: desc
    character(len=16) :: descdt
    character(len=16) :: nom(3), unit(3)
#ifdef _DISABLE_MED
    call u2mess('F', 'FERMETUR_2')
#else

#if med_int_kind != aster_int_kind
    med_int :: fid4, dim4, cret4, indic4, type4, dimb4, typtr4
    med_int :: nbseq4, typre4
    fid4 = fid
    indic4 = indice
!   -- initialisation des chaines (prudent car appel C ensuite):
    maa=' '
    desc=' '
    descdt=' '
    nom(:)=' '
    unit(:)=' '
    call mmhmii(fid4, indic4, maa, dim4, dimb4,&
                type4, desc, descdt, typtr4, nbseq4,&
                typre4, nom, unit, cret4)
    dim = dim4
    type = type4
    cret = cret4
#else
    call mmhmii(fid, indice, maa, dim, dimb,&
                type, desc, descdt, typtri, nbseq,&
                typrep, nom, unit, cret)
#endif

#endif
end subroutine
