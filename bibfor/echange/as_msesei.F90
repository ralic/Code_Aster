subroutine as_msesei(idfimd, imasup, nomaes, nvtymd, dimest,&
                  nomasu, medcel, nbnosu, nbmssu, tygems,&
                  nbattc, prespr, nbattv, codret)
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
#include "asterf.h"
#include "aster_types.h"
#include "med/msesei.h"
    character(len=*) :: nomaes, nomasu
    aster_int :: idfimd, imasup, nvtymd, dimest, medcel, nbnosu
    aster_int :: nbmssu, tygems, nbattc, prespr, nbattv, codret
#ifdef _DISABLE_MED
    call u2mess('F', 'FERMETUR_2')
#else

#if med_int_kind != aster_int_kind
    med_int :: idfim4, imasu4, nvtym4, dimes4, medce4, nbnos4
    med_int :: nbmss4, tygem4, nbatc4, presp4, nbatv4, codre4
    idfim4 = idfimd
    imasu4 = imasup
    call msesei(idfim4, imasu4, nomaes, nvtym4, dimes4,&
                nomasu, medce4, nbnos4, nbmss4, tygem4,&
                nbatc4, presp4, nbatv4, codre4)
    nvtymd = nvtym4
    dimest = dimes4
    medcel = medce4
    nbnosu = nbnos4
    nbmssu = nbmss4
    tygems = tygem4
    nbattc = nbatc4
    prespr = presp4
    nbattv = nbatv4
    codret = codre4
#else
    call msesei(idfimd, imasup, nomaes, nvtymd, dimest,&
                nomasu, medcel, nbnosu, nbmssu, tygems,&
                nbattc, prespr, nbattv, codret)
#endif

#endif
end subroutine
