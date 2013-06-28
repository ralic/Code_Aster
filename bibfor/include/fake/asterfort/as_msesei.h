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
#include "types/aster_types.h"
interface
    subroutine as_msesei(idfimd, imasup, nomaes, nvtymd, dimest,&
                      nomasu, medcel, nbnosu, nbmssu, tygems,&
                      nbattc, prespr, nbattv, codret)
        ast_int :: idfimd
        ast_int :: imasup
        character(*) :: nomaes
        ast_int :: nvtymd
        ast_int :: dimest
        character(*) :: nomasu
        ast_int :: medcel
        ast_int :: nbnosu
        ast_int :: nbmssu
        ast_int :: tygems
        ast_int :: nbattc
        ast_int :: prespr
        ast_int :: nbattv
        ast_int :: codret
    end subroutine as_msesei
end interface
