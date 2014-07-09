subroutine numer3(modele, lischa, solveu, nu)
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
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/idenob.h"
#include "asterfort/infbav.h"
#include "asterfort/infmue.h"
#include "asterfort/iunifi.h"
#include "asterfort/jeimpo.h"
#include "asterfort/numero.h"
#include "asterfort/utmess.h"
    character(len=*) :: modele, solveu, lischa
    character(len=*) :: nu
!
!
! ----------------------------------------------------------------------
! ROUTINE APPELLEE PAR : CONLIG
! ----------------------------------------------------------------------
!
! MODIFIE LE NUME_DDL NU POUR TENIR COMPTE DES ELEMENTS FINIS DE
! CONTACT (LIGRCF)
!
! IN  MODELE : NOM DU MODELE
! IN  SOLVEU : OBJET SOLVEUR
! IN  LISCHA : L_CHARGES CONTENANT LES CHARGES APPLIQUEES
! IN  NUMEDD : NOM DU NUME_DDL
!
!
! ----------------------------------------------------------------------
!
    integer :: iul
    character(len=14) :: nu2, nuav
    character(len=24) :: ob1, ob2
    character(len=2) :: base
!
! ----------------------------------------------------------------------
!
! --- BASE(1:1) : BASE POUR CREER LE NUME_DDL (SAUF LE PROF_CHNO)
! --- BASE(2:2) : BASE POUR CREER LE PROF_CHNO
!
    base = 'VG'
!
    call infmue()
    nu2=nu
    nuav='&&NUMER3.NUAV'
!
    call copisd('NUME_DDL', 'V', nu, nuav)
    call detrsd('NUME_DDL', nu)
!
    call numero(nu, solveu, base,&
                modelz = modele , list_loadz = lischa)

!
    ob1=nu2//'.NUME.DEEQ'
    ob2=nuav//'.NUME.DEEQ'
    if (.not. idenob(ob1,ob2)) then
        iul = iunifi('MESSAGE')
        call jeimpo(iul, ob1, ' ')
        call jeimpo(iul, ob2, ' ')
        call utmess('F', 'ASSEMBLA_31')
    endif
!
    call detrsd('NUME_DDL', nuav)
    call infbav()
!
end subroutine
