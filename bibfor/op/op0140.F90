subroutine op0140()
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
!***********************************************************************
!    T. KERBER      DATE 12/05/93
!-----------------------------------------------------------------------
!  BUT: ASSEMBLER UN VECTEUR ISSU D'UN MODELE GENERALISE
!
!     CONCEPT CREE: VECT_ASSE_GENE
!
!-----------------------------------------------------------------------
!
!
!
!
!
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/getvid.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/vecgcy.h"
#include "asterfort/vecgen.h"
    character(len=8) :: nomres, numeg
    character(len=9) :: method
    character(len=16) :: nomcon, nomope
    character(len=24) :: seliai
    integer :: ibid, iopt, elim
!
!-----------------------------------------------------------------------
!
!-------PHASE DE VERIFICATION-------------------------------------------
!
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    call jemarq()
    call infmaj()
!
    call getres(nomres, nomcon, nomope)
!
    call getvid(' ', 'NUME_DDL_GENE', scal=numeg, nbret=ibid)
!
    call getvtx(' ', 'METHODE', scal=method, nbret=iopt)
!
    elim=0
    seliai=numeg(1:8)//'      .ELIM.BASE'
    call jeexin(seliai, elim)
!
!
    if (method .eq. 'CLASSIQUE') then
        call vecgen(nomres, numeg)
    else if (elim .ne. 0) then
        call vecgen(nomres, numeg)
    else
        call vecgcy(nomres, numeg)
    endif
    call jeecra(nomres//'           .DESC', 'DOCU', cval='VGEN')
!
    call jedema()
end subroutine
