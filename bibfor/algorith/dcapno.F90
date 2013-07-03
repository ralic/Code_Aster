subroutine dcapno(resuz, typchz, iord, chavaz)
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
!***********************************************************************
!    P. RICHARD     DATE 28/03/91
!-----------------------------------------------------------------------
!  BUT:  RECUPERER L'ADRESSE D'UN .VALE D'UN CHAMNO A PARTIR DE SON
!  TYPE ET DE NUMERO D'ORDRE DANS UN RESULTAT COMPOSE
    implicit none
!
!-----------------------------------------------------------------------
!
! RESUZ    /I/: NOM DU RESULTAT COMPOSE
! TYPCHZ   /I/: TYPE DU CHAMPS
! IORD     /I/: NUMERO D'ORDRE DU CHAMNO DANS CONCEPT RESULTAT
! CHAVAZ   /0/: NOM K24 DE L'OBJET JEVEUX DEMANDE
!
!
#include "jeveux.h"
!
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rsexch.h"
    character(len=*) :: chavaz
    character(len=*) :: resuz, typchz
!
!
!
    character(len=8) :: resu, typch
    character(len=19) :: chacou
    character(len=24) :: chaval
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: iad, ier, iord
!-----------------------------------------------------------------------
    call jemarq()
    chaval = chavaz
    resu = resuz
    typch = typchz
    call rsexch('F', resu, typch, iord, chacou,&
                ier)
!
    chaval = chacou//'.VALE'
    call jeveuo(chaval, 'L', iad)
!
    chavaz = chaval
    goto 9999
!
9999  continue
    call jedema()
end subroutine
