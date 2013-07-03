subroutine nmcrpc(result)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit     none
#include "asterfort/exisd.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/ltcrsd.h"
#include "asterfort/ltnotb.h"
#include "asterfort/tbajpa.h"
#include "asterfort/tbcrsd.h"
    character(len=8) :: result
!
! ----------------------------------------------------------------------
!
! ROUTINE *_NON_LINE (STRUCTURES DE DONNES)
!
! CREATION DE LA TABLE DES PARAMETRES CALCULES
!
! ----------------------------------------------------------------------
!
! IN  RESULT : NOM SD RESULTAT
!
! ----------------------------------------------------------------------
!
    integer :: nbpar
    parameter   (nbpar=8)
    character(len=2) :: typpar(nbpar)
    character(len=10) :: nompar(nbpar)
! ----------------------------------------------------------------------
    integer :: ifm, niv, iret
    character(len=19) :: tablpc
    data         nompar / 'NUME_REUSE','INST'      ,'TRAV_EXT  ',&
     &                      'ENER_CIN'  ,'ENER_TOT'  ,'TRAV_AMOR ',&
     &                      'TRAV_LIAI' ,'DISS_SCH'/
    data         typpar / 'I'         ,'R'         ,'R'         ,&
     &                      'R'         ,'R'         ,'R'         ,&
     &                      'R'         ,'R'/
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- CREATION DE LA LISTE DE TABLES SI ELLE N'EXISTE PAS
!
    call jeexin(result//'           .LTNT', iret)
    if (iret .eq. 0) call ltcrsd(result, 'G')
!
! --- RECUPERATION DU NOM DE LA TABLE CORRESPONDANT
!     AUX PARAMETRE CALCULES
!
    tablpc = ' '
    call ltnotb(result, 'PARA_CALC', tablpc)
!
! --- LA TABLE PARA_CALC EXISTE-T-ELLE ?
!
    call exisd('TABLE', tablpc, iret)
!
! --- NON, ON LA CREE
!
    if (iret .eq. 0) then
        call tbcrsd(tablpc, 'G')
        call tbajpa(tablpc, nbpar, nompar, typpar)
    endif
!
    call jedema()
!
end subroutine
