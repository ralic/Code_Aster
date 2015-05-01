subroutine mmmcrf(noma, ddepla, depplu, nfrot, vfrot)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit     none
#include "jeveux.h"
!
#include "asterc/r8vide.h"
#include "asterfort/cnomax.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jexnum.h"
    character(len=8) :: noma
    character(len=19) :: depplu, ddepla
    character(len=16) :: nfrot
    real(kind=8) :: vfrot
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - ALGORITHME - UTILITAIRE)
!
! CALCUL RESIDU DE FROTTEMENT
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  DDEPLA : INCREMENT SOLUTION
! IN  DEPPLU : SOLUTION A L'INSTANT COURANT
! OUT NFROT  : LIEU OU LE CRITERE EST MAX
! OUT VFROT  : VALEUR DU CRITERE MAX
!
! ----------------------------------------------------------------------
!
    integer :: ncmp
    parameter    (ncmp=1)
    character(len=8) :: liscmp(ncmp)
!
    real(kind=8) :: vmax1, vmax2, vmaxi
    real(kind=8) :: crilbd
    character(len=8) :: nomnoe
    integer :: numno1, numno2, numnoe
!
    data liscmp  /'LAGS_C'/
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    vmax1 = 0.d0
    vmax2 = 0.d0
    crilbd = 0.d0
    nfrot = ' '
    vfrot = r8vide()
!
! --- TRI DES COMPOSANTES
!
    call cnomax(ddepla, ncmp, liscmp, vmax1, numno1)
    call cnomax(depplu, ncmp, liscmp, vmax2, numno2)
!
! --- CRITERE: VARIATION RELATIVE SUR LES LAGS_C
!
    if (vmax2 .gt. 0.d0) then
        crilbd = vmax1/vmax2
    else
        crilbd = 0.d0
    endif
!
! --- INFORMATIONS SUR LE CRITERE
!
    numnoe = numno1
    vmaxi = crilbd
    if (numnoe .eq. 0) then
        nomnoe = ' '
    else
        call jenuno(jexnum(noma//'.NOMNOE', numnoe), nomnoe)
    endif
    nfrot = nomnoe//'        '
    vfrot = vmaxi
!
    call jedema()
end subroutine
