subroutine rvrepm(mailla, courbe, repere, sdnewr)
    implicit none
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
#include "jeveux.h"
!
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/rvnchm.h"
#include "asterfort/rvrlln.h"
    character(len=19) :: sdnewr
    character(len=8) :: courbe, mailla, repere
!
!***********************************************************************
!
!  OPERATION REALISEE
!  ------------------
!
!     CALCUL  DU REPERE LOCAL OU POLAIRE LA LONG D' UNE COURBE CHEMIN
!
!  ARGUMENTS EN ENTREE
!  -------------------
!
!     COURBE : NOM DU CONCEPT COURBE
!     MAILLA : NOM DU CONCEPT MAILLAGE
!     REPERE : VAUT 'LOCAL' OU 'POLAIRE'
!
!  ARGUMENTS EN SORTIE
!  -------------------
!
!     SDNEWR : NOM DE LA SD DU REPERE CALCULE
!              (DOC. C.F. RVCPCN)
!
!***********************************************************************
!
!  -----------------------------------------
!
!
!
!  ---------------------------------
!
!  VARIABLES LOCALES
!  -----------------
!
    character(len=15) :: nchmin
    character(len=24) :: nvec1, nvec2, ntabnd, nkarnd
    integer :: avec1, avec2, atabnd
    integer :: nbchm, ichm, nbm, achm, acoord, nbpt
!
!====================== CORPS DE LA ROUTINE ===========================
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    call jemarq()
!
    nvec1 = sdnewr//'.VEC1'
    nvec2 = sdnewr//'.VEC2'
    nchmin = courbe//'.CHEMIN'
    ntabnd = '&&RVREPM.LISTE.NOEUD'
    nkarnd = '&&RVREPM.LISTE.NOMND'
!
    call jelira(nchmin, 'NMAXOC', nbchm)
    call jeveuo(mailla//'.COORDO    .VALE', 'L', acoord)
!
    call jecrec(nvec1, 'V V R', 'NU', 'DISPERSE', 'VARIABLE',&
                nbchm)
    call jecrec(nvec2, 'V V R', 'NU', 'DISPERSE', 'VARIABLE',&
                nbchm)
!
    do 100, ichm = 1, nbchm, 1
!
    call jelira(jexnum(nchmin, ichm), 'LONMAX', nbm)
    call jeveuo(jexnum(nchmin, ichm), 'L', achm)
!
    nbm = nbm - 1
!
    call rvnchm(mailla, zi(achm), nbm, ntabnd, nkarnd)
    call jeveuo(ntabnd, 'L', atabnd)
    call jelira(ntabnd, 'LONMAX', nbpt)
    call jecroc(jexnum(nvec1, ichm))
    call jeecra(jexnum(nvec1, ichm), 'LONMAX', 2*nbpt)
    call jeveuo(jexnum(nvec1, ichm), 'E', avec1)
    call jecroc(jexnum(nvec2, ichm))
    call jeecra(jexnum(nvec2, ichm), 'LONMAX', 2*nbpt)
    call jeveuo(jexnum(nvec2, ichm), 'E', avec2)
!
    call rvrlln(zr(acoord), zi(atabnd), nbpt, repere, zr(avec1),&
                zr( avec2))
!
    call jedetr(ntabnd)
    call jedetr(nkarnd)
!
    100 end do
!
    call jedema()
end subroutine
