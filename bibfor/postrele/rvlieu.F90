subroutine rvlieu(mailla, typco, nlsnac, sdlieu)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/codent.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/rvabsc.h"
#include "asterfort/rvnchm.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/assert.h"
!
    character(len=24) :: nlsnac, sdlieu
    character(len=8) :: typco, mailla
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     ------------------------------------------------------------------
!     GENERATION DE LA SD LIEU (MAILLAGE DU LIEU)
!     ------------------------------------------------------------------
! IN  MAILLA : K : NOM DU MAILLAGE
! IN  NLSNAC : K : NOM DU VECTEUR DES NOEUDS ACTIFS
! OUT SDLIEU : K : NOM DU VECTEUR DES NOMS DE SD LIEU
!     ------------------------------------------------------------------
!     ORGANISATION SD_LIEU
!       .REFE : S E K8 DOCU('LSTN')
!               <-- NOM_MAILLAGE CAS LSTN
!       .ABSC : XD V R NB_OC = NBR_PART_CONNEXE
!               <-- ABSCISSES CURVILIGNE
!       .COOR : XD V R  NB_OC = NBR_PART_CONNEXE
!               <-- COORDONNEE X, Y ET Z
!       .DESC : CHOIX DOCU PARMI
!               -----      -----
!                 LSTN      : LISTE DES NOMS DE NOEUDS
!               FIN_CHOIX
!               ---------
!        .NUME : S E I
!               <-- NUMERO DE PARTIE DANS LE CAS D' UNE COURBE
!     ------------------------------------------------------------------
!
!
!
    character(len=24) :: nabsc, nrefe, ndesc, lnumnd, nnume, ncoor
    character(len=19) :: sdcour
    character(len=10) :: iden
    character(len=4) :: docu
    integer :: aabsc, arefe, adesc, acoor
    integer :: ansdl, adr, anumnd, anume
    integer :: nbsd, isd, nbpt, ipt
    real(kind=8) ::  zero
!
!====================== CORPS DE LA ROUTINE ===========================
!
    call jemarq()
    zero = 0.0d0
    lnumnd = '&&RVLIEU.LISTE.NUM.NOEUD'
    ASSERT(typco.ne.'CHEMIN')


    call jelira(nlsnac, 'LONMAX', nbpt)
    nbsd = 1

    call wkvect(sdlieu, 'V V K24', nbsd, ansdl)
    do 100 isd = 1, nbsd, 1
        call codent(isd, 'G', iden)
        sdcour = '&&RVLIEU.'//iden
        zk24(ansdl + isd-1)(1:19) = sdcour
        nrefe = sdcour//'.REFE'
        nabsc = sdcour//'.ABSC'
        ndesc = sdcour//'.DESC'
        nnume = sdcour//'.NUME'
        ncoor = sdcour//'.COOR'
        call wkvect(nrefe, 'V V K8', 1, arefe)
        call wkvect(nnume, 'V V I', 1, anume)
        zi(anume) = isd

        zk8(arefe) = mailla
        docu = 'LSTN'
        call jelira(nlsnac, 'LONMAX', nbpt)
        call jeveuo(nlsnac, 'L', anumnd)
        call wkvect(ndesc, 'V V K8', nbpt, adesc)
        do 30 ipt = 1, nbpt, 1
            call jenuno(jexnum(mailla//'.NOMNOE', zi(anumnd+ ipt-1)), zk8(adesc + ipt-1))
 30     continue

        call jecrec(nabsc, 'V V R', 'NU', 'DISPERSE', 'VARIABLE',&
                    1)
        call jecroc(jexnum(nabsc, 1))
        call jeecra(jexnum(nabsc, 1), 'LONMAX', nbpt)
        call jeveuo(jexnum(nabsc, 1), 'E', aabsc)
        call jecrec(ncoor, 'V V R', 'NU', 'DISPERSE', 'VARIABLE',&
                    1)
        call jecroc(jexnum(ncoor, 1))
        call jeecra(jexnum(ncoor, 1), 'LONMAX', 3*nbpt)
        call jeveuo(jexnum(ncoor, 1), 'E', acoor)
        call rvabsc(mailla, zi(anumnd), nbpt, zr(aabsc), zr(acoor))
        call jeexin(lnumnd, adr)
        if (adr .ne. 0) then
            call jedetr(lnumnd)
        endif

        call jeecra(nrefe, 'DOCU', cval=docu)
100 end do
    call jedema()
end subroutine
