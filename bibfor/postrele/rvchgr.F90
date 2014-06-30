subroutine rvchgr(mailla, courbe, nlsnac, repere, sdnewr,&
                  iret)
    implicit none
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rvegal.h"
#include "asterfort/rvrepc.h"
#include "asterfort/rvrepm.h"
#include "asterfort/rvrepn.h"
#include "asterfort/utmess.h"
    character(len=24) :: nlsnac
    character(len=19) :: sdnewr
    character(len=8) :: courbe, mailla, repere
    integer :: iret
!
!***********************************************************************
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
!  OPERATION REALISEE
!  ------------------
!
!     CALCUL DU REPERE DE TRAVAIL (LOCALE, DIRECTION OU POLAIRE)
!
!  ARGUMENTS EN ENTREE
!  -------------------
!
!     COURBE : NOM DU CONCEPT COURBE LIEU DU POST-TRAITEMENT
!     MAILLA : NOM DU CONCEPT MAILLAGE
!     NLSNAC : NOM DU VECTEUR DES NOEUDS LIEU DE POST-TRAITEMENT
!     REPERE : VAUT 'LOCAL' OU 'POLAIRE'
!
!  ARGUMENTS EN SORTIE
!  -------------------
!
!     IRET   : CODE RETOUR : 1 RAS, 0 ERREUR (EMISSION D' UN MESSAGE)
!     SDNEWR : NOM DE LA SD CONSERVANT LE NOUVEAU REPERE
!
!              .VEC1 : XD V R8 -->   COORD. DU VECTEUR 1 DANS (X,Y)
!              .VEC2 : XD V R8 -->   COORD. DU VECTEUR 2 DANS (X,Y)
!
!              VECJ(2I-1) <--  X(VECT_J,POINT_I)
!              VECJ(2I  ) <--  Y(VECT_J,POINT_I)
!
!              NB_OC = NB_PARTIE DU LIEU
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
    integer :: i, nd, nbnac, ind, alsnac,  ierd
    logical(kind=1) :: egal
    real(kind=8) :: znd, zref, aux
    character(len=8) :: k8b
    character(len=24) :: valk(2)
    real(kind=8), pointer :: vale(:) => null()
!
!====================== CORPS DE LA ROUTINE ===========================
!
    call jemarq()
    i = 0
    iret = 1
!
    if (repere(1:7) .eq. 'POLAIRE') then
        call dismoi('Z_CST', mailla, 'MAILLAGE', repk=k8b, arret='C',&
                    ier=ierd)
        if (k8b(1:3) .eq. 'NON') then
            iret = 0
            call utmess('A', 'POSTRELE_28')
            goto 999
        endif
    endif
!
    if (courbe(1:1) .ne. '&') then
!
        call jeexin(courbe//'.TYPCOURBE', ierd)
        if (ierd .eq. 0) then
            iret = 0
            valk (1) = courbe
            valk (2) = repere
            call utmess('A', 'POSTRELE_29', nk=2, valk=valk)
            goto 999
        endif
!
        call jeveuo(courbe//'.TYPCOURBE', 'L', i)
!
        if (zk8(i) .eq. 'LISTMAIL') then
!
            call rvrepm(mailla, courbe, repere, sdnewr)
!
        else
!
            call rvrepc(courbe, repere, sdnewr)
!
        endif
!
    else
!
        ind = 1
!
        call jelira(nlsnac, 'LONMAX', nbnac)
        call jeveuo(nlsnac, 'L', alsnac)
        call jeveuo(mailla//'.COORDO    .VALE', 'L', vr=vale)
!
        nd = zi(alsnac + 1-1)
!
        zref = vale(3)
!
 10     continue
        if ((iret .ne. 0) .and. (ind .le. nbnac)) then
!
            nd = zi(alsnac + ind-1)
            znd = vale(1+ 3*nd-1)
!
            call rvegal(1.0d-3, 'R', zref, znd, egal,&
                        aux)
!
            if (.not. egal) then
!
                iret = 0
!
            endif
!
            ind = ind + 1
!
            goto 10
!
        endif
!
        if (iret .ne. 0) then
!
            call rvrepn(mailla, nlsnac, repere, sdnewr)
!
        else
!
            call utmess('A', 'POSTRELE_28')
!
        endif
!
    endif
!
999 continue
!
    call jedema()
end subroutine
