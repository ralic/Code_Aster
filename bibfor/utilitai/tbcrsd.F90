subroutine tbcrsd(nomta, baseta)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    character(len=*) :: nomta, baseta
!     ------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!      CREATION D'UNE STRUCTURE DE DONNEES "TABLE".
!      LA STRUCTURE D'UNE TABLE :
!       .TBBA : K8  : DEFINITION DE LA BASE
!       .TBNP :  I  : (1) NOMBRE DE PARAMETRES DE LA TABLE
!                     (2) NOMBRE DE LIGNES DE LA TABLE
!       .TBLP : K24 : DECRIT LES PARAMETRES DE LA TABLE
!                     (1) NOM DU PARAMETRE
!                     (2) TYPE DU PARAMETRE
!                     (3) NOM OBJET JEVEUX CONTENANT LES VALEURS
!                     (4) NOM OBJET JEVEUX CONTENANT DES LOGIQUES
!     ------------------------------------------------------------------
! IN  : NOMTA  : NOM DE LA STRUCTURE "TABLE" A CREER.
! IN  : BASETA : BASE SUR LAQUELLE ON CREE LA "TABLE".
!     ------------------------------------------------------------------
    integer :: jtbba, jtbnp
    character(len=1) :: base
    character(len=19) :: nomtab
! DEB------------------------------------------------------------------
!
    call jemarq()
!
    nomtab = nomta
    if (nomtab(18:19) .ne. '  ') then
        call utmess('F', 'UTILITAI4_75')
    endif
!
    base = baseta(1:1)
    ASSERT(base.eq.'V' .or. base.eq.'G')
!
!     --- CREATION DU .TBBA ---
!
    call wkvect(nomtab//'.TBBA', base//' V K8', 1, jtbba)
    zk8(jtbba) = base
!
!     --- CREATION DU .TBNP ---
!
    call wkvect(nomtab//'.TBNP', base//' V I', 2, jtbnp)
    zi(jtbnp ) = 0
    zi(jtbnp+1) = 0
!
!     --- CREATION DU .TBLP ---
!
!     LE VECTEUR EST CREE DANS LA ROUTINE "TBAJPA"
!      CALL JECREO(NOMTAB//'.TBLP',BASE//' V K24')
!      CALL JEECRA(NOMTAB//'.TBLP','LONMAX',...,' ')
!      CALL JEECRA(NOMTAB//'.TBLP','LONUTI',0,' ')
!
    call jedema()
end subroutine
