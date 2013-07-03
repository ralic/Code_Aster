subroutine chmrck(chmat, nomrc, nommat, nbmtrc)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utfk16.h"
    character(len=8) :: chmat, nommat(*)
    character(len=16) :: nomrc
    integer :: nbmtrc
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
!     ==================================================================
!     ! UTILITAIRE - RECHERCHE DES MATERIAUX D'UN CHAM_MATER QUI       !
!     ! UTILISENT UNE RELATION DE COMPORTEMENT DONNEE               RM !
!     ==================================================================
!     !                                                                !
!     !    ETANT DONNES UN CHAM_MATER ET UNE RELATION DE COMPORTEMENT  !
!     !    CHERCHER LES MATERIAUX DU CHAM_MATER QUI UTILISE CETTE RC   !
!     !                                                                !
!     !    DANS UN MATERIAU, IL N'Y A QU'UNE RC D'UN TYPE DONNE        !
!     !                                                                !
!     ==================================================================
! IN  ! CHMAT  ! K8  ! NOM DU CONCEPT CHAM_MATER                       !
! IN  ! NOMRC  ! K8  ! NOM DE LA RC CHERCHEE                           !
! IN  ! NBMTCH ! IS  ! LONGUEUR DU .VALE DU CHAM_MATER                 !
! OUT ! NBMTRC ! IS  ! NOMBRE DE MAT QUI UTILISE LA RC                 !
! OUT ! NOMMAT ! K8  !LISTE DES MAT DU CHAM_MATER QUI UTILISENT LA RC  !
!     ==================================================================
!
!
! --- VARIABLES LOCALES ---
    character(len=8) :: kmat, kbid
    character(len=24) :: krc
    integer :: jvale, arc, imat, nbrc, ipos, ncmpmx, jdesc, izone, i, nbzone
    integer :: l1, nbzmax
    parameter (ncmpmx=30)
!
! ====================== DEBUT DU PROGRAMME ============================
!
    call jemarq()
    kbid = ' '
    call jeveuo(chmat//'.CHAMP_MAT .VALE', 'L', jvale)
    call jelira(chmat//'.CHAMP_MAT .VALE', 'LONMAX', l1, kbid)
    call jeveuo(chmat//'.CHAMP_MAT .DESC', 'L', jdesc)
    nbzmax=zi(jdesc-1+2)
    nbzone=zi(jdesc-1+3)
!     ON VERIFIE QUE LA TAILLE DE LA CARTE EST BIEN TOUJOURS DE 30
!     PAR ZONE
    call assert(l1.eq.(nbzmax*ncmpmx))
!
!
    nbmtrc = 0
    do 50 izone = 1, nbzone
        do 100 i = 1, ncmpmx
            imat=(izone-1)*ncmpmx+i
            kmat = zk8(jvale-1 + imat)
            if (kmat .eq. ' ') goto 50
            if (kmat .eq. 'TREF=>') goto 50
            krc = kmat//'.MATERIAU.NOMRC'
            call jeveuo(krc, 'L', arc)
            call jelira(krc, 'LONMAX', nbrc, kbid)
            call utfk16(zk16(arc), nbrc, nomrc, ipos)
            if (ipos .gt. 0) then
                nbmtrc = nbmtrc + 1
                nommat(nbmtrc) = kmat
            endif
100      continue
50  end do
!
!
    call jedema()
end subroutine
