subroutine xlenri(noma, fiss, goinop, lismae, lisnoe)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvem.h"
#include "asterfort/jedema.h"
#include "asterfort/jedupo.h"
#include "asterfort/jemarq.h"
#include "asterfort/reliem.h"
#include "asterfort/wkvect.h"
    character(len=8) :: fiss, noma
    character(len=24) :: lismae, lisnoe
    logical :: goinop
!
! ----------------------------------------------------------------------
!
! ROUTINE XFEM (CREATION DES SD)
!
! LECTURE DONNEES GROUP_MA_ENRI
!
! ----------------------------------------------------------------------
!
!
! OUT FISS   : NOM DE LA SD FISS_XFEM
!                 FISS//'.GROUP_MA_ENRI'
!                 FISS//'.GROUP_NO_ENRI'
! IN  NOMA   : NOM DU MAILLAGE
! IN  GOINOP : .TRUE.  SI  PASSAGE DANS OPOO10
!              .FALSE. SINON
! OUT LISMAE : NOM DE LA LISTE DES MAILLES ENRICHIES
! OUT LISNOE : NOM DE LA LISTE DES NOEUDS ENRICHIS
!
!
!
!
    integer :: nbmae, nbnoe, n, jmae, jnoe, i
    character(len=8) :: k8b
    integer :: iarg
!
! ----------------------------------------------------------------------
!
    call jemarq()
    k8b=' '
!
!     RECUPERATION DU MOT-CLE FACULTATIF GROUP_MA_ENRI
!     (GOINOP PERMET DE DISTINGUER OP0041 et OP0010)
    if (.not.goinop) then
        call getvem(noma, 'GROUP_MA', ' ', 'GROUP_MA_ENRI', 1,&
                    iarg, 0, k8b, n)
    else
        n=0
    endif
!
    if (n .eq. 0) then
!
!       GROUP_MA_ENRI N'EST PAS RENSEIGNE : ON PREND TOUT LE MAILLAGE
!
        call dismoi('NB_MA_MAILLA', noma, 'MAILLAGE', repi=nbmae)
        call dismoi('NB_NO_MAILLA', noma, 'MAILLAGE', repi=nbnoe)
!
        call wkvect(lismae, 'V V I  ', nbmae, jmae)
        call wkvect(lisnoe, 'V V I  ', nbnoe, jnoe)
!
        do 10 i = 1, nbmae
            zi(jmae-1+i)=i
 10     continue
!
        do 20 i = 1, nbnoe
            zi(jnoe-1+i)=i
 20     continue
!
    else
!
!       GROUP_MA_ENRI EST RENSEIGNE
!
        call reliem(' ', noma, 'NU_MAILLE', ' ', 1,&
                    1, 'GROUP_MA_ENRI', 'GROUP_MA', lismae, nbmae)
!
        call reliem(' ', noma, 'NU_NOEUD', ' ', 1,&
                    1, 'GROUP_MA_ENRI', 'GROUP_MA', lisnoe, nbnoe)
!
    endif
!
!     ENREGISTREMENT DANS LA BASE GLOBALE
    call jedupo(lismae, 'G', fiss(1:8)//'.GROUP_MA_ENRI', .false.)
    call jedupo(lisnoe, 'G', fiss(1:8)//'.GROUP_NO_ENRI', .false.)
!
    call jedema()
end subroutine
