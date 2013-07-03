subroutine utcmp2(nomgd, mcfac, iocc, dim, nomcmp,&
                  numcmp, nbcmp)
    implicit   none
#include "jeveux.h"
!
#include "asterc/getres.h"
#include "asterc/getvtx.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/knincl.h"
#include "asterfort/lxliis.h"
#include "asterfort/u2mesk.h"
    integer :: iocc, dim, nbcmp, numcmp(*)
    character(len=*) :: nomgd, mcfac, nomcmp(*)
! ----------------------------------------------------------------------
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
! BUT :  SCRUTER LE MOT CLE MFAC/NOM_CMP ET RENDRE LA LISTE DES CMPS
! -----
!  ARGUMENTS :
!  -----------
!  NOMGD  IN  K8 : NOM DE LA GRANDEUR CONCERNEE
!  MCFAC  IN  K* : NOM DU MOT CLE FACTEUR A SCRUTER
!  IOCC   IN  I  : NUMERO DE L'OCCURRENCE DE MCFAC
!  DIM    IN  I  : LONGUEUR DES TABLEAUX NOMCMP ET NUMCMP
!
!  NOMCMP(*) OUT K8 : NOMS DES COMPOSANTES TROUVEES
!  NUMCMP(*) OUT I  : NUMEROS DES COMPOSANTES TROUVEES (SI VARI_R)
!  NBCMP     OUT I  : NOMBRE DE CMPS TROUVEES
!
! ----------------------------------------------------------------------
    integer :: n2, i, nucmp, iret, jnocmp, lgncmp
    character(len=8) :: k8b, nocmp
    character(len=16) :: nomcmd
    integer :: iarg
!     ------------------------------------------------------------------
!
    call jemarq()
    call getres(k8b, k8b, nomcmd)
!
!
    call getvtx(mcfac, 'NOM_CMP', iocc, iarg, 0,&
                k8b, n2)
    nbcmp=-n2
    call assert(dim.ge.nbcmp)
!
    call getvtx(mcfac, 'NOM_CMP', iocc, iarg, nbcmp,&
                nomcmp, n2)
!
!
    if (nomgd(1:6) .eq. 'VARI_R') then
!     -----------------------------------------
        do 10 i = 1, nbcmp
            nocmp=nomcmp(i)
            call assert(nocmp(1:1).eq.'V')
            call lxliis(nocmp(2:8), nucmp, iret)
            call assert(iret.eq.0)
            numcmp(i)=nucmp
10      continue
!
!
!     -- CAS NOMGD /= VARI_R
!     -----------------------
    else
        call jeveuo(jexnom('&CATA.GD.NOMCMP', nomgd), 'L', jnocmp)
        call jelira(jexnom('&CATA.GD.NOMCMP', nomgd), 'LONMAX', lgncmp, k8b)
        call knincl(8, nomcmp, nbcmp, zk8(jnocmp), lgncmp,&
                    iret)
        if (iret .ne. 0) call u2mesk('F', 'CALCULEL5_6', 1, nomgd)
    endif
!
!
!
    call jedema()
end subroutine
