subroutine i2imam(conec, type, lstmai, nbmlst, chemin,&
                  ptchm, nbchm, mail1, mail2)
    implicit none
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!*********************************************************************
!
!     REPERAGE D' UN ENSEMBLE DE MAILLES LINEIQUES DANS UN
!     MAILLAGE 2D
!
!       CONEC  (IN)  : TABLE DE CONNECTIVITE DU MAILLAGE
!
!       TYPE   (IN)  : TABLE DES TYPE DE MAILLES
!
!       LSTMAI (IN)  : ENSEMBLE DE MAILLES A TRAITE
!
!       NBMLST (IN)  : NBR DE MAILLES DE L' ENSEMBLE A TRAITE
!
!       CHEMIN (OUT) : TABLE DE STRUCTURE DES CHEMINS TROUVES
!
!       PTCHM  (OUT) : TABLE D' ACCES A CHEMIN PAR NUMERO DE CHEMIN
!
!       NBCHM  (OUT) : NBR DE CHEMINS TROUVES
!
!       MAIL1  (OUT) : TABLE DES PREMIERES MAILLES SURFACIQUES
!                      CONTENANT UN ELEMENT DE L' ENSEMBLE
!
!       MAIL2  (OUT) : TABLE DES SECONDES MAILLES SURFACIQUES
!                      CONTENANT UN ELEMENT DE L' ENSEMBLE
!
!*********************************************************************
!
#include "jeveux.h"
#include "asterfort/i2repr.h"
#include "asterfort/i2tgrm.h"
#include "asterfort/i2vois.h"
#include "asterfort/jecreo.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=24) :: conec, type
    integer :: lstmai(*), nbmlst, chemin(*), ptchm(*), nbchm
    integer :: mail1(*), mail2(*)
!
!
!
!
    integer :: av1, av2, i
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    call jemarq()
    i = 0
    av1 = 0
    av2 = 0
!
    call jecreo('&INTVOISIN1', 'V V I')
    call jeecra('&INTVOISIN1', 'LONMAX', nbmlst)
    call jeveuo('&INTVOISIN1', 'E', av1)
!
    call jecreo('&INTVOISIN2', 'V V I')
    call jeecra('&INTVOISIN2', 'LONMAX', nbmlst)
    call jeveuo('&INTVOISIN2', 'E', av2)
!
    do 10, i = 1, nbmlst, 1
!
    zi(av1 + i-1) = 0
    zi(av2 + i-1) = 0
!
10  continue
!
    call i2vois(conec, type, lstmai, nbmlst, zi(av1),&
                zi(av2))
    call i2tgrm(zi(av1), zi(av2), nbmlst, chemin, ptchm,&
                nbchm)
    call i2repr(conec, type, lstmai, chemin, ptchm,&
                nbchm, mail1, mail2)
!
    call jedetr('&INTVOISIN1')
    call jedetr('&INTVOISIN2')
!
    call jedema()
end subroutine
