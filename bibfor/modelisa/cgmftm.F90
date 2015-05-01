subroutine cgmftm(tymaz, nomaz, lisma, nbma, ierr)
    implicit none
    integer :: nbma, ierr
    character(len=*) :: nomaz, tymaz
! ----------------------------------------------------------------------
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
!       OPERATEUR: DEFI_GROUP/CREA_GROUP_MA
!
!       CGMFTM -- TRAITEMENT DU FILTRE DES MAILLES
!                 EN FONCTION DE LEUR TYPE.
!
! -------------------------------------------------------
!  TYMA          - IN    - K8   - : TYPE DE MAILLE RETENU
!                                   ("0D","1D","2D","3D",
!                                    "POI1","SEG2","QUAD4",...,)
!  NOMAZ         - IN    - K8   - : NOM DU MAILLAGE
!  LISMAZ        - INOUT - K24  - : NOM DE LA LISTE DE MAILLES A FILTRER
!                                   ET FILTREE
!  NBMA          - INOUT -  I   - : LONGUEUR DE CETTE LISTE
!  IERR          - OUT   -  I   - : CODE RETOUR (=0 OU 1)
!
!
!  REMARQUES :
!     IERR=0 : OK
!        => ON A OBTENU DES MAILLES EN FILTRANT LA LISTE DE MAILLES,
!           LA LISTE DE MAILLES RETOURNEE EST LA LISTE FILTREE,
!           LE NOMBRE DE MAILLES RETOURNE EST LA LONGUEUR DE LA LISTE
!           FILTREE.
!     IERR=1 : NOOK
!        => AUCUNE MAILLE N'A ETE RETENUE CAR LE TYPE DE MAILLE SOUHAITE
!           PAR L'UTILISATEUR NE CORRESPOND PAS AUX TYPES DES MAILLES
!           DE LA LISTE.
!           LA LISTE DE MAILLE RETOURNEE EST LA LISTE INITIALE,
!           LE NOMBRE DE MAILLES RETOURNE EST LA LONGUEUR DE LA LISTE
!           INITIALE.
!
! ----------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utflmd.h"
!
! ----------------------------------------------------------------------
!
    integer :: i, dim, jlima, jlimaf, nbmaf
    character(len=8) :: noma, tyma, typmai
    character(len=24) :: lisma, lismaf
!
    call jemarq()
!
    noma=nomaz
    tyma=tymaz
    lismaf='&&CGMFTM.MAILLES_FILTRE'
!
    if (tyma .eq. '0D') then
        dim = 0
    else if (tyma.eq.'1D') then
        dim = 1
    else if (tyma.eq.'2D') then
        dim = 2
    else if (tyma.eq.'3D') then
        dim = 3
    else
!       -- ON FILTRE SUR LE LE NOM D'UN TYPE DE MAILLE :
        dim=-1
        typmai=tyma
    endif
!
    call utflmd(noma, lisma, nbma, dim, typmai, &
                nbmaf, lismaf)
!
    if (nbmaf .eq. 0) then
        ierr = 1
    else
        ierr = 0
        nbma = nbmaf
        call jeveuo(lismaf, 'L', jlimaf)
        call jeveuo(lisma, 'E', jlima)
        do i = 1, nbma
            zi(jlima+i-1)=zi(jlimaf+i-1)
        end do
!
    endif
!
    call jedetr(lismaf)
!
    call jedema()
!
end subroutine
