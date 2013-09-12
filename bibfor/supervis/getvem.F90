subroutine getvem(noma, typent, motfac, motcle, iocc,&
                  iarg, mxval, vk, nbval)
    implicit none
#include "asterfort/getvtx.h"
#include "asterfort/verima.h"
    character(len=*) :: noma, typent, motfac, motcle, vk(*)
    integer :: iocc, iarg, mxval, nbval
!     ------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!       RECUPERATION DES VALEURS D'UNE LISTE (VOIR POINT D'ENTREE)
!     ------------------------------------------------------------------
! IN  MOTFAC : CH*(*) : MOT CLE FACTEUR
!          CONVENTION :  POUR UN MOT CLE SIMPLE   MOTFAC = ' '
! IN  MOTCLE : CH*(*) : MOT CLE
! IN  IOCC   : IS     : IOCC-IEME OCCURENCE DU MOT-CLE-FACTEUR
! IN  IARG   : IS     : IARG-IEME ARGUMENT DEMANDE
! IN  MXVAL  : IS     : TAILLE MAXIMUM DU TABLEAU PASSE
!                     :                   (RELATIVEMENT AU TYPE)
!
! OUT   VAL  : ----   : TABLEAU DES VALEURS A FOURNIR
! OUT NBVAL  : IS     : NOMBRE DE VALEUR FOURNIT
!          CONVENTION : SI NBVAL = 0 ==> IL N'Y A PAS DE VALEUR
!                       SI NBVAL < 0 ==> NOMBRE DE VALEUR DE LA LISTE
!                                        SACHANT QUE L'ON NE FOURNIT QUE
!                                        LES MXVAL PREMIERES
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: mm
!-----------------------------------------------------------------------
    call getvtx(motfac, motcle, iocc=iocc, nbval=mxval, vect=vk,&
                nbret=nbval)
    if (mxval .ne. 0) then
        mm=min(mxval,abs(nbval))
        call verima(noma, vk, mm, typent)
    endif
end subroutine
