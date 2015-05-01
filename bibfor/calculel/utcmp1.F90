subroutine utcmp1(nomgd, mcfac, iocc, nomcmp, ivari)
    implicit none
#include "asterfort/assert.h"
#include "asterfort/getvtx.h"
#include "asterfort/lxliis.h"
#include "asterfort/utmess.h"
    integer :: iocc
    character(len=8) :: nomgd, nomcmp
    character(len=*) :: mcfac
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!  BUT : SCRUTER LE MOT CLE NOM_CMP ET RENDRE LE NOM DE LA CMP
!        SI LA GRANDEUR EST VARI_R, RENDRE EGALEMENT LE NUMERO (N) DE VN
!  ATTENTION : IL NE FAUT UTILISER CETTE ROUTINE QUE SI LE MOT CLE
!              NOM_CMP N'ATTEND QU'UNE SEULE VALEUR.
!
!  ARGUMENTS :
!  -----------
!  NOMGD  IN  K8 : NOM DE LA GRANDEUR CONCERNEE
!  MCFAC  IN  K* : NOM DU MOT CLE FACTEUR A SCRUTER
!  IOCC   IN  I  : NUMERO DE L'OCCURRENCE DE MCFAC
!  NOMCMP OUT K8 : NOM DE LA COMPOSANTE TROUVEE DERRIERE MCFAC/NOM_CMP
!  IVARI  OUT I  : NUMERO DE LA VARIABLE INTERNE SI NOMGD='VARI_R'
!                  0 SINON
!
! ----------------------------------------------------------------------
    integer :: ibid, n2, iret, ivari
    character(len=24) :: valk(2)
!     ------------------------------------------------------------------
!
    call getvtx(mcfac, 'NOM_CMP', iocc=iocc, scal=nomcmp, nbret=n2)
    ASSERT(n2.eq.1)
!
!
    if (nomgd .eq. 'VARI_R') then
!     ------------------------------
        call lxliis(nomcmp(2:8), ibid, iret)
        ivari=ibid
!
        if ((nomcmp(1:1).ne.'V') .or. (iret.ne.0)) then
            valk (1) = nomcmp
            valk (2) = 'VARI_R'
            call utmess('F', 'CALCULEL6_49', nk=2, valk=valk)
        endif
!
!     -- SI GRANDEUR /= VARI_R :
!     --------------------------
    else
        ivari=0
    endif
!
end subroutine
