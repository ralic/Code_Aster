subroutine utcmp1(nomgd, mcfac, iocc, nomcmp, ivari, nom_vari)
    implicit none
#include "asterfort/assert.h"
#include "asterfort/getvtx.h"
#include "asterfort/lxliis.h"
#include "asterfort/utmess.h"
    integer :: iocc
    character(len=8) :: nomgd, nomcmp
    character(len=*) :: mcfac
    character(len=16) :: nom_vari
! ----------------------------------------------------------------------
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
!
!  but : Scruter le mot cle NOM_CMP et rendre le nom de la cmp
!        Si la grandeur est VARI_R, rendre egalement le numero (n) de vn
!  attention : il ne faut utiliser cette routine que si le mot cle
!              NOM_CMP n'attend qu'une seule valeur.
!
!  arguments :
!  -----------
!  nomgd  in  k8 : nom de la grandeur concernee
!  mcfac  in  k* : nom du mot cle facteur a scruter
!  iocc   in  i  : numero de l'occurrence de mcfac
!  nomcmp out k8 : nom de la composante trouvee derriere mcfac/nom_cmp
!  ivari  out i  : numero de la variable interne si nomgd='VARI_R'
!                   0 Si nomgd /= 'VARI_R'
!                  -1 si nomgd='VARI_R' + motcle NOM_VARI.
!
! ----------------------------------------------------------------------
    integer :: ibid, n2, iret, ivari
    character(len=24) :: valk(2)
!     ------------------------------------------------------------------
!
!
    nom_vari = ' '
!
    if (nomgd .eq. 'VARI_R') then
!     ------------------------------
        call getvtx(mcfac, 'NOM_CMP', iocc=iocc, scal=nomcmp, nbret=n2)
        ASSERT(n2.eq.1 .or. n2.eq.0)
        if (n2.eq.0) then
            call getvtx(mcfac, 'NOM_VARI', iocc=iocc, scal=nom_vari, nbret=n2)
            ASSERT(n2.eq.1)
            ivari=-1
        else
            call lxliis(nomcmp(2:8), ibid, iret)
            ivari=ibid
            if ((nomcmp(1:1).ne.'V') .or. (iret.ne.0)) then
                valk (1) = nomcmp
                valk (2) = 'VARI_R'
                call utmess('F', 'CALCULEL6_49', nk=2, valk=valk)
            endif
        endif
!
!     -- SI GRANDEUR /= VARI_R :
!     --------------------------
    else
        call getvtx(mcfac, 'NOM_CMP', iocc=iocc, scal=nomcmp, nbret=n2)
        ASSERT(n2.eq.1)
        ivari=0
    endif
!
end subroutine
