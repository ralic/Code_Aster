subroutine asdir(monoap, muapde, id, neq, nbsup,&
                 nsupp, tcosup, recmod, repdir)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
    integer :: id, neq, nbsup, nsupp(*), tcosup(nbsup, *)
    real(kind=8) :: recmod(nbsup, neq, *), repdir(neq, *)
    aster_logical :: monoap, muapde
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
!     ------------------------------------------------------------------
!     COMMANDE : COMB_SISM_MODAL
!        CALCUL DES REPONSES DIRECTIONNELLES DES SUPPORTS
!     ------------------------------------------------------------------
! IN  : MONOAP : =.TRUE.  , CAS DU MONO-SUPPORT
!                =.FALSE. , CAS DU MULTI-SUPPORT
! IN  : MUAPDE : =.TRUE.  , CAS DU MULTI-SUPPORTS DECORRELES
!                =.FALSE. , CAS DU MULTI-SUPPORTS CORRELES
! IN  : ID     : LA DIRECTION
! IN  : NEQ    : NOMBRE D'EQUATIONS
! IN  : NBSUP  : NOMBRE DE SUPPORTS
! IN  : NSUPP  : MAX DU NOMBRE DE SUPPORT PAR DIRECTION
! IN  : TCOSUP : VECTEUR DES TYPES DE RECOMBINAISON DES SUPPORTS
! IN  : RECMOD : VECTEUR DES RECOMBINAISONS MODALES PAR APPUIS
! OUT : REPDIR : VECTEUR DES RECOMBINAISONS PAR DIRECTIONS
!     ------------------------------------------------------------------
    integer :: in, is, jabs
    real(kind=8) :: xxx, xx1, xx2
    real(kind=8), pointer :: line(:) => null()
    real(kind=8), pointer :: quad(:) => null()
!     ------------------------------------------------------------------
!
    call jemarq()
!
    if (monoap .or. .not.muapde) then
        do 10 in = 1, neq
            repdir(in,id)=recmod(1,in,id)
 10     continue
    else
        AS_ALLOCATE(vr=quad, size=neq)
        AS_ALLOCATE(vr=line, size=neq)
        call wkvect('&&ASDIR.ABS ', 'V V R', neq, jabs)
        do 20 is = 1, nsupp(id)
            if (tcosup(is,id) .eq. 1) then
!              --- COMBINAISON QUADRATIQUE ---
                do 12 in = 1, neq
                    xxx = recmod(is,in,id)
                    quad(in)= quad(in)+ xxx
 12             continue
            else if (tcosup(is,id).eq.2) then
!              --- COMBINAISON LINEAIRE ---
                do 14 in = 1, neq
                    if (recmod(is,in,id) .ge. 0.d0) then
                        xxx = sqrt(recmod(is,in,id))
                        line(in)= line(in)+ xxx
                    endif
 14             continue
            else
!              --- COMBINAISON VALEUR ABSOLUE ---
                do 16 in = 1, neq
                    xxx = sqrt(abs(recmod(is,in,id)))
                    zr(jabs+in-1)= zr(jabs+in-1)+ xxx
 16             continue
            endif
 20     continue
        do 30 in = 1, neq
            xx1 = line(in) * line(in)
            xx2 = zr(jabs+in-1) * zr(jabs+in-1)
            repdir(in,id) = quad(in)+xx1+xx2
 30     continue
        AS_DEALLOCATE(vr=quad)
        AS_DEALLOCATE(vr=line)
        call jedetr('&&ASDIR.ABS ')
    endif
    call jedema()
end subroutine
