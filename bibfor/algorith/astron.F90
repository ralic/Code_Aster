subroutine astron(nomsy, psmo, monoap, muapde, nbsup,&
                  nsupp, neq, nbmode, id, vecmod,&
                  momec, gamma0, nomsup, reasup, recmor,&
                  recmop, nopara, nordr)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsorac.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    integer :: nbsup, nsupp(*), neq, nbmode, id, tordr(1), nordr(*)
    real(kind=8) :: vecmod(neq, *), gamma0(*)
    real(kind=8) :: reasup(nbsup, nbmode, *), recmop(nbsup, neq, *)
    real(kind=8) :: recmor(nbsup, neq, *)
    character(len=16) :: nomsy, nopara(*)
    character(len=*) :: psmo, nomsup(nbsup, *), momec
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
!        CALCUL DES TERMES DE TRONCATURES
!     ------------------------------------------------------------------
! IN  : NOMSY  : OPTION DE CALCUL
! IN  : PSMO   : PSEUDO-MODES
! IN  : MONOAP : =.TRUE.  , CAS DU MONO-SUPPORT
!                =.FALSE. , CAS DU MULTI-SUPPORT
! IN  : MUAPDE : =.TRUE.  , CAS DU MULTI-SUPPORTS DECORRELES
!                =.FALSE. , CAS DU MULTI-SUPPORTS CORRELES
! IN  : NBSUP  : NOMBRE DE SUPPORTS
! IN  : NEQ    : NOMBRE D'EQUATIONS
! IN  : NBMODE : NOMBRE DE MODES
! IN  : ID     : LA DIRECTION DE CALCUL
! IN  : VECMOD : VECTEUR DES DEFORMEES MODALES
! IN  : MOMEC  : MODES MECANIQUES
! IN  : GAMMA0 : TABLEAU DES CORRECTIONS STATIQUES (PAR SUPPORT ET DIRECTION) 
! IN  : NOMSUP : VECTEUR DES NOMS DES SUPPORTS
! IN  : REASUP : VECTEUR DES REACTIONS MODALES AUX SUPPORTS
! OUT : RECMOP : VECTEUR DES COMBINAISONS DES REPONSES PERIO DES MODES
! OUT : RECMOR : VECTEUR DES COMBINAISONS DES REPONSES RIGIDES DES MODES
! IN  : NORDR  : LISTE DES NUMEROS DE MODES
!     ------------------------------------------------------------------
    integer :: ibid, im, in, iordr, iret, is, jmod, jvale, nbtrou, ival
    real(kind=8) :: r8b, rni, un, xxx, omega2
    complex(kind=8) :: cbid
    character(len=8) :: k8b, noeu, cmp, nomcmp(3)
    character(len=16) :: monacc, acces(3)
    character(len=19) :: chextr
!     ------------------------------------------------------------------
    data nomcmp / 'DX' , 'DY' , 'DZ' /
    data acces  / 'ACCE    X       ' , 'ACCE    Y       ',&
     &              'ACCE    Z       ' /
!     ------------------------------------------------------------------
!
    call jemarq()
    un = 1.d0
    if (nomsy(1:4) .ne. 'ACCE') then
        if (nomsy(1:4) .eq. 'VITE') then
            call utmess('A', 'SEISME_10', sk=nomsy)
            goto 9999
        endif
        if (monoap) then
!
!           --- CONTRIBUTION MODALE ---
            call wkvect('&&ASTRON.VECTEUR_MODA', 'V V R', neq, jmod)
            do im = 1, nbmode
                call rsadpa(momec, 'L', 1, nopara(1), nordr(im),&
                            0, sjv=ival, istop=0)
                omega2 = zr(ival)
                call rsadpa(momec, 'L', 1, nopara(2+id), nordr(im),&
                            0, sjv=ival, istop=0)
                xxx = zr(ival) / omega2
                do in = 1, neq
                    zr(jmod+in-1) = zr(jmod+in-1) + xxx*vecmod(in,im)
                enddo
            enddo
!
!           --- DEFORMEE STATIQUE ---
            call rsorac(psmo, 'NOEUD_CMP', ibid, r8b, acces(id),&
                        cbid, r8b, k8b, tordr, 1,&
                        nbtrou)
            iordr=tordr(1)            
            call rsexch('F', psmo, nomsy, iordr, chextr,&
                        iret)
            call jeexin(chextr//'.VALE', ibid)
            if (ibid .gt. 0) then
                call jeveuo(chextr//'.VALE', 'L', jvale)
            else
                call jeveuo(chextr//'.CELV', 'L', jvale)
            endif
!
            do in = 1, neq
                xxx = gamma0(id) * ( zr(jvale+in-1) - zr(jmod+in-1) )
                recmor(nbsup,in,id) = recmor(nbsup,in,id) + xxx
            enddo
            call jedetr('&&ASTRON.VECTEUR_MODA')
!
        else
!
            cmp = nomcmp(id)
            do is = 1, nsupp(id)
                noeu = nomsup(is,id)
                monacc = noeu//cmp
                call rsorac(psmo, 'NOEUD_CMP', ibid, r8b, monacc,&
                            cbid, r8b, k8b, tordr, 1,&
                            nbtrou)
                iordr=tordr(1)            
                call rsexch('F', psmo, nomsy, iordr, chextr,&
                            iret)
                call jeexin(chextr//'.VALE', ibid)
                if (ibid .gt. 0) then
                    call jeveuo(chextr//'.VALE', 'L', jvale)
                else
                    call jeveuo(chextr//'.CELV', 'L', jvale)
                endif
!
!              --- CONTRIBUTION MODALE ---
                call wkvect('&&ASTRON.VECTEUR_MODA', 'V V R', neq, jmod)
                do im = 1, nbmode
                    rni = -un*reasup(is,im,id)
                    call rsadpa(momec, 'L', 1, nopara(1), nordr(im),&
                                0, sjv=ival, istop=0)
                    omega2 = zr(ival)
                    call rsadpa(momec, 'L', 1, nopara(2), nordr(im),&
                                0, sjv=ival, istop=0)
                    xxx = rni/(zr(ival)*omega2*omega2)
                    do in = 1, neq
                        zr(jmod+in-1) = zr(jmod+in-1) + xxx*vecmod(in, im)
                    enddo
                enddo
                if (muapde) then
                    do in = 1, neq
                        xxx = gamma0(is+nbsup*(id-1)) * ( zr(jvale+in-1) - zr(jmod+in- 1) )
                        recmop(is,in,id) = recmop(is,in,id) + xxx*xxx
                    enddo
                else
                    do in = 1, neq
                        xxx = gamma0(is+nbsup*(id-1)) * ( zr(jvale+in-1) - zr(jmod+in- 1) )
                        recmop(1,in,id) = recmop(1,in,id) + xxx*xxx
                    enddo
                endif
                call jedetr('&&ASTRON.VECTEUR_MODA')
            enddo
        endif
    endif
!
9999 continue
    call jedema()
end subroutine
