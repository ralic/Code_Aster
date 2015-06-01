subroutine extmod_sorted(basemo, numddl, nume, nbnumo, dmode,&
                  nbeq, nbnoe, iddl, nbddl)
    implicit none
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
! EXTRAIRE D'UN CONCEPT MODE_MECA LA DEFORMEE POUR UN OU PLUSIEURS DDL
! LES LAGRANGES SONT SUPPRIMES.
!-----------------------------------------------------------------------
! IN  :BASEMO : CONCEPT DE TYPE MODE_MECA
! IN  :NUMDDL : PERMET D'ACCEDER AU PROFIL DU CHAMP_NO EXTRAIT
! IN  :NUME   : LISTE DES NUMEROS D'ORDRE DES MODES CONSIDERES
! IN  :NBNUMO : NB DE MODES CONSIDERES
! IN  :NBEQ   : NB D'EQUATIONS
! IN  :NBNOE  : NB DE NOEUDS DU MAILLAGE
! IN  :IDDL   : LISTE DES INDICES DES DDL A EXTRAIRE
! IN  :NBDDL  : NB DE DDLS A EXTRAIRE
! OUT :DMODE  : VECTEUR => DEFORMEES MODALES
!-----------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rsexch.h"
    integer :: nbddl, nbnumo, nbnoe, nume(nbnumo), iddl(nbddl)
    real(kind=8) :: dmode(nbddl*nbnoe*nbnumo)
    character(len=8) :: basemo
    character(len=14) :: numddl
    character(len=24) :: deeq, nomcha
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: iadmod, ideeq, inumo, iret
    integer :: k, nbeq, iinod, iiddl, ieq, im
!-----------------------------------------------------------------------
    call jemarq()
    deeq = numddl//'.NUME.DEEQ'
    call jeveuo(deeq, 'L', ideeq)
    do im = 1, nbnumo
        inumo = nume(im)
        call rsexch('F', basemo, 'DEPL', inumo, nomcha,&
                    iret)
        nomcha = nomcha(1:19)//'.VALE'
        call jeveuo(nomcha, 'L', iadmod)
        do ieq = 1, nbeq
!           --- iinod is the node index w.r.t the order in the original mesh file
            iinod = zi(ideeq+(2*ieq)-2)
            iiddl = zi(ideeq+(2*ieq)-1)
            do k = 1, nbddl
                if (iiddl .eq. iddl(k)) then
                    dmode((im-1)*nbnoe*nbddl+(iinod-1)*nbddl+k) = zr(iadmod+ieq-1)
                    goto 22
                endif
            end do
22          continue
        end do
    end do
!
    call jedema()
end subroutine
