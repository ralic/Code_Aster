subroutine chbord(nomo, nbmail, listma, mabord, nbmapr,&
                  nbmabo)
    implicit   none
#include "jeveux.h"
!
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/teattr.h"
    integer :: nbmail, listma(*), mabord(*), nbmapr, nbmabo
    character(len=*) :: nomo
! ======================================================================
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!      OPERATEURS :     AFFE_CHAR_MECA ET AFFE_CHAR_MECA_C
!                                      ET AFFE_CHAR_MECA_F
!
! IN  : NOMO   : NOM DU MODELE
! IN  : NBMAIL : NOMBRE DE MAILLES
! IN  : LISTMA : LISTE DES NUMEROS DE MAILLE
! OUT : MABORD : MABORD(IMA) = 0 , MAILLE "PRINCIPAL"
!                MABORD(IMA) = 1 , MAILLE "BORD"
! OUT : NBMAPR : NOMBRE DE MAILLES "PRINCIPAL"
! OUT : NBMABO : NOMBRE DE MAILLES "BORD"
!
!-----------------------------------------------------------------------
    integer :: iret, nbgrel, igrel, ialiel, nel, itypel, ima, ier, numail, iel
    integer :: traite
    character(len=1) :: k1b
    character(len=8) :: modele, dmo, dma
    character(len=16) :: nomte
    character(len=19) :: nolig
!     ------------------------------------------------------------------
!
    modele = nomo
    nbmapr = 0
    nbmabo = 0
    nolig = modele//'.MODELE'
!
    call jeexin(nolig//'.LIEL', iret)
    if (iret .eq. 0) goto 999
!
    call jelira(nolig//'.LIEL', 'NUTIOC', nbgrel, k1b)
    if (nbgrel .le. 0) goto 999
!
    traite = 0
    do 10 igrel = 1, nbgrel
        call jeveuo(jexnum(nolig//'.LIEL', igrel), 'L', ialiel)
        call jelira(jexnum(nolig//'.LIEL', igrel), 'LONMAX', nel, k1b)
        itypel = zi(ialiel -1 +nel)
        call jenuno(jexnum('&CATA.TE.NOMTE', itypel), nomte)
        do 20 ima = 1, nbmail
            numail = listma(ima)
            do 30 iel = 1, nel-1
                if (numail .eq. zi(ialiel-1+iel)) then
                    traite = traite + 1
                    call teattr(nomte, 'S', 'DIM_TOPO_MODELI', dmo, ier)
                    call teattr(nomte, 'S', 'DIM_TOPO_MAILLE', dma, ier)
                    if (dmo .eq. dma) then
!                    on a un element principal
                        mabord(ima) = 0
                        nbmapr = nbmapr + 1
                    else
!                    on a un element de bord
                        mabord(ima) = 1
                        nbmabo = nbmabo + 1
                    endif
                    goto 20
                endif
30          continue
20      continue
        if (traite .eq. nbmail) goto 999
10  end do
!
999  continue
!
end subroutine
