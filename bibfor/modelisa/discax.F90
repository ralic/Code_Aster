subroutine discax(noma, nbn, iaxe, nuno, diax)
    implicit none
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!     CREATION D'UNE LISTE ORDONNEE DE NOEUDS SUR UNE STRUCTURE POUTRE
!     DROITE : ORDRE CROISSANT DU PARAMETRE LE LONG DE L'AXE DIRECTEUR
!     DE LA POUTRE
!     APPELANT : SPECFF
!-----------------------------------------------------------------------
! IN  : NOMA   : NOM DU CONCEPT MAILLAGE
! IN  : NBN    : NOMBRE DE NOEUDS DU MAILLAGE
! IN  : IAXE   : ENTIER DEFINISSANT L'AXE DIRECTEUR
!       IAXE = 1 L'AXE DIRECTEUR EST L'AXE DES X DU REPERE GLOBAL
!       IAXE = 2 L'AXE DIRECTEUR EST L'AXE DES Y DU REPERE GLOBAL
!       IAXE = 3 L'AXE DIRECTEUR EST L'AXE DES Z DU REPERE GLOBAL
! OUT : NUNO   : LISTE DES NUMEROS DES NOEUDS DU MAILLAGE, REORDONNEE
!                PAR VALEURS CROISSANTES DU PARAMETRE LE LONG DE L'AXE
! OUT : DIAX   : LISTE DES VALEURS DU PARAMETRE LE LONG DE L'AXE
!                ORDRE CROISSANT
!
!
#include "jeveux.h"
!
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
    character(len=8) :: noma
    integer :: nbn, iaxe, nuno(nbn)
    real(kind=8) :: diax(nbn)
!
    character(len=8) :: nomnoe
    character(len=24) :: coorma, nnoema
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: icoma, imin, innoe, ino, jno
    real(kind=8) :: xmin
!-----------------------------------------------------------------------
    call jemarq()
!
! --- 1.ACCES AUX OBJETS DU CONCEPT MAILLAGE
!
    coorma = noma//'.COORDO    .VALE'
    call jeveuo(coorma, 'L', icoma)
    nnoema = noma//'.NOMNOE'
!
! --- 2.ON RECOPIE LA DISCRETISATION (NON ORDONNEE) LUE DANS L'OBJET
! ---   .COORDO    .VALE DU CONCEPT MAILLAGE
! ---   ON RECOPIE SIMULTANEMENT LA LISTE DES NOMS DES NOEUDS
!
    call wkvect('&&DISCAX.TEMP.NNOE', 'V V K8', nbn, innoe)
    do 10 ino = 1, nbn
        diax(ino) = zr(icoma+3*(ino-1)+iaxe-1)
        call jenuno(jexnum(nnoema, ino), zk8(innoe+ino-1))
10  end do
!
! --- 3.ON REORDONNE LA DISCRETISATION PAR VALEURS CROISSANTES
! ---   ON REORDONNE SIMULTANEMENT LA LISTE DES NOMS DES NOEUDS
! ---   ON EN DEDUIT LA LISTE ORDONNEE DES NUMEROS DES NOEUDS
!
    do 20 ino = 1, nbn-1
        xmin = diax(ino)
        nomnoe = zk8(innoe+ino-1)
        imin = ino
        do 21 jno = ino+1, nbn
            if (diax(jno) .lt. xmin) then
                xmin = diax(jno)
                nomnoe = zk8(innoe+jno-1)
                imin = jno
            endif
21      continue
        diax(imin) = diax(ino)
        zk8(innoe+imin-1) = zk8(innoe+ino-1)
        diax(ino) = xmin
        zk8(innoe+ino-1) = nomnoe
        call jenonu(jexnom(nnoema, zk8(innoe+ino-1)), nuno(ino))
20  end do
    call jenonu(jexnom(nnoema, zk8(innoe+nbn-1)), nuno(nbn))
!
    call jedetr('&&DISCAX.TEMP.NNOE')
    call jedema()
end subroutine
