subroutine srlima(mo, mail2d, mail3d, mailto, nbma2d)
    implicit none
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/reliem.h"
#include "asterfort/utflmd.h"
#include "asterfort/utmamo.h"
#include "asterfort/utmasu.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    integer :: nbma2d
    character(len=8) :: mo
    character(len=24) :: mail2d, mail3d, mailto
! ----------------------------------------------------------------------
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     BUT: CONSTRUIRE 3 LISTES DE MAILLES A PARTIR DES DONNEES
!          UTILISATEUR :
!          LISTE DES MAILLES 2D
!          LISTE DES MAILLES 3D SOUS-JACENTES
!          LISTE DE L'ENSEMBLE DES MAILLES 2D + MAILLES 3D SOUS-JACENTES
!
!     IN  MO     : NOM DU MODELE
!     IN  MAIL2D : NOM OBJET JEVEUX CONTENANT LA LISTE DES MAILLES 2D
!     IN  MAIL3D : NOM OBJET JEVEUX CONTENANT LA LISTE DES MAILLES 2D
!     IN  MAILTO : NOM OBJET JEVEUX CONTENANT LA LISTE DES MAILLES 2D+3D
!     OUT NBMA2D : NOMBRE DE MAILLES 2D TROUVEES == NB MAILLES 3D
!
! ----------------------------------------------------------------------
!
!
    integer :: jma2d, jcoor, jma3d
    integer :: ima
    integer :: nbma, nbmamo, jlima, nbmat, jmato
!
    character(len=8) :: ma, limocl(3), tymocl(3)
    character(len=24) :: mesmai, limamo
!
    data limocl/'TOUT','MAILLE','GROUP_MA'/
    data tymocl/'TOUT','MAILLE','GROUP_MA'/
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ON RECUPERE LES MAILLES DE PEAU
    call dismoi('NOM_MAILLA', mo, 'MODELE', repk=ma)
    call dismoi('NB_MA_MAILLA', ma, 'MAILLAGE', repi=nbmat)
    mesmai = '&&SRLIMA.MAILLU'
    call reliem(mo, ma, 'NU_MAILLE', ' ', 0,&
                3, limocl, tymocl, mesmai, nbma)
!
! --- ON NE GARDE QUE LES MAILLES SURFACIQUES
    call utflmd(ma, mesmai, nbma, 2, ' ',&
                nbma2d, mail2d)
    if (nbma2d .gt. 0) then
        call jeveuo(mail2d, 'L', jma2d)
    else
        call utmess('F', 'CALCULEL5_54')
    endif
!
! --- ON RECHERCHE LA MAILLES 3D SUPPORT DE CHAQUE MAILLE 2D FOURNIE
! --- IL FAUT SE PROTEGER DES MAILLES QUI NE FONT PAS PARTIE DU MODELE :
    limamo = '&&SRLIMA.LIMAIL'
    call utmamo(mo, nbmamo, limamo)
    call jeveuo(limamo, 'L', jlima)
!
    call jeveuo(ma//'.COORDO    .VALE', 'L', jcoor)
    call utmasu(ma, '3D', nbma2d, zi(jma2d), mail3d,&
                zr(jcoor), nbmamo, zi(jlima), .true.)
    call jeveuo(mail3d, 'L', jma3d)
!
    call wkvect(mailto, 'V V I', nbma2d*2, jmato)
!
    do ima = 1, nbma2d
        zi(jmato-1+ima) = zi(jma2d-1+ima)
        zi(jmato-1+nbma2d+ima) = zi(jma3d-1+ima)
    end do
!
    call jedetr(mesmai)
    call jedetr(limamo)
!
    call jedema()
!
end subroutine
