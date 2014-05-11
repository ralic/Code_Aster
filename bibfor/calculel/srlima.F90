subroutine srlima(mo, mail2d, mail3d, mailto, nbma2d)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/alchml.h"
#include "asterfort/celces.h"
#include "asterfort/cesexi.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/detrsd.h"
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
    integer :: ima,iret,jcesd,jcesl,iad1
    integer :: nbma, nbmamo, jlima, nbmat, jmato
!
    character(len=8) :: ma, limocl(3), tymocl(3)
    character(len=24) :: mesmai, limamo
    character(len=19) :: ces,cel
!
    data limocl/'TOUT','MAILLE','GROUP_MA'/
    data tymocl/'TOUT','MAILLE','GROUP_MA'/
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
!   -- on recupere les mailles de peau
    call dismoi('NOM_MAILLA', mo, 'MODELE', repk=ma)
    call dismoi('NB_MA_MAILLA', ma, 'MAILLAGE', repi=nbmat)
    mesmai = '&&SRLIMA.MAILLU'
    call reliem(mo, ma, 'NU_MAILLE', ' ', 0,&
                3, limocl, tymocl, mesmai, nbma)


!   -- on ne garde que les mailles surfaciques :
!   ---------------------------------------------
    call utflmd(ma, mesmai, nbma, 2, ' ',&
                nbma2d, mail2d)
    if (nbma2d .gt. 0) then
        call jeveuo(mail2d, 'L', jma2d)
    else
        call utmess('F', 'CALCULEL5_54')
    endif


!   -- on recherche les mailles 3d qui bordent les mailles de peau :
!   -- il faut se limiter aux mailles du modele qui savent calculer SIGM_ELNO :
!   ----------------------------------------------------------------------------
    cel='&&SRLIMA.CEL_ELNO'
    ces='&&SRLIMA.CES_ELNO'
    call alchml(mo//'.MODELE', 'SIGM_ELNO', 'PSIEFNOR', 'V', cel, iret, ' ')
    ASSERT(iret.eq.0)
    call celces(cel, 'V', ces)
    call detrsd('CHAMP', cel)
    call jeveuo(ces//'.CESD', 'L', jcesd)
    call jeveuo(ces//'.CESL', 'L', jcesl)
    nbma = zi(jcesd-1+1)
    limamo = '&&SRLIMA.LIMAIL'
    call wkvect(limamo,'V V I',nbma,jlima)
    nbmamo=0
    do ima=1,nbma
       call cesexi('C', jcesd, jcesl, ima, 1, 1, 1, iad1)
       if (iad1.gt.0) then
         nbmamo=nbmamo+1
         zi(jlima-1+nbmamo)=ima
       endif
    enddo
    call detrsd('CHAM_ELEM_S', ces)


!   -- on recherche les mailles 3d associees aux mailles de peau :
!   ---------------------------------------------------------------
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
