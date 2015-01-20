subroutine pjeftg(igeom, geomi, nomai, motfac, iocc)
!
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
! --------------------------------------------------------------------------------------------------
!
!   Transformer la géometrie des noeuds du MAILLAGE_2 avant la la projection.
!       Cela permet par exemple de projeter :
!           - un mailllage sur un autre maillage homothetique
!           - un maillage 2d sur un maillage 3d "ecrasé" dans un plan (2d axis -> 3d axis)
!
!   in :
!       igeom       : numéro de la géométrie à transformer 1 ou 2
!       nomai       : nom du maillage initial
!
!   out :
!       geomi (k24) : nom de l'objet contenant la géométrie transformée du MAILLAGE_[1|2]
!                     si pas de géométrie transformée geomi=' '
!
! --------------------------------------------------------------------------------------------------
!
    implicit none
    integer :: igeom
    character(len=8) :: nomai
    character(len=24) :: geomi
    character(len=*) :: motfac
    integer :: iocc
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/assert.h"
#include "asterfort/copisd.h"
#include "asterfort/fointe.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvtx.h"
#include "asterfort/infniv.h"
#include "asterfort/irmail.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedupo.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/lxlgut.h"
#include "asterfort/ulnume.h"
#include "asterfort/ulopen.h"
#include "asterfort/utmess.h"
!
! --------------------------------------------------------------------------------------------------
!
    integer :: n1, nbnoi, ifonc, ibid, niveau, nkcar
    integer :: nfonc, jgeomi, inoi, ier, unite
    real(kind=8) :: vx(3)
    character(len=1) :: kgeom
    character(len=8) :: lfonc(3), lparx(3), maili, k8bid
    character(len=16) :: formar, method
    character(len=16) :: concept, nomcmd
    character(len=19) :: resout
    character(len=80) :: fichier
    aster_logical :: limpr
!
    character(len=14) :: trans12(2),geome12(2)
    data trans12/ 'TRANSF_GEOM_1',  'TRANSF_GEOM_2'  /
    data geome12/ '&&PJEFTG.GEOM1', '&&PJEFTG.GEOM2' /
!
! --------------------------------------------------------------------------------------------------
    call jemarq()
!
    ASSERT((igeom.eq.1).or.(igeom.eq.2))
!
!   Prise en compte du mot-cle TRANSF_GEOM_[1|2]
    call getvid(motfac, trans12(igeom), iocc=iocc, nbval=3, vect=lfonc, nbret=nfonc)
    ASSERT(nfonc.ge.0)
!   Transformation de la géométrie ou pas
    geomi = ' '
    maili = ' '
    if (nfonc .gt. 0) then
        ASSERT(nfonc.eq.2 .or. nfonc.eq.3)
        if (nfonc .eq. 2) lfonc(3)='&FOZERO'
        geomi = geome12(igeom)
        call jedetr(geomi)
!       Copiage du maillage initial
        maili='&&PJEFTG'
        call jedetr(maili)
        call copisd('MAILLAGE', 'V', nomai, maili)
!       Nombre de noeuds, adresse des coordonnées
        call jelira(maili//'.COORDO    .VALE', 'LONMAX', n1)
        call jeveuo(maili//'.COORDO    .VALE', 'E', jgeomi)
        nbnoi=n1/3
        ASSERT(n1.eq.nbnoi*3)
        lparx(1)='X'
        lparx(2)='Y'
        lparx(3)='Z'
!       Modification des coordonnées de 'maili'
        do inoi=1,nbnoi
            do ifonc=1,3
                call fointe('F', lfonc(ifonc), 3, lparx, zr(jgeomi+3*(inoi-1)), vx(ifonc), ier)
                ASSERT(ier.eq.0)
            enddo
            zr(jgeomi-1+3*(inoi-1)+1)=vx(1)
            zr(jgeomi-1+3*(inoi-1)+2)=vx(2)
            zr(jgeomi-1+3*(inoi-1)+3)=vx(3)
        enddo
!       Copiage de la géométrie modifiée dans geomi
        call jedupo(maili//'.COORDO    .VALE', 'V', geomi, ASTER_FALSE )
    endif

 
!   Si INFO=2, on imprime au format MED les maillages utilisés lors de la projection
!   si ceux-ci ne sont pas connus de l'utilisateur, c'est à dire si :
!     METHODE='SOUS_POINT' (pour le maillage "2")
!     et/ou si l'utilisateur a demandé TRANSF_GEOM_1/_2   
    call infniv(ibid, niveau)
    if (niveau.le.1) goto 999

    call getvtx(' ', 'METHODE', scal=method, nbret=ibid)
    if ( ibid.eq.0 ) method=' '

    limpr=.false.
    if ( method.eq.'SOUS_POINT') then
       if ((nfonc.gt.0).or.(igeom.eq.2)) limpr=.true.
    else
       if (nfonc.gt.0) limpr=.true.
    endif

    if ( limpr ) then
        ibid   = 0
        k8bid  = '        '
        formar = '        '
!       Réservation d'une unité et codage du nom en dur
!           Récupération du nom de la commande
!               le nom utilisateur du résultat : resout
!               le nom du concept résultat     : concept (en cas de reuse)
!               le nom de la commande          : nomcmd
        call getres(resout, concept, nomcmd)
!       numéro de la géométrie
        write(kgeom,'(I1)') igeom
        nkcar = lxlgut(resout)
        fichier='REPE_OUT/'//resout(1:nkcar)//'_'//kgeom//'.med'
!       unite libre
        unite = ulnume()
        if (unite.le.0) call utmess('F', 'UTILITAI5_10')
!       On ouvre et écriture
        call ulopen(unite, fichier, ' ', 'N', 'O')
        if ( geomi .ne. ' ') then
            call irmail('MED', unite, ibid, maili, ASTER_FALSE , k8bid, ibid, 1, formar)
        else
            call irmail('MED', unite, ibid, nomai, ASTER_FALSE , k8bid, ibid, 1, formar)
        endif
!       On ferme
        call ulopen(-unite, k8bid, k8bid, k8bid, k8bid)
    endif
!
999 continue
    if ( maili .ne. ' ') call jedetr(maili)
    call jedema()
end subroutine
