subroutine arllec(motcle,iocc  ,modele,noma  ,nomb  , &
    model ,cine  ,dime  )

! ======================================================================
! COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.

! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.

! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================


    implicit none

#include "jeveux.h"
#include "asterfort/jemarq.h"
#include "asterfort/infniv.h"
#include "asterfort/getvtx.h"
#include "asterfort/getvid.h"
#include "asterfort/wkvect.h"
#include "asterfort/arlver.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedema.h"

    character(len=16) :: motcle
    integer ::      iocc
    character(len=8) ::  modele
    character(len=10) :: noma,nomb
    character(len=8) ::  model(3),cine(3)
    integer ::      dime

! ----------------------------------------------------------------------

! ROUTINE ARLEQUIN

! LECTURE ET VERIFICATION DES MAILLES DES MODELES

! ----------------------------------------------------------------------


! IN  MOTCLE : MOT-CLEF FACTEUR POUR ARLEQUIN
! IN  IOCC   : OCCURRENCE DU MOT CLEF-FACTEUR ARLEQUIN
! IN  MODELE : NOM DU MODELE
! I/O NOMA   : NOM DE LA SD POUR STOCKAGE MAILLES GROUP_MA_1
! I/O NOMB   : NOM DE LA SD POUR STOCKAGE MAILLES GROUP_MA_2
! OUT MODEL  : MODELISATION ASSOCIEE AUX MAILLES '3D',
!              UN POUR CHAQUE GROUPE + ZONE DE COLLAGE
! OUT CINE   : CINEMATIQUE ASSOCIEE AUX MAILLES
!              'SOLIDE' OU 'POUTRE'
!              UN POUR CHAQUE GROUPE + ZONE DE COLLAGE
! OUT DIME   : DIMENSION DE L'ESPACE GLOBAL 2 OU 3


    integer ::      nbev1,nbev2
    character(len=8) ::  k8bid
    character(len=16) ::  option
    integer ::      jgrm1,jgrm2
    integer ::      ifm,niv,iarg,iop
    character(len=6) :: nompro
    parameter   (nompro='ARLLEC')

! ----------------------------------------------------------------------

    call jemarq()
    call infniv(ifm,niv)

! --- LECTURE MAILLE GROUPE_MA_1

    call getvtx(motcle,'OPTION', iocc=iocc, scal=option, nbret=iop)
    if (option .eq. '3D_POU_ARLEQUIN') then
       call getvtx(motcle,'GROUP_MA_1',iocc=iocc,nbval=0,&
                nbret=nbev1,scal=k8bid,isdefault=iarg)
       call wkvect('&&'//nompro//'.GMA1','V V K8', -nbev1, jgrm1 )
       call getvtx(motcle,'GROUP_MA_1',iocc=iocc,nbval=-nbev1,&
                vect=zk8(jgrm1),nbret=nbev1,isdefault=iarg)
       call arlver(modele,zk8(jgrm1),nbev1,noma,model(1),cine(1))

! --- LECTURE MAILLE GROUPE_MA_2

       call getvtx(motcle,'GROUP_MA_2',iocc,0,k8bid,nbret=nbev2,&
                isdefault=iarg)
       call wkvect('&&'//nompro//'.GMA2','V V K8', -nbev2, jgrm2 )
       call getvtx(motcle,'GROUP_MA_2',iocc,-nbev2,zk8(jgrm2),&
                nbret=nbev2,isdefault=iarg)
       call arlver(modele,zk8(jgrm2),nbev2,nomb,model(2),cine(2))
    endif

! --- DIMENSION DE L'ESPACE GLOBAL

    dime = 3

! --- MENAGE

    call jedetr('&&'//nompro//'.GMA1')
    call jedetr('&&'//nompro//'.GMA2')

    call jedema()
end subroutine
