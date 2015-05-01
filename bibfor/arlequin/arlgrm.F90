subroutine arlgrm(mail  ,nomgrp,dime  ,ima  ,connex,loncum, &
                  nummai,nommai,itypm ,nbno , cxno)

! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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

! ROUTINE ARLEQUIN

! DONNE LES COORDONNEES D'UNE MAILLE D'UN GROUPE

! ----------------------------------------------------------------------


! IN  NOMGRP : NOM DU GROUPE DE MAILLES ARLEQUIN
! IN  MAIL   : NOM DU MAILLAGE
! IN  NOM    : NOM DE LA SD DE STOCKAGE DES MAILLES ARLEQUIN GROUP_MA_*
! IN  DIME   : DIMENSION DE L'ESPACE
! IN  IMA    : NUMERO D'ORDRE DE LA MAILLE DANS LE GROUPE ARLEQUIN
! IN  CONNEX : CONNEXITE DES MAILLES
! IN  LONCUM : LONGUEUR CUMULEE DE CONNEX
! OUT NUMMAI : NUMERO ABSOLU DE LA MAILLE DANS LE MAILLAGE
! OUT NOMMAI : NOM DE LA MAILLE
! OUT ITYPM  : NUMERO ABSOLU DU TYPE DE LA MAILLE
! OUT NBNO   : NOMBRE DE NOEUDS DE LA MAILLE
! OUT CXNO   : CONNECTIVITE DE LA MAILLE
!                CONTIENT NUMEROS ABSOLUS DES NOEUDS DANS LE MAILLAGE
! ----------------------------------------------------------------------

    implicit none

#include "jeveux.h"
#include "asterfort/jemarq.h"
#include "asterfort/assert.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jenuno.h"
#include "asterfort/jexnum.h"
#include "asterfort/arlcnn.h"
#include "asterfort/jedema.h"

!     ARGUMENTS:
!     ----------
    character(len=19) :: nomgrp
    character(len=8) :: mail
    integer :: ima,dime
    integer :: connex(*),loncum(*)
    integer :: nummai
    character(len=8) :: nommai
    integer :: nbno,itypm
    integer :: cxno(27)
!-----------------------------------------------------------------------
    integer :: jgrp,jtyp,aima
!-----------------------------------------------------------------------

    call jemarq()

! --- INITIALISATIONS

    ASSERT((dime.gt.0).and.(dime.le.3))
    aima = abs(ima)

! --- LECTURE DONNEES MAILLAGE

    call jeveuo(mail(1:8)//'.TYPMAIL        ','L',jtyp)

! --- LECTURE DONNEES GROUPE DE MAILLES

    call jeveuo(nomgrp(1:19),'L',jgrp)

! --- NUMERO ABSOLU ET NOM DE LA MAILLE

    nummai = zi(jgrp-1+aima)
    call jenuno(jexnum(mail(1:8)//'.NOMMAI',nummai),nommai)

! --- TYPE DE LA MAILLE

    itypm  = zi(jtyp-1+nummai)

! --- CONNECTIVITE DE LA MAILLE

    call arlcnn(nummai,connex,loncum,nbno,cxno)

    call jedema()

end subroutine
