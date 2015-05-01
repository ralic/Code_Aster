subroutine cfcarm(noma, defico, newgeo, posmai, typmai,&
                  nummai, alias, nommai, ndim, nnomam,&
                  coorma)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit     none
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/cfnben.h"
#include "asterfort/cfnumm.h"
#include "asterfort/cftypm.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/mmtypm.h"
    character(len=8) :: noma, alias
    character(len=24) :: defico
    character(len=19) :: newgeo
    integer :: posmai, nummai
    integer :: nnomam, ndim
    real(kind=8) :: coorma(27)
    character(len=8) :: nommai
    character(len=4) :: typmai
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES MAILLEES - UTILITAIRE)
!
! CARACTERISTIQUES DE LA MAILLE
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  DEFICO : SD DE CONTACT (DEFINITION)
! IN  NEWGEO : COORDONNEES REACTUALISEES DES NOEUDS DU MAILLAGE
! IN  POSMAI : INDICE DE LA MAILLE (DANS SD CONTACT)
! OUT TYPMAI : TYPE DE LA MAILLE (MAITRE OU ESCLAVE)
! OUT NUMMAI : INDICE ABSOLU DE LA MAILLE (DANS SD MAILLAGE)
! OUT NNOMAM : NOMBRE DE NOEUDS DE LA MAILLE (AU SENS DES SD CONTACT)
! OUT NDIM   : DIMENSION DE LA MAILLE
! OUT ALIAS  : TYPE GEOMETRIQUE DE LA MAILLE
! OUT NOMMAI : NOM DE LA MAILLE
! OUT COORMA : COORDONNEES DES NOEUDS DE LA MAILLE
!
!
!
!
    integer :: nbnmax
    parameter   (nbnmax = 9)
!
    integer :: no(nbnmax)
    integer :: ino, i, ibid
    integer :: jdec
    real(kind=8), pointer :: vale(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- RECUPERATION DE QUELQUES DONNEES
!
    call jeveuo(newgeo(1:19)//'.VALE', 'L', vr=vale)
!
! --- INITIALISATIONS
!
    do 7 i = 1, 27
        coorma(i) = 0.d0
 7  end do
!
! --- TYPE DE LA MAILLE
!
    call cftypm(defico, posmai, typmai)
!
! --- NUMERO ABSOLU DE LA MAILLE
!
    call cfnumm(defico, posmai, nummai)
!
! --- NOMBRE DE NOEUDS DE LA MAILLE
!
    call cfnben(defico, posmai, 'CONNEX', nnomam, ibid)
    if (nnomam .gt. nbnmax) then
        ASSERT(.false.)
    endif
!
! --- TYPE DE LA MAILLE
!
    call mmtypm(noma, nummai, nnomam, alias, ndim)
!
! --- NUMEROS ABSOLUS DES NOEUDS DE LA MAILLE
!
    call jeveuo(jexnum(noma//'.CONNEX', nummai), 'L', jdec)
    do 61 ino = 1, nnomam
        no(ino) = zi(jdec+ino-1)
61  end do
!
! --- COORDONNEES DES NOEUDS DE LA MAILLE
!
    do 70 ino = 1, nnomam
        coorma(3*(ino-1)+1) = vale(1+3*(no(ino)-1))
        coorma(3*(ino-1)+2) = vale(1+3*(no(ino)-1)+1)
        coorma(3*(ino-1)+3) = vale(1+3*(no(ino)-1)+2)
70  end do
!
! --- NOM DE LA MAILLE
!
    call jenuno(jexnum(noma//'.NOMMAI', nummai), nommai)
!
    call jedema()
!
end subroutine
