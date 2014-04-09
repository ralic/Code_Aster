subroutine arlcou(mail  ,iocc   ,nomo  ,typmai,        &
    nom1  ,nom2  ,cine  , &
    dime, lisrel, charge)

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
#include "asterfort/jelira.h"
#include "asterfort/arlcpl.h"
#include "asterfort/jedema.h"

!     ARGUMENTS:
!     ----------
    character(len=24) :: typmai
    character(len=8) ::  mail,nomo
    character(len=8) ::  charge
    character(len=19) :: lisrel
    character(len=10) :: nom1,nom2
    character(len=8) ::  cine(3)
    integer ::      dime,iocc

! ----------------------------------------------------------------------

! ROUTINE ARLEQUIN

! CALCUL DES MATRICES DE COUPLAGE ELEMENTAIRE ARLEQUIN
! ROUTINE D'AIGUILLAGE SUIVANT GROUPE MEDIATEUR

! ----------------------------------------------------------------------


! IN  MAIL   : NOM DU MAILLAGE
! IN  NOMO   : NOM DU MODELE
! IN  TYPMAI : SD CONTENANT NOM DES TYPES ELEMENTS (&&CATA.NOMTM)
! IN  NOM1   : NOM DE LA SD DE STOCKAGE MAILLES GROUP_MA_1
! IN  NOM2   : NOM DE LA SD DE STOCKAGE MAILLES GROUP_MA_2
! IN  CINE   : CINEMATIQUES DES GROUPES DE MAILLE
! IN  DIME   : DIMENSION DE L'ESPACE GLOBAL (2 OU 3)


! ----------------------------------------------------------------------

    character(len=10) :: nomgr1,nomgr2
    character(len=8) ::  cine1,cine2
    integer ::      nbma1,nbma2
    character(len=19) :: ngrm1,ngrm2
    character(len=8) ::  k8bid

! ----------------------------------------------------------------------
    call jemarq()

! --- INITIALISATIONS

    nomgr1 = nom1
    nomgr2 = nom2
    cine1  = cine(1)
    cine2  = cine(2)

! --- CALCUL DES MATRICES DE COUPLAGE ELEMENTAIRE ARLEQUIN


    ngrm1 = nom1(1:10)//'.GROUPEMA'
    ngrm2 = nom2(1:10)//'.GROUPEMA'
    call jelira(ngrm1,'LONMAX',nbma1,k8bid)
    call jelira(ngrm2,'LONMAX',nbma2,k8bid)

    call arlcpl(iocc ,nbma1 ,nbma2 , &
                mail  ,nomo  ,typmai,        &
                nomgr1,nomgr2,dime, lisrel, charge)

    call jedema()

end subroutine
