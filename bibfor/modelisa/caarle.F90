subroutine caarle (numdlz, iocc, lisrez, chargz)

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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
implicit none
#include "jeveux.h"
#include "asterfort/jemarq.h"
#include "asterc/getfac.h"
#include "asterfort/utmess.h"
#include "asterfort/getvid.h"
#include "asterfort/getvtx.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jelira.h"
#include "asterfort/wkvect.h"
#include "asterfort/jenuno.h"
#include "asterfort/jexnum.h"
#include "asterfort/arllec.h"
#include "asterfort/arlcou.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedema.h"

    integer :: iocc
    character(len=*) :: numdlz, chargz, lisrez

! ----------------------------------------------------------------------

!              CREATION D'UN CHARGEMENT DE TYPE ARLEQUIN

! ----------------------------------------------------------------------

! I/O  CHARGE : SD CHARGE

! SD EN ENTREE :
! ==============

! .CHME.MODEL.NOMO       : NOM DU MODELE ASSOCIE A LA CHARGE
! .TYPE                  : TYPE DE LA CHARGE

! SD EN SORTIE ENRICHIE PAR :
! ===========================
! .CHME.LIGRE     : LIGREL DE CHARGE
! .CHME.CIMPO     : CARTE COEFFICIENTS IMPOSES
! .CHME.CMULT     : CARTE COEFFICIENTS MULTIPLICATEURS


    character(len=8) :: charge
    character(len=14) :: numddl
    character(len=19) :: lisrel
    character(len=24) :: typmai
    character(len=10) :: noma,nomb,nom1,nom2
    character(len=8) :: nomo,mail,model(3),cine(3)
    character(len=8) :: k8bid
    integer :: dime,nocc,iop
    integer :: nbtyp
    integer :: zocc,ibid,i
    integer :: jtypm,jlgrf
    character(len=16) :: motfac,option

    data typmai /'&&CAARLE.NOMTM'/

! ----------------------------------------------------------------------
    call jemarq()

    numddl = numdlz
    charge = chargz
    lisrel = lisrez
    zocc = iocc
    motfac = 'LIAISON_ELEM'

! --- IMPOSE-T-ON UNE CHARGE ARLEQUIN ?
!
    call getfac(motfac,nocc)
    call getvtx(motfac, 'OPTION', iocc=zocc, scal=option, nbret=iop)
    if (option .ne. '3D_POU_ARLEQUIN') then
        call utmess('F', 'MODELISA6_39', sk=option)
    endif

    call getfac(motfac, nocc)
    if (nocc .eq. 0) goto 999

! --- LECTURE NOMS DU MODELE ET DU MAILLAGE

    call getvid(' ', 'MODELE',iocc=0,nbval=1,scal=nomo,nbret=ibid)
    call jeveuo(nomo(1:8)//'.MODELE    .LGRF','L',jlgrf)
    mail = zk8(jlgrf)

! --- STRUCTURES DE DONNEES

    noma   = charge(1:8)//'.A'
    nomb   = charge(1:8)//'.B'
    nom1   = charge(1:8)//'.1'
    nom2   = charge(1:8)//'.2'

! --- CREATION D'UN VECTEUR CONTENANT LE NOM DES TYPES DE MAILLES

    call jelira('&CATA.TM.NOMTM','NOMMAX',nbtyp,k8bid)
    call wkvect(typmai, 'V V K8',nbtyp, jtypm)
    do 10 i = 1,nbtyp
        call jenuno(jexnum('&CATA.TM.NOMTM',i),zk8(jtypm-1+i))
    10 end do

! --- LECTURE ET VERIFICATION DES MAILLES DES MODELES

    call arllec(motfac,zocc  ,nomo  ,noma  ,nomb  , &
                model ,cine  ,dime)

! --- CALCUL DES EQUATIONS DE COUPLAGE

    call arlcou(mail,zocc  ,nomo  ,typmai,        &
                noma  ,nomb  ,cine  , &
                dime, lisrel, charge)

! --- DESALLOCATION GROUPES 1 ET 2

    call jedetr(nom1)
    call jedetr(nom2)
    call jedetr(noma//'.GROUPEMA')
    call jedetr(nomb//'.GROUPEMA')

! --- DESALLOCATION AUTRES OBJETS ARLEQUIN

    call jedetr(typmai)

    999 continue

    call jedema()

end subroutine
