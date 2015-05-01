subroutine arlmai(mail,mailar,ndim,nom1,nom2, &
                  tabcor,nbma1,nbma2)

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

! CREATION DU PSEUDO-MAILLAGE

! ----------------------------------------------------------------------


! IN  MAIL   : NOM DU MAILLAGE
! IN  ULPMAI : UNITE LOGIQUE POUR L'IMPRESSION DU PSEUDO-MAILLAGE
! OUT MAILAR : NOM DU PSEUDO-MAILLAGE
! IN  NDIM   : DIMENSION DU PROBLEME
! IN  NOM1   : NOM DE LA SD DE STOCKAGE PREMIER GROUPE
! IN  NOM2   : NOM DE LA SD DE STOCKAGE SECOND GROUPE
! OUT TABCOR : TABLEAU DE CORRESPONDANCE
!            POUR CHAQUE NOUVEAU NUMERO ABSOLU DANS MAILAR
!             -> ANCIEN NUMERO ABSOLU DANS MAIL
!             -> SI NEGATIF, LA NOUVELLE MAILLE EST ISSUE D'UNE
!                DECOUPE DE LA MAILLE DE NUMERO ABSOLU ABS(NUM) DANS
!                MAIL

    implicit none

#include "jeveux.h"
#include "asterfort/jemarq.h"
#include "asterfort/exisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jeveuo.h"
#include "asterfort/arlini.h"
#include "asterfort/dismoi.h"
#include "asterfort/jexatr.h"
#include "asterfort/jenuno.h"
#include "asterfort/jexnum.h"
#include "asterfort/jeexin.h"
#include "asterfort/jexnom.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/arlmaf.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jeecra.h"
#include "asterfort/jedema.h"


!     ARGUMENTS:
!     ----------
    character(len=8) :: mail,mailar
    integer :: ndim,nbma1,nbma2
    character(len=10) :: nom1,nom2
    character(len=24) :: tabcor


    integer :: nbnomx
    parameter    (nbnomx=27)
    integer :: nmain1,nmain2
    integer :: nnoin1,nnoin2,cxcumu
    character(len=24) ::  k8bid,nom
    integer :: icpl,i
    integer :: numma1,numma2
    integer :: imail
    integer :: nbno ,nbmat,nctot
    integer :: iret
    character(len=24) :: maidim,cooval,nomnoe
    integer :: jdime ,jcoor
    integer :: jcooro,jconxo,jcumuo
    character(len=19) :: ngrm1,ngrm2
    integer :: jtabco

! ----------------------------------------------------------------------

    call jemarq()

! --- NOM DES SDS TEMPORAIRES

    mailar = '&&ARL.MA'
    tabcor = '&&ARLMAI.TABCOR'

! --- DESTRUCTION DU PSEUDO-MAILLAGE S'IL EXISTE

    call exisd('MAILLAGE', mailar, iret)
    if (iret .ne. 0) then
       call detrsd('MAILLAGE',mailar)
    endif

! --- INITIALISATIONS

    ngrm1 = nom1(1:10)//'.GROUPEMA'
    ngrm2 = nom2(1:10)//'.GROUPEMA'
    call jelira(ngrm1,'LONMAX',nbma1,k8bid)
    call jelira(ngrm2,'LONMAX',nbma2,k8bid)

! --- MAILLAGE INITIAL

    call jeveuo(mail(1:8)//'.COORDO    .VALE','L',jcooro)
    call jeveuo(mail(1:8)//'.CONNEX         ','L',jconxo)
    call jeveuo(jexatr(mail(1:8)//'.CONNEX         ','LONCUM'),'L',jcumuo)
    call dismoi('NB_NO_MAILLA',mail,'MAILLAGE',nbno,k8bid,'F',iret)

! --- ESTIMATION GROSSIERE DU NOMBRE D'ELEMENTS TOTAL

    nmain1 = nbma1
    nmain2 = nbma2
    nbmat = nmain1 + nmain2

! --- ESTIMATION GROSSIERE DE LA LONGUEUR CUMULEE DES CONNECTIVITES

    nnoin1 = nbma1*nbnomx
    nnoin2 = nbma2*2
    nctot = nnoin1 + nnoin2

! --- ESTIMATION DU NOMBRE DE NOEUDS TOTAL
! --- CREATIONS DES OBJETS DE BASE POUR LE PSEUDO MAILLAGE

    call arlini(mailar,'V'  ,ndim  ,nbno ,nbmat ,nctot)

! --- ACCES AUX OBJETS DU PSEUDO MAILLAGE

    maidim  = mailar(1:8)//'.DIME           '
    cooval  = mailar(1:8)//'.COORDO    .VALE'
    nomnoe  = mailar(1:8)//'.NOMNOE         '
    call jeveuo(maidim,'E',jdime)
    call jeveuo(cooval,'E',jcoor)

! --- RECOPIE DES COORDONNEES DES ANCIENS NOEUDS

    do 10 i = 1,3*nbno
        zr(jcoor+i-1) = zr(jcooro+i-1)
    10 end do

! --- RECOPIE DES NOMS DES ANCIENS NOEUDS

    do 11 i = 1,nbno
        call jenuno(jexnum(mail(1:8)//'.NOMNOE         ',i),nom)
        call jeexin(jexnom(nomnoe,nom),iret)
        if (iret == 0) then
            call jecroc(jexnom(nomnoe,nom))
        else
            call utmess('F','MODELISA7_10',1,nom)
        endif
    11 end do

! --- TABLEAU DE CORRESPONDANCE NOUV. MAILLES -> ANCIENNES MAILLES

    call wkvect(tabcor,'V V I',nbmat,jtabco)

! --- INDICES POUR REMPLIR LES TABLEAUX

    imail  = 0
    cxcumu = 0

! --- CREATION DE LA MAILLE DANS LE PSEUDO-MAILLAGE

    do 30 icpl = 1 , nmain1
    
    ! ----- MAILLAGE 1

        imail = imail + 1
        call arlmaf(mail  ,mailar,ndim  ,ngrm1  ,icpl  , &
                    zi(jconxo)   ,zi(jcumuo)    ,imail ,numma1 , &
                    cxcumu)
        zi(jtabco+imail-1) = numma1
    30 end do
    do 31 icpl = 1 , nmain2
    
    ! ----- MAILLAGE 2

        imail = imail + 1
        call arlmaf(mail  ,mailar,ndim  ,ngrm2  ,icpl  , &
                    zi(jconxo)   ,zi(jcumuo)    ,imail ,numma2 , &
                    cxcumu)
        zi(jtabco+imail-1) = numma2
    31 end do

! --- VRAI NOMBRE D'ELEMENTS

    zi(jdime - 1 + 3) =  imail

! --- VRAI NOMBRE DE NOEUDS

    zi(jdime - 1 + 1) =  cxcumu

    call jedema()

end subroutine
