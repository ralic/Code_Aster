subroutine foston(chval, vecnom, nbfonc)
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/jecroc.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jelibe.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    integer :: nbfonc
    character(len=*) :: chval, vecnom(nbfonc)
!     ------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!     STOCKAGE DANS LA COLLECTION CHVAL DES FONCTIONS COMPOSANT LA NAPPE
!     ACCES PAR NOM OU NUMERO DANS LA COLLECTION
!     ------------------------------------------------------------------
! IN  CHVAL : NOM JEVEUX DE LA COLLECTION
! IN  VECNOM: NOMS DES FONCTIONS, ISSUS DES COMMANDES UTILISATEUR
! IN  NBFONC: NOMBRE DE FONCTIONS
!     ------------------------------------------------------------------
!     OBJETS SIMPLES LUS
!         CHFVAL=VECNOM(I)//'.VALE'
!     OBJETS SIMPLES CREES
!         JEXNUM(CHVAL,I)
    character(len=1) :: k1bid
!     ------------------------------------------------------------------
    integer :: i, j, nbp, lvaln, lvalf, nbpt
    character(len=24) :: chfval
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    call jemarq()
    nbpt=0
    chfval(20:24) = '.VALE'
    do 1 i = 1, nbfonc
        chfval(1:19) = vecnom(i)
        call jelira(chfval, 'LONUTI', nbp, k1bid)
        nbpt=nbpt+nbp
 1  end do
    call jeecra(chval, 'LONT', nbpt, ' ')
    do 3 i = 1, nbfonc
        chfval(1:19) = vecnom(i)
        call jeveuo(chfval, 'L', lvalf)
        call jelira(chfval, 'LONUTI', nbp, k1bid)
        call jecroc(jexnum(chval, i))
        call jeecra(jexnum(chval, i), 'LONMAX', nbp, ' ')
        call jeecra(jexnum(chval, i), 'LONUTI', nbp, ' ')
        call jeveuo(jexnum(chval, i), 'E', lvaln)
        do 2 j = 1, nbp
            zr(lvaln+j-1)=zr(lvalf+j-1)
 2      continue
        call jelibe(chfval)
 3  end do
    call jedema()
end subroutine
