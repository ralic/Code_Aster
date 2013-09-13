subroutine cmlqna(nbma, nbno, lima, connez, typema,&
                  mxar, milieu, nomima, nomipe, mxnomi,&
                  nbtyma, defare)
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
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
!
    integer :: nbma, nbno, lima(*), mxar, mxnomi, typema(*)
    integer :: milieu(2, mxar, nbno), nomima(12, nbma), nomipe(2, *), nbtyma
    integer :: defare(2, 0:12, nbtyma)
    character(len=*) :: connez
    character(len=24) :: connex
! ----------------------------------------------------------------------
!                   DETERMINATION DES NOEUDS ARETES
! ----------------------------------------------------------------------
! IN  NBMA    NOMBRE DE MAILLES A TRAITER
! IN  NBNO    NOMBRE TOTAL DE NOEUDS DU MAILLAGE
! IN  LIMA    LISTE DES MAILLES A TRAITER
! IN  CONNEX  CONNECTION DES MAILLES (COLLECTION JEVEUX)
! IN  TYPEMA  LISTE DES TYPES DES MAILLES
! IN  MXAR    NOMBRE MAXIMAL D'ARETE PAR NOEUD (POUR VERIFICATION)
! OUT MILIEU  REFERENCE DES ARETES ET NOEUD MILIEU CORRESPONDANT
! OUT NOMIMA  LISTE DES NOEUDS MILIEUX PAR MAILLE
! OUT NOMIPE  LISTE DES NOEUDS PERES PAR NOEUDS MILIEUX
! OUT MXNOMI  NOMBRE DE NOEUDS MILIEUX CREES
! ----------------------------------------------------------------------
!
!
    integer :: m, a, no, ma, nbar, no1, no2, tmp, i, nomi, tyma
    integer :: jnoma
! ----------------------------------------------------------------------
    call jemarq()
    connex = connez
!
! --- INITIALISATION
!
    mxnomi = 0
    do 2 m = 1, nbma
        do 3 a = 1, 12
            nomima(a,m) = 0
 3      continue
 2  end do
!
    do 5 no = 1, nbno
        do 6 a = 1, mxar
            milieu(1,a,no) = 0
            milieu(2,a,no) = 0
 6      continue
 5  end do
!
    do 10 m = 1, nbma
        ma = lima(m)
        tyma = typema(ma)
        call jeveuo(jexnum(connex, ma), 'L', jnoma)
!
! ------ PARCOURS DES ARETES DE LA MAILLE COURANTE
!
        nbar = defare(1,0,tyma)
        do 20 a = 1, nbar
!
! --------- NOEUDS SOMMETS DE L'ARETE
!
            no1 = zi(jnoma-1 + defare(1,a,tyma))
            no2 = zi(jnoma-1 + defare(2,a,tyma))
!
            if (no1 .gt. no2) then
                tmp = no2
                no2 = no1
                no1 = tmp
            endif
!
! --------- EST-CE QUE L'ARETE EST DEJA REFERENCEE
!
            do 30 i = 1, mxar
!
! ------------ ARETE DEJA REFERENCEE
                if (milieu(1,i,no1) .eq. no2) then
                    nomi = milieu(2,i,no1)
                    goto 31
!
! ------------ NOUVELLE ARETE
                else if (milieu(1,i,no1) .eq.0) then
                    mxnomi = mxnomi + 1
                    milieu(1,i,no1) = no2
                    milieu(2,i,no1) = mxnomi
                    nomi = mxnomi
                    goto 31
                endif
30          continue
            ASSERT(.false.)
31          continue
!
            nomima(a,m) = nomi
            nomipe(1,nomi) = no1
            nomipe(2,nomi) = no2
!
20      continue
10  end do
!
    if (mxnomi .eq. 0) then
        call utmess('A', 'MODELISA3_44')
    endif
!
    call jedema()
end subroutine
