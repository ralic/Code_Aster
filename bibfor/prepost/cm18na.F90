subroutine cm18na(main, nbma, nbno, lima, typema,&
                  milieu, nomima, nomipe, mxnofa, nbtyma,&
                  deffac)
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
    implicit none
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/u2mesg.h"
#include "asterfort/uttrii.h"
    integer :: nfmax
    parameter  ( nfmax = 24 )
    integer :: nbma, nbno, lima(*), mxnofa, typema(*), nbtri
    integer :: milieu(4, nfmax, nbno), nomima(3, nbma), nomipe(8, *), nbtyma
    integer :: deffac(8, 0:6, nbtyma), noeud(4), face, ino, nbfa
    character(len=8) :: main
    character(len=24) :: connex
! ----------------------------------------------------------------------
!                   DETERMINATION DES NOEUDS DES FACES
! ----------------------------------------------------------------------
! IN  MAIN    MAILLAGE EN ENTREE (POUR CONNECTIVITE ET )
! IN  NBMA    NOMBRE DE MAILLES A TRAITER
! IN  NBNO    NOMBRE TOTAL DE NOEUDS DU MAILLAGE
! IN  LIMA    LISTE DES MAILLES A TRAITER
! IN  TYPEMA  LISTE DES TYPES DES MAILLES
! OUT MILIEU  REFERENCE DES FACES ET NOEUD MILIEU CORRESPONDANT
! OUT NOMIMA  LISTE DES NOEUDS MILIEUX PAR MAILLE
! OUT NOMIPE  LISTE DES NOEUDS PERES PAR NOEUDS MILIEUX
! OUT MXNOFA  NOMBRE DE NOEUDS MILIEUX CREES
! IN  NBTYMA  NOMBRE DE TYPE DE MAILLES (ACTUELLEMENT 27)
! IN  DEFFAC  DEFINITION DES FACES (VOIR LA ROUTINE CM1518)
! ----------------------------------------------------------------------
!
!
    integer :: m, a, no, ma, no1, no2, no3, no4, i, nomi, tyma
    integer :: jnoma
    character(len=8) :: nomnoe
    real(kind=8) :: rbid
! ----------------------------------------------------------------------
    call jemarq()
    connex = main//'.CONNEX'
!
! --- INITIALISATION
!
    mxnofa = 0
    do 2 m = 1, nbma
        do 3 a = 1, 3
            nomima(a,m) = 0
 3      continue
 2  end do
!
    do 5 no = 1, nbno
        do 6 face = 1, nfmax
            do 7 ino = 1, 4
                milieu(ino,face,no) = 0
 7          continue
 6      continue
 5  end do
!
    do 10 m = 1, nbma
        ma = lima(m)
        tyma = typema(ma)
!
        call jeveuo(jexnum(connex, ma), 'L', jnoma)
!
! ------ PARCOURS DES FACE DE LA MAILLE COURANTE
!
        nbfa = deffac(1,0,tyma)
        do 20 face = 1, nbfa
!
! --------- NOEUDS SOMMETS DE LA FACE
            no1 = zi(jnoma-1 + deffac(1,face,tyma))
            no2 = zi(jnoma-1 + deffac(2,face,tyma))
            no3 = zi(jnoma-1 + deffac(3,face,tyma))
            no4 = zi(jnoma-1 + deffac(4,face,tyma))
!
            noeud(1)=no1
            noeud(2)=no2
            noeud(3)=no3
            noeud(4)=no4
            nbtri=4
            call uttrii(noeud, nbtri)
!
            call assert(nbtri.eq.4)
            no1=noeud(1)
            no2=noeud(2)
            no3=noeud(3)
            no4=noeud(4)
!
! --------- EST-CE QUE LA FACE EST DEJA REFERENCEE
!
            do 30 i = 1, nfmax
! ------------ FACE DEJA REFERENCEE
                if ((milieu(1,i,no1) .eq. no2) .and. (milieu(2,i,no1) .eq. no3) .and.&
                    (milieu(3,i,no1) .eq. no4)) then
                    nomi = milieu(4,i,no1)
                    goto 31
!
! ------------ NOUVELLE FACE
                else if (milieu(1,i,no1) .eq.0) then
                    mxnofa = mxnofa + 1
                    milieu(1,i,no1) = no2
                    milieu(2,i,no1) = no3
                    milieu(3,i,no1) = no4
                    milieu(4,i,no1) = mxnofa
                    nomi = mxnofa
                    goto 31
                endif
30          continue
!           PLUS DE NFMAX FACES TOUCHENT NO1 ?
            call jenuno(jexnum(main//'.NOMNOE', no1), nomnoe)
            call u2mesg('F', 'MAIL0_11', 1, nomnoe, 1,&
                        nfmax, 0, rbid)
31          continue
            nomima(face,m) = nomi
            nomipe(1,nomi) = zi(jnoma-1 + deffac(1,face,tyma))
            nomipe(2,nomi) = zi(jnoma-1 + deffac(2,face,tyma))
            nomipe(3,nomi) = zi(jnoma-1 + deffac(3,face,tyma))
            nomipe(4,nomi) = zi(jnoma-1 + deffac(4,face,tyma))
            nomipe(5,nomi) = zi(jnoma-1 + deffac(5,face,tyma))
            nomipe(6,nomi) = zi(jnoma-1 + deffac(6,face,tyma))
            nomipe(7,nomi) = zi(jnoma-1 + deffac(7,face,tyma))
            nomipe(8,nomi) = zi(jnoma-1 + deffac(8,face,tyma))
20      continue
10  end do
!
    call jedema()
end subroutine
