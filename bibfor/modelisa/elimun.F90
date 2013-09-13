subroutine elimun(noma, nomo, motfac, nzocu, nbgdcu,&
                  compcu, nopono, nolino, lisnoe, poinoe,&
                  nnoco)
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
#include "asterfort/exiscp.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/palino.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=8) :: noma, nomo
    character(len=16) :: motfac
    integer :: nzocu
    character(len=24) :: nbgdcu
    character(len=24) :: compcu
    character(len=24) :: nopono
    character(len=24) :: nolino
    character(len=24) :: lisnoe
    character(len=24) :: poinoe
    integer :: nnoco
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (LIAISON_UNILATER - LECTURE)
!
! ELIMINATION AU SEIN DE CHAQUE SURFACE DE CONTACT POTENTIELLE DES
! NOEUDS ET MAILLES REDONDANTS. MODIFICATION DES POINTEURS ASSOCIES.
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  NOMO   : NOM DU MODELE
! IN  MOTFAC : MOT-CLEF FACTEUR POUR LIAISON UNILATERALE
! IN  NZOCU  : NOMBRE DE ZONES DE LIAISON_UNILATERALE
! IN  NBGDCU : NOM JEVEUX DE LA SD INFOS POINTEURS GRANDEURS
! IN  COMPCU : NOM JEVEUX DE LA SD CONTENANT LES GRANDEURS DU MEMBRE
!              DE GAUCHE
! IN  NOPONO : NOM DE L'OBJET CONTENANT LE VECTEUR D'INDIRECTION
! IN  NOLINO : NOM DE L'OBJET CONTENANT LA LISTE DES NOEUDS
! IN  POINOE : NOM DE L'OBJET CONTENANT LE VECTEUR D'INDIRECTION
!               DES NOEUDS APRES NETTOYAGE
! IN  LISNOE : NOM DE L'OBJET CONTENANT LES NOEUDS APRES NETTOYAGE
! IN  NBNOE  : NOMBRE DE NOEUDS DANS LA LISTE RESULTANTE
!                VAUT NBNOE = NBTOT-NBSUP
! I/O NNOCO  : NOMBRE DE TOTAL DE NOEUDS POUR TOUTES LES OCCURRENCES
!
!
!
!
    integer :: jdebut, juelim, jdecal, jdecat
    integer :: nbelim
    character(len=8) :: k8bla, cmp, nomnoe
    integer :: i, j, icmp, izone, ino, numno1, numno2
    integer :: nbno, nbsup, nb, nbcmp, ntsup
    integer :: n1, n2, n3
    integer :: jnl, jnp, jpoi, jnoe
    character(len=24) :: nelim
    integer :: jelim
    integer :: jnbgd, jcmpg
    integer :: exist(1)
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ACCES SD
!
    call jeveuo(nolino, 'E', jnl)
    call jeveuo(nopono, 'L', jnp)
    call jeveuo(nbgdcu, 'L', jnbgd)
    call jeveuo(compcu, 'L', jcmpg)
!
    ntsup = 0
    k8bla = ' '
    nelim = '&&ELIMUN.ELIM'
!
! --- CREATION DU POINTEUR
!
    call wkvect(poinoe, 'V V I', nzocu+1, jpoi)
    zi(jpoi) = 1
!
    do 1000 izone = 1, nzocu
!
! --- VECTEUR CONTENANT LES NOEUDS ZONE
!
        nbno = zi(jnp+izone) - zi(jnp+izone-1)
        jdebut = zi(jnp+izone-1)
        n1 = 0
        n2 = 0
        n3 = 0
!
! --- ELIMINATION DES PURS DOUBLONS
!
        do 10 i = 1, nbno
            numno1 = zi(jnl-2+jdebut+i)
            if (numno1 .ne. 0) then
                do 11 j = i+1, nbno
                    numno2 = zi(jnl-2+jdebut+j)
                    if ((numno1.eq.numno2) .and. (numno2.ne.0)) then
                        zi(jnl-2+jdebut+j) = 0
                        n1 = n1 + 1
                    endif
11              continue
            endif
10      continue
!
! --- RECUPERATION INFOS SANS_NOEUD, SANS_GROUP_NO
!
        call palino(noma, motfac, 'SANS_GROUP_NO', 'SANS_NOEUD', izone,&
                    nelim)
        call jeveuo(nelim, 'L', juelim)
        nbelim = zi(juelim)
!
! --- ELIMINATION DES SANS_GROUP_NO, SANS_NOEUD
!
        call jelira(nelim, 'LONMAX', nbelim)
        call jeveuo(nelim, 'E', jelim)
        do 20 i = 1, nbelim
            numno1 = zi(juelim-1+i)
            if (numno1 .ne. 0) then
                do 21 j = 1, nbno
                    numno2 = zi(jnl-2+jdebut+j)
                    if ((numno1.eq.numno2) .and. (numno2.ne.0)) then
                        zi(jnl-2+jdebut+j) = 0
                        n2 = n2 + 1
                    endif
21              continue
            endif
20      continue
!
! --- ELIMINATION DES NOEUDS NE COMPORTANT AUCUNE DES GRANDEURS
!
        nbcmp = zi(jnbgd+izone) - zi(jnbgd+izone-1)
        jdecat = zi(jnbgd+izone-1)
!
        do 30 ino = 1, nbno
            numno1 = zi(jnl-2+jdebut+ino)
            if (numno1 .ne. 0) then
                call jenuno(jexnum(noma//'.NOMNOE', numno1), nomnoe)
                nb = 0
                do 31 icmp = 1, nbcmp
!
                    cmp = zk8(jcmpg-1+jdecat+icmp-1)
                    call exiscp(cmp, k8bla, nomo, 1, 'NUM',&
                                k8bla, numno1, exist)
                    if (exist(1) .eq. 0) then
                        nb = nb + 1
                    endif
31              continue
                if (nb .eq. nbcmp) then
                    zi(jnl-2+jdebut+ino) = 0
                    n3 = n3 + 1
                endif
            endif
30      continue
!
! --- NOMBRE DE NOEUDS A SUPPRIMER
!
        nbsup = n1 + n2 + n3
        ntsup = ntsup + nbsup
!
! --- MAJ VECTEUR POINTEUR INDIRECT (POINOE)
!
        zi(jpoi+izone) = zi(jpoi+izone-1) + nbno - nbsup
        if (nbno .eq. nbsup) then
            call utmess('F', 'UNILATER_48')
        endif
1000  end do
!
! --- CREATION DU VECTEUR RESULTANT
!
    nnoco = nnoco - ntsup
    call wkvect(lisnoe, 'V V I', nnoco, jnoe)
!
! --- ELIMINATION EFFECTIVE DES NOEUDS
!
    jdecal = 0
    do 50 izone = 1, nzocu
        nbno = zi(jnp+izone) - zi(jnp+izone-1)
        jdebut = zi(jnp+izone-1)
        do 51 ino = 1, nbno
            numno1 = zi(jnl-2+jdebut+ino)
            if (numno1 .ne. 0) then
                zi(jnoe+jdecal) = numno1
                jdecal = jdecal +1
            endif
51      continue
50  end do
!
    call jedetr(nelim)
!
    call jedema()
!
end subroutine
