subroutine pacoor(nomma, ima, nbno, coor)
    implicit none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
!
    character(len=8) :: nomma
    integer :: ima, nbno
    real(kind=8) :: coor(*)
!---------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!---------------------------------------------------------------------
!     BUT: DONNER LA LISTE DES COORDONNEES DES NBNO 1ERS NOEUDS DE LA
!          MAILLE IMA DU MAILLAGE NOMMA OU D'UN NOEUD SI NBNO = 0
!     VERIFICTION : NBNO < OU = NBRE DE NOUDS DE LA MAILLE
! ATTENTION IL FAUT QUE DIM DE COOR >= 3 MEME POUR UN NOEUD EN 2D
!---------------------------------------------------------------------
! ARGUMENTS D'ENTREE:
! IN   NOMMA  K8  : NOM DU MAILLAGE
! IN   IMA    I   : NUMERO DE LA MAILLE OU DU NOEUD SI NBNO = 0
! IN   NBNO   I   : NOMBRE DE NOEUDS DE LA MAILLE A EXTRAIRE, OU 0 POUR
!                   UN NOEUD
! OUT  COOR   R(*): COORDONNEES DES NBNO 1ERS NOEUDS DE LA MAILLE
!                   OU COORDONNEES DU NOEUD IMA
!                   POUR INO = 1,NBNO  OU INO = IMA SI NBNO = 0
!                   COOR(3*(INO-1)+1)= X1(INO)
!                   COOR(3*(INO-1)+2)= X2(INO)
!                   COOR(3*(INO-1)+3)= X3(INO) ( EN 2D 0)
    character(len=24) :: desc, vale, connex
    real(kind=8) :: x(3)
! --- DEBUT
!-----------------------------------------------------------------------
    integer :: i, icmp, icoor, idconn, iddesc, idino, idvale
    integer :: ino, inoma, nbcmp, nbnomx
!-----------------------------------------------------------------------
    call jemarq()
    desc = nomma(1:8)//'.COORDO    .DESC'
    vale = nomma(1:8)//'.COORDO    .VALE'
    connex = nomma(1:8)//'.CONNEX'
    call jeveuo(desc, 'L', iddesc)
    nbcmp = -zi(iddesc+1)
    if (nbcmp .eq. 2) then
        if (nbno .gt. 0) x(3) = 0.d0
        if (nbno .eq. 0) coor(3) = 0.d0
    endif
    call jeveuo(vale, 'L', idvale)
    if (nbno .gt. 0) then
        call jeveuo(jexnum(connex, ima), 'L', idconn)
        call jelira(jexnum(connex, ima), 'LONMAX', nbnomx)
        if (nbno .gt. nbnomx) then
            call utmess('F', 'MODELISA6_5')
        endif
        do 1 inoma = 1, nbno
            ino = zi(idconn-1+inoma)
            idino = idvale+ nbcmp*(ino-1)-1
            do 2 icmp = 1, nbcmp
                x(icmp) = zr(idino+icmp)
 2          continue
            icoor = 3*(inoma-1)
            do 3 i = 1, 3
                coor(icoor+i) = x(i)
 3          continue
 1      continue
    else if (nbno.eq.0) then
        idino = idvale+ nbcmp*(ima-1)-1
        do 4 icmp = 1, nbcmp
            coor(icmp) = zr(idino+icmp)
 4      continue
    else
        call utmess('F', 'MODELISA6_6')
    endif
    call jedema()
end subroutine
