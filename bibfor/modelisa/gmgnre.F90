subroutine gmgnre(noma, nbnoto, litrav, listma, nbma,&
                  listno, nbno, selez)
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
    implicit none
!
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
!
    character(len=8) :: noma
    character(len=*) :: selez
    character(len=16) :: selec
    integer :: nbma, nbnoto, nbno, listma(*), listno(*), litrav(*)
! ----------------------------------------------------------------------
!     BUT: REMPLIR LA LISTE DE NOEUD SOUS-JACENTE A LA LISTE DE MAILLE
!
!     IN: NOMA   : NOM DU MAILLAGE
!         NBNOTO : NOMBRE DE NOEUDS TOTAL DU MAILLAGE.
!         LISTMA : LISTE DES NUMEROS DE MAILLES A TRAITER.
!           NBMA : NOMBRE DE MAILLES DANS LA LISTE.
!         LITRAV : VECTEUR DE TRAVAIL.
!         SELEC  :  SELECTION DES NOEUDS (TOUS, SOMMET, MILIEU, CENTRE)
!
!     OUT:
!         LISTNO : LISTE DES NOEUDS TROUVES
!          NBNO  : NOMBRE DE NOEUDS TROUVE.
! ----------------------------------------------------------------------
!
!     FONCTIONS EXTERNES:
!     -------------------
!
!     VARIABLES LOCALES:
!     ------------------
    character(len=8) :: typm, notyma(19)
    integer :: posini, posfin,  sel, nutyma
    integer :: pini(3, 19), pfin(3, 19)
!
!-----------------------------------------------------------------------
    integer :: i, iacnex, ima, ino, nbnoma, numno
    integer, pointer :: typmail(:) => null()
!-----------------------------------------------------------------------
    data notyma / 'POI1'  ,&
     &              'SEG2'  , 'SEG3'   ,&
     &              'TRIA3' , 'TRIA6'  , 'TRIA7',&
     &              'QUAD4' , 'QUAD8'  , 'QUAD9',&
     &              'TETRA4', 'TETRA10',&
     &              'PENTA6', 'PENTA15','PENTA18',&
     &              'PYRAM5', 'PYRAM13',&
     &              'HEXA8' , 'HEXA20' , 'HEXA27' /
!
!
!
!
    data pini / 1, 0, 0,&
     &            1, 0, 0,&
     &            1, 3, 0,&
     &            1, 0, 0,&
     &            1, 4, 0,&
     &            1, 4, 7,&
     &            1, 0, 0,&
     &            1, 5, 0,&
     &            1, 5, 9,&
     &            1, 0, 0,&
     &            1, 5, 0,&
     &            1, 0, 0,&
     &            1, 7, 0,&
     &            1, 7, 16,&
     &            1, 0, 0,&
     &            1, 6, 0,&
     &            1, 0, 0,&
     &            1, 9, 0,&
     &            1, 9, 21 /
!
    data pfin / 1, 0, 0,&
     &            2, 0, 0,&
     &            2, 3, 0,&
     &            3, 0, 0,&
     &            3, 6, 0,&
     &            3, 6, 7,&
     &            4, 0, 0,&
     &            4, 8, 0,&
     &            4, 8, 9,&
     &            4, 0, 0,&
     &            4, 10,0,&
     &            6, 0, 0,&
     &            6, 15,0,&
     &            6, 15,18,&
     &            5, 0, 0,&
     &            5, 13,0,&
     &            8, 0, 0,&
     &            8, 20,0,&
     &            8, 20,27 /
!
!
!
!     -- ON PARCOURE LA LISTE DES MAILLES ET ON COCHE LES NOEUDS
!     -- DANS LITRAV:
!
    call jemarq()
    selec = selez
    call jeveuo(noma // '.TYPMAIL', 'L', vi=typmail)
!
    if (selec .eq. 'TOUS') sel=0
    if (selec .eq. 'SOMMET') sel=1
    if (selec .eq. 'MILIEU') sel=2
    if (selec .eq. 'CENTRE') sel=3
!
    do 1 i = 1, nbnoto
        litrav(i) =0
 1  end do
!
    do 2 i = 1, nbma
        ima=listma(i)
        call jeveuo(jexnum(noma//'.CONNEX', ima), 'L', iacnex)
        call jelira(jexnum(noma//'.CONNEX', ima), 'LONMAX', nbnoma)
!
        if (sel .eq. 0) then
            posini = 1
            posfin = nbnoma
        else
            call jenuno(jexnum('&CATA.TM.NOMTM', typmail(ima)), typm)
            do 10 nutyma = 1, 18
                if (typm .eq. notyma(nutyma)) then
                    posini = pini(sel,nutyma)
                    posfin = pfin(sel,nutyma)
                    goto 20
                endif
10          continue
            call utmess('F', 'MODELISA4_68', sk=typm)
20          continue
            if (posfin .eq. 0) goto 2
        endif
!
        do 3 ino = posini, posfin
            numno=zi(iacnex-1+ino)
            litrav(numno)= litrav(numno) +1
 3      continue
 2  end do
!
!     -- ON COMPTE LES NOEUDS COCHES ET ON LES RECOPIE DANS LISTNO:
!
    nbno=0
    do 4 i = 1, nbnoto
        if (litrav(i) .gt. 0) then
            nbno=nbno+1
            listno(nbno)=i
        endif
 4  end do
!
    call jedema()
end subroutine
