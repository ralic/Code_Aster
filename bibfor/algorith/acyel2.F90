subroutine acyel2(nmcolz, nomobz, nobl, nobc, okpart,&
                  lilig, nblig, licol, nbcol, cmat,&
                  ndim, ideb, jdeb, x)
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
!***********************************************************************
!    P. RICHARD     DATE 10/04/91
!-----------------------------------------------------------------------
!  BUT:  ASSEMBLER SI ELLE EXISTE LA SOUS-MATRICE  CORRESPONDANT
!  A UN NOM OBJET DE COLLECTION DANS UNE MATRICE COMPLEXE AVEC
!   UN ASSEMBLAGE EN DEUX TEMPS (ADAPTE AU CYCLIQUE)
!
!-----------------------------------------------------------------------
!
! NMCOLZ   /I/: NOM K24 DE LA COLLECTION
! NOMOBZ   /I/: NOM K8 DE L'OBJET DE COLLECTION
! NOBL     /I/: NOMBRE DE LIGNE DE LA MATRICE ELEMENTAIRE
! NOBC     /I/: NOMBRE DE COLONNES DE LA MATRICE ELEMENTAIRE
! OKPART   /I/: INDICATEUR SI ASSEMBLAGE PARTIEL
! LILIG    /I/: LISTE DES INDICE DE LIGNE A ASSEMBLER (SI OKPART=TRUE)
! NBLIG    /I/: NOMBRE DE LIGNES DE LA LISTE
! LICOL    /I/: LISTE INDICES DE COLONNES A ASSEMBLER (SI OKPART=TRUE)
! NBLIG    /I/: NOMBRE DE COLONNES DE LA LISTE
! XMAT     /M/: MATRICE RECEPTRICE
! NDIM     /I/: DIMENSION DE LA MATRICE RECEPTRICE CARREE
! IDEB     /I/: INDICE DE PREMIERE LIGNE RECEPTRICE
! JDEB     /I/: INDICE DE PREMIERE COLONNE RECEPTRICE
! X        /I/: COEFFICIENT ASSEMBLAGE
!
!
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/ampcpr.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
!-----------------------------------------------------------------------
    integer :: i, iad, ibid, ideb, iret, j, jdeb
    integer :: llob, nbcol, nblig, ndim, nobc, nobl
    real(kind=8) :: x    
    character(len=8) :: nomob
    character(len=24) :: nomcol
    character(len=*) :: nmcolz, nomobz
    complex(kind=8) :: cmat(ndim, ndim)
    integer :: lilig(nblig), licol(nbcol)
    aster_logical :: okpart
!-----------------------------------------------------------------------
    call jemarq()
    nomcol = nmcolz
    nomob = nomobz
    call jenonu(jexnom(nomcol(1:15)//'.REPE.MAT', nomob), iret)
!
    if (iret .eq. 0) goto 9999
!
    call jenonu(jexnom(nomcol(1:15)//'.REPE.MAT', nomob), ibid)
    call jeveuo(jexnum(nomcol, ibid), 'L', llob)
!
    if (okpart) then
!
! SI ASSEMBLAGE PARTIEL ON TRAITE LIGNE PAR LIGNE
!        ET COLONNE PAR COLONNE
!
        do 10 j = 1, nbcol
            do 20 i = 1, nblig
                iad=llob+(licol(j)-1)*nobl+lilig(i)-1
                call ampcpr(cmat, ndim, ndim, zr(iad), 1,&
                            1, ideb+i-1, jdeb+ j-1, x, 1,&
                            1)
                call ampcpr(cmat, ndim, ndim, zr(iad), 1,&
                            1, jdeb+j-1, ideb+ i-1, x, 1,&
                            1)
 20         continue
 10     continue
!
    else
        call ampcpr(cmat, ndim, ndim, zr(llob), nobl,&
                    nobc, ideb, jdeb, x, 1,&
                    1)
        call ampcpr(cmat, ndim, ndim, zr(llob), nobl,&
                    nobc, jdeb, ideb, x, 1,&
                    - 1)
!
    endif
!
!
9999 continue
    call jedema()
end subroutine
