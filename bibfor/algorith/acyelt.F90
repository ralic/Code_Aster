subroutine acyelt(nmcolz, nomobz, nob, cmat, ndim,&
                  ideb, jdeb, x)
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
    implicit none
!
!***********************************************************************
!    P. RICHARD     DATE 10/04/91
!-----------------------------------------------------------------------
!  BUT:  ASSEMBLER SI ELLE EXISTE LA SOUS-MATRICE  CORRESPONDANT
!  A UN NOM OBJET DE COLLECTION DANS UNE MATRICE COMPLEXE AVEC
!   UN ASSEMBLAGE EN UN TEMPS (ADAPTE AU CYCLIQUE)
!   LA SOUS-MATRICE EST SYMETRIQUE ET SE SITUE A CHEVAL SUR LA DIAGONALE
!   ELLE EST ELLE-MEME STOCKEE TRIANGULAIRE SUPERIEURE
!
!-----------------------------------------------------------------------
!
! NMCOLZ   /I/: NOM K24 DE LA COLLECTION
! NOMOBZ   /I/: NOM K8 DE L'OBJET DE COLLECTION
! NOB     /I/: NOMBRE DE LIGNE ET COLONNES DE LA MATRICE ELEMENTAIRE
! CMAT     /M/: MATRICE RECEPTRICE COMPLEXE
! NDIM     /I/: DIMENSION DE LA MATRICE RECEPTRICE CARREE
! IDEB     /I/: INDICE DE PREMIERE LIGNE RECEPTRICE
! JDEB     /I/: INDICE DE PREMIERE COLONNE RECEPTRICE
! X        /I/: COEFFICIENT ASSEMBLAGE
!
!
!
    include 'jeveux.h'
!
    include 'asterfort/ampcpr.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
!
!
    character(len=8) :: nomob
    character(len=24) :: nomcol
    complex(kind=8) :: cmat(*)
    character(len=*) :: nmcolz, nomobz
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, iad, ibid, ideb, iret, j, jdeb
    integer :: llob, ndim, nob
    real(kind=8) :: x
!-----------------------------------------------------------------------
    call jemarq()
    nomob = nomobz
    nomcol = nmcolz
!
    call jenonu(jexnom(nomcol(1:15)//'.REPE.MAT', nomob), iret)
    if (iret .eq. 0) goto 9999
!
    call jenonu(jexnom(nomcol(1:15)//'.REPE.MAT', nomob), ibid)
    call jeveuo(jexnum(nomcol, ibid), 'L', llob)
!
    iad = llob - 1
    do 30 j = 1, nob
        do 30 i = j, 1, -1
            iad = iad + 1
            call ampcpr(cmat, ndim, ndim, zr(iad), 1,&
                        1, ideb-1+i, jdeb-1+j, x, 1,&
                        1)
30      continue
!
!
9999  continue
    call jedema()
end subroutine
