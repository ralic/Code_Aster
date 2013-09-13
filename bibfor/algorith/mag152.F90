subroutine mag152(n9, n10, nomres, nugene, modmec,&
                  modgen, nbloc, indice)
    implicit none
!---------------------------------------------------------------------
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
!---------------------------------------------------------------------
! AUTEUR : G.ROUSSEAU
! CREATION DE LA MATRICE ASSEMBLEE GENERALISEE AU FORMAT LDLT :
!      - OBJET    .UALF
!      - STOCKAGE .SLCS
! ET REMPLISSAGE DE SES OBJETS AUTRES QUE LE .UALF
!---------------------------------------------------------------------
#include "jeveux.h"
#include "asterfort/getvid.h"
#include "asterfort/jecrec.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    integer :: indice, imodeg
    integer :: jrefa, i, iscbl, iaconl
    integer :: ialime, iblo
    integer :: ischc
    integer :: somme
    integer :: jscde, n1bloc, n2bloc
    integer :: nbid, nbloc, ntbloc, nueq, nhmax
    integer :: n9, n10
    character(len=8) :: nomres, modmec, nummod
    character(len=8) :: modgen
    character(len=14) :: num14, nugene
    character(len=19) :: nomsto
! -----------------------------------------------------------------
!
!        CAS NUME_DDL_GENE  PRESENT
!
    call jemarq()
!
    call wkvect(nomres//'           .REFA', 'G V K24', 11, jrefa)
    zk24(jrefa-1+11)='MPI_COMPLET'
    nomsto=nugene//'.SLCS'
!
    if ((n9.gt.0)) then
        call jeveuo(nomsto//'.SCDE', 'L', jscde)
        nueq = zi(jscde-1+1)
        ntbloc = zi(jscde-1+2)
        nbloc = zi(jscde-1+3)
        nhmax = zi(jscde-1+4)
!
!
! TEST SUR LE MODE DE STOCKAGE : SI ON N EST PAS EN STOCKAGE
! LIGNE DE CIEL PLEIN ON PLANTE
!
        if (nueq .ne. nhmax) then
            call utmess('A', 'ALGORITH5_16')
        endif
!
        if ((nueq* (nueq+1)/2) .gt. (nbloc*ntbloc)) then
            call utmess('F', 'ALGORITH5_17')
        endif
!
! CALCUL DU NOMBRE DE TERME PAR BLOC ET TOTAL
!
        call jeveuo(nomsto//'.SCBL', 'L', iscbl)
        call jeveuo(nomsto//'.SCHC', 'L', ischc)
!
        somme = 0
!
        do 20 iblo = 1, nbloc
!
!----------------------------------------------------------------
!
!         BOUCLE SUR LES COLONNES DE LA MATRICE ASSEMBLEE
!
            n1bloc = zi(iscbl+iblo-1) + 1
            n2bloc = zi(iscbl+iblo)
!
!
            do 10 i = n1bloc, n2bloc
                somme = somme + zi(ischc+i-1)
10          continue
20      continue
!
        write (6,*) 'SOMME=',somme
        if ((nueq* (nueq+1)/2) .ne. somme) then
            call utmess('F', 'ALGORITH5_18')
        endif
!
!
!
        call jecrec(nomres//'           .UALF', 'G V R', 'NU', 'DISPERSE', 'CONSTANT',&
                    nbloc)
        call jeecra(nomres//'           .UALF', 'LONMAX', ntbloc)
!
!
        call wkvect(nomres//'           .LIME', 'G V K24', 1, ialime)
        call wkvect(nomres//'           .CONL', 'G V R', nueq, iaconl)
!
!       CAS DU CHAM_NO
!
    else
!
        call jeveuo(nomsto//'.SCDE', 'L', jscde)
        nueq = zi(jscde-1+1)
        nbloc = 1
        ntbloc = nueq* (nueq+1)/2
!
        call jecrec(nomres//'           .UALF', 'G V R', 'NU', 'DISPERSE', 'CONSTANT',&
                    nbloc)
        call jeecra(nomres//'           .UALF', 'LONMAX', ntbloc)
        call wkvect(nomres//'           .LIME', 'G V K24', 1, ialime)
        call wkvect(nomres//'           .CONL', 'G V R', nueq, iaconl)
!
    endif
!
! ----------- REMPLISSAGE DU .REFA ET DU .LIME---------------
!---------------------ET DU .CONL ---------------------------
!
!
    if (n10 .gt. 0) then
        zk24(jrefa-1+1) = ' '
!
    else if (indice.eq.1) then
        call getvid(' ', 'NUME_DDL_GENE', scal=nummod, nbret=nbid)
        num14 = nummod
        call jeveuo(num14//'.NUME.REFN', 'L', imodeg)
        zk24(jrefa-1+1) = zk24(imodeg)
!
    else
        zk24(jrefa-1+1) = modmec
    endif
!
    zk24(jrefa-1+2) = nugene
    zk24(jrefa-1+9) = 'MS'
    zk24(jrefa-1+10) = 'GENE'
!
    if (n10 .gt. 0) then
        zk24(ialime) = modgen
!
    else
        zk24(ialime) = '  '
    endif
!
    do 30 i = 1, nueq
        zr(iaconl+i-1) = 1.0d0
30  end do
!
    call jedema()
end subroutine
