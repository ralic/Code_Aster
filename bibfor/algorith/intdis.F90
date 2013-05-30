subroutine intdis(coint, nnoint, noddli, ddlsst, nbsst)
    implicit none
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
!-----------------------------------------------------------------------
!    M. CORUS     DATE 05/02/10
!-----------------------------------------------------------------------
!
!  BUT:      < DETERMINATION DES PARTIES D'INTERFACES DISJOINTES >
!
!-----------------------------------------------------------------------
!  IN  : COINT  : DEFINITION DE LA CONNECTIVITE DE L'INTERFACE
!  IN  : NNOINT  : NOMBRE DE NOEUD A L'INTERFACE
!  IN  : NODDLI : DEFINITION DES DDL PORTES PAR LES NOEUDS D'INTERFACE
!  OUT : DDLSST   : DEFINITION DES DDL POUR CHAQUE PARTIE D'INTERFACE
!  OUT : NBSST    : NOMBRE DE PARTIE D'INTERFACE DISJOINTES
!-----------------------------------------------------------------------
!
!
!
!
!
!     ------------------------------------------------------------------
!
!-- VARIABLES EN ENTREES / SORTIE
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/wkvect.h'
    integer :: nnoint, nbsst
    character(len=24) :: coint, noddli, ddlsst
!
!-- VARIABLES DE LA ROUTINE
    integer :: i1, j1, k1, n1, l1, lindno, ldefin, lconnc, lnoeu, lvec, lind
    integer :: lsst, nz0, nz1, lindin, decal, nbno, nbvois, no, lnddli
!
!-----------C
!--       --C
!-- DEBUT --C
!--       --C
!-----------C
!
    call jemarq()
!
!-- CONSTRUCTION DE LA CONNECTIVITE REDUITE
!
    call jeveuo('&&MOIN93.IND_NOEUD', 'L', lindno)
    call wkvect('&&INTDIS.DEFI_SS_LIB', 'V V R', nnoint**2, ldefin)
    call jeveuo(coint, 'L', lconnc)
!
    do 10 i1 = 1, nnoint
        nbvois=zi(lconnc+i1-1)
        do 20 k1 = 1, nbvois
            no=zi(lconnc+nnoint*k1+i1-1)
            j1=zi(lindno+no-1)
            zr(ldefin+(j1-1)*nnoint+i1-1)=1.d0
            zr(ldefin+(j1-1)*nnoint+j1-1)=1.d0
            zr(ldefin+(i1-1)*nnoint+i1-1)=1.d0
            zr(ldefin+(i1-1)*nnoint+j1-1)=1.d0
20      continue
10  end do
!
    call wkvect('&&INTDIS.NUMERO_NOEUDS', 'V V I', nnoint, lnoeu)
!
    do 30 i1 = 1, nnoint
        zi(lnoeu+i1-1)=i1
30  end do
!
    call wkvect('&&INTDIS.VECT_TEMP', 'V V R', nnoint, lvec)
    call wkvect('&&INTDIS.VECT_IND_MAT', 'V V I', nnoint, lind)
    call wkvect('&&INTDIS.VECT_INDSST', 'V V I', nnoint, lsst)
!
!-- INITIALISATION
!
    decal=0
    nbsst=0
    nbno=0
    zi(lsst)=1
!
!-- RECHERCHE DES PARTIES DISJOINTES
!
!      DO WHILE (NBNO .LT. NNOINT)
666  continue
    nz0=0
    k1=1
!        DO WHILE (NZ0 .EQ. 0)
667  continue
    if (zi(lnoeu+k1-1) .gt. 0) then
        nz0=1
        zi(lind+decal)=k1
    endif
    k1=k1+1
    if (nz0 .eq. 0) then
        goto 667
    endif
!        END DO
!
    nz1=1
!        DO WHILE (NZ1 .GT. NZ0)
668  continue
    nz0=nz1
    do 40 j1 = 1, nz1
        do 50 i1 = 1, nnoint
            zr(lvec+i1-1)=zr(lvec+i1-1)+ zr(ldefin+(zi(lind+decal+j1-&
            1)-1)*nnoint+i1-1)
50      continue
40  continue
!
    nz1=0
    do 60 i1 = 1, nnoint
        if (zr(lvec+i1-1) .gt. 0.d0) then
            nz1=nz1+1
            zi(lind+decal+nz1-1)=i1
            zi(lnoeu+i1-1)=0
            zr(lvec+i1-1)=0.d0
        endif
60  continue
!
    if (nz1 .gt. nz0) then
        goto 668
    endif
!        END DO
!
    nbsst=nbsst+1
    decal=decal+nz1
    nbno=nbno+nz1
    zi(lsst+2*nbsst-1)=decal
    zi(lsst+2*nbsst)=nbno+1
!
    if (nbno .lt. nnoint) then
        goto 666
    endif
!      END DO
!
    call jeveuo(noddli, 'L', lnddli)
    call wkvect(ddlsst, 'V V I', nbsst*6*nnoint, lindin)
    do 70 i1 = 1, nbsst
        k1=zi(lsst+2*(i1-1))
        l1=zi(lsst+2*(i1-1)+1)
!
        do 80 j1 = k1, l1
            do 90 n1 = 1, 6
                zi(lindin+6*nnoint*(i1-1)+ 6*(zi(lind+j1-1)-1)+n1-1 )&
                = 1
90          continue
80      continue
70  end do
!
!----------------------------------------C
!--                                    --C
!-- DESTRUCTION DES OBJETS TEMPORAIRES --C
!--                                    --C
!----------------------------------------C
!
    call jedetr('&&INTDIS.DEFI_SS_LIB')
    call jedetr('&&INTDIS.NUMERO_NOEUDS')
    call jedetr('&&INTDIS.VECT_TEMP')
    call jedetr('&&INTDIS.VECT_IND_MAT')
    call jedetr('&&INTDIS.VECT_INDSST')
!
!---------C
!--     --C
!-- FIN --C
!--     --C
!---------C
!
    call jedema()
end subroutine
