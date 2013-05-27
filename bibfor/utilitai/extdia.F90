subroutine extdia(matr, numddl, icode, diag)
    implicit none
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
!***********************************************************************
! 15/03/91    G.JACQUART AMV/P61 47 65 49 41
!***********************************************************************
!
!     FONCTION : EXTRACTION DE LA DIAGONALE D'UNE MATRICE
!
!-----------------------------------------------------------------------
!    MATR   /I/ : NOM DE LA MATRICE
!    NUMDDL /I/ : NUMEROTATION ASSOCIEE A MATR
!    ICODE  /I/ : 2 SI CALCUL TRANSITOIRE DIRECT
!                 1 SI SOUS-STRUCTURATION DYNAMIQUE TRANSITOIRE
!                   SANS DOUBLE PROJECTION
!                 0 SINON
!    DIAG   /O/ : VECTEUR CONTENANT LA DIAGONALE DE MATR
!-----------------------------------------------------------------------
!
!
!
!
!
    include 'jeveux.h'
!
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/mtdscr.h'
    include 'asterfort/typddl.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    character(len=24) :: numddl
    character(len=8) :: matr
    real(kind=8) :: diag(*)
    integer :: icode
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: idia, j, jadia, jbloc, jsmde, jtyp, k
    integer :: l, lmat, nbacti, nbbloq, nblagr, nbliai, neq
!
!-----------------------------------------------------------------------
    call jemarq()
    call mtdscr(matr)
    call jeveuo(matr//'           .&INT', 'L', lmat)
!
    call jeveuo(numddl(1:8)//'      .SMOS.SMDE', 'L', jsmde)
    neq = zi(jsmde-1+1)
!
    call wkvect('&&EXTDIA.TYPDDL', 'V V I', neq, jtyp)
    call typddl('ACTI', numddl(1:8), neq, zi(jtyp), nbacti,&
                nbbloq, nblagr, nbliai)
    if (icode .eq. 2) then
        if (nbliai .gt. 0) then
            call u2mess('F', 'UTILITAI_76')
        endif
    endif
!
    call jeveuo(numddl(1:8)//'      .SMOS.SMDI', 'L', jadia)
    k = 0
    l = 0
    call jeveuo(jexnum(matr//'           .VALM', 1), 'L', jbloc)
    do 40 j = 1, neq
        k = k + 1
        if (zi(jtyp-1+k) .ne. 0) then
            idia=zi(jadia+k-1)
            l=l+1
            diag(l)=zr(jbloc-1+idia)
        else if (icode.eq.0.or.icode.eq.2) then
            l=l+1
            diag(l)=0.d0
        endif
40  end do
    call jedetr('&&EXTDIA.TYPDDL')
!
    call jedema()
end subroutine
