subroutine conlag(matasz, cond)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/dismoi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    real(kind=8) :: cond
    character(len=*) :: matasz
!
! ----------------------------------------------------------------------
!
! RECUPERATION DU CONDITIONNEMENT DES LAGRANGES D'UNE MATRICE ASSEMBLEE
!
! ----------------------------------------------------------------------
!
! IN   MATASZ : SD MATRICE ASSEMBLEE
! OUT  COND   : CONDITIONNEMENT DES LAGRANGES
!
!
!
!
!
    integer :: jconl, neq, iret, jcol, nbsd, jfetm, idd
    character(len=8) :: k8bid
    character(len=19) :: matass, matdd
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    matass=matasz
    cond=1.d0
    call jeexin(matass//'.FETM', iret)
!
! --- CAS FETI - ON BOUCLE SUR TOUTES LES MATRICES DES SOUS-DOMAINES.
!     DES QUE L'ON TROUVE UN CONDITIONNEMENT DE LAGRANGE, ON SORT CAR
!     CETTE VALEUR EST LA MEME POUR TOUTES LES MATRICES
!
    if (iret .ne. 0) then
        call jeveuo(matass//'.FETM', 'L', jfetm)
        call jelira(matass//'.FETM', 'LONUTI', nbsd, k8bid)
        do 10 idd = 1, nbsd
            matdd=zk24(jfetm+idd-1)
            call jeexin(matdd//'.CONL', iret)
            if (iret .ne. 0) then
                call dismoi('F', 'NB_EQUA', matdd, 'MATR_ASSE', neq,&
                            k8bid, iret)
                call jeveuo(matdd//'.CONL', 'L', jconl)
                do 20 jcol = 1, neq
                    cond = 1.d0/zr(jconl-1+jcol)
                    if (cond .ne. 1.d0) then
                        goto 9999
                    endif
20              continue
            endif
10      continue
!
! --- CAS AUTRE QUE FETI - ON SORT DES QUE L'ON TROUVE UN
!     CONDITIONNEMENT DE LAGRANGE
!
    else
        call jeexin(matass//'.CONL', iret)
        if (iret .ne. 0) then
            call dismoi('F', 'NB_EQUA', matass, 'MATR_ASSE', neq,&
                        k8bid, iret)
            call jeveuo(matass//'.CONL', 'L', jconl)
            do 30 jcol = 1, neq
                cond = 1.d0/zr(jconl-1+jcol)
                if (cond .ne. 1.d0) then
                    goto 9999
                endif
30          continue
        endif
    endif
!
9999  continue
!
    call jedema()
end subroutine
