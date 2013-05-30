function entcod(admodl, lcmodl, nec, mode, k,&
                l)
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
!-----------------------------------------------------------------------
    implicit none
    integer :: entcod
!     IN:
!     MODE: MODE_LOCAL DE TYPE CHNO,VECT,OU MATR.
!     NEC : NBRE D ENTIERS POUR LA GRANDEUR
!     K : NUMERO DE NOEUD ( LOCAL ) ; L : NUMERO D ENTIER CODE
!     OUT:
!     ENTCOD: KEME ENTIER CODE.
!
!-----------------------------------------------------------------------
    include 'jeveux.h'
    include 'asterfort/codent.h'
    include 'asterfort/u2mesk.h'
    integer :: admodl, lcmodl, mode, m1, m2, code, code1
    character(len=8) :: k8b1, k8b2, k8b3, k8b4
    character(len=24) :: valk(4)
!
!
!
!-----------------------------------------------------------------------
    integer :: iad, iadm, iadm1, k, l, n1, n2
    integer :: nec
!-----------------------------------------------------------------------
    iadm = admodl + zi(lcmodl+mode-1) - 1
    code = zi(iadm)
    if (code .gt. 3) then
        if (code .eq. 4) then
            m1 = zi(iadm+3)
        else if (code.eq.5) then
            m1 = zi(iadm+3)
            m2 = zi(iadm+4)
            if (m1 .ne. m2) then
                call codent(m1, 'D', k8b1)
                call codent(m2, 'D', k8b2)
                valk(1) = k8b1
                valk(2) = k8b2
                call u2mesk('F', 'CALCULEL2_46', 2, valk)
            endif
        endif
        iadm1 = admodl + zi(lcmodl+m1-1) - 1
        code1 = zi(iadm1)
        if (code1 .gt. 3) then
            call codent(mode, 'D', k8b1)
            call codent(code, 'D', k8b2)
            call codent(m1, 'D', k8b3)
            call codent(code1, 'D', k8b4)
            valk(1) = k8b1
            valk(2) = k8b2
            valk(3) = k8b3
            valk(4) = k8b4
            call u2mesk('F', 'CALCULEL2_47', 4, valk)
        endif
    else
        iadm1 = iadm
        m1 = mode
    endif
    n1 = zi(iadm1+3)
!
    if (n1 .gt. 10000) then
        n2 = n1 - 10000
        if (k .gt. n2) then
            call codent(m1, 'D', k8b1)
            call codent(n2, 'D', k8b2)
            call codent(k, 'D', k8b3)
            valk(1) = k8b1
            valk(2) = k8b2
            valk(3) = k8b3
            call u2mesk('F', 'CALCULEL2_48', 3, valk)
        endif
        iad = 4 + nec* (k-1) + l
    else
        iad = 4 + l
    endif
    entcod = zi(iadm1+iad-1)
!
end function
