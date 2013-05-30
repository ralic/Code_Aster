subroutine knincl(long, lk1, l1, lk2, l2,&
                  iret)
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
! person_in_charge: jacques.pellet at edf.fr
! A_UTIL
    implicit none
    include 'asterfort/assert.h'
    include 'asterfort/knindi.h'
    integer :: l1, l2, iret, long
    character(len=*) :: lk1(l1), lk2(l2)
! ---------------------------------------------------------------------
! BUT: VERIFIER QU'UNE LISTE DE K8 (LK1) EST INCLUSE DANS UNE AUTRE
!      (LK2)
! ---------------------------------------------------------------------
!     ARGUMENTS:
! LONG   IN   I     : 8,16 OU 24 : LONGUEUR DES CHAINES DE LK1 ET LK2
! LK1    IN   V(K*) : LISTE DE K* QUI DOIT ETRE INCLUSE DANS LK2
! L1     IN   I     : LONGUEUR DE LA LISTE LK1
! LK2    IN   V(K*) : LISTE DES K* QUI DOIT CONTENIR LK1
! L2     IN   I     : LONGUEUR DE LA LISTE LK2
! IRET   OUT  I     : CODE RETOUR :
!                     /0 : OK : TOUS LES ELEMENTS DE LK1 SONT DANS LK2
!                     /I1>0 : NUMERO DU 1ER ELEMENT DE LK1 NON PRESENT
!                             DANS LK2
!----------------------------------------------------------------------
    integer :: k1, k2
! DEB
!
    call assert((long.eq.8).or.(long.eq.16).or.(long.eq.24))
!
    iret = 0
    do 10,k1 = 1,l1
!          -- ON VERIFIE QUE LK1(K1) SE TROUVE DANS LK2 :
    k2 = knindi(long,lk1(k1),lk2,l2)
!
    if (k2 .eq. 0) then
        iret = k1
        goto 20
    endif
!
    10 end do
!
20  continue
!
!
end subroutine
