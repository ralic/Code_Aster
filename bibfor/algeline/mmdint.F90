subroutine mmdint(neqns, xadj, dhead, dforw, dbakw,&
                  qsize, llist, marker)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! person_in_charge: olivier.boiteau at edf.fr
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
!--- SPARSPAK-A (ANSI FORTRAN) RELEASE III --- NAME = MMDINT
!  (C)  UNIVERSITY OF WATERLOO   JANUARY 1984
!***************************************************************
!***************************************************************
!***     MMDINT ..... MULT MINIMUM DEGREE INITIALIZATION     ***
!***************************************************************
!***************************************************************
!
!     PURPOSE - THIS ROUTINE PERFORMS INITIALIZATION FOR THE
!        MULTIPLE ELIMINATION VERSION OF THE MINIMUM DEGREE
!        ALGORITHM.
!
!     INPUT PARAMETERS -
!        NEQNS  - NUMBER OF EQUATIONS.
!        XADJ   - ADJACENCY STRUCTURE.
!
!     OUTPUT PARAMETERS -
!        (DHEAD,DFORW,DBAKW) - DEGREE DOUBLY LINKED STRUCTURE.
!        QSIZE  - SIZE OF SUPERNODE (INITIALIZED TO ONE).
!        LLIST  - LINKED LIST.
!        MARKER - MARKER VECTOR.
!
!***************************************************************
!
    integer :: dbakw(*), dforw(*), dhead(*), llist(*), marker(*), qsize(*)
    integer :: xadj(*)
    integer :: fnode, ndeg, neqns, node
!
!***************************************************************
!
    do 100 node = 1, neqns
        dhead(node) = 0
        qsize(node) = 1
        marker(node) = 0
        llist(node) = 0
100  continue
!        ------------------------------------------
!        INITIALIZE THE DEGREE DOUBLY LINKED LISTS.
!        ------------------------------------------
    do 200 node = 1, neqns
        ndeg = xadj(node+1) - xadj(node) + 1
        fnode = dhead(ndeg)
        dforw(node) = fnode
        dhead(ndeg) = node
        if (fnode .gt. 0) dbakw(fnode) = node
        dbakw(node) = - ndeg
200  continue
    goto 9999
!
9999  continue
end subroutine
