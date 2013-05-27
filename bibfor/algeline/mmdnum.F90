subroutine mmdnum(neqns, perm, invp, qsize)
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
!--- SPARSPAK-A (ANSI FORTRAN) RELEASE III --- NAME = MMDNUM
!  (C)  UNIVERSITY OF WATERLOO   JANUARY 1984
!***************************************************************
!***************************************************************
!*****     MMDNUM ..... MULTI MINIMUM DEGREE NUMBERING     *****
!***************************************************************
!***************************************************************
!
!     PURPOSE - THIS ROUTINE PERFORMS THE FINAL STEP IN
!        PRODUCING THE PERMUTATION AND INVERSE PERMUTATION
!        VECTORS IN THE MULTIPLE ELIMINATION VERSION OF THE
!        MINIMUM DEGREE ORDERING ALGORITHM.
!
!     INPUT PARAMETERS -
!        NEQNS  - NUMBER OF EQUATIONS.
!        QSIZE  - SIZE OF SUPERNODES AT ELIMINATION.
!
!     UPDATED PARAMETERS -
!        INVP   - INVERSE PERMUTATION VECTOR.  ON INPUT,
!                 IF QSIZE(NODE)=0, THEN NODE HAS BEEN MERGED
!                 INTO THE NODE -INVP(NODE), OTHERWISE,
!                 -INVP(NODE) IS ITS INVERSE LABELLING.
!
!     OUTPUT PARAMETERS -
!        PERM   - THE PERMUTATION VECTOR.
!
!***************************************************************
!
    integer :: invp(*), perm(*), qsize(*)
    integer :: father, neqns, nextf, node, nqsize, num, root
!
!***************************************************************
!
    do 100 node = 1, neqns
        nqsize = qsize(node)
        if (nqsize .le. 0) perm(node) = invp(node)
        if (nqsize .gt. 0) perm(node) = - invp(node)
100  continue
!        ------------------------------------------------------
!        FOR EACH NODE WHICH HAS BEEN MERGED, DO THE FOLLOWING.
!        ------------------------------------------------------
    do 500 node = 1, neqns
        if (perm(node) .gt. 0) goto 500
!                -----------------------------------------
!                TRACE THE MERGED TREE UNTIL ONE WHICH HAS
!                NOT BEEN MERGED, CALL IT ROOT.
!                -----------------------------------------
        father = node
200      continue
        if (perm(father) .gt. 0) goto 300
        father = - perm(father)
        goto 200
300      continue
!                -----------------------
!                NUMBER NODE AFTER ROOT.
!                -----------------------
        root = father
        num = perm(root) + 1
        invp(node) = - num
        perm(root) = num
!                ------------------------
!                SHORTEN THE MERGED TREE.
!                ------------------------
        father = node
400      continue
        nextf = - perm(father)
        if (nextf .le. 0) goto 500
        perm(father) = - root
        father = nextf
        goto 400
500  continue
!        ----------------------
!        READY TO COMPUTE PERM.
!        ----------------------
    do 600 node = 1, neqns
        num = - invp(node)
        invp(node) = num
        perm(num) = node
600  continue
    goto 9999
!
9999  continue
end subroutine
