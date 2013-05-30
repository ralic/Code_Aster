subroutine mmdupd(ehead, neqns, xadj, adjncy, delta,&
                  mdeg, dhead, dforw, dbakw, qsize,&
                  llist, marker, maxint, tag)
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
!--- SPARSPAK-A (ANSI FORTRAN) RELEASE III --- NAME = MMDUPD
!  (C)  UNIVERSITY OF WATERLOO   JANUARY 1984
!***************************************************************
!***************************************************************
!*****     MMDUPD ..... MULTIPLE MINIMUM DEGREE UPDATE     *****
!***************************************************************
!***************************************************************
!
!     PURPOSE - THIS ROUTINE UPDATES THE DEGREES OF NODES
!        AFTER A MULTIPLE ELIMINATION STEP.
!
!     INPUT PARAMETERS -
!        EHEAD  - THE BEGINNING OF THE LIST OF ELIMINATED
!                 NODES (I.E., NEWLY FORMED ELEMENTS).
!        NEQNS  - NUMBER OF EQUATIONS.
!        (XADJ,ADJNCY) - ADJACENCY STRUCTURE.
!        DELTA  - TOLERANCE VALUE FOR MULTIPLE ELIMINATION.
!        MAXINT - MAXIMUM MACHINE REPRESENTABLE (SHORT)
!                 INTEGER.
!
!     UPDATED PARAMETERS -
!        MDEG   - NEW MINIMUM DEGREE AFTER DEGREE UPDATE.
!        (DHEAD,DFORW,DBAKW) - DEGREE DOUBLY LINKED STRUCTURE.
!        QSIZE  - SIZE OF SUPERNODE.
!        LLIST  - WORKING LINKED LIST.
!        MARKER - MARKER VECTOR FOR DEGREE UPDATE.
!        TAG    - TAG VALUE.
!
!***************************************************************
!
    integer :: adjncy(*), dbakw(*), dforw(*), dhead(*), llist(*), marker(*)
    integer :: qsize(*)
    integer :: xadj(*)
    integer :: deg, deg0, delta, ehead, elmnt, enode, fnode, i, iq2, istop
    integer :: istrt, j, jstop, jstrt, link, maxint, mdeg, mdeg0, mtag, nabor
    integer :: neqns, node, q2head, qxhead, tag
!
!***************************************************************
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    mdeg0 = mdeg + delta
    elmnt = ehead
100  continue
!            -------------------------------------------------------
!            FOR EACH OF THE NEWLY FORMED ELEMENT, DO THE FOLLOWING.
!            (RESET TAG VALUE IF NECESSARY.)
!            -------------------------------------------------------
    if (elmnt .le. 0) goto 9999
    mtag = tag + mdeg0
    if (mtag .lt. maxint) goto 300
    tag = 1
    do 200 i = 1, neqns
        if (marker(i) .lt. maxint) marker(i) = 0
200  continue
    mtag = tag + mdeg0
300  continue
!            ---------------------------------------------
!            CREATE TWO LINKED LISTS FROM NODES ASSOCIATED
!            WITH ELMNT: ONE WITH TWO NABORS (Q2HEAD) IN
!            ADJACENCY STRUCTURE, AND THE OTHER WITH MORE
!            THAN TWO NABORS (QXHEAD).  ALSO COMPUTE DEG0,
!            NUMBER OF NODES IN THIS ELEMENT.
!            ---------------------------------------------
    q2head = 0
    qxhead = 0
    deg0 = 0
    link = elmnt
400  continue
    istrt = xadj(link)
    istop = xadj(link+1) - 1
    do 700 i = istrt, istop
        enode = adjncy(i)
        link = - enode
        if (enode) 400, 800, 500
!
500      continue
        if (qsize(enode) .eq. 0) goto 700
        deg0 = deg0 + qsize(enode)
        marker(enode) = mtag
!                        ----------------------------------
!                        IF ENODE REQUIRES A DEGREE UPDATE,
!                        THEN DO THE FOLLOWING.
!                        ----------------------------------
        if (dbakw(enode) .ne. 0) goto 700
!                            ---------------------------------------
!                            PLACE EITHER IN QXHEAD OR Q2HEAD LISTS.
!                            ---------------------------------------
        if (dforw(enode) .eq. 2) goto 600
        llist(enode) = qxhead
        qxhead = enode
        goto 700
600      continue
        llist(enode) = q2head
        q2head = enode
700  continue
800  continue
!            --------------------------------------------
!            FOR EACH ENODE IN Q2 LIST, DO THE FOLLOWING.
!            --------------------------------------------
    enode = q2head
    iq2 = 1
900  continue
    if (enode .le. 0) goto 1500
    if (dbakw(enode) .ne. 0) goto 2200
    tag = tag + 1
    deg = deg0
!                    ------------------------------------------
!                    IDENTIFY THE OTHER ADJACENT ELEMENT NABOR.
!                    ------------------------------------------
    istrt = xadj(enode)
    nabor = adjncy(istrt)
    if (nabor .eq. elmnt) nabor = adjncy(istrt+1)
!                    ------------------------------------------------
!                    IF NABOR IS UNELIMINATED, INCREASE DEGREE COUNT.
!                    ------------------------------------------------
    link = nabor
    if (dforw(nabor) .lt. 0) goto 1000
    deg = deg + qsize(nabor)
    goto 2100
1000  continue
!                        --------------------------------------------
!                        OTHERWISE, FOR EACH NODE IN THE 2ND ELEMENT,
!                        DO THE FOLLOWING.
!                        --------------------------------------------
    istrt = xadj(link)
    istop = xadj(link+1) - 1
    do 1400 i = istrt, istop
        node = adjncy(i)
        link = - node
        if (node .eq. enode) goto 1400
        if (node) 1000, 2100, 1100
!
1100      continue
        if (qsize(node) .eq. 0) goto 1400
        if (marker(node) .ge. tag) goto 1200
!                                -------------------------------------
!                                CASE WHEN NODE IS NOT YET CONSIDERED.
!                                -------------------------------------
        marker(node) = tag
        deg = deg + qsize(node)
        goto 1400
1200      continue
!                            ----------------------------------------
!                            CASE WHEN NODE IS INDISTINGUISHABLE FROM
!                            ENODE.  MERGE THEM INTO A NEW SUPERNODE.
!                            ----------------------------------------
        if (dbakw(node) .ne. 0) goto 1400
        if (dforw(node) .ne. 2) goto 1300
        qsize(enode) = qsize(enode) + qsize(node)
        qsize(node) = 0
        marker(node) = maxint
        dforw(node) = - enode
        dbakw(node) = - maxint
        goto 1400
1300      continue
!                            --------------------------------------
!                            CASE WHEN NODE IS OUTMATCHED BY ENODE.
!                            --------------------------------------
        if (dbakw(node) .eq. 0) dbakw(node) = - maxint
1400  continue
    goto 2100
1500  continue
!                ------------------------------------------------
!                FOR EACH ENODE IN THE QX LIST, DO THE FOLLOWING.
!                ------------------------------------------------
    enode = qxhead
    iq2 = 0
1600  continue
    if (enode .le. 0) goto 2300
    if (dbakw(enode) .ne. 0) goto 2200
    tag = tag + 1
    deg = deg0
!                        ---------------------------------
!                        FOR EACH UNMARKED NABOR OF ENODE,
!                        DO THE FOLLOWING.
!                        ---------------------------------
    istrt = xadj(enode)
    istop = xadj(enode+1) - 1
    do 2000 i = istrt, istop
        nabor = adjncy(i)
        if (nabor .eq. 0) goto 2100
        if (marker(nabor) .ge. tag) goto 2000
        marker(nabor) = tag
        link = nabor
!                                ------------------------------
!                                IF UNELIMINATED, INCLUDE IT IN
!                                DEG COUNT.
!                                ------------------------------
        if (dforw(nabor) .lt. 0) goto 1700
        deg = deg + qsize(nabor)
        goto 2000
1700      continue
!                                    -------------------------------
!                                    IF ELIMINATED, INCLUDE UNMARKED
!                                    NODES IN THIS ELEMENT INTO THE
!                                    DEGREE COUNT.
!                                    -------------------------------
        jstrt = xadj(link)
        jstop = xadj(link+1) - 1
        do 1900 j = jstrt, jstop
            node = adjncy(j)
            link = - node
            if (node) 1700, 2000, 1800
!
1800          continue
            if (marker(node) .ge. tag) goto 1900
            marker(node) = tag
            deg = deg + qsize(node)
1900      continue
2000  continue
2100  continue
!                    -------------------------------------------
!                    UPDATE EXTERNAL DEGREE OF ENODE IN DEGREE
!                    STRUCTURE, AND MDEG (MIN DEG) IF NECESSARY.
!                    -------------------------------------------
    deg = deg - qsize(enode) + 1
    fnode = dhead(deg)
    dforw(enode) = fnode
    dbakw(enode) = - deg
    if (fnode .gt. 0) dbakw(fnode) = enode
    dhead(deg) = enode
    if (deg .lt. mdeg) mdeg = deg
2200  continue
!                    ----------------------------------
!                    GET NEXT ENODE IN CURRENT ELEMENT.
!                    ----------------------------------
    enode = llist(enode)
    if (iq2 .eq. 1) goto 900
    goto 1600
2300  continue
!            -----------------------------
!            GET NEXT ELEMENT IN THE LIST.
!            -----------------------------
    tag = mtag
    elmnt = llist(elmnt)
    goto 100
!
9999  continue
end subroutine
