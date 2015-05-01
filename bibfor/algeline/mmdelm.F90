subroutine mmdelm(mdnode, xadj, adjncy, dhead, dforw,&
                  dbakw, qsize, llist, marker, maxint,&
                  tag, parent)
! person_in_charge: olivier.boiteau at edf.fr
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!--- SPARSPAK-A (ANSI FORTRAN) RELEASE III --- NAME = MMDELM
!  (C)  UNIVERSITY OF WATERLOO   JANUARY 1984
!   C.ROSE AJ    DE PARENT + MODIFICA STRUCTURE : IF THEN ELSE ET DO WHI
!***************************************************************
!***************************************************************
!**     MMDELM ..... MULTIPLE MINIMUM DEGREE ELIMINATION     ***
!***************************************************************
!***************************************************************
!
!     PURPOSE - THIS ROUTINE ELIMINATES THE NODE MDNODE OF
!        MINIMUM DEGREE FROM THE ADJACENCY STRUCTURE, WHICH
!        IS STORED IN THE QUOTIENT GRAPH FORMAT.  IT ALSO
!        TRANSFORMS THE QUOTIENT GRAPH REPRESENTATION OF THE
!        ELIMINATION GRAPH.
!
!     INPUT PARAMETERS -
!        MDNODE - NODE OF MINIMUM DEGREE.
!        MAXINT - ESTIMATE OF MAXIMUM REPRESENTABLE (SHORT)
!                 INTEGER.
!        TAG    - TAG VALUE.
!
!     UPDATED PARAMETERS -
!        (XADJ,ADJNCY) - UPDATED ADJACENCY STRUCTURE.
!        (DHEAD,DFORW,DBAKW) - DEGREE DOUBLY LINKED STRUCTURE.
!        QSIZE  - SIZE OF SUPERNODE.
!        MARKER - MARKER VECTOR.
!        LLIST  - TEMPORARY LINKED LIST OF ELIMINATED NABORS.
!
!***************************************************************
!
    integer :: adjncy(*), dbakw(*), dforw(*)
    integer :: llist(*), marker(*), qsize(*), dhead(*)
    integer :: xadj(*), parent(*)
    integer :: elmnt, i, istop, istrt, j, jstop, jstrt, link, maxint, mdnode
    integer :: nabor, node, npv, nqnbrs, nxnode, pvnode, rlmt, rloc, rnode, tag
    integer :: xqnbr
!
!***************************************************************
!
!        -----------------------------------------------
!        FIND REACHABLE SET AND PLACE IN DATA STRUCTURE.
!        -----------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    marker(mdnode) = tag
    istrt = xadj(mdnode)
    istop = xadj(mdnode+1) - 1
!        -------------------------------------------------------
!        ELMNT POINTS TO THE BEGINNING OF THE LIST OF ELIMINATED
!        NABORS OF MDNODE, AND RLOC GIVES THE STORAGE LOCATION
!        FOR THE NEXT REACHABLE NODE.
!        -------------------------------------------------------
    elmnt = 0
    rloc = istrt
    rlmt = istop
    do 110 i = istrt, istop
        nabor = adjncy(i)
        if (nabor .eq. 0) goto 120
        if (marker(nabor) .lt. tag) then
            marker(nabor) = tag
            if (dforw(nabor) .ge. 0) then
                adjncy(rloc) = nabor
                rloc = rloc + 1
            else
                llist(nabor) = elmnt
                elmnt = nabor
                parent(nabor) = mdnode
            endif
        endif
110  end do
120  continue
!            -----------------------------------------------------
!            MERGE WITH REACHABLE NODES FROM GENERALIZED ELEMENTS.
!            -----------------------------------------------------
!      DO WHILE (ELMNT.GT.0)
130  continue
    if (elmnt .gt. 0) then
        adjncy(rlmt) = -elmnt
        link = elmnt
140      continue
        jstrt = xadj(link)
        jstop = xadj(link+1) - 1
        do 160 j = jstrt, jstop
            node = adjncy(j)
            link = -node
            if (node .lt. 0) goto 140
            if (node .eq. 0) goto 170
            if (marker(node) .lt. tag .and. dforw(node) .ge. 0) then
                marker(node) = tag
!                            ---------------------------------
!                            USE STORAGE FROM ELIMINATED NODES
!                            IF NECESSARY.
!                            ---------------------------------
!            DO WHILE (RLOC.GE.RLMT)
150              continue
                if (rloc .ge. rlmt) then
                    link = -adjncy(rlmt)
                    rloc = xadj(link)
                    rlmt = xadj(link+1) - 1
                    goto 150
! FIN DO WHILE
                endif
                adjncy(rloc) = node
                rloc = rloc + 1
            endif
160      continue
170      continue
        elmnt = llist(elmnt)
        goto 130
! FIN DO WHILE
    endif
    if (rloc .le. rlmt) adjncy(rloc) = 0
!        --------------------------------------------------------
!        FOR EACH NODE IN THE REACHABLE SET, DO THE FOLLOWING ...
!        --------------------------------------------------------
    link = mdnode
180  continue
    istrt = xadj(link)
    istop = xadj(link+1) - 1
    do 210 i = istrt, istop
        rnode = adjncy(i)
        link = -rnode
        if (rnode .eq. 0) goto 220
        if (rnode .lt. 0) goto 180
!                --------------------------------------------
!                IF RNODE IS IN THE DEGREE LIST STRUCTURE ...
!                --------------------------------------------
        pvnode = dbakw(rnode)
        if (pvnode .ne. 0 .and. pvnode .ne. (-maxint)) then
!                    -------------------------------------
!                    THEN REMOVE RNODE FROM THE STRUCTURE.
!                    -------------------------------------
            nxnode = dforw(rnode)
            if (nxnode .gt. 0) dbakw(nxnode) = pvnode
            if (pvnode .gt. 0) dforw(pvnode) = nxnode
            npv = -pvnode
            if (pvnode .lt. 0) dhead(npv) = nxnode
        endif
!                ----------------------------------------
!                PURGE INACTIVE QUOTIENT NABORS OF RNODE.
!                ----------------------------------------
        jstrt = xadj(rnode)
        jstop = xadj(rnode+1) - 1
        xqnbr = jstrt
        do 190 j = jstrt, jstop
            nabor = adjncy(j)
            if (nabor .eq. 0) goto 200
            if (marker(nabor) .lt. tag) then
                adjncy(xqnbr) = nabor
                xqnbr = xqnbr + 1
            endif
190      continue
200      continue
!                ----------------------------------------
!                IF NO ACTIVE NABOR AFTER THE PURGING ...
!                ----------------------------------------
        nqnbrs = xqnbr - jstrt
        if (nqnbrs .le. 0) then
!                    -----------------------------
!                    THEN MERGE RNODE WITH MDNODE.
!                    -----------------------------
            qsize(mdnode) = qsize(mdnode) + qsize(rnode)
            qsize(rnode) = 0
            marker(rnode) = maxint
            dforw(rnode) = -mdnode
            dbakw(rnode) = -maxint
        else
!                --------------------------------------
!                ELSE FLAG RNODE FOR DEGREE UPDATE, AND
!                ADD MDNODE AS A NABOR OF RNODE.
!                --------------------------------------
            dforw(rnode) = nqnbrs + 1
            dbakw(rnode) = 0
            adjncy(xqnbr) = mdnode
            xqnbr = xqnbr + 1
            if (xqnbr .le. jstop) adjncy(xqnbr) = 0
        endif
!
210  end do
220  continue
end subroutine
