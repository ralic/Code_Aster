subroutine genmmd(neqns, neqp1, nadj, xadj, adjncy,&
                  maxint, delta, invp, perm, nbsn,&
                  supnd, adress, parent, gssubs, fctnzs,&
                  fctops, dhead, qsize, llist, marker)
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
!--- SPARSPAK-A (ANSI FORTRAN) RELEASE III --- NAME = GENMMD
!  (C)  UNIVERSITY OF WATERLOO   JANUARY 1984
!       MODIFIE C.ROSE 24/8/92 : PARENT ET IF THEN ELSE DO WHILE
!         AJ  CALCUL DE GSSUBS,FCTNZS,FCTOPS,ADRESS
!***************************************************************
!***************************************************************
!****     GENMMD ..... MULTIPLE MINIMUM EXTERNAL DEGREE     ****
!***************************************************************
!***************************************************************
!
!     PURPOSE - THIS ROUTINE IMPLEMENTS THE MINIMUM DEGREE
!        ALGORITHM.  IT MAKES USE OF THE IMPLICIT REPRESENTATION
!        OF ELIMINATION GRAPHS BY QUOTIENT GRAPHS, AND THE
!        NOTION OF INDISTINGUISHABLE NODES.  IT ALSO IMPLEMENTS
!        THE MODIFICATIONS BY MULTIPLE ELIMINATION AND MINIMUM
!        EXTERNAL DEGREE.
!        ---------------------------------------------
!        CAUTION - THE ADJACENCY VECTOR ADJNCY WILL BE
!        DESTROYED.
!        ---------------------------------------------
!
!     INPUT PARAMETERS -
!        NEQNS  - NUMBER OF EQUATIONS.
!        (XADJ,ADJNCY) - THE ADJACENCY STRUCTURE.
!        DELTA  - TOLERANCE VALUE FOR MULTIPLE ELIMINATION.
!        MAXINT - MAXIMUM MACHINE REPRESENTABLE (SHORT) INTEGER
!                 (ANY SMALLER ESTIMATE WILL DO) FOR MARKING
!                 NODES.
!
!     OUTPUT PARAMETERS -
!        PERM   - THE MINIMUM DEGREE ORDERING.
!        INVP   - THE INVERSE OF PERM.
!        NOFSUB - AN UPPER BOUND ON THE NUMBER OF NONZERO
!                 SUBSCRIPTS FOR THE COMPRESSED STORAGE SCHEME.
!        LLIST  - VECTOR FOR TEMPORARY LINKED LISTS. DEVIENT INVSUP
!
!     WORKING PARAMETERS -
!        DHEAD  - VECTOR FOR HEAD OF DEGREE LISTS.
!        INVP   - USED TEMPORARILY FOR DEGREE FORWARD LINK.
!        PERM   - USED TEMPORARILY FOR DEGREE BACKWARD LINK.
!        QSIZE  - VECTOR FOR SIZE OF SUPERNODES.
!        MARKER - A TEMPORARY MARKER VECTOR.
!
!     PROGRAMME  ROUTINES -
!        MMDELM, MMDINT, MMDNUM, MMDUPD.
!
!***************************************************************
!
    include 'asterfort/mmdelm.h'
    include 'asterfort/mmdint.h'
    include 'asterfort/mmdnum.h'
    include 'asterfort/mmdpar.h'
    include 'asterfort/mmdupd.h'
    integer :: adjncy(nadj), dhead(neqns), invp(neqns), llist(neqns)
    integer :: marker(neqns), perm(neqns), qsize(neqns)
    integer :: xadj(neqp1), supnd(neqp1), adress(neqp1)
    integer :: delta, ehead, i, maxint, mdeg, mdlmt, mdnode, neqns, nextmd
    integer :: nofsub, num, tag
    integer :: nbsn, parent(neqns)
    integer :: neqp1, nadj, gssubs, fctnzs
    real(kind=8) :: fctops
    integer :: il, is, j, jdeb, jfin, nabor, nbsn1
    integer :: ncol, nlig
!-----------------------------------------------------------------------
!
    if (neqns .le. 0) goto 9999
!
!        ------------------------------------------------
!        INITIALIZATION FOR THE MINIMUM DEGREE ALGORITHM.
!        ------------------------------------------------
    nofsub = 0
    call mmdint(neqns, xadj, dhead, invp, perm,&
                qsize, llist, marker)
!
!        ----------------------------------------------
!        NUM COUNTS THE NUMBER OF ORDERED NODES PLUS 1.
!        ----------------------------------------------
    num = 1
!.ROSE AJ
    nbsn = 0
    do 110 i = 1, neqns
        parent(i) = 0
110  end do
    adress(1) = 1
!.ROSE FIN AJ
!
!        -----------------------------
!        ELIMINATE ALL ISOLATED NODES.
!        -----------------------------
    nextmd = dhead(1)
!      DO WHILE (NEXTMD.GT.0)
120  continue
    if (nextmd .gt. 0) then
        mdnode = nextmd
        nextmd = invp(mdnode)
        marker(mdnode) = maxint
!.ROSE AJ
        nbsn = nbsn + 1
        supnd(nbsn) = num
        adress(nbsn+1) = 1
!.ROSE FIN AJ
        invp(mdnode) = -num
        num = num + 1
        goto 120
! FIN DO WHILE
    endif
!        ----------------------------------------
!        SEARCH FOR NODE OF THE MINIMUM DEGREE.
!        MDEG IS THE CURRENT MINIMUM DEGREE,
!        TAG IS USED TO FACILITATE MARKING NODES.
!        ----------------------------------------
    if (num .gt. neqns) goto 190
    tag = 1
    dhead(1) = 0
    mdeg = 2
130  continue
!      DO WHILE (DHEAD(MDEG).LE.0)
140  continue
    if (dhead(mdeg) .le. 0) then
        mdeg = mdeg + 1
        goto 140
! FIN DO WHILE
    endif
!            -------------------------------------------------
!            USE VALUE OF DELTA TO SET UP MDLMT, WHICH GOVERNS
!            WHEN A DEGREE UPDATE IS TO BE PERFORMED.
!            -------------------------------------------------
    mdlmt = mdeg + delta
    ehead = 0
!
150  continue
    mdnode = dhead(mdeg)
!      DO WHILE (MDNODE.LE.0)
160  continue
    if (mdnode .le. 0) then
        mdeg = mdeg + 1
        if (mdeg .gt. mdlmt) goto 180
        mdnode = dhead(mdeg)
        goto 160
! FIN DO WHILE
    endif
!                ----------------------------------------
!                REMOVE MDNODE FROM THE DEGREE STRUCTURE.
!                ----------------------------------------
    nextmd = invp(mdnode)
    dhead(mdeg) = nextmd
    if (nextmd .gt. 0) perm(nextmd) = -mdeg
!.ROSE AJ
    nbsn = nbsn + 1
    supnd(nbsn) = num
    adress(nbsn+1) = mdeg + qsize(mdnode) - 1
!.ROSE FIN AJ   .................................................
    invp(mdnode) = -num
    nofsub = nofsub + mdeg + qsize(mdnode) - 2
    if (num+qsize(mdnode) .gt. neqns) goto 190
!                ----------------------------------------------
!                ELIMINATE MDNODE AND PERFORM QUOTIENT GRAPH
!                TRANSFORMATION.  RESET TAG VALUE IF NECESSARY.
!                ----------------------------------------------
    tag = tag + 1
    if (tag .ge. maxint) then
        tag = 1
        do 170 i = 1, neqns
            if (marker(i) .lt. maxint) marker(i) = 0
170      continue
    endif
    call mmdelm(mdnode, xadj, adjncy, dhead, invp,&
                perm, qsize, llist, marker, maxint,&
                tag, parent)
!                                     AJ   ...........................
    num = num + qsize(mdnode)
    llist(mdnode) = ehead
    ehead = mdnode
    if (delta .ge. 0) goto 150
180  continue
!            -------------------------------------------
!            UPDATE DEGREES OF THE NODES INVOLVED IN THE
!            MINIMUM DEGREE NODES ELIMINATION.
!            -------------------------------------------
    if (num .gt. neqns) goto 190
    call mmdupd(ehead, neqns, xadj, adjncy, delta,&
                mdeg, dhead, invp, perm, qsize,&
                llist, marker, maxint, tag)
    goto 130
!
190  continue
    if (mdnode .gt. 0) then
!        ON TERMINE PARENT NODAL
        do 200 i = xadj(mdnode), xadj(mdnode+1) - 1
            nabor = adjncy(i)
            if (nabor .eq. 0) goto 210
            if (invp(nabor) .lt. 0) then
                parent(nabor) = mdnode
            endif
200      continue
    endif
210  continue
    nbsn1 = nbsn + 1
    supnd(nbsn1) = neqns + 1
    gssubs = 0
    fctnzs = 0
    fctops = 0.0d0
    do 230 is = 1, nbsn
        jdeb = supnd(is)
        jfin = supnd(is+1) - 1
        ncol = jfin - jdeb + 1
        nlig = adress(is+1)
        gssubs = gssubs + nlig
        fctnzs = fctnzs + nlig*ncol - (ncol* (ncol+1))/2
        il = nlig
        do 220 j = jdeb, jfin
            il = il - 1
            fctops = fctops + dble(il* (il+3))
220      continue
        adress(is+1) = adress(is) + nlig
230  end do
    call mmdnum(neqns, perm, invp, qsize)
!        CALCUL DE PARENT EN SUPERNOEUDS MMDPAR SUR CRAY UTILISE QSIZE
!                          EN DERNIER ARGUMENT ??? TABLEAU DE TRAVAIL ?
!          CALL MMDPAR(NEQNS,NBSN,NBSN1,SUPND,INVP,PARENT,DHEAD,QSIZE)
    call mmdpar(neqns, nbsn, nbsn1, supnd, invp,&
                parent, dhead, llist)
    goto 9999
!
9999  continue
end subroutine
