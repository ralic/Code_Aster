subroutine preml0(n1, n2, diag, col, delg,&
                  prno, deeq, nec, p, q,&
                  lbd1, lbd2, rl, rl1, rl2,&
                  nrl, lt, lmat)
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
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/infniv.h"
#include "asterfort/utmess.h"
    integer :: n1, diag(0:*), col(*)
    integer :: delg(*), prno(*), deeq(*), nec, lbd1(n1), lbd2(n1)
    integer :: rl(4, *), rl1(*), rl2(*)
    integer :: p(*), q(*)
!     VARIABLES LOCALES
    integer :: nrl, lt, n2, ino, num, nobl, i, j, lmat, i2, iddl, ier, ifm, niv
    integer :: idiai, idiai1, ii, li, iconne, nfois, vali(3)
    aster_logical :: nivdbg
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    nivdbg=.false.
    nfois = 0
    iconne = 0
    call infniv(ifm, niv)
!
!---------------------------------------------INITIALISATIONS
    diag(0) = 0
    do 10 i = 1, n1
        p(i) = 0
        lbd1(i) = 0
        lbd2(i) = 0
        q(i) = 0
        rl1(i) = 0
        rl2(i) = 0
 10 end do
!---------------------------------------------CALCUL DE ADJNC1
    lmat = diag(n1)
    n2 = 0
    nrl = 0
    do 30 iddl = 1, n1
        if (delg(iddl) .eq. 0) then
            n2 = n2 + 1
            p(iddl) = n2
            q(n2) = iddl
        else
            ino = deeq(2*iddl-1)
            if (ino .ne. 0) then
!     IDDL EST UN LAGRANGE DE BLOCAGE
                num = -deeq(2*iddl)
                if (num .eq. 0) then
                    vali (1) = iddl
                    vali (2) = ino
                    vali (3) = num
                    call utmess('F', 'ALGELINE5_31', ni=3, vali=vali)
                endif
                nobl = prno((nec+2)* (ino-1)+1)
!     RECHERCHE DE NOBL : NUMERO DU DDL BLOQUE
!     DO WHILE (DEEQ(2*NOBL).NE.NUM)
 20             continue
                if (deeq(2*nobl) .ne. num) then
                    nobl = nobl + 1
                    ASSERT(nobl.le.n1)
                    goto 20
!     FIN DO WHILE
                endif
                if (delg(iddl) .eq. -1) then
                    if (lbd1(nobl) .ne. 0) nfois = nfois + 1
                    lbd1(nobl) = iddl
                else if (delg(iddl).eq.-2) then
                    if (lbd2(nobl) .ne. 0) nfois = nfois + 1
                    lbd2(nobl) = iddl
                else
                    vali (1) = delg(iddl)
                    call utmess('F', 'ALGELINE5_32', si=vali(1))
                endif
                if (nfois .gt. 0) then
                    vali (1) = nobl
                    call utmess('F', 'ALGELINE5_33', si=vali(1))
                endif
            else
!     IDDL EST UN LAGRANGE DE RELATION LINEAIRE
!     POUR CHQE REL. LIN. I,ON A
!     RL(2,I) = LAMBDA2 ET RL(1,I) = LAMBDA1
                if (delg(iddl) .eq. -2) then
                    nrl = nrl + 1
                    rl(2,nrl) = iddl
!     RL(1,NRL) SERA DEFINI DANS PREMLC, COMME LE NO DE COLONNE
!     DU 1ER TERME DE LA LIGNE RL(2,NRL).
                endif
            endif
        endif
 30 end do
!     CALCUL DE LA TAILLE DE LA LISTE
    lt = 0
    do 40 i = 1, nrl
        i2 = rl(2,i)
        lt = lt + (diag(i2)-diag(i2-1))
 40 end do
!     ON MAJORE LT POUR LES PETITS CAS-TESTS
    if (lt .le. 10) then
        lt = lt**2
    else
        lt = lt*10
    endif
!
!     VERIFICATION DES CONNEXIONS DES LAGRANGES
    if (nivdbg) then
        do 80 i = 1, n1
            li = lbd1(i)
            if (li .ne. 0) then
                idiai1 = diag(li-1) + 1
                idiai = diag(li)
                if (idiai1 .lt. idiai) then
!
                    write(ifm,*)'LE DDL BLOQUE: ',i,' A POUR LAMBDA1: ',lbd1(i)
                    write(ifm,*)'LE DDL BLOQUE: ',i,' A POUR LAMBDA2: ',lbd2(i)
                    write(ifm,*)'LE LAMBDA1 ',lbd1(i),&
     &               ' A POUR VOISIN INATTENDUS '
                    do 50 j = idiai1, idiai - 1
                        write(ifm,*) 'LE DDL ', col(j)
                        iconne = iconne + 1
 50                 continue
                endif
                do 70 ii = li + 1, n1
                    idiai1 = diag(ii-1) + 1
                    idiai = diag(ii)
                    do 60 j = idiai1, idiai
                        if (col(j) .eq. li) then
                            if (ii .ne. i .and. ii .ne. lbd2(i)) then
                                write(ifm,*)'LE DDL BLOQUE: ',i,&
                                ' A POUR LAMBDA1: ',lbd1(i)
                                write(ifm,*)'LE DDL BLOQUE: ',i,&
                                ' A POUR LAMBDA2: ',lbd2(i)
                                write(ifm,*)'LE LAMBDA1 ',lbd1(i),&
                                ' A POUR VOISIN INATTENDU',ii
                                iconne = iconne + 1
                            endif
                        endif
 60                 continue
!
 70             continue
            endif
 80     end do
        if (iconne .gt. 0) then
            call utmess('A', 'ALGELINE5_53')
            write(ifm,*) 2*iconne ,' TERMES SUPPLEMENTAIRES DANS'&
     &'    LA MATRICE INITIALE'
        endif
    endif
!
    if (niv .eq. 2) then
        ier = 0
        do 90 i = 1, n1
            if (lbd1(i) .ne. 0) then
!            WRITE (IFM,*) 'LE DDL BLOQUE: ',I,' A POUR LAMBDA1: ',
!     &        LBD1(I)
!            WRITE (IFM,*) 'LE DDL BLOQUE: ',I,' A POUR LAMBDA2: ',
!     &        LBD2(I)
!            IF (LBD2(I).EQ.0) IER = 1
            else if (lbd2(i).ne.0) then
                ier = 1
            endif
            if (ier .eq. 1) then
                vali (1) = i
                vali (2) = lbd1(i)
                vali (3) = lbd1(i)
                call utmess('F', 'ALGELINE5_34', ni=3, vali=vali)
            endif
!
 90     continue
    endif
end subroutine
