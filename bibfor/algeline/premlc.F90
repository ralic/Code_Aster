subroutine premlc(n1, diag, col, parent, parend,&
                  anc, nouv, supnd, supnd2, nouvsn,&
                  ancsn, p, q, lbd1, lbd2,&
                  rl, rl1, rl2, nrl, invp,&
                  perm, lgind, ddlmoy, nbsnd)
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
!     VERSION O2000 AVEC CREATION D'UN NOUVEAU SN POUR
!     CHAQUE LAMBDA1 DE LAGRANGE
!     11/12/98
!
! aslint: disable=W1504
    implicit none
#include "asterfort/infbav.h"
#include "asterfort/infmue.h"
#include "asterfort/infniv.h"
#include "asterfort/jeexin.h"
#include "asterfort/utmess.h"
    integer :: n1, diag(0:*), col(*), lgind, ddlmoy
    integer :: parent(*)
    integer :: nbsn, parend(*)
    integer :: anc(n1), nouv(n1), supnd(n1), supnd2(n1), lbd1(n1), lbd2(n1)
    integer :: invp(n1), perm(n1)
    integer :: rl(4, *), rl1(*), rl2(*)
!     VARIABLES LOCALES
    integer :: i, j, ier, ifm, niv
    integer :: i1, i2, iddl, iddl1, iddl2, num, isn
    integer :: nouvsn(0:n1), ancsn(*), p(*), q(*)
    integer :: nrl, maxrl, minrl, nbsnd, j1, j2, ianc, ip, ipp
    integer :: vali(3)
!--------------------------------------------------------------
!      5) POUR LES REL.LIN.,ON FAIT RL1(I)=LAMBD1,I ETANT LE
!        DDL DE REL.LIN.
!          DONT L'IMAGE PAR LA NOUVELLE NUMEROTATION EST
!          L'INF DES DDL. ENCADRES
!      6) ON ECRIT LA NOUVELLE NUMEROTATION DE TOUS
!          LES DDL APRES GENMMD
!         => TAB NOUV ET ANC (1:N1) <-> (1:N2)
!            LAMBDA1  EST AMALGAME AU PREMIER ND ENCADRE (*)
! (*)        CECI EST POSSIBLE CAR TOUS LES NOEUDS D'UNE RELATION
!            LINEAIRE SONT FORCES A ETRE VOISINS ( DS CALADJ)
!            LAMBDA2 LUI EST AMALGAME AU DERNIER ND ENCADRE
!            NBSND : NBRE DE SND AVEC LES LAMBDA1 = NBSN + NRL
!            PARENTD REPRESENTE LE NOUVEL ARBRE D'ELIMINATION
!       RQE GENERALE AVEC LES REL.LIN. ON UTILISE LA DONNEE SUIVANTE :
!       LES DDL ENCADRES SONT DEFINIS PAR
!      ( COL(J),J=DIAG(LAMBDA2-1)+2,DIAG(LAMBDA2)-1 )
!-----------------------------------------------------------------------
    call infniv(ifm, niv)
!
!------------------------------- RELATIONS LINEAIRES
    do i = 1, nrl
        iddl2 = rl(2,i)
        minrl = n1 + 1
        maxrl = 0
        iddl1 = col(diag(iddl2-1)+1)
        if ((diag(iddl2)-diag(iddl2-1)) .le. 2) then
            vali (1) = iddl2
            call utmess('F', 'ALGELINE5_35', si=vali(1))
        endif
        rl(1,i) = iddl1
        j1 = diag(iddl2-1) + 2
        j2 = diag(iddl2) - 1
        lgind = lgind + 2* (j2-j1+2) * ddlmoy
        do j = j1, j2
            iddl = col(j)
            ipp = p(iddl)
!
            if (ipp .gt. 0) then
                ip = invp(ipp)
                if (ip .gt. maxrl) maxrl = invp(p(iddl))
                if (ip .lt. minrl) minrl = invp(p(iddl))
            endif
!
        end do
!                      RL1 ET RL2 MARQUENT LES DDL T.Q.
!                      LEURS IMAGES PAR LA RENUMEROTATION
!                      SOIENT LES PREMIERS ET DERNIERS ENCADRES
        rl(3,i) = minrl
        rl(4,i) = maxrl
        rl1(minrl) = 1
        rl2(maxrl) = 1
    end do
!--------------------------------- CALCUL DE NOUV,ANC,SUPND
    nouvsn(0) = 0
    nbsn = nbsnd
    do i = 1, nbsn
        nouvsn(i) = i
        ancsn(i) = i
    end do
!                       NOUVSN ET ANCSN SERVENT DE TAB NOUV ET ANC
!                     POUR LES  SUPERNDS
    nbsnd = 1
    num = 1
    supnd(nbsnd)= num
    do isn = 1, nbsn
        i1 = supnd2(isn)
        i2 = supnd2(isn+1) - 1
!
!                                 ON MET EN TETE DU SUPERNOEUD :
!                                LES LAMBDA1 DE RELATION LINEAIRES
!                                PUIS LES LAMBDA1 DE BLOCAGE
        do i = i1, i2
            if (rl1(i) .ne. 0) then
!         I EST LE 1ER DDL D UN RELATION LINEAIRE ( LA J EME)
                do j = 1, nrl
                    if (rl(3,j) .eq. i) then
                        nouv(rl(1,j)) = num
                        anc(num) = rl(1,j)
                        num = num + 1
!       CREATION D UN NOUVEAU SN ANCSN A UNE VALEUR NEGATIVE POUR
!       MARQUER LA NOUVEAUTE
                        ancsn(nbsnd) = - isn
!        PRINT *, ' ON CREE UN NV SN LAMBD1 DE RL : '
                        nbsnd = nbsnd + 1
                        supnd(nbsnd) = num
                    endif
                end do
!
            endif
!
        end do
!
        do i = i1, i2
            ianc = q(perm(i))
            if (lbd1(ianc) .ne. 0) then
!         I EST UN DDL BLOQUE
                nouv(lbd1(ianc)) = num
                anc(num) = lbd1(ianc)
                num = num + 1
!       CREATION D UN NOUVEAU SN ANCSN A UNE VALEUR NEGATIVE POUR
!       MARQUER LA NOUVEAUTE
                ancsn(nbsnd) = - isn
!        PRINT *, ' ON CREE UN NV SN LAMBD1 DE BLOCAGE : '
                nbsnd = nbsnd + 1
                supnd(nbsnd) = num
            endif
!
        end do
!       ADDITION DES DDL NON LAGRANGES
        do i = i1, i2
            ianc = q(perm(i))
            nouv(ianc) = num
            anc(num) = ianc
            num = num + 1
        end do
!                         ON MET EN QUEUS DU SUPERNOEUD :
!                            LES LAMBDA2 DE BLOCAGE,PUIS
!                           LES LAMBDA2 DE RELATION LINEAIRES
        do i = i1, i2
            ianc = q(perm(i))
            if (lbd2(ianc) .ne. 0) then
                nouv(lbd2(ianc)) = num
                anc(num) = lbd2(ianc)
                num = num + 1
            endif
!
        end do
        do i = i1, i2
            if (rl2(i) .ne. 0) then
                do j = 1, nrl
                    if (rl(4,j) .eq. i) then
                        nouv(rl(2,j)) = num
                        anc(num) = rl(2,j)
                        num = num + 1
                    endif
!
                end do
            endif
!
        end do
!        PRINT *, ' ON CREE UN NV SN DDL ORDINAIRE : '
        ancsn(nbsnd) = isn
        nouvsn(isn) = nbsnd
        nbsnd = nbsnd + 1
        supnd(nbsnd) = num
    end do
    nbsnd = nbsnd-1
    num = num -1
    if (num .ne. n1) then
        vali (1) = num
        vali (2) = n1
        call utmess('F+', 'ALGELINE5_36', ni=2, vali=vali)
        do i = 1, n1
            if (lbd1(i) .ne. 0) then
                write(ifm,*)'LE DDL BLOQUE: ',i,' A POUR LAMBDA1: ',&
                lbd1(i)
                write(ifm,*)'LE DDL BLOQUE: ',i,' A POUR LAMBDA2: ',&
                lbd2(i)
                if (lbd2(i) .eq. 0) ier =1
            else if (lbd2(i).ne.0) then
                ier =1
            endif
            if (ier .eq. 1) then
                vali (1) = i
                vali (2) = lbd1(i)
                vali (3) = lbd2(i)
                call utmess('F+', 'ALGELINE5_37', ni=3, vali=vali)
            endif
        end do
        vali (1) = nrl
        call utmess('F+', 'ALGELINE5_38', si=vali(1))
        do i = 1, nrl
            vali (1) = rl(1,i)
            vali (2) = rl(2,i)
            call utmess('F+', 'ALGELINE5_39', ni=2, vali=vali)
        end do
    endif
!----------------------    CALCUL DU NOUVEAU PARENT
    do isn = 1, nbsnd
        if (ancsn(isn) .gt. 0) then
            parend(isn) = nouvsn( parent( ancsn( isn ) ) )
        else
!       C'EST UN NOUVEAU SN (LAMBDA1)
            parend(isn) = nouvsn( - ancsn( isn ) )
        endif
    end do
    if (niv .ge. 2) then
        write(ifm,*)'   --- APRES ADDITION  DES  RELATIONS LINEAIRES '
        write(ifm,*)'   --- NOMBRE DE SUPERNOEUDS ',nbsnd
    endif
!
end subroutine
