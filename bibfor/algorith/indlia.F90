subroutine indlia(modgen, seliai, nindep, nbddl, sst,&
                  sizlia)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!-----------------------------------------------------------------------
!    M. CORUS     DATE 25/01/10
!-----------------------------------------------------------------------
!
!  BUT:      < CONSTRUIRE LE SOUS ESPACE POUR L'ELIMINATION >
!
!  ON IMPOSE DES LIAISONS DU TYPE C.Q=0. ON CONSTRUIT UN BASE R DU NOYAU
!  DE C. LES DDL GENERALISEES Y VERIFIENT DONC NATURELLEMENT C.R.Y=0, ET
!  Q=R.Y
!
!-----------------------------------------------------------------------
!  MODGEN  /I/ : NOM DU CONCEPT DE MODELE GENERALISE
!  SELIAI  /O/ : BASE DU NOYAU DES EQUATIONS DE LIAISON
!  NINDEP  /O/ : NOMBRE DE LIAISONS INDEPENDANTES ENTRE LES SOUS
!                  STRUCTURES
!  NBDDL   /O/ : NOMBRE DE DDL IMPLIQUES DANS LES LIAISONS
!  SST     /O/ : VECTEUR CONTENANT LES NOMS DES SOUS STRUCTURES
!  SIZLIA  /O/ : VECTEUR CONTENANT LE NB DE DDL DE CHAQUE SOUS STRUCTURE
!-----------------------------------------------------------------------
!
    implicit none
!
!
!
!
!
!-- VARIABLES EN ENTREES / SORTIE
#include "jeveux.h"
!
#include "asterc/matfpe.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
#include "blas/dgeqrf.h"
#include "blas/dorgqr.h"
    integer :: nindep, nbddl
    character(len=8) :: modgen
    character(len=24) :: seliai, sizlia, sst
!
!-- VARIABLES DE LA ROUTINE
    integer :: i1, j1, k1, l1, m1, nbsst, nblia, ne, nd1, nd2, lnoli1, lnoli2
    integer :: nedec, nd1deq, nd2deq, nbeqt, inds, lds, ldelia, llprof, lknoms
    integer :: lmalia, lmats, lsilia, lwork, jwork, lselia, ltau, neq
    integer(kind=4) :: info
    integer :: ldiaqr
!
    character(len=8) :: int1, int2
    character(len=24) :: deflia, fprofl, nomsst, nomlia, matlia, mats
    real(kind=8) :: eps, tol, swork(1), temp
    parameter    (eps=2.3d-16)
!
!-----------C
!--       --C
!-- DEBUT --C
!--       --C
!-----------C
!
    call jemarq()
!
!----------------------------------------------C
!--                                          --C
!-- INITIALISATION DES DIFFERENTES GRANDEURS --C
!--                                          --C
!----------------------------------------------C
!
    deflia=modgen//'      .MODG.LIDF'
    fprofl=modgen//'      .MODG.LIPR'
    nomsst=modgen//'      .MODG.SSNO'
    nomlia=modgen//'      .MODG.LIMA'
!
!-- NOMBRE DE SOUS STRUCTURES
    call jelira(nomsst, 'NOMMAX', nbsst)
!
!-- NOMBRE D'INTERFACES
    call jelira(deflia, 'NMAXOC', nblia)
!
!-- LISTE DES NOMS DES SOUS-STRUCTURES
    call wkvect(sst, 'G V K8', nbsst, lknoms)
!
!-- LISTE DES TAILLES DES SOUS-STRUCTURES
    call wkvect(sizlia, 'G V I', nbsst, lsilia)
!
!-- ON DETERMINE LA TAILLE DE LA MATRICE DES LIAISONS
    call jeveuo(fprofl, 'L', llprof)
    nbddl=0
    nbeqt=0
!
    do 10 i1 = 1, nblia
!
!-- NOMBRE D'EQUATIONS
        ne=zi(llprof+(i1-1)*9)
        nd1=zi(llprof+(i1-1)*9+1)
        nd2=zi(llprof+(i1-1)*9+4)
        nbeqt=nbeqt+ne
!
!-- NOM DES SOUS STRUCTURES ET TAILLES
        call jeveuo(jexnum(deflia, i1), 'L', ldelia)
        int1=zk8(ldelia)
        int2=zk8(ldelia+2)
        if (i1 .eq. 1) then
            zk8(lknoms)=int1
            zk8(lknoms+1)=int2
            zi(lsilia)=nd1
            zi(lsilia+1)=nd2
            k1=2
            nbddl=nbddl+nd1+nd2
        else
            l1=0
            m1=0
            do 20 j1 = 1, k1
                if (int1 .eq. zk8(lknoms+j1-1)) then
                    l1=1
                endif
                if (int2 .eq. zk8(lknoms+j1-1)) then
                    m1=1
                endif
20          continue
            if (l1 .eq. 0) then
                zk8(lknoms+k1)=int1
                zi(lsilia+k1)=nd1
                nbddl=nbddl+nd1
                k1=k1+1
            endif
            if (m1 .eq. 0) then
                zk8(lknoms+k1)=int2
                zi(lsilia+k1)=nd2
                nbddl=nbddl+nd2
                k1=k1+1
            endif
        endif
!
10  end do
!
!--------------------------------------------------------------C
!--                                                          --C
!-- ALLOCATION DE LA MATRICE L CONTENANT TOUTES LES LIAISONS --C
!--   ON CONSTRUIT SA TRANSPOSEE L^T, DIRECTEMENT UTILISABLE --C
!--   POUR LA DECOMPOSITION QR (CONSTRUCTION DU NOYAU DE L)  --C
!--                                                          --C
!--------------------------------------------------------------C
!
!-- EN FONCTION DE LA TAILLE DE LA MATRICE, IL PEUT
!-- RESTER INTERESSANT DE FAIRE LA SVD, PUISQU'AVEC QR, ON EST
!-- OBLIGE D'AVOIR UNE MATRICE CARREE, AVEC BEAUCOUP DE ZEROS
!-- A INTEGRER DANS LA SUITE
!
    matlia='&&MATRICE_LIAISON_TEMPO'
!
!-- MATRICE QUI FAIT JUSTE LA TAILLE (SI SVD)
!      NEQ=NBEQT
!-- MATRICE CARRE NEQ*NBDDL
    neq=max(nbddl,nbeqt)
    call wkvect(matlia, 'V V R', neq*nbddl, lmalia)
!      CALL WKVECT('&&MATRICE_TEST','V V R',NEQ*NBDDL,BIDON)
!
!-- ON PARCOURS LES INTERFACES POUR LA REMPLIR
!
    nedec=0
    do 30 k1 = 1, nblia
        ne=zi(llprof+(k1-1)*9)
        nd1=zi(llprof+(k1-1)*9+1)
        nd2=zi(llprof+(k1-1)*9+4)
!
!-- RECHERCHE DE LA POSITION DE LA SOUS MATRICE DE LA
!-- LIAISON COURANTE DANS LA MATRICE GLOBALE
!
        nd1deq=0
        nd2deq=0
        call jeveuo(jexnum(deflia, k1), 'L', ldelia)
        int1=zk8(ldelia)
        int2=zk8(ldelia+2)
        do 40 i1 = 1, nbsst
            if (int1 .eq. zk8(lknoms+i1-1)) then
                do 50 j1 = 1, i1-1
                    nd1deq=nd1deq+zi(lsilia+j1-1)
50              continue
            endif
            if (int2 .eq. zk8(lknoms+i1-1)) then
                do 60 j1 = 1, i1-1
                    nd2deq=nd2deq+zi(lsilia+j1-1)
60              continue
            endif
40      continue
!
!-- REMPLISSAGE DE LA SOUS MATRICE POUR LA LIAISON K1
!
        call jeveuo(jexnum(nomlia, (k1-1)*3+1), 'L', lnoli1)
        call jeveuo(jexnum(nomlia, (k1-1)*3+2), 'L', lnoli2)
!
        do 70 j1 = 1, ne
            do 80 i1 = 1, nd1
                if (abs(zr(lnoli1+(i1-1)*ne+j1-1)) .gt. eps) then
                    zr(lmalia+(j1-1+nedec)*nbddl+i1-1+nd1deq)=&
                    zr(lnoli1+(i1-1)*ne+j1-1)
                else
                    zr(lmalia+(j1-1+nedec)*nbddl+i1-1+nd1deq)=0.d0
                endif
80          continue
!
            do 90 i1 = 1, nd2
                if (abs(zr(lnoli2+(i1-1)*ne+j1-1)) .gt. eps) then
                    zr(lmalia+(j1-1+nedec)*nbddl+i1-1+nd2deq)=&
                    zr(lnoli2+(i1-1)*ne+j1-1)
                else
                    zr(lmalia+(j1-1+nedec)*nbddl+i1-1+nd2deq)=0.d0
                endif
90          continue
70      continue
!
!-- ANCIENNE VERSION - MATRICE NON TRANSPOSEE --C
!
!        DO 71 I1=1,NE
!          DO 81 J1=1,ND1
!            IF (ABS(ZR(LNOLI1+(J1-1)*NE+I1-1)) .GT. EPS) THEN
!              ZR(BIDON+(J1-1+ND1DEQ)*NEQ+I1-1+NEDEC)=
!     &           ZR(LNOLI1+(J1-1)*NE+I1-1)
!            ELSE
!              ZR(BIDON+(J1-1+ND1DEQ)*NEQ+I1-1+NEDEC)=0.
!            ENDIF
!   81     CONTINUE
!          DO 91 J1=1,ND2
!            IF (ABS(ZR(LNOLI2+(J1-1)*NE+I1-1)) .GT. EPS) THEN
!              ZR(BIDON+(J1-1+ND2DEQ)*NEQ+I1-1+NEDEC)=
!     &           ZR(LNOLI2+(J1-1)*NE+I1-1)
!            ELSE
!              ZR(BIDON+(J1-1+ND2DEQ)*NEQ+I1-1+NEDEC)=0.
!            ENDIF
!   91     CONTINUE
!  71   CONTINUE
!
        nedec=nedec+ne
30  continue
!
!      WRITE(6,*)' '
!      WRITE(6,*)'%-- IMPRESSIONS DIVERSES'
!      WRITE(6,*)' '
!      WRITE(6,*)'clear all;'
!      WRITE(6,*)'LT=zeros(',NBDDL,',',NEQ,');Q=LT;'
!
!      DO 111 I1=1,NBDDL
!        DO 121 J1=1,NEQ
!         IF (ABS(ZR(LMALIA+(J1-1)*NBDDL+I1-1)) .GT. 1.D-20) THEN
!            WRITE(6,*)'LT(',I1,',',J1,')=',
!     &                ZR(LMALIA+(J1-1)*NBDDL+I1-1),';'
!          ENDIF
! 121    CONTINUE
! 111  CONTINUE
!
!      DO 113 I1=1,NEQ
!        DO 123 J1=1,NBDDL
!          IF (ABS(ZR(BIDON+(J1-1)*NEQ+I1-1)) .GT. 1.D-6) THEN
!            WRITE(6,*)'L(',I1,',',J1,')=',
!     &                ZR(BIDON+(J1-1)*NEQ+I1-1),';'
!          ENDIF
! 123    CONTINUE
! 113  CONTINUE
!      CALL JEDETR('&&MATRICE_TEST')
!
!
!
!-------------------------------------------------------------C
!--                                                         --C
!-- QR DE LA MATRICE POUR DETERMINER LA TAILLE DE SON NOYAU --C
!--                                                         --C
!-------------------------------------------------------------C
!
!-- CONSTRUCTION DES OBJETS TEMPORAIRES POUR LA DECOMPOSITION
!
!      WRITE(6,*)'NEQ=',NEQ
!      WRITE(6,*)'NBDDL=',NBDDL
    mats='&&MATRICE_DIAG_R'
    lds=int(min(neq,nbddl))
!
    call wkvect(mats, 'V V R', lds, lmats)
    call wkvect('&&MATR_TAU', 'V V R', neq, ltau)
!
!-- INITIALISATION DES VALEURS DIAGONALES A 0
    do 100 i1 = 1, lds
        zr(lmats+i1-1)=0.d0
100  end do
!
!-- DESACTIVATION DU TEST FPE
    call matfpe(-1)
!
!-- RECHERCHE DE LA TAILLE DE L'ESPACE DE TRAVAIL
    lwork=-1
    call dgeqrf(nbddl, neq, zr(lmalia), nbddl, zr(ltau),&
                swork, lwork, info)
    lwork=int(swork(1))
!      WRITE(6,*)'LWORK=',LWORK
!-- DECOMPOSITION QR
    call wkvect('&&MATR_QR_WORK', 'V V R', lwork, jwork)
    call dgeqrf(nbddl, neq, zr(lmalia), nbddl, zr(ltau),&
                zr(jwork), lwork, info)
!
!-- RECHERCHE DES ELEMENTS DIAGONAUX DE R
    temp=0.d0
    do 110 i1 = 1, lds
        zr(lmats+i1-1)=abs(zr(lmalia+(neq-nbddl+i1-1)*nbddl+i1-1))
        if (zr(lmats+i1-1) .gt. temp) then
            temp=zr(lmats+i1-1)
        endif
110  end do
!
!-- TEST SI ON DOIT AGRANDIR LA TAILLE DE L'ESPACE DE TRAVAIL
    call dorgqr(nbddl, neq, nbddl, zr(lmalia), nbddl,&
                zr(ltau), swork(1), -1, info)
    if (swork(1) .gt. lwork) then
        lwork=int(swork(1))
        call jedetr('&&MATR_QR_WORK')
        call wkvect('&&MATR_QR_WORK', 'V V R', lwork, jwork)
    endif
!-- CONSTRUCTION DE LA MATRICE Q
    call dorgqr(nbddl, neq, nbddl, zr(lmalia), nbddl,&
                zr(ltau), zr(jwork), lwork, info)
!
!-- REACTIVATION DU TEST FPE
    call matfpe(1)
!
    tol=temp*lds*1.d-16
    call wkvect('&&INDICES_DIAG_QR', 'V V I', lds, ldiaqr)
!
!-- IL NE PEUT PAS Y AVOIR MOINS DE DDL INDEPENDANTS QUE LE NOMBRE
!--  TOTAL DE DDL MOINS LE NOMBRE DE CONTRAINTES.
!
!      DO WHILE (NINDEP .LT. NBDDL-NBEQT)
666  continue
    nindep=0
    do 120 i1 = 1, lds
        if (zr(lmats+i1-1) .le. tol) then
            zi(ldiaqr+nindep)=i1
            nindep=nindep+1
        endif
120  continue
    tol=tol*10
    if (nindep .lt. nbddl-nbeqt) then
        goto 666
    endif
!      END DO
!
    write(6,*)'--------'
    write(6,*)' '
    write(6,*)'+++',nbddl,' DEGRES DE LIBERTE AU TOTAL'
    write(6,*)'+++',nbeqt,' CONTRAINTES CINEMATIQUES.'
    write(6,*)' '
    write(6,*)'ON A TROUVE',nindep,' RELATIONS INDEPENDANTES.'
    write(6,*)' '
    write(6,*)'--------'
!
!-- CONSTRUCTION DU SOUS ESPACE
!
    call wkvect(seliai, 'G V R', nindep*nbddl, lselia)
!      WRITE(6,*)'  '
!      WRITE(6,*)'T=zeros(',NBDDL,',',NINDEP,');'
    do 130 j1 = 1, nindep
        inds=zi(ldiaqr+j1-1)-1
!        WRITE(6,*)'INDS=',INDS
        do 140 i1 = 1, nbddl
            zr(lselia+(j1-1)*nbddl+i1-1)= zr(lmalia+inds*nbddl+i1-1)
!          IF (ABS(ZR(LMALIA+INDS*NBDDL+I1-1)) .GT. 1.D-20) THEN
!            WRITE(6,*)'T(',I1,',',J1,')=',
!     &        ZR(LSELIA+(J1-1)*NBDDL+I1-1),';'
!          ENDIF
140      continue
130  end do
!
!
!-- DESTRUCTION DES MATRICES TEMPORAIRES
!
    call jedetr(matlia)
    call jedetr(mats)
    call jedetr('&&MATR_QR_WORK')
    call jedetr('&&MATR_TAU')
!
!--   FIN
!
    call jedema()
!
end subroutine
