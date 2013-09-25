subroutine sspace(lraid, lmatra, lmass, neq, nbvec,&
                  nfreq, lprod, itemax, nperm, tol,&
                  toldyn, vect, valpro, nitjac, nitbat,&
                  solveu)
    implicit none
#include "jeveux.h"
#include "asterfort/ggubs.h"
#include "asterfort/jacobi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelibe.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/mrmult.h"
#include "asterfort/mtdsc2.h"
#include "asterfort/resoud.h"
#include "asterfort/sstriv.h"
#include "asterfort/utmess.h"
#include "asterfort/vpordi.h"
#include "asterfort/vpreco.h"
#include "asterfort/wkvect.h"
!
    integer :: lraid, lmatra, lmass, neq, nbvec, nfreq
    integer :: lprod(neq), itemax, nperm, nitjac, nitbat
    real(kind=8) :: tol, toldyn, valpro(nbvec), vect(neq, nbvec)
    character(len=19) :: solveu
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
!     ------------------------------------------------------------------
! BUT : RESOLUTION DU PROBLEME GENERALISE AUX VALEURS PROPRES PAR UNE
!       METHODE D ITERATIONS SIMULTANEES EN SOUS-ESPACE
!       CHOIX DES VECTEURS INITIAUX ET PREPARATION DES MATRICES
!       REDUITES POUR UTILISER L ALGORITHME DE RECHERCHE DE VALEURS
!       PROPRES BASE SUR LA DECOMPOSITION DE JACOBI
!
!     IN  : LRAID  : DESCRIPTEUR DE LA MATRICE DE RAIDEUR DECALEE
!     IN  : LMATRA : DESCRIPTEUR DE CETTE MEME MATRICE MAIS DECOMPOSEE
!     IN  : LMASS  : DESCRIPTEUR DE LA MATRICE DE MASSE
!     IN  : NEQ    : DIMENSION DU PROBLEME COMPLET
!     IN  : NBVEC  : NOMBRE DE VECTEURS INITIAUX
!     IN  : NFREQ  : NOMBRE DE FREQUENCES DEMANDEES
!     IN  : LPROD  : TABLEAUX DES DDL A UTILISER (ACTIFS + LAGRANGES)
!     IN  : ITEMAX : NOMBRE MAX D'ITERATIONS SUR LE SOUS ESPACE
!     IN  : NPERM  : NOMBRE MAX D'ITERATIONS DE LA METHODE DE JACOBI
!     IN  : TOL    : PRECISION DE CONVERGENCE
!     IN  : TOLDYN : PRECISION DE PETITESSE DYNAMIQUE
!     OUT : VECT  : VECTEURS PROPRES DU SYSTEME COMPLET
!     OUT : VALPRO : VALEURS PROPRES
!     OUT : NITJAC : NOMBRE MAXIMAL ATTEINT D'ITERATIONS DANS JACOBI
!     OUT : NITBAT : NOMBRE MAXIMAL ATTEINT D'ITERATIONS DE BATHE ET W.
!     IN  : SOLVEU : SD SOLVEUR POUR PARAMETRER LE SOLVEUR LINEAIRE
!-----------------------------------------------------------------------
!
    integer :: jsmdi, type, iordre
    logical :: iconvf
    character(len=24) :: valm
    complex(kind=8) :: cbid
    character(len=1) :: kbid
    character(len=19) :: k19bid, matass, chcine, criter
    integer :: iret
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, iaa, iar, ibr, icomp, iconv, ifpos
    integer :: ii, iipos, ijac, irdiak, irdiam, itempo, iter
    integer :: itolve, ivecpr, ivect, jj, kk, ll, nfrcv
    integer :: nitja
    real(kind=8) :: art, brt, dseed
!-----------------------------------------------------------------------
    data valm/'                   .VALM'/
!
!     INIT. OBJETS ASTER
    matass=zk24(zi(lmatra+1))
    chcine=' '
    criter=' '
    k19bid=' '
!     ------------------------------------------------------------------
!     ------   PREMIERE DECLARATION DE VARIABLES AUXILLIAIRES   --------
!     ------------------------------------------------------------------
!
    call jemarq()
    call wkvect('&&SSPACE.IPOS', 'V V I', neq, iipos)
    call wkvect('&&SSPACE.FPOS', 'V V I', nbvec, ifpos)
    call wkvect('&&SSPACE.RDIAK', 'V V R', neq, irdiak)
    call wkvect('&&SSPACE.RDIAM', 'V V R', neq, irdiam)
    call wkvect('&&SSPACE.VECT', 'V V R', neq, ivect)
!
!     ------------------------------------------------------------------
!     -----   CONSTRUCTION DES VECTEURS INITIAUX DU SOUS ESPACE   ------
!     ------------------------------------------------------------------
!
!     - ON CHERCHE LA DIAGONALE DE LA RAIDEUR ET ON REMPLIT ZR(IRDIAK) -
!
    call mtdsc2(zk24(zi(lraid+1)), 'SMDI', 'L', jsmdi)
    valm(1:19) = zk24(zi(lraid+1))
    call jeveuo(jexnum(valm, 1), 'L', iaa)
    do 10 jj = 1, neq
        zr(irdiak+jj-1) = zr(iaa+zi(jsmdi+jj-1)-1)
10  continue
    call jelibe(jexnum(valm, 1))
!
!     - ON CHERCHE LA DIAGONALE DE LA MASSE ET ON REMPLIT ZR(IRDIAM) -
!
    call mtdsc2(zk24(zi(lmass+1)), 'SMDI', 'L', jsmdi)
    valm(1:19) = zk24(zi(lmass+1))
    call jeveuo(jexnum(valm, 1), 'L', iaa)
    do 30 jj = 1, neq
        zr(irdiam+jj-1) = zr(iaa+zi(jsmdi+jj-1)-1)
30  continue
    call jelibe(jexnum(valm, 1))
!
!     --- ON CHERCHE LES PLUS PETITS RAPPORTS K/M ---
!
    call sstriv(zr(irdiak), zr(irdiam), lprod, zi(iipos), neq)
!
!     --- ON INITIALISE LES VECTEURS ---
!
    do 50 i = 1, neq
        vect(i,1) = zr(irdiam+i-1)*lprod(i)
50  end do
!
    icomp = 0
    do 70 i = 2, nbvec - 1
60      continue
        if (lprod(zi(iipos+i+icomp-1-1)) .eq. 0) then
            icomp = icomp + 1
            goto 60
        else
            vect(zi(iipos+i+icomp-1-1),i) = 1.d0
        endif
70  end do
!
    dseed = 123457.d0
    call ggubs(dseed, neq, vect(1, nbvec))
    do 80 i = 1, neq
        vect(i,nbvec) = vect(i,nbvec)*lprod(i)
80  end do
!
!     ------------------------------------------------------------------
!     ------   DEUXIEME DECLARATION DE VARIABLES AUXILLIAIRES   --------
!     -- DESTRUCTION D'UNE PARTIE DES PREMIERES VARIABLES AUXILLIAIRES -
!     ------------------------------------------------------------------
!
    call wkvect('&&SSPACE.AR', 'V V R', nbvec* (nbvec+1)/2, iar)
    call wkvect('&&SSPACE.BR', 'V V R', nbvec* (nbvec+1)/2, ibr)
    call wkvect('&&SSPACE.VECPRO', 'V V R', nbvec*nbvec, ivecpr)
    call wkvect('&&SSPACE.TEMPOR', 'V V R', nbvec, itempo)
    call wkvect('&&SSPACE.TOLVEC', 'V V R', nbvec, itolve)
    call wkvect('&&SSPACE.JACOBI', 'V V R', nbvec, ijac)
!
!     ------------------------------------------------------------------
!     -------------------   PROCESSUS ITERATIF   -----------------------
!     ------------------------------------------------------------------
!
    nitjac = 0
    nitbat = 0
    iconv = 0
    iconvf = .false.
    iter = 0
!     --- NOMBRE DE FREQUENCES CONVERGEES SOUHAITEES
    nfrcv = nfreq + (nbvec-nfreq)/2
!
90  continue
!
    iter = iter + 1
!
!     --- CALCUL DE LA MATRICE SYMETRIQUE DE RAIDEUR PROJETEE ---
!     ---       ON NE STOCKE QUE LA MOITIE DE LA MATRICE      ---
!
    ii = 0
    do 140 jj = 1, nbvec
        do 100 kk = 1, neq
            zr(ivect+kk-1) = vect(kk,jj)*lprod(kk)
100      continue
        call resoud(matass, k19bid, solveu, chcine, 1,&
                    k19bid, k19bid, kbid, zr(ivect), [cbid],&
                    criter, .false., 0, iret)
        do 120 ll = jj, nbvec
            art = 0.d0
            do 110 kk = 1, neq
                art = art + vect(kk,ll)*zr(ivect+kk-1)*lprod(kk)
110          continue
            ii = ii + 1
            zr(iar+ii-1) = art
120      continue
        do 130 kk = 1, neq
            vect(kk,jj) = zr(ivect+kk-1)*lprod(kk)
130      continue
140  end do
!
!     --- CALCUL DE LA MATRICE SYMETRIQUE DE MASSE PROJETEE ---
!     ---       ON NE STOCKE QUE LA MOITIE DE LA MATRICE    ---
!
    ii = 0
    do 180 jj = 1, nbvec
        call mrmult('ZERO', lmass, vect(1, jj), zr(ivect), 1,&
                    .false.)
        do 160 ll = jj, nbvec
            brt = 0.0d0
            do 150 kk = 1, neq
                brt = brt + vect(kk,ll)*zr(ivect+kk-1)*lprod(kk)
150          continue
            ii = ii + 1
            zr(ibr+ii-1) = brt
160      continue
        if (iconv .le. 0) then
            do 170 kk = 1, neq
                vect(kk,jj) = zr(ivect+kk-1)
170          continue
        endif
180  end do
!
!     --- ALGORITHME DE JACOBI SUR LES MATRICES PROJETEES ---
!     --- CLASSEMENT PAR ORDRE CROISSANT DES VALEURS PROPRES  ---
!     ---   ET DONC DES VECTEURS PROPRES DU SYSTEME PROJETE   ---
!
    type = 1
    iordre = 0
    call jacobi(nbvec, nperm, tol, toldyn, zr(iar),&
                zr(ibr), zr(ivecpr), valpro, zr(ijac), nitja,&
                type, iordre)
    if (nitja .gt. nitjac) then
        nitjac = nitja
    endif
!
!     --- CALCUL DES VECTEURS PROPRES DU SYSTEME COMPLET ---
!
    call vpreco(nbvec, neq, zr(ivecpr), vect)
!
!     --- TEST DE CONVERGENCE SUR LES VALEURS PROPRES ---
!     ---        SEULEMENT LES NFREQ PREMIERES        ---
!
    if (iconv .le. 0) then
        do 200 i = 1, nfrcv
            zr(itolve+i-1) = abs(valpro(i)-zr(itempo+i-1))
200      continue
        do 210 i = 1, nfrcv
            if (zr(itolve+i-1) .gt. tol*abs(valpro(i))) then
                if (i .gt. nfreq) then
                    iconvf = .true.
                endif
                goto 220
            endif
210      continue
        iconv = 1
        iconvf = .true.
        goto 90
220      continue
        if (iter .lt. itemax) goto 230
        iconv = 2
        goto 90
230      continue
        do 240 i = 1, nfrcv
            zr(itempo+i-1) = valpro(i)
240      continue
        goto 90
    endif
!
!
!     --- CLASSEMENT PAR ORDRE CROISSANT DES VALEURS PROPRES ---
!     ---   ET DONC DES VECTEURS PROPRES DU SYSTEME COMPLET  ---
!
    nitbat = iter
    call vpordi(1, 0, nbvec, valpro, vect,&
                neq, zi(ifpos))
    if (.not.iconvf) then
        call utmess('A', 'ALGELINE3_45')
    endif
!
!     ------------------------------------------------------------------
!     ------------- DESTRUCTION DES VARIABLES AUXILLIAIRES -------------
!     ------------------------------------------------------------------
    call jedetr('&&SSPACE.IPOS')
    call jedetr('&&SSPACE.FPOS')
    call jedetr('&&SSPACE.RDIAK')
    call jedetr('&&SSPACE.RDIAM')
    call jedetr('&&SSPACE.VECT')
    call jedetr('&&SSPACE.AR')
    call jedetr('&&SSPACE.BR')
    call jedetr('&&SSPACE.VECPRO')
    call jedetr('&&SSPACE.TEMPOR')
    call jedetr('&&SSPACE.TOLVEC')
    call jedetr('&&SSPACE.JACOBI')
!
    call jedema()
end subroutine
