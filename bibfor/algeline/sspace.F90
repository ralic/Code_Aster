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
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
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
    integer :: i, iaa, iar, ibr, icomp, iconv
    integer :: ii,      iter
    integer ::    jj, kk, ll, nfrcv
    integer :: nitja
    real(kind=8) :: art, brt, dseed
    integer, pointer :: fpos(:) => null()
    integer, pointer :: ipos(:) => null()
    real(kind=8), pointer :: vjacobi(:) => null()
    real(kind=8), pointer :: rdiak(:) => null()
    real(kind=8), pointer :: rdiam(:) => null()
    real(kind=8), pointer :: tempor(:) => null()
    real(kind=8), pointer :: tolvec(:) => null()
    real(kind=8), pointer :: vecpro(:) => null()
    real(kind=8), pointer :: vvect(:) => null()
    cbid = dcmplx(0.d0, 0.d0)
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
    AS_ALLOCATE(vi=ipos, size=neq)
    AS_ALLOCATE(vi=fpos, size=nbvec)
    AS_ALLOCATE(vr=rdiak, size=neq)
    AS_ALLOCATE(vr=rdiam, size=neq)
    AS_ALLOCATE(vr=vvect, size=neq)
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
    do jj = 1, neq
        rdiak(jj) = zr(iaa+zi(jsmdi+jj-1)-1)
    end do
    call jelibe(jexnum(valm, 1))
!
!     - ON CHERCHE LA DIAGONALE DE LA MASSE ET ON REMPLIT ZR(IRDIAM) -
!
    call mtdsc2(zk24(zi(lmass+1)), 'SMDI', 'L', jsmdi)
    valm(1:19) = zk24(zi(lmass+1))
    call jeveuo(jexnum(valm, 1), 'L', iaa)
    do jj = 1, neq
        rdiam(jj) = zr(iaa+zi(jsmdi+jj-1)-1)
    end do
    call jelibe(jexnum(valm, 1))
!
!     --- ON CHERCHE LES PLUS PETITS RAPPORTS K/M ---
!
    call sstriv(rdiak, rdiam, lprod, ipos, neq)
!
!     --- ON INITIALISE LES VECTEURS ---
!
    do i = 1, neq
        vect(i,1) = rdiam(i)*lprod(i)
    end do
!
    icomp = 0
    do i = 2, nbvec - 1
 60     continue
        if (lprod(ipos(1+i+icomp-1-1)) .eq. 0) then
            icomp = icomp + 1
            goto 60
        else
            vect(ipos(1+i+icomp-1-1),i) = 1.d0
        endif
    end do
!
    dseed = 123457.d0
    call ggubs(dseed, neq, vect(1, nbvec))
    do i = 1, neq
        vect(i,nbvec) = vect(i,nbvec)*lprod(i)
    end do
!
!     ------------------------------------------------------------------
!     ------   DEUXIEME DECLARATION DE VARIABLES AUXILLIAIRES   --------
!     -- DESTRUCTION D'UNE PARTIE DES PREMIERES VARIABLES AUXILLIAIRES -
!     ------------------------------------------------------------------
!
    call wkvect('&&SSPACE.AR', 'V V R', nbvec* (nbvec+1)/2, iar)
    call wkvect('&&SSPACE.BR', 'V V R', nbvec* (nbvec+1)/2, ibr)
    AS_ALLOCATE(vr=vecpro, size=nbvec*nbvec)
    AS_ALLOCATE(vr=tempor, size=nbvec)
    AS_ALLOCATE(vr=tolvec, size=nbvec)
    AS_ALLOCATE(vr=vjacobi, size=nbvec)
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
 90 continue
!
    iter = iter + 1
!
!     --- CALCUL DE LA MATRICE SYMETRIQUE DE RAIDEUR PROJETEE ---
!     ---       ON NE STOCKE QUE LA MOITIE DE LA MATRICE      ---
!
    ii = 0
    do jj = 1, nbvec
        do kk = 1, neq
            vvect(kk) = vect(kk,jj)*lprod(kk)
        end do
        call resoud(matass, k19bid, solveu, chcine, 1,&
                    k19bid, k19bid, kbid, vvect, [cbid],&
                    criter, .false., 0, iret)
        do ll = jj, nbvec
            art = 0.d0
            do kk = 1, neq
                art = art + vect(kk,ll)*vvect(kk)*lprod(kk)
            end do
            ii = ii + 1
            zr(iar+ii-1) = art
        end do
        do kk = 1, neq
            vect(kk,jj) = vvect(kk)*lprod(kk)
        end do
    end do
!
!     --- CALCUL DE LA MATRICE SYMETRIQUE DE MASSE PROJETEE ---
!     ---       ON NE STOCKE QUE LA MOITIE DE LA MATRICE    ---
!
    ii = 0
    do jj = 1, nbvec
        call mrmult('ZERO', lmass, vect(1, jj), vvect, 1,&
                    .false.)
        do ll = jj, nbvec
            brt = 0.0d0
            do kk = 1, neq
                brt = brt + vect(kk,ll)*vvect(kk)*lprod(kk)
            end do
            ii = ii + 1
            zr(ibr+ii-1) = brt
        end do
        if (iconv .le. 0) then
            do kk = 1, neq
                vect(kk,jj) = vvect(kk)
            end do
        endif
    end do
!
!     --- ALGORITHME DE JACOBI SUR LES MATRICES PROJETEES ---
!     --- CLASSEMENT PAR ORDRE CROISSANT DES VALEURS PROPRES  ---
!     ---   ET DONC DES VECTEURS PROPRES DU SYSTEME PROJETE   ---
!
    type = 1
    iordre = 0
    call jacobi(nbvec, nperm, tol, toldyn, zr(iar),&
                zr(ibr), vecpro, valpro, vjacobi, nitja,&
                type, iordre)
    if (nitja .gt. nitjac) then
        nitjac = nitja
    endif
!
!     --- CALCUL DES VECTEURS PROPRES DU SYSTEME COMPLET ---
!
    call vpreco(nbvec, neq, vecpro, vect)
!
!     --- TEST DE CONVERGENCE SUR LES VALEURS PROPRES ---
!     ---        SEULEMENT LES NFREQ PREMIERES        ---
!
    if (iconv .le. 0) then
        do i = 1, nfrcv
            tolvec(i) = abs(valpro(i)-tempor(i))
        end do
        do i = 1, nfrcv
            if (tolvec(i) .gt. tol*abs(valpro(i))) then
                if (i .gt. nfreq) then
                    iconvf = .true.
                endif
                goto 220
            endif
        end do
        iconv = 1
        iconvf = .true.
        goto 90
220     continue
        if (iter .lt. itemax) goto 230
        iconv = 2
        goto 90
230     continue
        do i = 1, nfrcv
            tempor(i) = valpro(i)
        end do
        goto 90
    endif
!
!
!     --- CLASSEMENT PAR ORDRE CROISSANT DES VALEURS PROPRES ---
!     ---   ET DONC DES VECTEURS PROPRES DU SYSTEME COMPLET  ---
!
    nitbat = iter
    call vpordi(1, 0, nbvec, valpro, vect,&
                neq,fpos)
    if (.not.iconvf) then
        call utmess('A', 'ALGELINE3_45')
    endif
!
!     ------------------------------------------------------------------
!     ------------- DESTRUCTION DES VARIABLES AUXILLIAIRES -------------
!     ------------------------------------------------------------------
    AS_DEALLOCATE(vi=ipos)
    AS_DEALLOCATE(vi=fpos)
    AS_DEALLOCATE(vr=rdiak)
    AS_DEALLOCATE(vr=rdiam)
    AS_DEALLOCATE(vr=vvect)
    call jedetr('&&SSPACE.AR')
    call jedetr('&&SSPACE.BR')
    AS_DEALLOCATE(vr=vecpro)
    AS_DEALLOCATE(vr=tempor)
    AS_DEALLOCATE(vr=tolvec)
    AS_DEALLOCATE(vr=vjacobi)
!
    call jedema()
end subroutine
