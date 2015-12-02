subroutine vp2ini(ldynam, lmasse, ldynfa, neq, nbvect,&
                  nborto, prorto, ddlexc, ddllag, alpha,&
                  beta, signes, vect, prsudg, nstoc,&
                  omeshi, solveu)
    implicit none
#include "jeveux.h"
#include "asterc/r8miem.h"
#include "asterfort/ggubs.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelibe.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/mrmult.h"
#include "asterfort/mtdsc2.h"
#include "asterfort/resoud.h"
#include "asterfort/utmess.h"
#include "asterfort/vplcor.h"
#include "asterfort/wkvect.h"
!
    integer :: ldynam, lmasse, ldynfa, neq, nbvect, nborto, ddlexc(*), ddllag(*)
    integer :: nstoc
    real(kind=8) :: prsudg, prorto, omeshi
    real(kind=8) :: alpha(*), beta(*), signes(*), vect(neq, *)
    character(len=19) :: solveu
!     ------------------------------------------------------------------
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
!     GENERATION DE VECTEURS ORTHOGONAUX PAR LA METHODE DE LANCZOS
!     LES VECTEURS OBTENUS SONT K-ORTHOGONAUX ET PAS FORCEMENT M-ORTHOG.
!     ------------------------------------------------------------------
! IN  LDYNAM : IS : DESCRIPTEUR MATRICE DE "RAIDEUR"
! IN  LMASSE : IS : DESCRIPTEUR MATRICE DE "MASSE"
! IN  LDYNFA : IS : DESCRIPTEUR MATRICE DE "RAIDEUR" FACTORISEE
! IN  NEQ    : IS : DIMENSION DES VECTEURS
! IN  NBVECT : IS : NOMBRE DE VECTEURS A GENERER
! IN  PRORTO : R8 : PRECISON DEMANDEE POUR L'ORTHOGONALISATION
! IN  NBORTO : IS : NOMBRE MAXIMUM D'ORTHOGONALISATION PERMISE.
! IN  DDLEXC : IS : POSITION DES DDLS BLOQUES
! IN  DDLLAG : IS : POSITION DES LAGRANGES
! IN  PRSUDG : R8 : PRECISION TERME BETA NUL PAR RAPPORT A ALPHA
! IN  NSTOC  : IS : NOMBRE DE MODES DE CORPS RIGIDE
! OUT ALPHA  : R8 : TERME DIAGONAL DE LA MATRICE TRIDIAGONALE
! OUT BETA   : R8 : TERME SURDIAGONAL DE LA MATRICE TRIDIAGONALE
! OUT SIGNES : R8 : (+/- 1)  SIGNE DU TERME SOUS-DIAGONAL
! OUT VECT   : R8 : VECT(1..NEQ,1..NBVECT) VECTEURS DE LANCZOS
! IN  SOLVEU : K19: SD SOLVEUR POUR PARAMETRER LE SOLVEUR LINEAIRE
!     ------------------------------------------------------------------
!       ON A DEJA CALCULE DYNAM = RAIDEUR - SHIFT * MASSE
!       PRENDRE  V(1)  ALEATOIRE
!             % A = ALPHA
!             % B = BETA
!       A(1) = V(1) * MASSE * V(1)    % TERME DIAGONAL
!       B(1) = 0.
!       POUR I= 2, N FAIRE
!          A(I) = V(I)   * MASSE * V(I)    % TERME DIAGONAL
!          B(I) = V(I-1) * MASSE * V(I)    % TERME EXTRA DIAGONAL
!          W(I+1) = (DYNAM**-1) * MASSE * V(I) - A(I)*V(I) - B(I)*V(I-1)
!          V(I+1) = W(I+1) / SQRT( W(I+1) * DYNAM * W(I+1) )
!       FIN_FAIRE
!
!     ------------------------------------------------------------------
!
!
!     -----------------------------------------------------------------
    real(kind=8) :: ai, coef, bi, xikxi, xjkxi, invome, rmin
    real(kind=8) :: valr(2)
    real(kind=8) :: dseed
    complex(kind=8) :: cbid
    integer :: jsmdi
    integer :: lx, lmx, lkx, irdiak, ivecd, isto, lkxsto, ieq
    integer :: iaa, jj, lkx1, ivec, ivecp1, ivecm1, lkxp1, jvec
    character(len=1) :: kbid
    character(len=24) :: vale
    character(len=19) :: k19bid, matass, chcine, criter
    integer :: iret
!     -----------------------------------------------------------------
    character(len=24) :: work(4)
    cbid = dcmplx(0.d0, 0.d0)
    data work(1)/'&&VP2INI.VECTEUR_INITIAL'/
    data work(2)/'&&VP2INI.VECTEUR_MX     '/
    data work(3)/'&&VP2INI.VECTEURS_KX    '/
    data work(4)/'&&VP2INI.RDIAK          '/
    data vale   /'                   .VALM'/
!     -----------------------------------------------------------------
!
! INIT. OBJETS ASTER
    matass=zk24(zi(ldynfa+1))
    chcine=' '
    criter=' '
    k19bid=' '
!     -----------------------------------------------------------------
!     ---------------- ALLOCATION DES ZONES DE TRAVAIL ----------------
!     -----------------------------------------------------------------
    call jemarq()
    call wkvect(work(1), 'V V R', neq, lx)
    call wkvect(work(2), 'V V R', neq, lmx)
    call wkvect(work(3), 'V V R', neq*nbvect, lkx)
    call wkvect(work(4), 'V V R', neq, irdiak)
!     -----------------------------------------------------------------
!     -------------- CALCUL DU PREMIER VECTEUR DE LANCZOS -------------
!     -----------------------------------------------------------------
!
    ivecd = 1
!
    ivecd = ivecd + nstoc
 
    rmin = 100.d0*r8miem()

! TEST DU SHIFT EN CAS DE PRE-DETECTION DE MODES RIGIDES
    if (nstoc.ge.1) then
        if (abs(omeshi).lt.rmin) then
            valr(1)=omeshi
            call utmess('F', 'ALGELINE4_2', nr=1, valr=valr)
        else
            invome=1.d0/omeshi
        endif
    endif
    do isto = 1, nstoc
        alpha(isto) = invome
        alpha(isto) = -alpha(isto)
        beta(isto) = 0.d0
        lkxsto = lkx + (isto-1)*neq
        call mrmult('ZERO', ldynam, vect(1, isto), zr(lkxsto), 1,&
                    .false._1)
        xikxi = 0.d0
        do ieq = 1, neq
            xikxi = xikxi + vect(ieq,isto)*zr(lkxsto+ieq-1)
        end do
        signes(isto) = sign(1.d0,xikxi)
        coef = 1.d0/sqrt(abs(xikxi))
        do ieq = 1, neq
            vect(ieq,isto) = coef*vect(ieq,isto)
            zr(lkxsto+ieq-1) = coef*zr(lkxsto+ieq-1)
        end do
!
        if (isto .ne. 1) then
            call vplcor(ldynam, neq, nbvect, nborto, prorto,&
                        signes, vect, isto, zr(lkx), zr(lx))
        endif
        alpha(isto) = signes(isto)*alpha(isto)
    end do
!
!
!
    call mtdsc2(zk24(zi(ldynam+1)), 'SMDI', 'L', jsmdi)
    vale(1:19) = zk24(zi(ldynam+1))
    call jeveuo(jexnum(vale, 1), 'L', iaa)
    do jj = 1, neq
        zr(irdiak+jj-1) = zr(iaa+zi(jsmdi+jj-1)-1)
    end do
    call jelibe(jexnum(vale, 1))
!
!     --- VECTEUR INITIAL : ALEATOIRE ---
!
 60 continue
    dseed = 123457.d0*ivecd
    call ggubs(dseed, neq, zr(lx))
    do ieq = 1, neq
        zr(lx+ieq-1) = zr(lx+ieq-1)*ddllag(ieq)*zr(irdiak+ieq-1)
    end do
    call resoud(matass, k19bid, solveu, chcine, 1,&
                k19bid, k19bid, kbid, zr(lx), [cbid],&
                criter, .false._1, 0, iret)
    do ieq = 1, neq
        zr(lx+ieq-1) = zr(lx+ieq-1)*ddllag(ieq)
    end do
!
!     --- CALCUL DE (LDYNAM**-1)*MASSE * X0 ---
!
    lkx1 = lkx + neq* (ivecd-1) + neq*nstoc
!
    call mrmult('ZERO', lmasse, zr(lx), zr(lmx), 1,&
                .false._1)
    do ieq = 1, neq
        vect(ieq,ivecd) = zr(lmx+ieq-1)*ddlexc(ieq)
    end do
    call resoud(matass, k19bid, solveu, chcine, 1,&
                k19bid, k19bid, kbid, vect(1, ivecd), [cbid],&
                criter, .false._1, 0, iret)
!
!     --- K-ORTHONORMALISATION DU 1-ER VECTEUR ---
!
    call mrmult('ZERO', ldynam, vect(1, ivecd), zr(lkx1), 1,&
                .false._1)
    xikxi = 0.d0
    do ieq = 1, neq
        xikxi = xikxi + vect(ieq,ivecd)*zr(lkx1+ieq-1)
    end do
    signes(ivecd) = sign(1.d0,xikxi)
    coef = 1.d0/sqrt(abs(xikxi))
    do ieq = 1, neq
        vect(ieq,ivecd) = coef*vect(ieq,ivecd)
        zr(lkx1+ieq-1) = coef*zr(lkx1+ieq-1)
    end do
!
    if (ivecd .gt. 1) then
        call vplcor(ldynam, neq, nbvect, nborto, prorto,&
                    signes, vect, ivecd, zr(lkx), zr(lx))
    endif
!
!
!     --- CALCUL DE ALPHA(1)
!
    call mrmult('ZERO', lmasse, vect(1, ivecd), zr(lx), 1,&
                .false._1)
    ai = 0.d0
    do ieq = 1, neq
        ai = ai + vect(ieq,ivecd)*zr(lx+ieq-1)
    end do
    alpha(ivecd) = ai
    beta(ivecd) = 0.d0
!
!     -----------------------------------------------------------------
!     -------------- CALCUL DES AUTRES VECTEURS DE LANCZOS ------------
!     -----------------------------------------------------------------
!
    do ivec = ivecd, nbvect - 1
!
        ivecp1 = ivec + 1
        ivecm1 = ivec - 1
        lkxp1 = lkx + neq*ivec
!
        call mrmult('ZERO', lmasse, vect(1, ivec), zr(lmx), 1,&
                    .false._1)
        ai = 0.d0
        do ieq = 1, neq
            ai = ai + vect(ieq,ivec)*zr(lmx+ieq-1)
        end do
        do ieq = 1, neq
            vect(ieq,ivecp1) = zr(lmx+ieq-1)*ddlexc(ieq)
        end do
        call resoud(matass, k19bid, solveu, chcine, 1,&
                    k19bid, k19bid, kbid, vect(1, ivecp1), [cbid],&
                    criter, .false._1, 0, iret)
!
        if (ivecm1 .eq. (ivecd-1)) then
            do ieq = 1, neq
                vect(ieq,ivecp1) = vect(ieq,ivecp1) - ai*signes(ivec)* vect(ieq,ivec)
            end do
        else
            bi = 0.d0
            do ieq = 1, neq
                bi = bi + vect(ieq,ivecm1)*zr(lmx+ieq-1)
            end do
            do ieq = 1, neq
                vect(ieq,ivecp1) = vect(ieq,ivecp1) - ai*signes(ivec)* vect(ieq,ivec) - bi*signes&
                                   &(ivecm1)*vect(ieq,ivecm1)
            end do
        endif
!
!         --- K-NORMALISATION DU VECTEUR IVECP1 ---
!
        call mrmult('ZERO', ldynam, vect(1, ivecp1), zr(lkxp1), 1,&
                    .false._1)
        xikxi = 0.d0
        do ieq = 1, neq
            xikxi = xikxi + vect(ieq,ivecp1)*zr(lkxp1+ieq-1)
        end do
        signes(ivecp1) = sign(1.d0,xikxi)
        coef = 1.d0/sqrt(abs(xikxi))
        do ieq = 1, neq
            vect(ieq,ivecp1) = coef*vect(ieq,ivecp1)
            zr(lkxp1+ieq-1) = coef*zr(lkxp1+ieq-1)
        end do
!
!         --- K-REORTHOGONALISATION COMPLETE DU VECTEUR IVECP1
!
        call vplcor(ldynam, neq, nbvect, nborto, prorto,&
                    signes, vect, ivecp1, zr(lkx), zr(lx))
!
!
!         --- CALCUL DE ALPHA ET BETA ---
!
        do jvec = ivec, ivecp1
            call mrmult('ZERO', lmasse, vect(1, ivecp1), zr(lx), 1,&
                        .false._1)
            xjkxi = 0.d0
            do ieq = 1, neq
                xjkxi = xjkxi + vect(ieq,jvec)*zr(lx+ieq-1)
            end do
            if (jvec .eq. ivec) beta(ivecp1) = xjkxi
        end do
        alpha(ivecp1) = xjkxi
        if (abs(beta(ivecp1)) .le. (prsudg*abs(alpha(ivecp1)))) then
            ivecd = ivecp1
            valr (1) = beta(ivecp1)
            valr (2) = alpha(ivecp1)
            call utmess('I', 'ALGELINE4_64', nr=2, valr=valr)
            goto 60
        endif
!
    end do
!
!     -----------------------------------------------------------------
!     --------------- DESTRUCTION DES ZONES DE TRAVAIL ----------------
!     -----------------------------------------------------------------
!
    call jedetr(work(1))
    call jedetr(work(2))
    call jedetr(work(3))
    call jedetr(work(4))
!
    call jedema()
end subroutine
